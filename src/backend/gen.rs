use super::x86::*;
use super::{BinOp, Expr, UnOp};

use std::fmt;
use std::fs::File;
use std::io;
use std::io::prelude::*;

struct Generator {
    functions: Vec<GeneratedCode>,
}

impl Generator {
    fn new() -> Generator {
        Generator { functions: vec![] }
    }

    fn add(&mut self, code: GeneratedCode) {
        self.functions.push(code)
    }
}

impl fmt::Display for Generator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\t.text")?;
        write!(f, "\n\t.extern alloc")?;
        write!(f, "\n\t.extern make_inl")?;
        write!(f, "\n\t.extern make_inr")?;
        write!(f, "\n\t.extern make_pair")?;
        write!(f, "\n\t.extern fst")?;
        write!(f, "\n\t.extern snd")?;
        write!(f, "\n\t.extern make_ref")?;
        write!(f, "\n\t.extern deref")?;
        write!(f, "\n\t.extern assign")?;
        write!(f, "\n\t.extern make_closure")?;
        write!(f, "\n\t.extern make_recursive_closure")?;
        write!(f, "\n\t.extern apply")?;
        write!(f, "\n\t.extern check_case")?;
        write!(f, "\n\t.extern case_inner")?;
        write!(f, "\n\t.globl entry")?;
        write!(f, "\n\t.type entry, @function")?;
        for function in self.functions.iter() {
            write!(f, "{}", function)?;
        }
        write!(f, "\n")
    }
}

impl Code {
    fn emit_constant(self, c: i64) -> Code {
        self.mov(constant(c), rax())
    }

    fn emit_var(self, v: String) -> Code {
        let loc = self.get(v);
        self.mov(loc, rax())
    }

    fn emit_unop(self, op: UnOp, expr: Expr, generator: &mut Generator) -> Code {
        use self::UnOp::*;
        let code = self.emit(expr, generator);
        match op {
            Neg => code.neg(rax()),
            Not => code.not(rax()),
            _ => unimplemented!(),
        }
    }

    fn emit_binop(self, op: BinOp, left: Expr, right: Expr, generator: &mut Generator) -> Code {
        use self::BinOp::*;
        match op {
            And => {
                let label = Label::new();
                self.emit(left, generator)
                    .cmp(constant(1), rax())
                    .jne(label)
                    .emit(right, generator)
                    .label(label)
            }
            Or => {
                let label = Label::new();
                self.emit(left, generator)
                    .cmp(constant(1), rax())
                    .je(label)
                    .emit(right, generator)
                    .label(label)
            }
            _ => {
                let code = self
                    .emit(left, generator)
                    .push(rax())
                    .emit(right, generator)
                    .mov(rax(), rbx())
                    .pop(rax());
                match op {
                    Add => code.add(rbx(), rax()),
                    Sub => code.sub(rbx(), rax()),
                    Mul => code.mul(rbx(), rax()),
                    Div => unimplemented!(),
                    Lt => {
                        let false_label = Label::new();
                        let exit_label = Label::new();
                        code.cmp(rbx(), rax())
                            .jge(false_label)
                            .mov(constant(1), rax())
                            .jmp(exit_label)
                            .label(false_label)
                            .mov(constant(0), rax())
                            .label(exit_label)
                    }
                    Eq => {
                        let false_label = Label::new();
                        let exit_label = Label::new();
                        code.cmp(rbx(), rax())
                            .jne(false_label)
                            .mov(constant(1), rax())
                            .jmp(exit_label)
                            .label(false_label)
                            .mov(constant(0), rax())
                            .label(exit_label)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn emit_if(self, condition: Expr, left: Expr, right: Expr, generator: &mut Generator) -> Code {
        let false_label = Label::new();
        let exit_label = Label::new();
        self.emit(condition, generator)
            .cmp(constant(1), rax())
            .jne(false_label)
            .emit(left, generator)
            .jmp(exit_label)
            .label(false_label)
            .emit(right, generator)
            .label(exit_label)
    }

    fn emit_while(self, condition: Expr, sub: Expr, generator: &mut Generator) -> Code {
        let loop_label = Label::new();
        let exit_label = Label::new();
        self.label(loop_label)
            .emit(condition, generator)
            .cmp(constant(1), rax())
            .jne(exit_label)
            .emit(sub, generator)
            .jmp(loop_label)
            .label(exit_label)
    }

    fn emit_seq(self, seq: Vec<Expr>, generator: &mut Generator) -> Code {
        let mut code = self;
        for sub in seq.into_iter() {
            code = code.emit(sub, generator);
        }
        code
    }

    fn emit_pair(self, left: Expr, right: Expr, generator: &mut Generator) -> Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .pop(rdi())
            .mov(rax(), rsi())
            .xor(rax(), rax())
            .call("make_pair")
    }

    fn emit_runtime_call(self, sub: Expr, name: &'static str, generator: &mut Generator) -> Code {
        self.emit(sub, generator)
            .mov(rax(), rdi())
            .xor(rax(), rax())
            .call(name)
    }

    fn emit_assign(self, left: Expr, right: Expr, generator: &mut Generator) -> Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .pop(rdi())
            .mov(rax(), rsi())
            .xor(rax(), rax())
            .call("assign")
            .xor(rax(), rax())
    }

    fn emit_lambda(mut self, v: String, expr: Expr, generator: &mut Generator) -> Code {
        let label = Label::new();
        let mut lambda = Code::new(label);
        let v = lambda.allocate(v);
        // TODO constrain to free variables
        for (i, (envv, _)) in self.get_env().iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda = lambda
                .mov(deref(rsi(), 8 * i as i64), rax())
                .mov(rax(), loc);
        }
        lambda = lambda.mov(rdi(), v).emit(expr, generator);
        generator.add(lambda.ret());
        for (i, (_, loc)) in self.get_env().clone().iter().enumerate().rev() {
            match i {
                0 => self = self.mov(*loc, rdx()),
                1 => self = self.mov(*loc, rcx()),
                2 => self = self.mov(*loc, r8()),
                3 => self = self.mov(*loc, r9()),
                _ => self = self.push(*loc),
            }
        }
        let env_len = self.get_env().len();
        self = self
            .lea(relative(rip(), label), rdi())
            .mov(constant(env_len as i64), rsi())
            .xor(rax(), rax())
            .call("make_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_recursive_lambda(
        mut self,
        f: String,
        v: String,
        expr: Expr,
        generator: &mut Generator,
    ) -> Code {
        let label = Label::new();
        let mut lambda = Code::new(label);
        let v = lambda.allocate(v);
        let f = lambda.allocate(f);
        // TODO constrain to free variables
        lambda = lambda.mov(deref(rsi(), 0), rax()).mov(rax(), f);
        for (i, (envv, _)) in self.get_env().iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda = lambda
                .mov(deref(rsi(), 8 * (i + 1) as i64), rax())
                .mov(rax(), loc);
        }
        lambda = lambda.mov(rdi(), v).emit(expr, generator);
        generator.add(lambda.ret());
        self = self.lea(relative(rip(), label), rdi());
        for (i, (_, loc)) in self.get_env().clone().iter().enumerate().rev() {
            match i {
                0 => self = self.mov(*loc, rdx()),
                1 => self = self.mov(*loc, rcx()),
                2 => self = self.mov(*loc, r8()),
                3 => self = self.mov(*loc, r9()),
                _ => self = self.push(*loc),
            }
        }
        let env_len = self.get_env().len();
        self = self
            .lea(relative(rip(), label), rdi())
            .mov(constant(env_len as i64), rsi())
            .xor(rax(), rax())
            .call("make_recursive_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_app(self, left: Expr, right: Expr, generator: &mut Generator) -> Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .pop(rdi())
            .mov(rax(), rsi())
            .xor(rax(), rax())
            .call("apply")
    }

    fn emit_case(
        self,
        sub: Expr,
        left: (String, Box<Expr>),
        right: (String, Box<Expr>),
        generator: &mut Generator,
    ) -> Code {
        let inr = Label::new();
        let skip = Label::new();
        self.emit(sub, generator)
            .push(rax())
            .mov(rax(), rdi())
            .xor(rax(), rax())
            .call("case_inner")
            .pop(rdi())
            .push(rax())
            .xor(rax(), rax())
            .call("check_case")
            .cmp(constant(0), rax())
            .jne(inr)
            .emit_lambda(left.0, *left.1, generator)
            .jmp(skip)
            .label(inr)
            .emit_lambda(right.0, *right.1, generator)
            .label(skip)
            .mov(rax(), rdi())
            .pop(rsi())
            .xor(rax(), rax())
            .call("apply")
    }

    fn emit_let_fun(
        self,
        f: String,
        sub: (String, Box<Expr>),
        body: Expr,
        generator: &mut Generator,
    ) -> Code {
        self.emit_lambda(f.clone(), body, generator)
            .push(rax())
            .emit_recursive_lambda(f, sub.0, *sub.1, generator)
            .pop(rdi())
            .mov(rax(), rsi())
            .xor(rax(), rax())
            .call("apply")
    }

    fn emit(self, expr: Expr, generator: &mut Generator) -> Code {
        use Expr::*;
        match expr {
            Constant(c) => self.emit_constant(c),
            Var(v) => self.emit_var(v),
            UnOp(op, sub) => self.emit_unop(op, *sub, generator),
            BinOp(op, left, right) => self.emit_binop(op, *left, *right, generator),
            If(condition, left, right) => self.emit_if(*condition, *left, *right, generator),
            While(condition, sub) => self.emit_while(*condition, *sub, generator),
            Seq(seq) => self.emit_seq(seq, generator),
            Pair(left, right) => self.emit_pair(*left, *right, generator),
            Fst(sub) => self.emit_runtime_call(*sub, "fst", generator),
            Snd(sub) => self.emit_runtime_call(*sub, "snd", generator),
            Inl(sub) => self.emit_runtime_call(*sub, "make_inl", generator),
            Inr(sub) => self.emit_runtime_call(*sub, "make_inr", generator),
            Ref(sub) => self.emit_runtime_call(*sub, "make_ref", generator),
            Deref(sub) => self.emit_runtime_call(*sub, "deref", generator),
            Assign(left, right) => self.emit_assign(*left, *right, generator),
            App(left, right) => self.emit_app(*left, *right, generator),
            Lambda(v, sub) => self.emit_lambda(v, *sub, generator),
            Case(sub, left, right) => self.emit_case(*sub, left, right, generator),
            LetFun(f, sub, body) => self.emit_let_fun(f, sub, *body, generator),
        }
    }
}

pub fn generate(file: &mut File, expr: Expr) -> io::Result<()> {
    let mut generator = Generator::new();
    let entry = Code::new("entry".into());
    let entry = entry.emit(expr, &mut generator);
    generator.add(entry.ret());
    write!(file, "{}", generator)
}

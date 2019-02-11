use super::frontend::ast::{BinOp, Expr, Free, UnOp};

mod x86;

use x86::*;

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
        writeln!(f, "\t.text")?;
        writeln!(f, "\t.extern alloc")?;
        writeln!(f, "\t.extern make_closure")?;
        writeln!(f, "\t.extern make_recursive_closure")?;
        writeln!(f, "\t.extern what")?;
        writeln!(f, "\t.globl entry")?;
        writeln!(f, "\t.type entry, @function")?;
        for function in self.functions.iter() {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

impl Code {
    fn emit_var(&mut self, v: String) -> &mut Code {
        let loc = self.get(v);
        self.mov(loc, rax())
    }

    fn emit_unop(&mut self, op: UnOp, expr: Expr, generator: &mut Generator) -> &mut Code {
        use self::UnOp::*;
        self.emit(expr, generator);
        match op {
            Neg => self.neg(rax()),
            Not => self.not(rax()),
        }
    }

    fn emit_binop(
        &mut self,
        op: BinOp,
        left: Expr,
        right: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
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
                self.emit(left, generator)
                    .push(rax())
                    .emit(right, generator)
                    .mov(rax(), rbx())
                    .pop(rax());
                match op {
                    Add => self.add(rbx(), rax()),
                    Sub => self.sub(rbx(), rax()),
                    Mul => self.mul(rbx(), rax()),
                    Div => self.cqto().div(rbx()),
                    Lt => {
                        let false_label = Label::new();
                        let exit_label = Label::new();
                        self.cmp(rbx(), rax())
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
                        self.cmp(rbx(), rax())
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

    fn emit_if(
        &mut self,
        condition: Expr,
        left: Expr,
        right: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
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

    fn emit_while(&mut self, condition: Expr, sub: Expr, generator: &mut Generator) -> &mut Code {
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

    fn emit_seq(&mut self, seq: Vec<Expr>, generator: &mut Generator) -> &mut Code {
        for sub in seq.into_iter() {
            self.emit(sub, generator);
        }
        self
    }

    fn emit_ref(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(sub, generator)
            .push(rax())
            .xor(rax(), rax())
            .call_rt("alloc")
            .pop(rbx())
            .mov(rbx(), deref(rax(), 0))
    }

    fn emit_pair(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .push(rax())
            .call_rt("alloc")
            .pop(deref(rax(), 8))
            .pop(deref(rax(), 0))
    }

    fn emit_assign(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .pop(rbx())
            .mov(rax(), deref(rbx(), 0))
            .mov(constant(0), rax())
    }

    fn emit_app(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(left, generator)
            .push(rax())
            .emit(right, generator)
            .mov(rax(), rdi())
            .pop(rax())
            .mov(deref(rax(), 8), rsi())
            .mov(deref(rax(), 0), rax())
            .call(rax())
    }

    fn emit_lambda(&mut self, lambda: (String, Box<Expr>), generator: &mut Generator) -> &mut Code {
        let fv = lambda
            .fv()
            .into_iter()
            .map(|x| x.clone())
            .collect::<Vec<_>>();
        let (v, expr) = (lambda.0, *lambda.1);
        let label = Label::new();
        let mut lambda = Code::new(label);
        let v = lambda.allocate(v);
        for (i, envv) in fv.iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda
                .mov(deref(rsi(), 8 * i as i64), rax())
                .mov(rax(), loc);
        }
        lambda.mov(rdi(), v).emit(expr, generator);
        generator.add(lambda.ret());
        for (i, envv) in fv.iter().enumerate().rev() {
            let loc = self.get(envv.clone());
            match i {
                0 => self.mov(loc, rdx()),
                1 => self.mov(loc, rcx()),
                2 => self.mov(loc, r8()),
                3 => self.mov(loc, r9()),
                _ => self.push(loc),
            };
        }
        let env_len = lambda.get_env().len() - 1;
        self.lea(relative(rip(), label), rdi())
            .mov(constant(env_len as i64), rsi())
            .xor(rax(), rax())
            .call_rt("make_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_recursive_lambda(
        &mut self,
        f: String,
        lambda: (String, Box<Expr>),
        generator: &mut Generator,
    ) -> &mut Code {
        let fv = lambda
            .fv()
            .into_iter()
            .map(|x| x.clone())
            .filter(|x| x != &f)
            .collect::<Vec<_>>();
        let (v, expr) = (lambda.0, *lambda.1);
        let label = Label::new();
        let mut lambda = Code::new(label);
        let v = lambda.allocate(v);
        let f = lambda.allocate(f);
        lambda.mov(deref(rsi(), 0), rax()).mov(rax(), f);
        for (i, envv) in fv.iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda
                .mov(deref(rsi(), 8 * (i + 1) as i64), rax())
                .mov(rax(), loc);
        }
        lambda.mov(rdi(), v).emit(expr, generator);
        generator.add(lambda.ret());
        for (i, envv) in fv.iter().enumerate().rev() {
            let loc = self.get(envv.clone());
            match i {
                0 => self.mov(loc, rdx()),
                1 => self.mov(loc, rcx()),
                2 => self.mov(loc, r8()),
                3 => self.mov(loc, r9()),
                _ => self.push(loc),
            };
        }
        let env_len = lambda.get_env().len() - 2;
        self.lea(relative(rip(), label), rdi())
            .mov(constant(env_len as i64), rsi())
            .xor(rax(), rax())
            .call_rt("make_recursive_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_inl(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(sub, generator)
            .push(rax())
            .xor(rax(), rax())
            .call_rt("alloc")
            .pop(deref(rax(), 8))
            .mov(constant(0), deref(rax(), 0))
    }

    fn emit_inr(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.emit(sub, generator)
            .push(rax())
            .xor(rax(), rax())
            .call_rt("alloc")
            .pop(deref(rax(), 8))
            .mov(constant(1), deref(rax(), 0))
    }

    fn emit_case(
        &mut self,
        sub: Expr,
        left: (String, Box<Expr>),
        right: (String, Box<Expr>),
        generator: &mut Generator,
    ) -> &mut Code {
        let inr = Label::new();
        let skip = Label::new();
        self.emit(sub, generator)
            .mov(deref(rax(), 0), rbx())
            .cmp(constant(0), rbx())
            .jne(inr)
            .mov(deref(rax(), 8), rax());
        let v_left = self.allocate(left.0.clone());
        self.mov(rax(), v_left).emit(*left.1, generator);
        self.deallocate(left.0);
        self.jmp(skip).label(inr).mov(deref(rax(), 8), rax());
        let v_right = self.allocate(right.0.clone());
        self.mov(rax(), v_right).emit(*right.1, generator);
        self.deallocate(right.0);
        self.label(skip)
    }

    fn emit_let(
        &mut self,
        v: String,
        sub: Expr,
        body: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        let loc = self.allocate(v.clone());
        self.emit(sub, generator)
            .mov(rax(), loc)
            .emit(body, generator);
        self.deallocate(v);
        self
    }

    fn emit_let_fun(
        &mut self,
        f: String,
        lambda: (String, Box<Expr>),
        body: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        self.emit_recursive_lambda(f.clone(), lambda, generator);
        let loc = self.allocate(f.clone());
        self.mov(rax(), loc).emit(body, generator);
        self.deallocate(f);
        self
    }

    fn emit(&mut self, expr: Expr, generator: &mut Generator) -> &mut Code {
        use Expr::*;
        match expr {
            Int(i) => self.mov(constant(i), rax()),
            Bool(b) => self.mov(constant(if b { 1 } else { 0 }), rax()),
            Unit => self.mov(constant(0), rax()),
            What => self.xor(rax(), rax()).call_rt("what"),
            Var(v) => self.emit_var(v),
            UnOp(op, sub) => self.emit_unop(op, *sub, generator),
            BinOp(op, left, right) => self.emit_binop(op, *left, *right, generator),
            If(condition, left, right) => self.emit_if(*condition, *left, *right, generator),
            While(condition, sub) => self.emit_while(*condition, *sub, generator),
            Seq(seq) => self.emit_seq(seq, generator),
            Ref(sub) => self.emit_ref(*sub, generator),
            Deref(sub) => self.emit(*sub, generator).mov(deref(rax(), 0), rax()),
            Fst(sub) => self.emit(*sub, generator).mov(deref(rax(), 0), rax()),
            Snd(sub) => self.emit(*sub, generator).mov(deref(rax(), 8), rax()),
            Pair(left, right) => self.emit_pair(*left, *right, generator),
            Assign(left, right) => self.emit_assign(*left, *right, generator),
            App(left, right) => self.emit_app(*left, *right, generator),
            Lambda(lambda) => self.emit_lambda(lambda, generator),
            Inl(sub) => self.emit_inl(*sub, generator),
            Inr(sub) => self.emit_inr(*sub, generator),
            Case(sub, left, right) => self.emit_case(*sub, left, right, generator),
            Let(v, sub, body) => self.emit_let(v, *sub, *body, generator),
            LetFun(f, lambda, body) => self.emit_let_fun(f, lambda, *body, generator),
        }
    }
}

pub fn generate(file: &mut File, expr: Expr) -> io::Result<()> {
    let mut generator = Generator::new();
    let mut entry = Code::new("entry".into());
    let entry = entry.emit(expr, &mut generator);
    generator.add(entry.ret());
    write!(file, "{}", generator)
}

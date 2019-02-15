use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone)]
pub enum Label {
    Generated(usize),
    Given(&'static str),
}

impl Label {
    pub fn new() -> Label {
        Label::Generated(LABEL_COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

impl From<&'static str> for Label {
    fn from(string: &'static str) -> Label {
        Label::Given(string)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Label::*;
        match *self {
            Generated(l) => write!(f, ".L{}", l),
            Given(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    Rip,
}

pub fn rax() -> Location {
    Location::Register(Register::Rax)
}

pub fn rbx() -> Location {
    Location::Register(Register::Rbx)
}

pub fn rcx() -> Location {
    Location::Register(Register::Rcx)
}

pub fn rdx() -> Location {
    Location::Register(Register::Rdx)
}

pub fn rsp() -> Location {
    Location::Register(Register::Rsp)
}

pub fn rbp() -> Location {
    Location::Register(Register::Rbp)
}

pub fn rsi() -> Location {
    Location::Register(Register::Rsi)
}

pub fn rdi() -> Location {
    Location::Register(Register::Rdi)
}

pub fn r8() -> Location {
    Location::Register(Register::R8)
}

pub fn r9() -> Location {
    Location::Register(Register::R9)
}

pub fn rip() -> Location {
    Location::Register(Register::Rip)
}

pub fn constant(c: i64) -> Location {
    Location::Constant(c)
}

pub fn deref(loc: Location, offset: i64) -> Location {
    match loc {
        Location::Register(reg) => Location::Memory(reg, offset),
        _ => panic!("Attempted to use constant as memory location"),
    }
}

pub fn relative(loc: Location, label: Label) -> Location {
    match loc {
        Location::Register(reg) => Location::Relative(reg, label),
        _ => panic!("Attempted to use constant as memory location"),
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Register::*;
        write!(f, "%")?;
        match *self {
            Rax => write!(f, "rax"),
            Rbx => write!(f, "rbx"),
            Rcx => write!(f, "rcx"),
            Rdx => write!(f, "rdx"),
            Rsp => write!(f, "rsp"),
            Rbp => write!(f, "rbp"),
            Rsi => write!(f, "rsi"),
            Rdi => write!(f, "rdi"),
            R8 => write!(f, "r8"),
            R9 => write!(f, "r9"),
            Rip => write!(f, "rip"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Location {
    Constant(i64),
    Register(Register),
    Memory(Register, i64),
    Relative(Register, Label),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Location::*;
        match *self {
            Constant(c) => write!(f, "${}", c),
            Register(r) => write!(f, "{}", r),
            Memory(r, o) => {
                if o != 0 {
                    write!(f, "{}({})", o, r)
                } else {
                    write!(f, "({})", r)
                }
            }
            Relative(r, l) => write!(f, "{}({})", l, r),
        }
    }
}

enum Instruction {
    Label(Label),
    Push(Location),
    Pop(Location),
    Not(Location),
    Neg(Location),
    Add(Location, Location),
    Sub(Location, Location),
    Mul(Location, Location),
    Div(Location),
    Cqto,
    Xor(Location, Location),
    Cmp(Location, Location),
    Jmp(Label),
    Je(Label),
    Jge(Label),
    Jne(Label),
    Mov(Location, Location),
    Lea(Location, Location),
    Call(Location),
    CallRuntime(&'static str),
    Comment(String),
    Ret,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Instruction::*;
        match *self {
            Label(ref label) => writeln!(f, "{}:", label),
            Push(loc) => writeln!(f, "\tpushq {}", loc),
            Pop(loc) => writeln!(f, "\tpopq {}", loc),
            Not(loc) => writeln!(f, "\tnotq {}", loc),
            Neg(loc) => writeln!(f, "\tnegq {}", loc),
            Add(source, target) => writeln!(f, "\taddq {},{}", source, target),
            Sub(source, target) => writeln!(f, "\tsubq {},{}", source, target),
            Mul(source, target) => writeln!(f, "\timulq {},{}", source, target),
            Div(source) => writeln!(f, "\tidivq {}", source),
            Cqto => writeln!(f, "\tcqto"),
            Xor(source, target) => writeln!(f, "\txorq {},{}", source, target),
            Cmp(source, target) => writeln!(f, "\tcmpq {},{}", source, target),
            Jmp(ref label) => writeln!(f, "\tjmp {}", label),
            Je(ref label) => writeln!(f, "\tje {}", label),
            Jge(ref label) => writeln!(f, "\tjge {}", label),
            Jne(ref label) => writeln!(f, "\tjne {}", label),
            Mov(source, target) => writeln!(f, "\tmovq {},{}", source, target),
            Lea(source, target) => writeln!(f, "\tleaq {},{}", source, target),
            Call(loc) => writeln!(f, "\tcall *{}", loc),
            CallRuntime(name) => writeln!(f, "\tcall {}", name),
            Comment(ref comment) => writeln!(f, "\t# {}", comment),
            Ret => writeln!(f, "\tret"),
        }
    }
}

pub struct GeneratedCode(String);

impl fmt::Display for GeneratedCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Code {
    comments: bool,
    label: Label,
    env: Vec<(String, Location, bool)>,
    allocated: usize,
    asm: Vec<Instruction>,
}

impl Code {
    pub fn new(label: Label, comments: bool) -> Code {
        Code {
            comments: comments,
            label: label,
            env: vec![],
            allocated: 0,
            asm: vec![],
        }
    }

    pub fn label(&mut self, label: Label) -> &mut Code {
        self.asm.push(Instruction::Label(label));
        self
    }

    pub fn push(&mut self, loc: Location) -> &mut Code {
        self.asm.push(Instruction::Push(loc));
        self
    }

    pub fn pop(&mut self, loc: Location) -> &mut Code {
        self.asm.push(Instruction::Pop(loc));
        self
    }

    pub fn mov(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Mov(source, target));
        self
    }

    pub fn lea(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Lea(source, target));
        self
    }

    pub fn not(&mut self, loc: Location) -> &mut Code {
        self.asm.push(Instruction::Not(loc));
        self
    }

    pub fn neg(&mut self, loc: Location) -> &mut Code {
        self.asm.push(Instruction::Neg(loc));
        self
    }

    pub fn add(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Add(source, target));
        self
    }

    pub fn sub(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Sub(source, target));
        self
    }

    pub fn mul(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Mul(source, target));
        self
    }

    pub fn div(&mut self, source: Location) -> &mut Code {
        self.asm.push(Instruction::Div(source));
        self
    }

    pub fn cqto(&mut self) -> &mut Code {
        self.asm.push(Instruction::Cqto);
        self
    }

    pub fn xor(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Xor(source, target));
        self
    }

    pub fn cmp(&mut self, source: Location, target: Location) -> &mut Code {
        self.asm.push(Instruction::Cmp(source, target));
        self
    }

    pub fn jmp(&mut self, label: Label) -> &mut Code {
        self.asm.push(Instruction::Jmp(label));
        self
    }

    pub fn je(&mut self, label: Label) -> &mut Code {
        self.asm.push(Instruction::Je(label));
        self
    }

    pub fn jge(&mut self, label: Label) -> &mut Code {
        self.asm.push(Instruction::Jge(label));
        self
    }

    pub fn jne(&mut self, label: Label) -> &mut Code {
        self.asm.push(Instruction::Jne(label));
        self
    }

    pub fn call(&mut self, loc: Location) -> &mut Code {
        self.asm.push(Instruction::Call(loc));
        self
    }

    pub fn call_rt(&mut self, name: &'static str) -> &mut Code {
        self.asm.push(Instruction::CallRuntime(name));
        self
    }

    pub fn comment(&mut self, comment: String) -> &mut Code {
        self.asm.push(Instruction::Comment(comment));
        self
    }

    pub fn ret(&mut self) -> GeneratedCode {
        self.comment(format!(
            "update stack pointer ('{}') to base pointer ('{}')",
            rsp(),
            rbp()
        ))
        .mov(rbp(), rsp())
        .comment(format!("drop back into previous stack frame"))
        .pop(rbp());
        if self.allocated > 0 {
            self.asm
                .insert(0, Instruction::Sub(constant(self.allocated as i64), rsp()));
            self.asm.insert(
                0,
                Instruction::Comment(format!(
                    "we need {} bytes for local variables so decrement stack pointer ('{}') by {}",
                    self.allocated,
                    rsp(),
                    self.allocated
                )),
            );
        }
        self.asm.insert(0, Instruction::Mov(rsp(), rbp()));
        self.asm.insert(
            0,
            Instruction::Comment(format!(
                "update base pointer ('{}') to stack pointer ('{}')",
                rbp(),
                rsp()
            )),
        );
        self.asm.insert(0, Instruction::Push(rbp()));
        self.asm.insert(
            0,
            Instruction::Comment(format!("save the base pointer ('{}')", rbp())),
        );
        self.asm.insert(0, Instruction::Label(self.label));
        self.asm.push(Instruction::Ret);
        GeneratedCode(format!("{}", self))
    }

    pub fn allocate(&mut self, v: String) -> Location {
        for (envv, loc, enabled) in self.env.iter_mut().rev() {
            if envv == &v && !*enabled {
                *enabled = true;
                return *loc;
            }
        }
        self.allocated += 8;
        let loc = deref(rbp(), -(self.allocated as i64));
        self.env.push((v, loc, true));
        loc
    }

    pub fn deallocate(&mut self, v: String) {
        for (envv, _, enabled) in self.env.iter_mut().rev() {
            if envv == &v && *enabled {
                *enabled = false;
                break;
            }
        }
    }

    pub fn get_env(&self) -> &Vec<(String, Location, bool)> {
        &self.env
    }

    pub fn get(&self, v: &str) -> Location {
        for (envv, loc, enabled) in self.env.iter().rev() {
            if v == envv && *enabled {
                return *loc;
            }
        }
        panic!("Attempted to get unbound variable")
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.asm.iter() {
            match line {
                c @ Instruction::Comment(_) => {
                    if self.comments {
                        write!(f, "{}", c)?
                    }
                }
                _ => write!(f, "{}", line)?,
            };
        }
        Ok(())
    }
}

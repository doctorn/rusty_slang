use super::types::TypeExpr;
use super::Locatable;

use std::fmt;

pub enum BinOp {
    Add,
    Mul,
    Div,
    Sub,
    Lt,
    And,
    Or,
    Eq,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOp::*;
        match *self {
            Add => write!(f, "+"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Sub => write!(f, "-"),
            Lt => write!(f, "<"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Eq => write!(f, "="),
        }
    }
}

pub enum UnOp {
    Neg,
    Not,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnOp::*;
        match *self {
            Neg => write!(f, "-"),
            Not => write!(f, "~"),
        }
    }
}

pub type Var = String;

pub type Lambda = (Var, TypeExpr, SubExpr);

pub type SubExpr = Box<Locatable<Expr>>;

impl fmt::Display for SubExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        let sub = (*self).borrow_raw();
        match *sub {
            Unit | What | Var(_) | Int(_) | Lambda(_) => write!(f, "{}", sub),
            _ => write!(f, "({})", sub),
        }
    }
}

pub enum Expr {
    Unit,
    What,
    Var(Var),
    Int(i64),
    Bool(bool),
    UnOp(UnOp, SubExpr),
    BinOp(BinOp, SubExpr, SubExpr),
    If(SubExpr, SubExpr, SubExpr),
    Pair(SubExpr, SubExpr),
    Fst(SubExpr),
    Snd(SubExpr),
    Inl(SubExpr, TypeExpr),
    Inr(SubExpr, TypeExpr),
    Case(SubExpr, Lambda, Lambda),
    Lambda(Lambda),
    While(SubExpr, SubExpr),
    Seq(Vec<SubExpr>),
    Ref(SubExpr),
    Deref(SubExpr),
    Assign(SubExpr, SubExpr),
    App(SubExpr, SubExpr),
    Let(Var, TypeExpr, SubExpr, SubExpr),
    LetFun(Var, Lambda, TypeExpr, SubExpr),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Unit => write!(f, "()"),
            What => write!(f, "?"),
            Var(ref v) => write!(f, "{}", v),
            Int(ref i) => write!(f, "{}", i),
            Bool(ref b) => write!(f, "{}", b),
            UnOp(ref op, ref sub) => write!(f, "{}{}", op, sub),
            BinOp(ref op, ref left, ref right) => write!(f, "{} {} {}", left, op, right),
            If(ref condition, ref left, ref right) => {
                write!(f, "if {} then {} else {} end", condition, left, right)
            }
            Pair(ref left, ref right) => write!(f, "({}, {})", left, right),
            Fst(ref sub) => write!(f, "fst {}", sub),
            Snd(ref sub) => write!(f, "snd {}", sub),
            Inl(ref sub, ref type_expr) => write!(f, "inl {} {}", type_expr, sub),
            Inr(ref sub, ref type_expr) => write!(f, "inr {} {}", type_expr, sub),
            Case(
                ref sub,
                (ref v_left, ref type_expr_left, ref sub_left),
                (ref v_right, ref type_expr_right, ref sub_right),
            ) => write!(
                f,
                "case {} of inl({}: {}) -> {} | inr({}: {}) -> {}",
                sub, v_left, type_expr_left, sub_left, v_right, type_expr_right, sub_right
            ),
            Lambda((ref v, ref type_expr, ref sub)) => {
                write!(f, "fun {}: {} -> {} end", v, type_expr, sub)
            }
            While(ref condition, ref sub) => write!(f, "while {} do {} end", condition, sub),
            Seq(ref seq) => {
                write!(f, "begin ")?;
                let mut first = true;
                for sub in seq.iter() {
                    if first {
                        write!(f, "{}", sub)?;
                        first = false;
                    } else {
                        write!(f, "; {}", sub)?;
                    }
                }
                write!(f, " end")
            }
            Ref(ref sub) => write!(f, "ref {}", sub),
            Deref(ref sub) => write!(f, "!{}", sub),
            Assign(ref left, ref right) => write!(f, "{} := {}", left, right),
            App(ref left, ref right) => write!(f, "{} {}", left, right),
            Let(ref v, ref type_expr, ref sub, ref body) => {
                write!(f, "let {}: {} = {} in {} end", v, type_expr, sub, body)
            }
            LetFun(
                ref v,
                (ref v_lambda, ref type_expr_lambda, ref sub_lambda),
                ref type_expr,
                ref body,
            ) => write!(
                f,
                "let {} ({}: {}): {} = {} in {} end",
                v, v_lambda, type_expr_lambda, type_expr, sub_lambda, body
            ),
        }
    }
}

use super::past;
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

type Var = String;

pub type Lambda = (Var, Box<Expr>);

pub enum Expr {
    Unit,
    What,
    Var(Var),
    Int(i64),
    Bool(bool),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),
    Inl(Box<Expr>),
    Inr(Box<Expr>),
    Case(Box<Expr>, Lambda, Lambda),
    While(Box<Expr>, Box<Expr>),
    Seq(Vec<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Lambda(Lambda),
    App(Box<Expr>, Box<Expr>),
    Let(Var, Box<Expr>, Box<Expr>),
    LetFun(Var, Lambda, Box<Expr>),
}

impl<'a> From<past::SubExpr> for Box<Expr> {
    fn from(sub: past::SubExpr) -> Box<Expr> {
        Box::new(Expr::from(sub.into_raw()))
    }
}

impl<'a> From<past::Expr> for Expr {
    fn from(past: past::Expr) -> Expr {
        use self::Expr::*;
        match past {
            past::Expr::Unit => Unit,
            past::Expr::What => What,
            past::Expr::Var(v) => Var(v),
            past::Expr::Bool(b) => Bool(b),
            past::Expr::Int(i) => Int(i),
            past::Expr::UnOp(op, sub) => UnOp(op.into(), sub.into()),
            past::Expr::BinOp(op, left, right) => BinOp(op.into(), left.into(), right.into()),
            past::Expr::If(condition, left, right) => {
                If(condition.into(), left.into(), right.into())
            }
            past::Expr::Pair(left, right) => Pair(left.into(), right.into()),
            past::Expr::Fst(sub) => Fst(sub.into()),
            past::Expr::Snd(sub) => Snd(sub.into()),
            past::Expr::Inl(sub, _) => Inl(sub.into()),
            past::Expr::Inr(sub, _) => Inr(sub.into()),
            past::Expr::Case(sub, (v_left, _, sub_left), (v_right, _, sub_right)) => Expr::Case(
                sub.into(),
                (v_left, sub_left.into()),
                (v_right, sub_right.into()),
            ),
            past::Expr::Lambda((v, _, sub)) => Lambda((v, sub.into())),
            past::Expr::While(condition, sub) => While(condition.into(), sub.into()),
            past::Expr::Seq(seq) => Seq(seq
                .into_iter()
                .map(|x| x.into_raw().into())
                .collect::<Vec<Expr>>()),
            past::Expr::Ref(sub) => Ref(sub.into()),
            past::Expr::Deref(sub) => Deref(sub.into()),
            past::Expr::Assign(left, right) => Assign(left.into(), right.into()),
            past::Expr::App(left, right) => App(left.into(), right.into()),
            past::Expr::Let(v, _, sub, body) => Let(v, sub.into(), body.into()),
            past::Expr::LetFun(f, (v, _, sub), _, body) => LetFun(f, (v, sub.into()), body.into()),
        }
    }
}

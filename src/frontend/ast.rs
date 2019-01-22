use super::past;

pub enum BinOp {
    Add,
    Mul,
    Div,
    Sub,
    Lt,
    And,
    Or,
    Eqb,
    Eqi,
}

impl From<past::BinOp> for BinOp {
    fn from(op: past::BinOp) -> BinOp {
        match op {
            past::BinOp::Add => BinOp::Add,
            past::BinOp::Mul => BinOp::Mul,
            past::BinOp::Div => BinOp::Div,
            past::BinOp::Sub => BinOp::Sub,
            past::BinOp::Lt => BinOp::Lt,
            past::BinOp::And => BinOp::And,
            past::BinOp::Or => BinOp::Or,
            past::BinOp::Eqb => BinOp::Eqb,
            past::BinOp::Eqi => BinOp::Eqi,
            _ => unreachable!(),
        }
    }
}

pub enum UnOp {
    Neg,
    Not,
    Read,
}

impl From<past::UnOp> for UnOp {
    fn from(op: past::UnOp) -> UnOp {
        match op {
            past::UnOp::Neg => UnOp::Neg,
            past::UnOp::Not => UnOp::Not,
        }
    }
}

type Var = String;

type Lambda = (Var, Box<Expr>);

pub enum Expr {
    Unit,
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
    LetFun(Var, Lambda, Box<Expr>),
    LetRecFun(Var, Lambda, Box<Expr>),
}

impl From<past::SubExpr> for Box<Expr> {
    fn from(sub: past::SubExpr) -> Box<Expr> {
        Box::new(Expr::from(sub.into_raw()))
    }
}

impl From<past::Expr> for Expr {
    fn from(past: past::Expr) -> Expr {
        use self::Expr::*;
        match past {
            past::Expr::Unit => Unit,
            past::Expr::What => UnOp(self::UnOp::Read, Box::new(Unit)),
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
            past::Expr::Let(v, _, sub, body) => App(Box::new(Lambda((v, body.into()))), sub.into()),
            past::Expr::LetFun(f, (v, _, sub), _, body) => LetFun(f, (v, sub.into()), body.into()),
            past::Expr::LetRecFun(f, (v, _, sub), _, body) => {
                LetRecFun(f, (v, sub.into()), body.into())
            }
        }
    }
}

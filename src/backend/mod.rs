use super::frontend::ast;
pub use ast::{BinOp, UnOp};

pub mod gen;
pub mod x86;

pub enum Expr {
    Constant(i64),
    Var(String),
    UnOp(ast::UnOp, Box<Expr>),
    BinOp(ast::BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Seq(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),
    Inl(Box<Expr>),
    Inr(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Case(Box<Expr>, (String, Box<Expr>), (String, Box<Expr>)),
    LetFun(String, (String, Box<Expr>), Box<Expr>),
}

impl From<ast::Expr> for Expr {
    fn from(expr: ast::Expr) -> Expr {
        match expr {
            ast::Expr::Int(i) => Expr::Constant(i),
            ast::Expr::Bool(true) => Expr::Constant(1),
            ast::Expr::Bool(false) => Expr::Constant(0),
            ast::Expr::Unit => Expr::Constant(0),
            ast::Expr::Var(v) => Expr::Var(v),
            ast::Expr::UnOp(op, sub) => Expr::UnOp(op, Box::new((*sub).into())),
            ast::Expr::BinOp(op, left, right) => {
                Expr::BinOp(op, Box::new((*left).into()), Box::new((*right).into()))
            }
            ast::Expr::If(condition, left, right) => Expr::If(
                Box::new((*condition).into()),
                Box::new((*left).into()),
                Box::new((*right).into()),
            ),
            ast::Expr::While(condition, sub) => {
                Expr::While(Box::new((*condition).into()), Box::new((*sub).into()))
            }
            ast::Expr::Seq(seq) => Expr::Seq(seq.into_iter().map(|x| x.into()).collect()),
            ast::Expr::Pair(left, right) => {
                Expr::Pair(Box::new((*left).into()), Box::new((*right).into()))
            }
            ast::Expr::Fst(sub) => Expr::Fst(Box::new((*sub).into())),
            ast::Expr::Snd(sub) => Expr::Snd(Box::new((*sub).into())),
            ast::Expr::Inl(sub) => Expr::Inl(Box::new((*sub).into())),
            ast::Expr::Inr(sub) => Expr::Inr(Box::new((*sub).into())),
            ast::Expr::Ref(sub) => Expr::Ref(Box::new((*sub).into())),
            ast::Expr::Deref(sub) => Expr::Deref(Box::new((*sub).into())),
            ast::Expr::Assign(left, right) => {
                Expr::Assign(Box::new((*left).into()), Box::new((*right).into()))
            }
            ast::Expr::Lambda((v, sub)) => Expr::Lambda(v, Box::new((*sub).into())),
            ast::Expr::App(left, right) => {
                Expr::App(Box::new((*left).into()), Box::new((*right).into()))
            }
            ast::Expr::Case(sub, (v_left, sub_left), (v_right, sub_right)) => Expr::Case(
                Box::new((*sub).into()),
                (v_left, Box::new((*sub_left).into())),
                (v_right, Box::new((*sub_right).into())),
            ),
            ast::Expr::LetFun(f, (v, sub), body) => {
                Expr::LetFun(f, (v, Box::new((*sub).into())), Box::new((*body).into()))
            }
        }
    }
}

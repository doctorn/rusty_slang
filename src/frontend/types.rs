use std::fmt;

use super::ast::{BinOp, UnOp};
use super::past::{Expr, Var};
use super::{log, Locatable};

#[derive(Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Unit,
    Bool,
    Int,
    Ref(Box<TypeExpr>),
    Arrow(Box<TypeExpr>, Box<TypeExpr>),
    Product(Box<TypeExpr>, Box<TypeExpr>),
    Union(Box<TypeExpr>, Box<TypeExpr>),
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TypeExpr::*;
        match *self {
            Unit => write!(f, "unit"),
            Bool => write!(f, "bool"),
            Int => write!(f, "int"),
            Ref(ref sub) => write!(f, "{} ref", sub),
            Arrow(ref left, ref right) => match **left {
                Arrow(_, _) => write!(f, "({}) -> {}", left, right),
                _ => write!(f, "{} -> {}", left, right),
            },
            Product(ref left, ref right) => write!(f, "{} * {}", left, right),
            Union(ref left, ref right) => write!(f, "{} + {}", left, right),
        }
    }
}

fn find(env: &Vec<(Var, TypeExpr)>, v: &Var) -> Result<TypeExpr, String> {
    for (env_v, type_expr) in env.iter().rev() {
        if env_v.eq(v) {
            return Ok(type_expr.clone());
        }
    }
    Err(format!("'{}' is not defined", v))
}

pub fn infer(env: &mut Vec<(Var, TypeExpr)>, expr: &Locatable<Expr>) -> Result<TypeExpr, String> {
    use Expr::*;
    let loc = expr.location();
    let expr = expr.borrow_raw();
    match expr {
        Unit => Ok(TypeExpr::Unit),
        What => Ok(TypeExpr::Int),
        Var(ref v) => Ok(find(&env, v)?),
        Int(_) => Ok(TypeExpr::Int),
        Bool(_) => Ok(TypeExpr::Bool),
        UnOp(op, sub) => {
            use self::UnOp::*;
            match (op, infer(env, sub)?) {
                (Neg, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Not, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                (Neg, t) => Err(log::type_error(
                    loc,
                    format!(
                        "'{}' expects an operand of type '{}', found '{}'",
                        Neg,
                        TypeExpr::Int,
                        t
                    ),
                    expr,
                )),
                (Not, t) => Err(log::type_error(
                    loc,
                    format!(
                        "'{}' expects an operand of type '{}', found '{}'",
                        Not,
                        TypeExpr::Bool,
                        t,
                    ),
                    expr,
                )),
            }
        }
        BinOp(op, left, right) => {
            use self::BinOp::*;
            match (op, infer(env, left)?, infer(env, right)?) {
                (Lt, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Bool),
                (Add, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Sub, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Mul, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Div, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Lt, t1, t2) | (Add, t1, t2) | (Sub, t1, t2) | (Mul, t1, t2) | (Div, t1, t2) => {
                    Err(log::type_error(
                        loc,
                        format!(
                            "'{}' expects operands of type '{}', found '{}' and '{}'",
                            op,
                            TypeExpr::Int,
                            t1,
                            t2
                        ),
                        expr,
                    ))
                }
                (Or, TypeExpr::Bool, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                (And, TypeExpr::Bool, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                (Or, _, _) | (And, _, _) => Err(format!("'{}' expects boolean operands", op)),
                (Eq, t1, t2) => {
                    if t1 == t2 {
                        Ok(TypeExpr::Bool)
                    } else {
                        Err(log::type_error(
                            loc,
                            format!(
                                "'=' expects operands of the same type, found '{}' and '{}'",
                                t1, t2
                            ),
                            expr,
                        ))
                    }
                }
            }
        }
        If(condition, left, right) => {
            let t1 = infer(env, condition)?;
            if let TypeExpr::Bool = t1 {
                let t2 = infer(env, left)?;
                let t3 = infer(env, right)?;
                if t2 == t3 {
                    Ok(t2)
                } else {
                    Err(log::type_error(
                        loc,
                        format!(
                            "branches must have the same type, found '{}' and '{}'",
                            t2, t3
                        ),
                        expr,
                    ))
                }
            } else {
                Err(log::type_error(
                    loc,
                    format!(
                        "a branch condition must have type '{}', found '{}'",
                        TypeExpr::Bool,
                        t1
                    ),
                    expr,
                ))
            }
        }
        Pair(left, right) => Ok(TypeExpr::Product(
            Box::new(infer(env, left)?),
            Box::new(infer(env, right)?),
        )),
        Fst(sub) => {
            let t = infer(env, sub)?;
            if let TypeExpr::Product(left, _) = t {
                Ok(*left)
            } else {
                Err(log::type_error(
                    loc,
                    format!("can only project from a product type, found '{}'", t),
                    expr,
                ))
            }
        }
        Snd(sub) => {
            let t = infer(env, sub)?;
            if let TypeExpr::Product(_, right) = t {
                Ok(*right)
            } else {
                Err(log::type_error(
                    loc,
                    format!("can only project from a product type, found '{}'", t),
                    expr,
                ))
            }
        }
        Inl(sub, type_expr) => Ok(TypeExpr::Union(
            Box::new(infer(env, sub)?),
            Box::new(type_expr.clone()),
        )),
        Inr(sub, type_expr) => Ok(TypeExpr::Union(
            Box::new(type_expr.clone()),
            Box::new(infer(env, sub)?),
        )),
        Case(sub, (v_left, type_expr_left, sub_left), (v_right, type_expr_right, sub_right)) => {
            let t = infer(env, sub)?;
            if let TypeExpr::Union(t1, t2) = t {
                if *t1 == *type_expr_left && *t2 == *type_expr_right {
                    env.push((v_left.to_string(), type_expr_left.clone()));
                    let left = infer(env, sub_left)?;
                    env.pop();
                    env.push((v_right.to_string(), type_expr_right.clone()));
                    let right = infer(env, sub_right)?;
                    env.pop();
                    if left == right {
                        Ok(left)
                    } else {
                        Err(log::type_error(
                            loc,
                            format!(
                                "branches must have the same type, found '{}' and '{}'",
                                left, right
                            ),
                            expr,
                        ))
                    }
                } else {
                    Err(format!(
                        "expected union of type '{}', found '{}'",
                        TypeExpr::Union(t1, t2),
                        TypeExpr::Union(
                            Box::new(type_expr_left.clone()),
                            Box::new(type_expr_right.clone())
                        )
                    ))
                }
            } else {
                Err(log::type_error(
                    loc,
                    format!("case expected a union type, found '{}'", t),
                    expr,
                ))
            }
        }
        Lambda((v, type_expr, sub)) => {
            env.push((v.to_string(), type_expr.clone()));
            let other_type_expr = infer(env, sub)?;
            env.pop();
            Ok(TypeExpr::Arrow(
                Box::new(type_expr.clone()),
                Box::new(other_type_expr),
            ))
        }
        While(condition, sub) => {
            let t = infer(env, condition)?;
            if let TypeExpr::Bool = t {
                infer(env, sub)?;
                Ok(TypeExpr::Unit)
            } else {
                Err(log::type_error(
                    loc,
                    format!(
                        "a loop condition must have type '{}', found '{}'",
                        TypeExpr::Bool,
                        t
                    ),
                    expr,
                ))
            }
        }
        Seq(seq) => {
            if seq.is_empty() {
                Err(log::type_error(
                    loc,
                    "found empty sequence".to_string(),
                    expr,
                ))
            } else {
                for sub in seq.iter() {
                    infer(env, sub)?;
                }
                infer(env, &seq[seq.len() - 1])
            }
        }
        Ref(sub) => Ok(TypeExpr::Ref(Box::new(infer(env, sub)?))),
        Deref(sub) => {
            let t = infer(env, sub)?;
            if let TypeExpr::Ref(t) = t {
                Ok(*t)
            } else {
                Err(log::type_error(
                    loc,
                    format!("cannot dereference something of type '{}'", t),
                    expr,
                ))
            }
        }
        Assign(left, right) => {
            let t = infer(env, left)?;
            if let TypeExpr::Ref(t) = t {
                let right = infer(env, right)?;
                if *t == right {
                    Ok(TypeExpr::Unit)
                } else {
                    Err(log::type_error(
                        loc,
                        format!(
                        "right hand side of assignment was expected to be of type '{}', found '{}'",
                        t, right
                    ),
                        expr,
                    ))
                }
            } else {
                Err(log::type_error(
                    loc,
                    format!(
                        "left hand side of assignment must be a reference type, found '{}'",
                        t
                    ),
                    expr,
                ))
            }
        }
        App(left, right) => {
            let t = infer(env, left)?;
            if let TypeExpr::Arrow(from, to) = t {
                let t = infer(env, right)?;
                if *from == t {
                    Ok(*to)
                } else {
                    Err(log::type_error(
                        loc,
                        format!(
                            "function was expecting argument of type '{}', found '{}'",
                            from, t
                        ),
                        expr,
                    ))
                }
            } else {
                Err(log::type_error(
                    loc,
                    format!("expected a function type, found '{}'", t),
                    expr,
                ))
            }
        }
        Let(v, type_expr, sub, body) => {
            let t = infer(env, sub)?;
            if t == *type_expr {
                env.push((v.to_string(), t));
                let body = infer(env, body)?;
                env.pop();
                Ok(body)
            } else {
                Err(log::type_error(
                    loc,
                    format!("expected expression of type '{}', found '{}'", type_expr, t),
                    expr,
                ))
            }
        }
        LetFun(fun, (v_lambda, type_expr_lambda, sub_lambda), type_expr, body) => {
            let fun_type_expr = TypeExpr::Arrow(
                Box::new(type_expr_lambda.clone()),
                Box::new(type_expr.clone()),
            );
            env.push((v_lambda.to_string(), type_expr_lambda.clone()));
            env.push((fun.to_string(), fun_type_expr.clone()));
            let lambda = infer(env, sub_lambda)?;
            env.pop();
            env.pop();
            if lambda == *type_expr {
                env.push((fun.to_string(), fun_type_expr));
                let body = infer(env, body)?;
                env.pop();
                Ok(body)
            } else {
                Err("Type mismatch".to_string())
            }
        }
    }
}

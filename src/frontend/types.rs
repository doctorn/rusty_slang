use std::fmt;

use super::past::{Expr, Var};

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

pub fn infer(env: &mut Vec<(Var, TypeExpr)>, expr: &Expr) -> Result<TypeExpr, String> {
    use Expr::*;
    match expr {
        Unit => Ok(TypeExpr::Unit),
        What => Ok(TypeExpr::Int),
        Var(ref v) => Ok(find(&env, v)?),
        Int(_) => Ok(TypeExpr::Int),
        Bool(_) => Ok(TypeExpr::Bool),
        UnOp(op, sub) => {
            use super::past::UnOp::*;
            match (op, infer(env, sub.borrow_raw())?) {
                (Neg, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Not, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                _ => Err("Type mismatch".to_string()), // TODO properly
            }
        }
        BinOp(op, left, right) => {
            use super::past::BinOp::*;
            match (
                op,
                infer(env, left.borrow_raw())?,
                infer(env, right.borrow_raw())?,
            ) {
                (Lt, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Bool),
                (Add, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Sub, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Mul, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Div, TypeExpr::Int, TypeExpr::Int) => Ok(TypeExpr::Int),
                (Lt, _, _) | (Add, _, _) | (Sub, _, _) | (Mul, _, _) | (Div, _, _) => {
                    Err(format!("'{}' expects integer operands", op))
                }
                (Or, TypeExpr::Bool, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                (And, TypeExpr::Bool, TypeExpr::Bool) => Ok(TypeExpr::Bool),
                (Or, _, _) | (And, _, _) => Err(format!("'{}' expects boolean operands", op)),
                (Eq, t1, t2) => {
                    if t1 == t2 {
                        Ok(TypeExpr::Bool)
                    } else {
                        Err("'=' expects operands of the same type".to_string())
                    }
                }
            }
        }
        If(condition, left, right) => {
            if let TypeExpr::Bool = infer(env, condition.borrow_raw())? {
                let left = infer(env, left.borrow_raw())?;
                let right = infer(env, right.borrow_raw())?;
                if left == right {
                    Ok(left)
                } else {
                    Err("Type mismatch".to_string()) // TODO proper errors
                }
            } else {
                Err("Condition must have type 'bool'".to_string())
            }
        }
        Pair(left, right) => Ok(TypeExpr::Product(
            Box::new(infer(env, left.borrow_raw())?),
            Box::new(infer(env, right.borrow_raw())?),
        )),
        Fst(sub) => {
            if let TypeExpr::Product(left, _) = infer(env, sub.borrow_raw())? {
                Ok(*left)
            } else {
                Err("Can only project from a product type".to_string())
            }
        }
        Snd(sub) => {
            if let TypeExpr::Product(_, right) = infer(env, sub.borrow_raw())? {
                Ok(*right)
            } else {
                Err("Can only project from a product type".to_string())
            }
        }
        Inl(sub, type_expr) => Ok(TypeExpr::Union(
            Box::new(infer(env, sub.borrow_raw())?),
            Box::new(type_expr.clone()),
        )),
        Inr(sub, type_expr) => Ok(TypeExpr::Union(
            Box::new(type_expr.clone()),
            Box::new(infer(env, sub.borrow_raw())?),
        )),
        Case(sub, (v_left, type_expr_left, sub_left), (v_right, type_expr_right, sub_right)) => {
            if let TypeExpr::Union(infered_left, infered_right) = infer(env, sub.borrow_raw())? {
                if *infered_left == *type_expr_left && *infered_right == *type_expr_right {
                    env.push((v_left.to_string(), type_expr_left.clone()));
                    let left = infer(env, sub_left.borrow_raw())?;
                    env.pop();
                    env.push((v_right.to_string(), type_expr_right.clone()));
                    let right = infer(env, sub_right.borrow_raw())?;
                    env.pop();
                    if left == right {
                        Ok(left)
                    } else {
                        Err("Type mismatch".to_string())
                    }
                } else {
                    Err("Type mismatch".to_string())
                }
            } else {
                Err("Expected a union".to_string())
            }
        }
        Lambda((v, type_expr, sub)) => {
            env.push((v.to_string(), type_expr.clone()));
            let other_type_expr = infer(env, sub.borrow_raw())?;
            env.pop();
            Ok(TypeExpr::Arrow(
                Box::new(type_expr.clone()),
                Box::new(other_type_expr),
            ))
        }
        While(condition, sub) => {
            if let TypeExpr::Bool = infer(env, condition.borrow_raw())? {
                infer(env, sub.borrow_raw())?;
                Ok(TypeExpr::Unit)
            } else {
                Err("Condition must have type 'bool'".to_string())
            }
        }
        Seq(seq) => {
            if seq.is_empty() {
                Err("Found empty sequence".to_string())
            } else {
                for sub in seq.iter() {
                    infer(env, sub.borrow_raw())?;
                }
                infer(env, seq[seq.len() - 1].borrow_raw())
            }
        }
        Ref(sub) => Ok(TypeExpr::Ref(Box::new(infer(env, sub.borrow_raw())?))),
        Deref(sub) => {
            if let TypeExpr::Ref(t) = infer(env, sub.borrow_raw())? {
                Ok(*t)
            } else {
                Err("Expected a reference".to_string())
            }
        }
        Assign(left, right) => {
            if let TypeExpr::Ref(t) = infer(env, left.borrow_raw())? {
                let right = infer(env, right.borrow_raw())?;
                if *t == right {
                    Ok(TypeExpr::Unit)
                } else {
                    Err("Type mismatch".to_string())
                }
            } else {
                Err("Not a reference".to_string())
            }
        }
        App(left, right) => {
            if let TypeExpr::Arrow(from, to) = infer(env, left.borrow_raw())? {
                if *from == infer(env, right.borrow_raw())? {
                    Ok(*to)
                } else {
                    Err("Type mismatch".to_string())
                }
            } else {
                Err("Not a function".to_string())
            }
        }
        Let(v, type_expr, sub, body) => {
            let sub = infer(env, sub.borrow_raw())?;
            if sub == *type_expr {
                env.push((v.to_string(), type_expr.clone()));
                let body = infer(env, body.borrow_raw())?;
                env.pop();
                Ok(body)
            } else {
                Err("Type mismatch".to_string())
            }
        }
        LetFun(fun, (v_lambda, type_expr_lambda, sub_lambda), type_expr, body) => {
            let fun_type_expr = TypeExpr::Arrow(
                Box::new(type_expr_lambda.clone()),
                Box::new(type_expr.clone()),
            );
            env.push((v_lambda.to_string(), type_expr_lambda.clone()));
            env.push((fun.to_string(), fun_type_expr.clone()));
            let lambda = infer(env, sub_lambda.borrow_raw())?;
            env.pop();
            env.pop();
            if lambda == *type_expr {
                env.push((fun.to_string(), fun_type_expr));
                let body = infer(env, body.borrow_raw())?;
                env.pop();
                Ok(body)
            } else {
                Err("Type mismatch".to_string())
            }
        }
    }
}

use std::fmt;

#[derive(PartialEq, Eq)]
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

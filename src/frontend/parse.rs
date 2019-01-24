use std::iter::Peekable;

use super::lex::{Kind, Token};
use super::past::{BinOp, Expr, UnOp};
use super::types::TypeExpr;
use super::{Locatable, Location};

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(t: T) -> Parser<T> {
        Parser {
            tokens: t.peekable(),
        }
    }

    fn location(&mut self) -> Location {
        if let Some(token) = self.tokens.peek() {
            token.location().clone()
        } else {
            // TODO throw proper error
            unimplemented!()
        }
    }

    fn eat(&mut self, kind: Kind) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            if !token.borrow_raw().eq(&kind) {
                Err(format!(
                    "Expected '{:?}' got '{:?}'",
                    kind,
                    token.borrow_raw()
                )) // TODO proper error (with location)
            } else {
                Ok(token)
            }
        } else {
            Err("Unexpected end of file".to_string())
        }
    }

    fn next_is(&mut self, kind: Kind) -> bool {
        if let Some(token) = self.tokens.peek() {
            token.borrow_raw() == &kind
        } else {
            false
        }
    }

    fn next_type_factor(&mut self) -> Result<TypeExpr, String> {
        let mut type_expr = if self.next_is(Kind::UnitType) {
            self.eat(Kind::UnitType)?;
            TypeExpr::Unit
        } else if self.next_is(Kind::IntType) {
            self.eat(Kind::IntType)?;
            TypeExpr::Int
        } else if self.next_is(Kind::BoolType) {
            self.eat(Kind::BoolType)?;
            TypeExpr::Bool
        } else if self.next_is(Kind::LParen) {
            self.eat(Kind::LParen)?;
            let type_expr = self.next_type_expression()?;
            self.eat(Kind::RParen)?;
            type_expr
        } else {
            return Err("Expected type expression".to_string());
        };
        if self.next_is(Kind::Ref) {
            type_expr = TypeExpr::Ref(Box::new(type_expr));
        }
        Ok(type_expr)
    }

    fn next_type_term(&mut self) -> Result<TypeExpr, String> {
        let mut type_expr = self.next_type_factor()?;
        while self.next_is(Kind::Mul) {
            self.eat(Kind::Mul)?;
            type_expr = TypeExpr::Product(Box::new(type_expr), Box::new(self.next_type_factor()?));
        }
        Ok(type_expr)
    }

    fn next_type_union(&mut self) -> Result<TypeExpr, String> {
        let mut type_expr = self.next_type_term()?;
        while self.next_is(Kind::Add) {
            self.eat(Kind::Add)?;
            type_expr = TypeExpr::Union(Box::new(type_expr), Box::new(self.next_type_term()?));
        }
        Ok(type_expr)
    }

    fn next_type_expression(&mut self) -> Result<TypeExpr, String> {
        let mut type_expr = self.next_type_union()?;
        if self.next_is(Kind::Arrow) {
            self.eat(Kind::Arrow)?;
            type_expr =
                TypeExpr::Arrow(Box::new(type_expr), Box::new(self.next_type_expression()?));
        }
        Ok(type_expr)
    }

    fn next_factor(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let factor = if self.next_is(Kind::Unit) {
            self.eat(Kind::Unit)?;
            Expr::Unit
        } else if self.next_is(Kind::What) {
            self.eat(Kind::What)?;
            Expr::What
        } else if self.next_is(Kind::Int(0)) {
            if let Kind::Int(i) = self.eat(Kind::Int(0))?.into_raw() {
                Expr::Int(i)
            } else {
                unreachable!()
            }
        } else if self.next_is(Kind::Ident(String::new())) {
            if let Kind::Ident(ident) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                Expr::Var(ident)
            } else {
                unreachable!()
            }
        } else if self.next_is(Kind::True) {
            Expr::Bool(true)
        } else if self.next_is(Kind::False) {
            Expr::Bool(false)
        } else if self.next_is(Kind::LParen) {
            self.eat(Kind::LParen)?;
            let expr = self.next_expression()?;
            let expr = if self.next_is(Kind::Comma) {
                self.eat(Kind::Comma)?;
                Expr::Pair(Box::new(expr), Box::new(self.next_expression()?))
            } else {
                expr.into_raw()
            };
            self.eat(Kind::RParen)?;
            expr
        } else if self.next_is(Kind::Ref) {
            self.eat(Kind::Ref)?;
            Expr::Ref(Box::new(self.next_factor()?))
        } else if self.next_is(Kind::Bang) {
            self.eat(Kind::Bang)?;
            Expr::Deref(Box::new(self.next_factor()?))
        } else if self.next_is(Kind::Not) {
            self.eat(Kind::Not)?;
            Expr::UnOp(UnOp::Not, Box::new(self.next_factor()?))
        } else {
            return Err("Expected an expression".to_string());
        };
        Ok((location, factor).into())
    }

    fn next_application(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let mut application = self.next_factor()?;
        while self.next_is(Kind::LParen)
            || self.next_is(Kind::True)
            || self.next_is(Kind::False)
            || self.next_is(Kind::What)
            || self.next_is(Kind::Unit)
            || self.next_is(Kind::Ref)
            || self.next_is(Kind::Bang)
            || self.next_is(Kind::Not)
            || self.next_is(Kind::Int(0))
            || self.next_is(Kind::Ident(String::new()))
        {
            application = (
                location.clone(),
                Expr::App(Box::new(application), Box::new(self.next_factor()?)),
            )
                .into()
        }
        Ok(application)
    }

    fn next_term(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let mut term = self.next_application()?;
        while self.next_is(Kind::Mul) || self.next_is(Kind::Div) {
            let op = if self.next_is(Kind::Mul) {
                self.eat(Kind::Mul)?;
                BinOp::Mul
            } else {
                self.eat(Kind::Div)?;
                BinOp::Div
            };
            term = (
                location.clone(),
                Expr::BinOp(op, Box::new(term), Box::new(self.next_application()?)),
            )
                .into();
        }
        Ok(term)
    }

    fn next_sum(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let mut sum = self.next_term()?;
        while self.next_is(Kind::Add) || self.next_is(Kind::Sub) {
            let op = if self.next_is(Kind::Add) {
                self.eat(Kind::Add)?;
                BinOp::Add
            } else {
                self.eat(Kind::Sub)?;
                BinOp::Sub
            };
            sum = (
                location.clone(),
                Expr::BinOp(op, Box::new(sum), Box::new(self.next_term()?)),
            )
                .into();
        }
        Ok(sum)
    }

    fn next_comparison(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let comparison = self.next_sum()?;
        let comparison = if self.next_is(Kind::Lt) {
            self.eat(Kind::Lt)?;
            Expr::BinOp(BinOp::Lt, Box::new(comparison), Box::new(self.next_sum()?))
        } else if self.next_is(Kind::Eq) {
            self.eat(Kind::Eq)?;
            Expr::BinOp(BinOp::Eq, Box::new(comparison), Box::new(self.next_sum()?))
        } else {
            comparison.into_raw()
        };
        Ok((location, comparison).into())
    }

    fn next_conjunction(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let mut conjunction = self.next_comparison()?;
        while self.next_is(Kind::AndOp) {
            self.eat(Kind::AndOp)?;
            conjunction = (
                location.clone(),
                Expr::BinOp(
                    BinOp::And,
                    Box::new(conjunction),
                    Box::new(self.next_comparison()?),
                ),
            )
                .into();
        }
        Ok(conjunction)
    }

    fn next_disjunction(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let mut disjunction = self.next_conjunction()?;
        while self.next_is(Kind::OrOp) {
            self.eat(Kind::OrOp)?;
            disjunction = (
                location.clone(),
                Expr::BinOp(
                    BinOp::Or,
                    Box::new(disjunction),
                    Box::new(self.next_conjunction()?),
                ),
            )
                .into();
        }
        Ok(disjunction)
    }

    fn next_expression(&mut self) -> Result<Locatable<Expr>, String> {
        let location = self.location();
        let expr = if self.next_is(Kind::Begin) {
            self.eat(Kind::Begin)?;
            let mut exprs = vec![Box::new(self.next_expression()?)];
            while self.next_is(Kind::Semi) {
                exprs.push(Box::new(self.next_expression()?));
            }
            self.eat(Kind::End)?;
            Expr::Seq(exprs)
        } else if self.next_is(Kind::Sub) {
            self.eat(Kind::Sub)?;
            Expr::UnOp(UnOp::Neg, Box::new(self.next_expression()?))
        } else if self.next_is(Kind::If) {
            self.eat(Kind::If)?;
            let condition = self.next_expression()?;
            self.eat(Kind::Then)?;
            let left = self.next_expression()?;
            self.eat(Kind::Else)?;
            let right = self.next_expression()?;
            self.eat(Kind::End)?;
            Expr::If(Box::new(condition), Box::new(left), Box::new(right))
        } else if self.next_is(Kind::While) {
            self.eat(Kind::While)?;
            let condition = self.next_expression()?;
            self.eat(Kind::Do)?;
            let body = self.next_expression()?;
            self.eat(Kind::End)?;
            Expr::While(Box::new(condition), Box::new(body))
        } else if self.next_is(Kind::Fst) {
            self.eat(Kind::Fst)?;
            Expr::Fst(Box::new(self.next_expression()?))
        } else if self.next_is(Kind::Snd) {
            self.eat(Kind::Snd)?;
            Expr::Snd(Box::new(self.next_expression()?))
        } else if self.next_is(Kind::Inl) {
            self.eat(Kind::Inl)?;
            let type_expr = self.next_type_expression()?;
            Expr::Inl(Box::new(self.next_expression()?), type_expr)
        } else if self.next_is(Kind::Inr) {
            self.eat(Kind::Inr)?;
            let type_expr = self.next_type_expression()?;
            Expr::Inr(Box::new(self.next_expression()?), type_expr)
        } else if self.next_is(Kind::Fun) {
            self.eat(Kind::Fun)?;
            self.eat(Kind::LParen)?;
            if let Kind::Ident(ident) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                self.eat(Kind::Colon)?;
                let type_expr = self.next_type_expression()?;
                self.eat(Kind::RParen)?;
                self.eat(Kind::Arrow)?;
                let body = self.next_expression()?;
                self.eat(Kind::End)?;
                Expr::Lambda((ident, type_expr, Box::new(body)))
            } else {
                unreachable!()
            }
        } else if self.next_is(Kind::Case) {
            self.eat(Kind::Case)?;
            let to_match = self.next_expression()?;
            self.eat(Kind::Of)?;
            self.eat(Kind::Inl)?;
            self.eat(Kind::LParen)?;
            let left_ident =
                if let Kind::Ident(ident) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                    ident
                } else {
                    unreachable!()
                };
            self.eat(Kind::Colon)?;
            let left_type_expr = self.next_type_expression()?;
            self.eat(Kind::RParen)?;
            self.eat(Kind::Arrow)?;
            let left_expr = self.next_expression()?;
            self.eat(Kind::Bar)?;
            self.eat(Kind::Inr)?;
            self.eat(Kind::LParen)?;
            let right_ident =
                if let Kind::Ident(ident) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                    ident
                } else {
                    unreachable!()
                };
            self.eat(Kind::Colon)?;
            let right_type_expr = self.next_type_expression()?;
            self.eat(Kind::RParen)?;
            self.eat(Kind::Arrow)?;
            let right_expr = self.next_expression()?;
            self.eat(Kind::End)?;
            Expr::Case(
                Box::new(to_match),
                (left_ident, left_type_expr, Box::new(left_expr)),
                (right_ident, right_type_expr, Box::new(right_expr)),
            )
        } else if self.next_is(Kind::Let) {
            self.eat(Kind::Let)?;
            if let Kind::Ident(ident) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                if self.next_is(Kind::Colon) {
                    self.eat(Kind::Colon)?;
                    let type_expr = self.next_type_expression()?;
                    self.eat(Kind::Eq)?;
                    let sub = self.next_expression()?;
                    self.eat(Kind::In)?;
                    let body = self.next_expression()?;
                    self.eat(Kind::End)?;
                    Expr::Let(ident, type_expr, Box::new(sub), Box::new(body))
                } else if self.next_is(Kind::LParen) {
                    self.eat(Kind::LParen)?;
                    if let Kind::Ident(arg) = self.eat(Kind::Ident(String::new()))?.into_raw() {
                        self.eat(Kind::Colon)?;
                        let arg_type_expr = self.next_type_expression()?;
                        self.eat(Kind::RParen)?;
                        self.eat(Kind::Colon)?;
                        let type_expr = self.next_type_expression()?;
                        self.eat(Kind::Eq)?;
                        let sub = self.next_expression()?;
                        self.eat(Kind::In)?;
                        let body = self.next_expression()?;
                        self.eat(Kind::End)?;
                        Expr::LetFun(
                            ident,
                            (arg, arg_type_expr, Box::new(sub)),
                            type_expr,
                            Box::new(body),
                        )
                    } else {
                        unreachable!();
                    }
                } else {
                    return Err("Expected type annotation".to_string()); // TOOD proper error
                }
            } else {
                unreachable!()
            }
        } else {
            let assign = self.next_disjunction()?;
            let assign = if self.next_is(Kind::Assign) {
                self.eat(Kind::Assign)?;
                Expr::Assign(Box::new(assign), Box::new(self.next_expression()?))
            } else {
                assign.into_raw()
            };
            assign
        };
        Ok((location, expr).into())
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        Ok(self.next_expression()?.into_raw())
    }
}

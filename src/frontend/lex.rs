use std::iter::Peekable;
use std::mem::discriminant;

use super::{Locatable, Location};

#[derive(Debug, Eq)]
pub enum Kind {
    LParen,
    RParen,
    Comma,
    Colon,
    Semi,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Eq,
    Assign,
    Lt,
    AndOp,
    OrOp,
    Bar,
    Arrow,
    What,
    Bang,
    Unit,
    And,
    True,
    False,
    Ref,
    Inl,
    Inr,
    Fst,
    Snd,
    Case,
    Of,
    If,
    Then,
    Else,
    Let,
    Fun,
    In,
    Begin,
    End,
    While,
    Do,
    BoolType,
    IntType,
    UnitType,
    Int(i64),
    Ident(String),
}

impl PartialEq for Kind {
    fn eq(&self, other: &Kind) -> bool {
        discriminant(self) == discriminant(other)
    }
}

pub type Token = Locatable<Kind>;

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    filename: String,
    line: usize,
    column: usize,
    chars: Peekable<T>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn over(filename: String, chars: T) -> Lexer<T> {
        Lexer {
            filename: filename,
            line: 0,
            column: 0,
            chars: chars.peekable(),
        }
    }

    fn location(&self) -> Location {
        Location::new(self.filename.clone(), self.line, self.column)
    }

    fn advance(&mut self) {
        self.column += 1;
        self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            match c {
                ' ' | '\t' => self.advance(),
                '\n' => {
                    self.column = 0;
                    self.line += 1;
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn skip_comment(&mut self) {
        while let Some(c) = self.chars.peek() {
            match c {
                '*' => {
                    self.advance();
                    if let Some(')') = self.chars.peek() {
                        self.advance();
                        break;
                    }
                }
                '(' => {
                    self.advance();
                    if let Some('*') = self.chars.peek() {
                        self.advance();
                        self.skip_comment();
                    }
                }
                '\n' => {
                    self.advance();
                    self.column = 0;
                    self.line += 1;
                }
                _ => self.advance(),
            }
        }
    }

    fn next_int(&mut self) -> Kind {
        let mut numeral = String::new();
        while let Some(c @ '0'...'9') = self.chars.peek() {
            numeral.push(*c);
            self.advance();
        }
        Kind::Int(numeral.parse::<i64>().unwrap())
    }

    fn next_keyword(&mut self) -> Kind {
        use self::Kind::*;
        let mut keyword = String::new();
        if let Some(c @ 'a'...'z') | Some(c @ 'A'...'Z') = self.chars.peek() {
            keyword.push(*c);
            self.advance();
            while let Some(c @ 'a'...'z') | Some(c @ 'A'...'Z') | Some(c @ '_') | Some(c @ '\'')
            | Some(c @ '0'...'9') = self.chars.peek()
            {
                keyword.push(*c);
                self.advance();
            }
            match keyword.as_str() {
                "and" => And,
                "true" => True,
                "false" => False,
                "ref" => Ref,
                "inl" => Inl,
                "inr" => Inr,
                "fst" => Fst,
                "snd" => Snd,
                "case" => Case,
                "of" => Of,
                "if" => If,
                "then" => Then,
                "else" => Else,
                "let" => Let,
                "fun" => Fun,
                "in" => In,
                "begin" => Begin,
                "end" => End,
                "while" => While,
                "do" => Do,
                "bool" => BoolType,
                "int" => IntType,
                "unit" => UnitType,
                _ => Ident(keyword),
            }
        } else {
            unreachable!()
        }
    }

    fn next_kind(&mut self) -> Result<Kind, String> {
        use self::Kind::*;
        if let Some(c) = self.chars.peek() {
            let kind = match c {
                '(' => {
                    self.advance();
                    if let Some(c) = self.chars.peek() {
                        match c {
                            '*' => {
                                self.advance();
                                self.skip_comment();
                                return self.next_kind();
                            }
                            ')' => Unit,
                            _ => return Ok(LParen),
                        }
                    } else {
                        return Ok(LParen);
                    }
                }
                ')' => RParen,
                ',' => Comma,
                ':' => {
                    self.advance();
                    if let Some(':') = self.chars.peek() {
                        Assign
                    } else {
                        return Ok(Colon);
                    }
                }
                ';' => Semi,
                '+' => Add,
                '-' => {
                    self.advance();
                    if let Some('>') = self.chars.peek() {
                        Arrow
                    } else {
                        return Ok(Sub);
                    }
                }
                '*' => Mul,
                '/' => Div,
                '~' => Not,
                '=' => Eq,
                '<' => Lt,
                '&' => {
                    self.advance();
                    if let Some('&') = self.chars.peek() {
                        AndOp
                    } else {
                        return Err("No matching token class".to_string());
                    }
                }
                '|' => {
                    self.advance();
                    if let Some('|') = self.chars.peek() {
                        OrOp
                    } else {
                        return Ok(Bar);
                    }
                }
                '?' => What,
                '!' => Bang,
                'a'...'z' | 'A'...'Z' => return Ok(self.next_keyword()),
                '0'...'9' => return Ok(self.next_int()),
                c if c.is_whitespace() => {
                    self.skip_whitespace();
                    return self.next_kind();
                }
                _ => return Err("No matching token class".to_string()),
            };
            self.advance();
            Ok(kind)
        } else {
            Err("Unexpected EOF".to_string())
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let location = self.location();
        if let Ok(kind) = self.next_kind() {
            Some((location, kind).into())
        } else {
            // TODO Print lex error
            None
        }
    }
}

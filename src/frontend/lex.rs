use std::fmt;
use std::iter::{FusedIterator, Peekable};
use std::mem::discriminant;

use super::{log, Locatable, Location};

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

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Kind::*;
        match *self {
            LParen => write!(f, "'('"),
            RParen => write!(f, "')'"),
            Comma => write!(f, "','"),
            Colon => write!(f, "':'"),
            Semi => write!(f, "';'"),
            Add => write!(f, "'+'"),
            Sub => write!(f, "'-'"),
            Mul => write!(f, "'*'"),
            Div => write!(f, "'/'"),
            Not => write!(f, "'!'"),
            Eq => write!(f, "'='"),
            Assign => write!(f, "':='"),
            Lt => write!(f, "'<'"),
            AndOp => write!(f, "'&&'"),
            OrOp => write!(f, "'||'"),
            Bar => write!(f, "'|'"),
            Arrow => write!(f, "'->'"),
            What => write!(f, "'?'"),
            Bang => write!(f, "'!'"),
            Unit => write!(f, "unit '()'"),
            And => write!(f, "keyword 'and'"),
            True => write!(f, "boolean 'true'"),
            False => write!(f, "boolean 'false'"),
            Ref => write!(f, "keyword 'ref'"),
            Inl => write!(f, "keyword 'inl'"),
            Inr => write!(f, "keyword 'inr'"),
            Fst => write!(f, "keyword 'fst'"),
            Snd => write!(f, "keyword 'snd'"),
            Case => write!(f, "keyword 'case'"),
            Of => write!(f, "keyword 'of'"),
            If => write!(f, "keyword 'if'"),
            Then => write!(f, "keyword 'then'"),
            Else => write!(f, "keyword 'else'"),
            Let => write!(f, "keyword 'let'"),
            Fun => write!(f, "keyword 'fun'"),
            In => write!(f, "keyword 'in'"),
            Begin => write!(f, "keyword 'begin'"),
            End => write!(f, "keyword 'end'"),
            While => write!(f, "keyword 'while'"),
            Do => write!(f, "keyword 'do'"),
            BoolType => write!(f, "typename 'bool'"),
            IntType => write!(f, "typename 'int'"),
            UnitType => write!(f, "typename 'unit'"),
            Int(_) => write!(f, "integer"),
            Ident(ref ident) => {
                write!(f, "identifier")?;
                if ident.len() > 0 {
                    write!(f, " ('{}')", ident)
                } else {
                    Ok(())
                }
            }
        }
    }
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

    pub fn location(&self) -> Location {
        Location::new(self.filename.clone(), self.line + 1, self.column)
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
                    if let Some('=') = self.chars.peek() {
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
                        return Err("no matching token class".to_string());
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
                _ => return Err("no matching token class".to_string()),
            };
            self.advance();
            Ok(kind)
        } else {
            Err("unexpected end of file".to_string())
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Result<Token, String>> {
        let location = self.location();
        match self.next_kind() {
            Ok(kind) => Some(Ok((location, kind).into())),
            Err(err) => Some(Err(log::parse_error(&location, err))),
        }
    }
}

impl<T> FusedIterator for Lexer<T> where T: Iterator<Item = char> {}

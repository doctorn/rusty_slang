use std::fmt;

pub mod ast;
mod lex;
mod parse;
mod past;
mod types;

#[derive(Clone)]
pub struct Location {
    filename: String,
    line: usize,
    column: usize,
}

impl Location {
    pub fn new(filename: String, line: usize, column: usize) -> Location {
        Location {
            filename,
            line,
            column,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: line {}: column {}: ",
            self.filename,
            self.line + 1,
            self.column
        )
    }
}

pub struct Locatable<T> {
    location: Location,
    t: T,
}

impl<T> From<(Location, T)> for Locatable<T> {
    fn from((location, t): (Location, T)) -> Locatable<T> {
        Locatable { location, t }
    }
}

impl<'a, T> Locatable<T> {
    fn into_raw(self) -> T {
        self.t
    }

    fn borrow_raw(&self) -> &T {
        &self.t
    }

    pub fn location(&self) -> &Location {
        &self.location
    }
}

impl<'a, T> Into<Location> for Locatable<T> {
    fn into(self) -> Location {
        self.location
    }
}

fn check(expr: &past::Expr) -> Result<(), String> {
    println!("{}", types::infer(&mut vec![], &expr)?);
    Ok(())
}

pub fn frontend(filename: String, text: String) -> Result<ast::Expr, String> {
    let lexer = self::lex::Lexer::over(filename, text.chars());
    let mut parser = parse::Parser::new(lexer);
    let past = parser.parse()?;
    println!("{}", past);
    check(&past)?;
    Ok(past.into())
}

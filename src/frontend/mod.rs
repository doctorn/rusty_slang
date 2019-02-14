pub mod ast;
mod lex;
mod log;
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

fn check(expr: &Locatable<past::Expr>) -> Result<(), String> {
    types::infer(&mut vec![], &expr)?;
    Ok(())
}

pub fn frontend(filename: &str, text: String) -> Result<ast::Expr, String> {
    let lexer = self::lex::Lexer::over(filename.to_string(), text.chars());
    let mut parser = parse::Parser::new(lexer);
    let past = parser.parse()?;
    check(&past)?;
    Ok(past.into_raw().into())
}

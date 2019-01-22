mod ast;
mod lexer;
mod parser;
mod past;

pub struct Location {
    file: &'static str,
    line: usize,
    column: usize,
    text: String,
}

pub struct Locatable<T> {
    location: Location,
    t: T,
}

impl<T> Locatable<T> {
    pub fn into_raw(self) -> T {
        self.t
    }

    fn borrow_raw(&self) -> &T {
        &self.t
    }
}

impl<T> Into<Location> for Locatable<T> {
    fn into(self) -> Location {
        self.location
    }
}

fn check(expr: &past::Expr) -> Result<(), String> {
    unimplemented!()
}

pub fn frontend(filename: String) -> ast::Expr {
    unimplemented!()
}

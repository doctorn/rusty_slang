use super::{past, Location};
use std::fmt;
use termion::{color, style};

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}: line {}: column {}: ",
            style::Bold,
            self.filename,
            style::Reset,
            self.line + 1,
            self.column
        )
    }
}

pub fn parse_error(location: &Location, message: String) -> String {
    format!(
        "{}{}parse error{}{}: {}{}",
        style::Bold,
        color::Fg(color::Red),
        color::Fg(color::Reset),
        style::Reset,
        location,
        message,
    )
}

pub fn type_error(location: &Location, message: String, expr: &past::Expr) -> String {
    let expr = format!("{}", expr);
    format!(
        "{}{}type error{}{}: {}{}\n |\n `-> {}{}\n     {}{}{}{}",
        style::Bold,
        color::Fg(color::Red),
        color::Fg(color::Reset),
        style::Reset,
        location,
        message,
        style::Bold,
        expr,
        color::Fg(color::Red),
        "^".repeat(expr.len()),
        color::Fg(color::Reset),
        style::Reset,
    )
}

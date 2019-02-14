extern crate termion;

use std::fs::OpenOptions;
use std::io::prelude::*;
use termion::{color, style};

mod backend;
mod frontend;

pub fn compile(input: &str, output: &str) -> Result<(), String> {
    let mut input_file = match OpenOptions::new().read(true).open(input) {
        Ok(file) => file,
        Err(_) => {
            return Err(format!(
                "{}{}error{}{}: failed to open '{}{}{}'",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset,
                style::Bold,
                input,
                style::Reset
            ))
        }
    };
    let mut text = String::new();
    if let Err(_) = input_file.read_to_string(&mut text) {
        return Err(format!(
            "{}{}error{}{}: failed to read '{}{}{}'",
            style::Bold,
            color::Fg(color::Red),
            color::Fg(color::Reset),
            style::Reset,
            style::Bold,
            input,
            style::Reset
        ));
    }
    let ast = frontend::frontend(input, text)?;
    let mut output_file = match OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output)
    {
        Ok(file) => file,
        Err(_) => {
            return Err(format!(
                "{}{}error{}{}: failed to open '{}{}{}'",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset,
                style::Bold,
                output,
                style::Reset
            ))
        }
    };
    let code = backend::generate(ast.into());
    if let Err(_) = write!(output_file, "{}", code) {
        return Err(format!(
            "{}{}error{}{}: failed to write to '{}{}{}'",
            style::Bold,
            color::Fg(color::Red),
            color::Fg(color::Reset),
            style::Reset,
            style::Bold,
            output,
            style::Reset
        ));
    }
    Ok(())
}

extern crate slang;
extern crate termion;

use std::time::Instant;
use termion::{color, style};

use std::env;

fn main() {
    println!("( {}slang{} ) ", style::Bold, style::Reset);
    let args: Vec<String> = env::args().collect();
    let mut args = args.into_iter().skip(1);
    let input = match args.next() {
        Some(input) => input,
        None => {
            println!(
                "{}{}error{}{}: no input file given! (expected two arguments)",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset
            );
            std::process::exit(1);
        }
    };
    let output = match args.next() {
        Some(input) => input,
        None => {
            println!(
                "{}{}error{}{}: no output file given! (expected two arguments)",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset
            );
            std::process::exit(1);
        }
    };
    println!(
        "{}{}compiling{}{}: '{}{}{}' to output file '{}{}{}'...",
        style::Bold,
        color::Fg(color::Blue),
        color::Fg(color::Reset),
        style::Reset,
        style::Bold,
        input,
        style::Reset,
        style::Bold,
        output,
        style::Reset
    );
    let now = Instant::now();
    match slang::compile(&input, &output) {
        Ok(_) => {
            println!(
                "{}{}success{}{}: compilation completed in {}{}ms{}",
                style::Bold,
                color::Fg(color::Green),
                color::Fg(color::Reset),
                style::Reset,
                style::Bold,
                now.elapsed().as_millis(),
                style::Reset
            );
        }
        Err(err) => {
            println!("{}", err);
            println!(
                "{}{}failure{}{}: compilation terminated after {}{}ms{}",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset,
                style::Bold,
                now.elapsed().as_millis(),
                style::Reset
            );
            std::process::exit(1);
        }
    }
}

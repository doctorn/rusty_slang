extern crate slang;
extern crate termion;

use std::time::Instant;
use termion::{color, style};

use std::env;

struct Options {
    comments: bool,
    help: bool,
    input: Option<String>,
    output: Option<String>,
}

impl Options {
    fn init() -> Options {
        let mut comments = false;
        let mut help = false;
        let mut input = None;
        let mut output = None;
        let args = env::args().collect::<Vec<String>>();
        for arg in args.into_iter().skip(1) {
            if arg == "-C" {
                comments = true;
            } else if arg == "--help" {
                help = true;
            } else if let None = input {
                input = Some(arg)
            } else {
                output = Some(arg)
            }
        }
        Options {
            comments,
            help,
            input,
            output,
        }
    }
}

fn usage() {
    println!("usage: slang [options] src dest");
    println!("options:");
    println!("  --help   display this information");
    println!("  -C       add comments to generated assembly");
}

fn main() {
    println!("( {}slang{} ) ", style::Bold, style::Reset);
    let options = Options::init();
    if options.help {
        usage();
        return;
    }
    let input = match options.input {
        Some(input) => input,
        None => {
            println!(
                "{}{}error{}{}: no input file given! (see '--help' for usage)",
                style::Bold,
                color::Fg(color::Red),
                color::Fg(color::Reset),
                style::Reset
            );
            std::process::exit(1);
        }
    };
    let output = match options.output {
        Some(input) => input,
        None => {
            println!(
                "{}{}error{}{}: no output file given! (see '--help' for usage)",
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
    if options.comments {
        println!(
            "{}{}note{}{}: including comments in assembly",
            style::Bold,
            color::Fg(color::Magenta),
            color::Fg(color::Reset),
            style::Reset,
        );
    }
    let now = Instant::now();
    match slang::compile(&input, &output, options.comments) {
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

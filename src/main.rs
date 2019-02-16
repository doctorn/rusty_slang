extern crate slang;
extern crate termion;

use std::path::Path;
use std::process::Command;
use std::time::Instant;
use termion::{color, style};

use std::env;

struct Options {
    comments: bool,
    autolink: bool,
    help: bool,
    input: Option<String>,
}

impl Options {
    fn init() -> Options {
        let mut comments = false;
        let mut autolink = false;
        let mut help = false;
        let mut input = None;
        let args = env::args().collect::<Vec<String>>();
        for arg in args.into_iter().skip(1) {
            if arg.starts_with("-") {
                if arg == "-C" {
                    comments = true;
                } else if arg == "--help" {
                    help = true;
                } else if arg == "-L" || arg == "--link" {
                    autolink = true;
                } else {
                    println!(
                        "{}{}error{}{}: unrecognised option '{}' (see '--help' for usage)",
                        style::Bold,
                        color::Fg(color::Red),
                        color::Fg(color::Reset),
                        style::Reset,
                        arg
                    );
                    std::process::exit(1);
                }
            } else if let None = input {
                input = Some(arg)
            } else {
                println!(
                    "{}{}error{}{}: too many input files '{}' (see '--help' for usage)",
                    style::Bold,
                    color::Fg(color::Red),
                    color::Fg(color::Reset),
                    style::Reset,
                    arg
                );
                std::process::exit(1);
            }
        }
        Options {
            comments,
            autolink,
            help,
            input,
        }
    }
}

fn usage() {
    println!("usage: slang [options] file");
    println!("options:");
    println!("  --help        display this information");
    println!("  -C            add comments to generated code");
    println!("  -L, --link    assemble and link generated code");
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
    let input = Path::new(&input);
    let output = &input.with_extension("s");
    println!(
        "{}{}compiling{}{}: '{}{}{}' to output file '{}{}{}'...",
        style::Bold,
        color::Fg(color::Blue),
        color::Fg(color::Reset),
        style::Reset,
        style::Bold,
        input.display(),
        style::Reset,
        style::Bold,
        output.display(),
        style::Reset
    );
    if options.comments {
        println!(
            "{}{}note{}{}: including comments in generated assembly...",
            style::Bold,
            color::Fg(color::Magenta),
            color::Fg(color::Reset),
            style::Reset,
        );
    }
    let now = Instant::now();
    match slang::compile(input, output, options.comments) {
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
            if options.autolink {
                let executable = &input.with_extension("");
                println!(
                    "{}{}note{}{}: linking into executable '{}{}{}'...",
                    style::Bold,
                    color::Fg(color::Magenta),
                    color::Fg(color::Reset),
                    style::Reset,
                    style::Bold,
                    executable.display(),
                    style::Reset,
                );
                Command::new("gcc")
                    .args(&[
                        "-o",
                        &format!("{}", executable.display()),
                        &format!("{}", output.display()),
                        concat!("-L", env!("OUT_DIR")),
                        "-lslangrt",
                    ])
                    .status()
                    .unwrap();
            }
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

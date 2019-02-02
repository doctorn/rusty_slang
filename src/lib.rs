use std::fs::OpenOptions;
use std::io::prelude::*;

mod backend;
mod frontend;

pub fn compile(input: String, output: String) {
    let mut input_file = OpenOptions::new().read(true).open(&input).unwrap(); // TODO proper error
    let mut output_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output)
        .unwrap(); // TODO proper error
    let mut text = String::new();
    input_file
        .read_to_string(&mut text)
        .expect("Failed to read file"); // TODO proper error
    match frontend::frontend(input, text) {
        Ok(ast) => {
            let x86 = backend::x86::to_x86(ast.into());
            for line in x86.iter() {
                println!("{}", line);
                output_file.write(format!("{}\n", line).as_bytes()).unwrap(); // TODO proper error
            }
        }
        Err(err) => println!("{}", err),
    }
}

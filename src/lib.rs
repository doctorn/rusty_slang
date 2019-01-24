use std::fs::OpenOptions;
use std::io::prelude::*;

mod backend;
mod frontend;

pub fn compile(input: String, output: String) {
    let mut input_file = OpenOptions::new().read(true).open(&input).unwrap(); // TODO proper error
    let mut output_file = OpenOptions::new()
        .create(true)
        .write(true)
        .open(output)
        .unwrap(); // TODO proper error
    let mut text = String::new();
    input_file
        .read_to_string(&mut text)
        .expect("Failed to read file"); // TODO proper error
    match frontend::frontend(input, text) {
        Ok(_) => (),
        Err(err) => println!("{}", err),
    }
    unimplemented!()
    // TODO produce x86
    // TODO output to file
}

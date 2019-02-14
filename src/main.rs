extern crate slang;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args = args.into_iter().skip(1);
    slang::compile(&args.next().unwrap(), &args.next().unwrap())
}

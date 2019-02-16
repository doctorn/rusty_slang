use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    Command::new("gcc")
        .args(&["src/crt0.c", "-fPIC", "-c", "-o"])
        .arg(&format!("{}/crt0.o", out_dir))
        .status()
        .unwrap();
    Command::new("ar")
        .args(&["-cvq", "libslangrt.a", "crt0.o"])
        .current_dir(&Path::new(&out_dir))
        .status()
        .unwrap();
}

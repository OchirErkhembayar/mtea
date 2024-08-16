use std::io::{stdin, stdout, Write};

use mtea::vm::Vm;

fn repl() {
    let stdin = stdin();
    let mut vm = Vm::default();
    let mut input = String::new();
    loop {
        input.clear();
        print!("> ");
        stdout().flush().unwrap();
        stdin.read_line(&mut input).expect("Failed to read input");
        let buf = input.as_bytes();
        mtea::run_buf(&mut vm, buf);
    }
}

fn file(path: String) {
    let string = std::fs::read_to_string(path).unwrap();
    let buf = string.trim().as_bytes();
    let mut vm = Vm::default();
    mtea::run_buf(&mut vm, buf);
}

fn main() {
    let mut args = std::env::args();
    args.next();
    if let Some(path) = args.next() {
        file(path);
    } else {
        repl();
    }
}

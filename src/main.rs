use std::io::{stdin, stdout, Write};

fn repl() {
    let mut input = String::new();
    let stdin = stdin();
    loop {
        input.clear();
        print!("> ");
        stdout().flush().unwrap();
        stdin.read_line(&mut input).expect("Failed to read input");
        eprintln!("Input: {}", input);
        let buf = input.trim().as_bytes();
        mtea::run_buf(buf);
    }
}

fn file(path: String) {
    let string = std::fs::read_to_string(path).unwrap();
    let buf = string.trim().as_bytes();
    mtea::run_buf(buf);
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

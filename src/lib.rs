use compile::{Compiler, Program};
use lex::Lexer;
use parse::Parser;
use vm::Vm;

pub mod compile;
pub mod lex;
pub mod parse;
pub mod value;
pub mod vm;

const PRINT_TOKENS: bool = false;

fn print_tokens(buf: &[u8]) {
    use lex::TokenType;
    let mut lexer = Lexer::new(buf);
    let mut token = lexer.next_token().unwrap();
    while token.token_type != TokenType::Eof {
        println!(
            " \"{}\", line: {}",
            std::str::from_utf8(&buf[token.start..token.end]).unwrap(),
            token.line
        );
        token = lexer.next_token().unwrap();
    }
    println!();
}

pub fn run_buf(vm: &mut Vm, buf: &'_ [u8]) {
    if PRINT_TOKENS {
        print_tokens(buf);
    }

    let mut lexer = Lexer::new(buf);
    let current = match lexer.next_token() {
        Ok(token) => token,
        Err(err) => {
            eprintln!("ERROR: {}", err);
            return;
        }
    };
    let ast = match Parser::new(lexer, current).parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error: {}", err);
            return;
        }
    };
    println!("Ast: {:?}", ast);
    let program = Compiler::new(buf).compile(ast);
    println!("Program: {:?}", program);
    vm.update_program(program);
    let _ = vm.run(buf);
    vm.update_program(Program::default());
}

fn get_str(start: usize, end: usize, buf: &'_ [u8]) -> &str {
    unsafe { std::str::from_utf8_unchecked(&buf[start..end]) }
}

// TODO: Turn this into a decl macro
#[inline(always)]
fn inner_write<T: std::fmt::Display>(val: T, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", val,)
}

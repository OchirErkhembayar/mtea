use compile::compile;
use lex::Lexer;
use parse::Parser;
use vm::Vm;

pub mod compile;
pub mod lex;
pub mod parse;
pub mod value;
pub mod vm;

pub fn run_buf(buf: &[u8]) {
    // {
    //     use mtea::lex::TokenType;
    //     let mut lexer = Lexer::new(buf);
    //     let mut token = lexer.next_token().unwrap();
    //     while token.token_type != TokenType::Eof {
    //         println!("Token: {:?}", token);
    //         token = lexer.next_token().unwrap();
    //     }
    // }
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
            eprintln!("ERROR: {}", err);
            return;
        }
    };
    println!("Ast: {:?}", ast);
    let program = compile(ast);
    println!("Program: {:?}", program);
    let mut vm = Vm::new(program, buf);
    let _ = vm.run();
}

// TODO: Turn this into a decl macro
#[inline(always)]
fn inner_write<T: std::fmt::Display>(val: T, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", val,)
}

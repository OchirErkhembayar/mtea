use std::{error::Error, fmt::Display};

use crate::inner_write;

type LexRes<T> = Result<T, LexErr>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub start: usize,
    pub end: usize,
    pub token_type: TokenType,
    pub line: u32,
}

impl Token {
    fn new(token_type: TokenType, start: usize, end: usize, line: u32) -> Self {
        Self {
            start,
            end,
            token_type,
            line,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    I32(i32),
    F32(f32),
    Arrow,
    Bang,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    LParen,
    RParen,
    Eq,
    EqEq,
    NotEq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
    True,
    False,
    Ident,
    String,
    Case,
    If,
    Do,
    End,
    Else,
    Nil,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Lexer<'a> {
    buf: &'a [u8],
    line: u32,
    start: usize,
    current: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self {
            buf,
            line: 1,
            start: 0,
            current: 0,
        }
    }

    fn advance(&mut self) -> u8 {
        if self.is_at_end() {
            return b'\0';
        }
        let byte = self.buf[self.current];
        self.current += 1;
        byte
    }

    fn peek(&self, dist: usize) -> u8 {
        let dist = self.current + dist;
        *self.buf.get(dist).unwrap_or(&b'\0')
    }

    fn current_lexeme(&self) -> &[u8] {
        &self.buf[self.start..self.current]
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.buf.len()
    }

    fn number(&mut self, first: u8) -> LexRes<TokenType> {
        let mut number: i32 = (first - 48) as i32;
        while self.peek(0).is_ascii_digit() {
            number *= 10;
            number += self.advance() as i32 - 48;
            eprintln!("{number}");
        }
        let token_type = if self.peek(0) == b'.' {
            self.advance();
            let mut decimal = 0_f32;
            let mut factor = 10_f32;
            while self.peek(0).is_ascii_digit() {
                decimal += (self.advance() - 48) as f32 / factor;
                factor *= 10.0;
            }
            TokenType::F32(number as f32 + decimal)
        } else {
            TokenType::I32(number)
        };

        if self.peek(0).is_ascii_alphabetic() {
            Err(LexErr::new("Invalid number".to_string(), self.line))
        } else {
            Ok(token_type)
        }
    }

    fn ident(&mut self, first: u8) -> TokenType {
        while !self.is_at_end() && self.peek(0).is_ascii_alphanumeric() {
            self.advance();
        }

        match first {
            b't' => self.keyword("true", TokenType::True),
            b'f' => self.keyword("false", TokenType::False),
            b'a' => self.keyword("and", TokenType::And),
            b'o' => self.keyword("or", TokenType::Or),
            b'i' => self.keyword("if", TokenType::If),
            b'd' => self.keyword("do", TokenType::Do),
            b'e' => match self.buf[self.start + 1] {
                b'l' => self.keyword("else", TokenType::Else),
                b'n' => self.keyword("end", TokenType::End),
                _ => TokenType::Ident,
            },
            b'n' => self.keyword("nil", TokenType::Nil),
            b'c' => self.keyword("case", TokenType::Case),
            _ => TokenType::Ident,
        }
    }

    fn keyword(&mut self, keyword: &str, token_type: TokenType) -> TokenType {
        if self.current_lexeme() == keyword.as_bytes() {
            token_type
        } else {
            TokenType::Ident
        }
    }

    fn string(&mut self) -> LexRes<TokenType> {
        while !self.is_at_end() && self.peek(0) != b'"' {
            self.advance();
        }
        let closing = self.advance();
        if closing != b'"' {
            Err(LexErr::new("Unterminated string".to_string(), self.line))
        } else {
            Ok(TokenType::String)
        }
    }

    fn whitespace(&mut self) -> LexRes<Token> {
        while self.peek(0).is_ascii_whitespace() {
            let next = self.advance();
            if next == b'\n' {
                self.line += 1;
            }
        }

        self.reset_start();
        self.next_token()
    }

    pub fn next_token(&mut self) -> LexRes<Token> {
        if self.is_at_end() {
            return Ok(Token::new(
                TokenType::Eof,
                self.start,
                self.current,
                self.line,
            ));
        }

        let next = self.advance();
        let token_type = match next {
            b if next.is_ascii_whitespace() => {
                if b == b'\n' {
                    self.line += 1;
                }
                return self.whitespace();
            }
            b'0'..=b'9' => self.number(next)?,
            b'-' => {
                if self.peek(0) == b'>' {
                    self.advance();
                    TokenType::Arrow
                } else {
                    TokenType::Minus
                }
            }
            b'+' => TokenType::Plus,
            b'*' => TokenType::Mul,
            b'/' => TokenType::Div,
            b'%' => TokenType::Mod,
            b'=' => {
                if self.peek(0) == b'=' {
                    self.advance();
                    TokenType::EqEq
                } else {
                    TokenType::Eq
                }
            }
            b'!' => {
                if self.peek(0) == b'=' {
                    self.advance();
                    TokenType::NotEq
                } else {
                    TokenType::Bang
                }
            }
            b'>' => {
                if self.peek(0) == b'=' {
                    self.advance();
                    TokenType::Gte
                } else {
                    TokenType::Gt
                }
            }
            b'<' => {
                if self.peek(0) == b'=' {
                    self.advance();
                    TokenType::Lte
                } else {
                    TokenType::Lt
                }
            }
            b'(' => TokenType::LParen,
            b')' => TokenType::RParen,
            b'"' => self.string()?,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.ident(next),
            _ => {
                return Err(LexErr::new(
                    format!("Unexpected character {}", next as char),
                    self.line,
                ))
            }
        };
        let token = Ok(Token::new(token_type, self.start, self.current, self.line));
        self.reset_start();
        token
    }

    fn reset_start(&mut self) {
        self.start = self.current;
    }
}

#[derive(Debug)]
pub struct LexErr {
    msg: String,
    line: u32,
}

impl LexErr {
    fn new(msg: String, line: u32) -> Self {
        Self { msg, line }
    }
}

impl Display for LexErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR line: {}, {}", self.line, self.msg)
    }
}

impl Error for LexErr {}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::I32(int) => inner_write(int, f),
            TokenType::F32(float) => inner_write(float, f),
            TokenType::Arrow => inner_write("->", f),
            TokenType::Plus => inner_write("+", f),
            TokenType::Minus => inner_write("-", f),
            TokenType::Mul => inner_write("*", f),
            TokenType::Div => inner_write("/", f),
            TokenType::Mod => inner_write("%", f),
            TokenType::LParen => inner_write("(", f),
            TokenType::RParen => inner_write(")", f),
            TokenType::Eq => inner_write("=", f),
            TokenType::EqEq => inner_write("==", f),
            TokenType::NotEq => inner_write("!=", f),
            TokenType::Gt => inner_write(">", f),
            TokenType::Gte => inner_write(">=", f),
            TokenType::Lt => inner_write("<", f),
            TokenType::Lte => inner_write("<=", f),
            TokenType::And => inner_write("and", f),
            TokenType::Or => inner_write("or", f),
            TokenType::True => inner_write("true", f),
            TokenType::False => inner_write("false", f),
            TokenType::Ident => inner_write("ident", f),
            TokenType::String => inner_write("string", f),
            TokenType::If => inner_write("if", f),
            TokenType::Do => inner_write("do", f),
            TokenType::Else => inner_write("else", f),
            TokenType::Nil => inner_write("nil", f),
            TokenType::Bang => inner_write("!", f),
            TokenType::Case => inner_write("case", f),
            TokenType::End => inner_write("end", f),
            TokenType::Eof => inner_write("EOF", f),
        }
    }
}

use std::{error::Error, fmt::Display};

use crate::{
    lex::{LexErr, Lexer, Token, TokenType},
    vm::{
        OP_ADD, OP_AND, OP_DIV, OP_EQ, OP_GT, OP_GTE, OP_LT, OP_LTE, OP_MOD, OP_MUL, OP_NEG,
        OP_NOT, OP_NOT_EQ, OP_OR, OP_SUB,
    },
};

type ParseRes<T> = Result<T, ParseErr>;

#[derive(Debug)]
pub enum Ast {
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    I32(i32),
    F32(f32),
    String {
        start: usize,
        end: usize,
    },
    Bool(bool),
    Unary {
        op: UnaryOp,
        arg: Box<Expr>,
    },
    Binary {
        op: BinOp,
        src1: Box<Expr>,
        src2: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Nil,
    Var {
        start: usize,
        end: usize,
    },
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}

impl BinOp {
    pub fn to_byte(&self) -> u8 {
        match self {
            BinOp::Add => OP_ADD,
            BinOp::Minus => OP_SUB,
            BinOp::Mul => OP_MUL,
            BinOp::Div => OP_DIV,
            BinOp::Mod => OP_MOD,
            BinOp::Eq => OP_EQ,
            BinOp::NotEq => OP_NOT_EQ,
            BinOp::Gt => OP_GT,
            BinOp::Gte => OP_GTE,
            BinOp::Lt => OP_LT,
            BinOp::Lte => OP_LTE,
            BinOp::And => OP_AND,
            BinOp::Or => OP_OR,
        }
    }
}

impl From<TokenType> for BinOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Minus,
            TokenType::Mul => Self::Mul,
            TokenType::Div => Self::Div,
            TokenType::Mod => Self::Mod,
            TokenType::EqEq => Self::Eq,
            TokenType::NotEq => Self::NotEq,
            TokenType::Gt => Self::Gt,
            TokenType::Gte => Self::Gte,
            TokenType::Lt => Self::Lt,
            TokenType::Lte => Self::Lte,
            TokenType::And => Self::And,
            TokenType::Or => Self::Or,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Bang,
}

impl UnaryOp {
    pub fn to_byte(&self) -> u8 {
        match self {
            UnaryOp::Neg => OP_NEG,
            UnaryOp::Bang => OP_NOT,
        }
    }
}

impl From<TokenType> for UnaryOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Minus => Self::Neg,
            TokenType::Bang => Self::Bang,
            _ => unreachable!(),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, current: Token) -> Self {
        Self { lexer, current }
    }

    fn next_token(&mut self) -> ParseRes<Token> {
        let current = self.current;
        self.current = self.lexer.next_token()?;
        Ok(current)
    }

    fn peek(&self) -> Token {
        self.current
    }

    fn consume(&mut self, token_type: TokenType) -> ParseRes<Token> {
        let token = self.next_token()?;
        if token.token_type != token_type {
            Err(ParseErr::new(format!(
                "Expected {} got {}",
                token_type, token.token_type
            )))
        } else {
            Ok(token)
        }
    }

    pub fn parse(&mut self) -> ParseRes<Ast> {
        let expr = self.expression()?;
        Ok(Ast::Expr(expr))
    }

    fn expression(&mut self) -> ParseRes<Expr> {
        self.if_expr()
    }

    fn if_expr(&mut self) -> ParseRes<Expr> {
        if self.peek().token_type == TokenType::If {
            self.next_token()?;
            let cond = self.or()?;
            self.consume(TokenType::Do)?;
            let then_expr = self.or()?;
            self.consume(TokenType::Else)?;
            let else_expr = self.or()?;
            Ok(Expr::If {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            })
        } else {
            self.or()
        }
    }

    fn or(&mut self) -> ParseRes<Expr> {
        let mut left = self.and()?;
        while matches!(self.peek().token_type, TokenType::Or) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.and()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            }
        }
        Ok(left)
    }

    fn and(&mut self) -> ParseRes<Expr> {
        let mut left = self.eq()?;
        while matches!(self.peek().token_type, TokenType::And) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.eq()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            }
        }
        Ok(left)
    }

    fn eq(&mut self) -> ParseRes<Expr> {
        let mut left = self.relational()?;
        while matches!(self.peek().token_type, TokenType::EqEq | TokenType::NotEq) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.relational()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            }
        }
        Ok(left)
    }

    fn relational(&mut self) -> ParseRes<Expr> {
        let mut left = self.term()?;
        while matches!(
            self.peek().token_type,
            TokenType::Gt | TokenType::Gte | TokenType::Lt | TokenType::Lte
        ) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.term()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            }
        }
        Ok(left)
    }

    fn term(&mut self) -> ParseRes<Expr> {
        let mut left = self.factor()?;
        while matches!(self.peek().token_type, TokenType::Plus | TokenType::Minus) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.factor()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            };
        }
        Ok(left)
    }

    fn factor(&mut self) -> ParseRes<Expr> {
        let mut left = self.unary()?;
        while matches!(
            self.peek().token_type,
            TokenType::Mul | TokenType::Div | TokenType::Mod
        ) {
            let op = BinOp::from(self.next_token()?.token_type);
            let right = self.unary()?;
            left = Expr::Binary {
                op,
                src1: Box::new(left),
                src2: Box::new(right),
            };
        }
        Ok(left)
    }

    fn unary(&mut self) -> ParseRes<Expr> {
        if matches!(self.peek().token_type, TokenType::Minus | TokenType::Bang) {
            let op = UnaryOp::from(self.next_token()?.token_type);
            let expr = self.primary()?;
            Ok(Expr::Unary {
                op,
                arg: Box::new(expr),
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseRes<Expr> {
        let token = self.next_token()?;
        let expr = match token.token_type {
            TokenType::I32(int) => Expr::I32(int),
            TokenType::F32(float) => Expr::F32(float),
            TokenType::True => Expr::Bool(true),
            TokenType::False => Expr::Bool(false),
            TokenType::Ident => Expr::Var {
                start: token.start,
                end: token.end,
            },
            TokenType::String => Expr::String {
                start: token.start,
                end: token.end,
            },
            TokenType::LParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RParen)?;
                expr
            }
            TokenType::Nil => Expr::Nil,
            _ => return Err(ParseErr::new(format!("Unexpected token: {:?}", token))),
        };
        Ok(expr)
    }
}

#[derive(Debug)]
pub struct ParseErr {
    msg: String,
}

impl ParseErr {
    fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for ParseErr {}

impl From<LexErr> for ParseErr {
    fn from(value: LexErr) -> Self {
        Self {
            msg: value.to_string(),
        }
    }
}

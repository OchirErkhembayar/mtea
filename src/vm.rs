use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{compile::Program, value::Value};

const STACK_SIZE: usize = u8::MAX as usize;

pub const OP_RET: u8 = 0;
pub const OP_CONST: u8 = 1;
pub const OP_ADD: u8 = 2;
pub const OP_SUB: u8 = 3;
pub const OP_MUL: u8 = 4;
pub const OP_DIV: u8 = 5;
pub const OP_MOD: u8 = 6;
pub const OP_EQ: u8 = 7;
pub const OP_NOT_EQ: u8 = 8;
pub const OP_GT: u8 = 9;
pub const OP_GTE: u8 = 10;
pub const OP_LT: u8 = 11;
pub const OP_LTE: u8 = 12;
pub const OP_AND: u8 = 13;
pub const OP_OR: u8 = 14;
pub const OP_NEG: u8 = 15;
pub const OP_TRUE: u8 = 16;
pub const OP_FALSE: u8 = 17;
pub const OP_JUMP_IF_FALSE: u8 = 18;
pub const OP_JUMP: u8 = 19;
pub const OP_POP: u8 = 20;
pub const OP_NOT: u8 = 21;
pub const OP_COPY: u8 = 22; // Copy the top of the stack
pub const OP_END_CASE: u8 = 23;
pub const OP_ERROR: u8 = 24;
pub const OP_END_BLOCK: u8 = 25;
pub const OP_DEFINE_LOCAL: u8 = 26;
pub const OP_GET_LOCAL: u8 = 27;
pub const OP_NIL: u8 = 28;

#[derive(Debug)]
pub struct Vm {
    instrs: Vec<u8>,
    consts: Vec<Value>,
    ip: usize,
    sp: usize,
    bp: usize,
    stack: [Value; STACK_SIZE],
    globals: HashMap<String, Value>,
}

pub enum InterpRes {
    Ok,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            instrs: vec![],
            consts: vec![],
            globals: HashMap::new(),
            ip: 0,
            sp: 0,
            bp: 0,
            stack: [Value::Nil; STACK_SIZE],
        }
    }

    pub fn update_program(&mut self, program: Program) {
        self.instrs = program.instrs;
        self.consts = program.consts;
        self.ip = 0;
    }

    fn read_byte(&mut self) -> u8 {
        let instr = self.instrs[self.ip];
        self.ip += 1;
        instr
    }

    fn read_short(&mut self) -> u16 {
        let fst = self.read_byte() as u16;
        let snd = self.read_byte() as u16;
        fst << 8 | snd
    }

    fn push(&mut self, value: Value) {
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp]
    }

    fn peek(&self, arg: usize) -> Value {
        self.stack[self.sp - 1 - arg]
    }

    fn peek_bottom(&self, depth: usize) -> Value {
        self.stack[self.bp + depth]
    }

    fn binary<F>(&mut self, operation: F) -> Result<(), RuntimeErr>
    where
        F: Fn(Value, Value) -> Result<Value, RuntimeErr>,
    {
        let right = self.pop();
        let left = self.pop();
        self.push(operation(left, right)?);
        Ok(())
    }

    pub fn run(&mut self, buf: &[u8]) -> Result<(), RuntimeErr> {
        loop {
            let instr = self.read_byte();
            self.debug_instr(instr, buf);
            match instr {
                OP_CONST => {
                    let idx = self.read_byte() as usize;
                    let value = self.consts[idx];
                    self.push(value);
                }
                OP_ADD => self.binary(|a, b| (a + b))?,
                OP_SUB => self.binary(|a, b| a - b)?,
                OP_MUL => self.binary(|a, b| a * b)?,
                OP_DIV => self.binary(|a, b| a / b)?,
                OP_MOD => self.binary(|a, b| a % b)?,
                OP_EQ => self.binary(|a, b| Ok(Value::Bool(a.eq(&b, buf))))?,
                OP_NOT_EQ => self.binary(|a, b| Ok(Value::Bool(!a.eq(&b, buf))))?,
                OP_GT => self.binary(|a, b| Ok(Value::Bool(a.gt(&b))))?,
                OP_GTE => self.binary(|a, b| Ok(Value::Bool(a.gte(&b, buf))))?,
                OP_LT => self.binary(|a, b| Ok(Value::Bool(a.lt(&b, buf))))?,
                OP_LTE => self.binary(|a, b| Ok(Value::Bool(a.lte(&b, buf))))?,
                OP_AND => self.binary(|a, b| Ok(Value::Bool(a.bool() && b.bool())))?,
                OP_OR => self.binary(|a, b| Ok(Value::Bool(a.bool() || b.bool())))?,
                OP_NEG => {
                    let arg = self.pop();
                    let result = (-arg)?;
                    self.push(result);
                }
                OP_TRUE => self.push(Value::Bool(true)),
                OP_FALSE => self.push(Value::Bool(false)),
                OP_POP => {
                    self.pop();
                }
                OP_END_CASE => {
                    let top = self.pop();
                    self.pop();
                    self.push(top);
                }
                OP_JUMP => {
                    let dist = self.read_short() as usize;
                    self.ip += dist;
                }
                OP_JUMP_IF_FALSE => {
                    let dist = self.read_short() as usize;
                    if !self.peek(0).bool() {
                        self.ip += dist;
                    }
                }
                OP_NIL => self.push(Value::Nil),
                OP_NOT => {
                    let value = self.pop();
                    self.push(Value::Bool(!value.bool()));
                }
                OP_DEFINE_LOCAL => {
                    let depth = self.read_byte();
                    self.stack[self.bp + depth as usize] = self.peek(0);
                }
                OP_GET_LOCAL => {
                    let depth = self.read_byte();
                    self.push(self.peek_bottom(depth as usize));
                }
                OP_END_BLOCK => {
                    let depth = self.read_byte();
                    let result = self.pop();
                    self.sp -= depth as usize;
                    self.push(result);
                }
                OP_COPY => {
                    let depth = self.read_byte() as usize;
                    self.push(self.peek(depth));
                }
                OP_ERROR => return Err(RuntimeErr::new("No match".to_string())),
                OP_RET => return Ok(()),
                _ => unreachable!("{}", instr),
            }
        }
    }

    fn debug_instr(&self, instr: u8, buf: &[u8]) {
        print!(
            "{} [ ",
            match instr {
                OP_RET => "RETURN",
                OP_CONST => "CONST",
                OP_ADD => "ADD",
                OP_SUB => "SUB",
                OP_MUL => "MUL",
                OP_DIV => "DIV",
                OP_MOD => "MOD",
                OP_EQ => "EQ",
                OP_NOT_EQ => "NOT_EQ",
                OP_GT => "GT",
                OP_GTE => "GTE",
                OP_LT => "LT",
                OP_LTE => "LTE",
                OP_AND => "AND",
                OP_OR => "OR",
                OP_NEG => "NEG",
                OP_TRUE => "TRUE",
                OP_FALSE => "FALSE",
                OP_JUMP_IF_FALSE => "JUMP_IF_FALSE",
                OP_JUMP => "JUMP",
                OP_POP => "POP",
                OP_NOT => "NOT",
                OP_NIL => "NIL",
                OP_COPY => "COPY",
                OP_ERROR => "ERROR",
                OP_END_BLOCK => "END_BLOCK",
                OP_GET_LOCAL => "GET_LOCAL",
                OP_DEFINE_LOCAL => "DEFINE_LOCAL",
                OP_END_CASE => "END_CASE",
                _ => todo!("{}", instr),
            }
        );
        self.stack[0..self.sp].iter().for_each(|v| {
            print!(
                "{} ",
                match v {
                    Value::String { start, end } => {
                        std::str::from_utf8(&buf[*start..*end]).unwrap().to_string()
                    }
                    v => v.to_string(),
                }
            )
        });
        print!("] ");
        print!("[ ");
        self.globals.iter().for_each(|(k, v)| {
            print!(
                "{}: {} ",
                *k,
                match v {
                    Value::String { start, end } => {
                        std::str::from_utf8(&buf[*start..*end]).unwrap().to_string()
                    }
                    v => v.to_string(),
                }
            );
        });
        println!("]");
    }
}

#[derive(Debug)]
pub struct RuntimeErr {
    msg: String,
}

impl RuntimeErr {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl Display for RuntimeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for RuntimeErr {}

use crate::{
    get_str,
    parse::{Ast, Expr},
    value::Value,
    vm::{
        OP_CONST, OP_COPY, OP_DEFINE_LOCAL, OP_END_BLOCK, OP_END_CASE, OP_EQ, OP_ERROR, OP_FALSE,
        OP_GET_LOCAL, OP_JUMP, OP_JUMP_IF_FALSE, OP_NIL, OP_POP, OP_RET, OP_TRUE,
    },
};

const LOCALS_MAX: usize = u8::MAX as usize;

#[derive(Debug)]
pub struct Program {
    pub instrs: Vec<u8>,
    pub consts: Vec<Value>,
}

impl Program {
    fn new() -> Self {
        Self {
            instrs: Vec::new(),
            consts: Vec::new(),
        }
    }

    fn add_const(&mut self, value: Value) {
        self.consts.push(value);
        self.add_instr(OP_CONST);
        let idx = self.consts.len() - 1;
        self.instrs.push(idx as u8);
    }

    fn add_instr(&mut self, instr: u8) {
        self.instrs.push(instr);
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Copy, Clone)]
struct Local<'a> {
    name: &'a str,
    depth: u32,
}

impl<'a> Local<'a> {
    fn new(name: &'a str, depth: u32) -> Self {
        Self { name, depth }
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    locals: [Local<'a>; LOCALS_MAX],
    locals_count: usize,
    depth: u32,
    buf: &'a [u8],
}

impl<'a> Compiler<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self {
            locals: [Local::new("foo", 10000); LOCALS_MAX],
            locals_count: 0,
            depth: 0,
            buf,
        }
    }

    pub fn compile(&mut self, ast: Ast) -> Program {
        let mut program = Program::default();
        match ast {
            Ast::Exprs(exprs) => exprs
                .into_iter()
                .for_each(|expr| self.compile_expr(expr, &mut program)),
        };
        program.add_instr(OP_RET);
        program
    }

    fn emit_jump(&self, op: u8, program: &mut Program) -> usize {
        program.add_instr(op);
        program.add_instr(0xff);
        program.add_instr(0xff);
        program.instrs.len() - 2
    }

    fn patch_jump(&self, offset: usize, program: &mut Program) {
        let jump = program.instrs.len() - offset - 2;

        if jump > u16::MAX as usize {
            eprintln!("Jump too high");
            std::process::exit(1);
        }

        *(unsafe { program.instrs.get_unchecked_mut(offset) }) = (jump >> 8) as u8;
        *(unsafe { program.instrs.get_unchecked_mut(offset + 1) }) = jump as u8;
    }

    fn resolve_local(&self, start: usize, end: usize) -> Option<usize> {
        for i in (0..self.locals_count).rev() {
            let local = self.locals[i];
            if get_str(start, end, self.buf) == local.name {
                return Some(i);
            }
        }

        None
    }

    fn compile_expr(&mut self, expr: Expr, program: &mut Program) {
        match expr {
            Expr::I32(int) => program.add_const(Value::Int(int)),
            Expr::F32(float) => program.add_const(Value::Float(float)),
            Expr::Unary { op, arg } => {
                self.compile_expr(*arg, program);
                program.add_instr(op.to_byte());
            }
            Expr::Binary { op, src1, src2 } => {
                self.compile_expr(*src1, program);
                self.compile_expr(*src2, program);
                program.add_instr(op.to_byte());
            }
            Expr::String { start, end } => program.add_const(Value::String { start, end }),
            Expr::Bool(bool) => program.add_instr(if bool { OP_TRUE } else { OP_FALSE }),
            Expr::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.compile_expr(*cond, program);
                let then_jump = self.emit_jump(OP_JUMP_IF_FALSE, program);
                program.add_instr(OP_POP);
                self.compile_expr(*then_expr, program);
                let else_jump = self.emit_jump(OP_JUMP, program);
                self.patch_jump(then_jump, program);
                program.add_instr(OP_POP);
                self.compile_expr(*else_expr, program);
                self.patch_jump(else_jump, program);
            }
            Expr::Match { target, arms } => {
                self.compile_expr(*target, program);
                let mut done_jumps = vec![];
                for arm in arms.into_iter() {
                    program.add_instr(OP_COPY);
                    program.add_instr(0);
                    self.compile_expr(arm.case, program);
                    program.add_instr(OP_EQ);
                    let next_case_jump = self.emit_jump(OP_JUMP_IF_FALSE, program);
                    program.add_instr(OP_POP);
                    self.compile_expr(arm.expr, program);
                    done_jumps.push(self.emit_jump(OP_JUMP, program));
                    self.patch_jump(next_case_jump, program);
                    program.add_instr(OP_POP);
                }
                program.add_instr(OP_ERROR);
                done_jumps
                    .into_iter()
                    .for_each(|j| self.patch_jump(j, program));
                program.add_instr(OP_END_CASE);
            }
            Expr::Nil => program.add_instr(OP_NIL),
            Expr::Assign { start, end, value } => {
                self.compile_expr(*value, program);
                self.locals[self.locals_count] =
                    Local::new(get_str(start, end, self.buf), self.depth);
                self.locals_count += 1;
                program.add_instr(OP_DEFINE_LOCAL);
                program.add_instr(self.locals_count as u8 - 1);
            }
            Expr::Var { start, end } => {
                let distance = self.resolve_local(start, end).unwrap_or_else(|| {
                    panic!(
                        "Variable not found {} {} {:?}",
                        get_str(start, end, self.buf),
                        self.depth,
                        &self.locals[0..self.locals_count]
                    )
                });
                program.add_instr(OP_GET_LOCAL);
                program.add_instr(distance as u8);
            }
            Expr::Block(exprs) => {
                self.depth += 1;
                exprs
                    .into_iter()
                    .for_each(|expr| self.compile_expr(expr, program));
                program.add_instr(OP_END_BLOCK);
                self.depth -= 1;
                let mut depth = 0;
                while self.locals_count > 0 && self.locals[self.locals_count - 1].depth > self.depth
                {
                    depth += 1;
                    self.locals_count -= 1;
                }
                program.add_instr(depth as u8);
            }
        };
    }
}

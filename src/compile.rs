use crate::{
    parse::{Ast, Expr},
    value::Value,
    vm::{
        OP_CONST, OP_COPY, OP_DEFINE_GLOBAL, OP_END_CASE, OP_EQ, OP_ERROR, OP_FALSE, OP_GET_GLOBAL,
        OP_JUMP, OP_JUMP_IF_FALSE, OP_NIL, OP_POP, OP_RET, OP_TRUE,
    },
};

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

pub fn compile(ast: Ast) -> Program {
    let mut program = Program::default();
    match ast {
        Ast::Exprs(exprs) => exprs
            .into_iter()
            .for_each(|expr| compile_expr(expr, &mut program)),
    };
    program.add_instr(OP_RET);
    program
}

fn emit_jump(op: u8, program: &mut Program) -> usize {
    program.add_instr(op);
    program.add_instr(0xff);
    program.add_instr(0xff);
    program.instrs.len() - 2
}

fn patch_jump(offset: usize, program: &mut Program) {
    let jump = program.instrs.len() - offset - 2;

    if jump > u16::MAX as usize {
        eprintln!("Jump too high");
        std::process::exit(1);
    }

    *(unsafe { program.instrs.get_unchecked_mut(offset) }) = (jump >> 8) as u8;
    *(unsafe { program.instrs.get_unchecked_mut(offset + 1) }) = jump as u8;
}

fn compile_expr(expr: Expr, program: &mut Program) {
    match expr {
        Expr::I32(int) => program.add_const(Value::Int(int)),
        Expr::F32(float) => program.add_const(Value::Float(float)),
        Expr::Unary { op, arg } => {
            compile_expr(*arg, program);
            program.add_instr(op.to_byte());
        }
        Expr::Binary { op, src1, src2 } => {
            compile_expr(*src1, program);
            compile_expr(*src2, program);
            program.add_instr(op.to_byte());
        }
        Expr::String { start, end } => program.add_const(Value::String { start, end }),
        Expr::Bool(bool) => program.add_instr(if bool { OP_TRUE } else { OP_FALSE }),
        Expr::If {
            cond,
            then_expr,
            else_expr,
        } => {
            compile_expr(*cond, program);
            let then_jump = emit_jump(OP_JUMP_IF_FALSE, program);
            program.add_instr(OP_POP);
            compile_expr(*then_expr, program);
            let else_jump = emit_jump(OP_JUMP, program);
            patch_jump(then_jump, program);
            program.add_instr(OP_POP);
            compile_expr(*else_expr, program);
            patch_jump(else_jump, program);
        }
        Expr::Match { target, arms } => {
            compile_expr(*target, program);
            let mut done_jumps = vec![];
            for arm in arms.into_iter() {
                program.add_instr(OP_COPY);
                program.add_instr(0);
                compile_expr(arm.case, program);
                program.add_instr(OP_EQ);
                let next_case_jump = emit_jump(OP_JUMP_IF_FALSE, program);
                program.add_instr(OP_POP);
                compile_expr(arm.expr, program);
                done_jumps.push(emit_jump(OP_JUMP, program));
                patch_jump(next_case_jump, program);
                program.add_instr(OP_POP);
            }
            program.add_instr(OP_ERROR);
            done_jumps.into_iter().for_each(|j| patch_jump(j, program));
            program.add_instr(OP_END_CASE);
        }
        Expr::Nil => program.add_instr(OP_NIL),
        Expr::Assign { start, end, value } => {
            program.add_const(Value::String { start, end });
            compile_expr(*value, program);
            program.add_instr(OP_DEFINE_GLOBAL);
        }
        Expr::Var { start, end } => {
            program.add_const(Value::String { start, end });
            program.add_instr(OP_GET_GLOBAL);
        }
    };
}

use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use crate::{inner_write, vm::RuntimeErr};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    Nil,
    String { start: usize, end: usize },
}

impl Value {
    pub fn get_string<'a>(&self, buf: &'a [u8]) -> &'a str {
        let Value::String { start, end } = self else {
            unreachable!()
        };
        unsafe { std::str::from_utf8_unchecked(&buf[*start..*end]) }
    }

    fn datatype(&self) -> &str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "boolean",
            Value::String { start: _, end: _ } => "string",
            Value::Nil => "null",
        }
    }

    pub fn bool(&self) -> bool {
        match self {
            Value::Int(int) => *int != 0,
            Value::Float(float) => *float != 0.0,
            Value::Bool(bool) => *bool,
            Value::String { start, end } => *start < *end - 2,
            Value::Nil => false,
        }
    }

    pub fn eq(&self, other: &Self, buf: &[u8]) -> bool {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::Int(int), Value::Float(float)) => *int as f32 == *float,
            (Value::Int(_), _) => false,
            (Value::Float(float), Value::Int(int)) => *float == *int as f32,
            (Value::Float(l), Value::Float(r)) => l == r,
            (Value::Float(_), _) => false,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Bool(_), _) => false,
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (Value::String { start: sl, end: el }, Value::String { start: sr, end: er }) => {
                buf[*sl..*el] == buf[*sr..*er]
            }
            (Value::String { start: _, end: _ }, _) => false,
        }
    }

    pub fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => l > r,
            (Value::Int(int), Value::Float(float)) => *int as f32 > *float,
            (Value::Int(_), _) => false,
            (Value::Float(float), Value::Int(int)) => *float > *int as f32,
            (Value::Float(l), Value::Float(r)) => l > r,
            (Value::Float(_), _) => false,
            (Value::Bool(l), Value::Bool(r)) => l > r,
            (Value::Bool(_), _) => false,
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (Value::String { start: sl, end: el }, Value::String { start: sr, end: er }) => {
                el - sl > er - sr
            }
            (Value::String { start: _, end: _ }, _) => false,
        }
    }

    pub fn gte(&self, other: &Self, buf: &[u8]) -> bool {
        self.gt(other) || self.eq(other, buf)
    }

    pub fn lt(&self, other: &Self, buf: &[u8]) -> bool {
        !self.gte(other, buf)
    }

    pub fn lte(&self, other: &Self, buf: &[u8]) -> bool {
        self.lt(other, buf) || self.eq(other, buf)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => inner_write(int, f),
            Self::Float(float) => inner_write(float, f),
            Self::Bool(bool) => inner_write(bool, f),
            Self::String { start: _, end: _ } => inner_write("string", f),
            Self::Nil => inner_write("null", f),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, RuntimeErr>;

    fn add(self, rhs: Self) -> Self::Output {
        let result = match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
            (Value::Int(left), Value::Float(right)) => Value::Float(left as f32 + right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left + right as f32),
            (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot add {} and {} types",
                    self.datatype(),
                    rhs.datatype()
                )))
            }
        };
        Ok(result)
    }
}

impl Sub for Value {
    type Output = Result<Value, RuntimeErr>;

    fn sub(self, rhs: Self) -> Self::Output {
        let result = match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
            (Value::Int(left), Value::Float(right)) => Value::Float(left as f32 - right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left - right as f32),
            (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot subtract {} and {} types",
                    self.datatype(),
                    rhs.datatype()
                )))
            }
        };
        Ok(result)
    }
}

impl Mul for Value {
    type Output = Result<Value, RuntimeErr>;

    fn mul(self, rhs: Self) -> Self::Output {
        let result = match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
            (Value::Int(left), Value::Float(right)) => Value::Float(left as f32 * right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left * right as f32),
            (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot multiply {} and {} types",
                    self.datatype(),
                    rhs.datatype()
                )))
            }
        };

        Ok(result)
    }
}

impl Div for Value {
    type Output = Result<Value, RuntimeErr>;

    fn div(self, rhs: Self) -> Self::Output {
        let result = match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
            (Value::Int(left), Value::Float(right)) => Value::Float(left as f32 / right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left / right as f32),
            (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot divide {} and {} types",
                    self.datatype(),
                    rhs.datatype()
                )))
            }
        };

        Ok(result)
    }
}

impl Rem for Value {
    type Output = Result<Value, RuntimeErr>;

    fn rem(self, rhs: Self) -> Self::Output {
        let result = match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left % right),
            (Value::Int(left), Value::Float(right)) => Value::Float(left as f32 % right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left % right as f32),
            (Value::Float(left), Value::Float(right)) => Value::Float(left % right),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot calculate remainder of {} and {} types",
                    self.datatype(),
                    rhs.datatype()
                )))
            }
        };

        Ok(result)
    }
}

impl Neg for Value {
    type Output = Result<Value, RuntimeErr>;

    fn neg(self) -> Self::Output {
        let result = match self {
            Value::Int(int) => Value::Int(-int),
            Value::Float(float) => Value::Float(-float),
            _ => {
                return Err(RuntimeErr::new(format!(
                    "Cannot negate {} type",
                    self.datatype()
                )))
            }
        };

        Ok(result)
    }
}

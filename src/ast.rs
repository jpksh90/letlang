use std::fmt::{Display, Formatter};

use super::*;

#[allow(unused_variables)]
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    ExprStmt(Expr),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub struct Program {
    pub program: Vec<Stmt>,
}

impl From<Vec<Stmt>> for Program {
    fn from(value: Vec<Stmt>) -> Self {
        Program { program: value }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Name(String),
    IntLit(i64),
    FloatLit(f64),
    Complex(f64, f64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>),
    Sqrt(Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    LambdaCall(String, Vec<Expr>),
}

impl Program {
    pub(crate) fn new(p: Vec<Stmt>) -> Self {
        Program { program: p }
    }

    pub fn empty() -> Self {
        Program {
            program: Vec::new(),
        }
    }

    #[allow(unused)]
    pub(crate) fn add(&mut self, s: Stmt) {
        self.program.push(s);
    }

    pub(crate) fn program(&self) -> &Vec<Stmt> {
        &self.program
    }
}

impl Display for ast::Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::IntLit(x) => {
                write!(f, "{}", x)
            }
            Expr::Complex(r, i) => {
                write!(f, "complex({},{})", r, i)
            }
            Expr::Add(x, y) => write!(f, "{}+{}", x, y),
            Expr::Sub(x, y) => write!(f, "{}-{}", x, y),
            Expr::Mul(x, y) => write!(f, "{}*{}", x, y),
            Expr::Div(x, y) => write!(f, "{}/{}", x, y),
            Expr::Name(x) => write!(f, "{}", x),
            Expr::FloatLit(x) => {
                write!(f, "{}", x)
            }
            Expr::Mod(c) => {
                write!(f, "|{}|", c)
            }
            Expr::Sqrt(x) => write!(f, "√{}", x),
            Expr::Pow(x, y) => write!(f, "{}**{}", x, y),
            Expr::Lambda(vars, e) => write!(f, "ƛ{}.{}", vars.join(","), e),
            Expr::LambdaCall(e, args) => {
                write!(
                    f,
                    "{}({})",
                    e,
                    args.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Let(n, x) => write!(f, "{} {}", n, x),
            Stmt::ExprStmt(e) => write!(f, "{}", e),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "not implemented")
    }
}

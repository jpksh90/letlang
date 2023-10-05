use std::collections::HashMap;

use crate::{
    ast::{Expr, Program, Stmt},
    complex::Complex,
    eager_interpreter::{self, InterpretErrors, Interpreter},
    visit::Visitor,
};

pub enum InterpretResult {
    NOP,
    OP(Complex),
}

struct LazyInterpreter {
    symbol_table: HashMap<String, Expr>,
    output: Vec<Complex>,
}

impl LazyInterpreter {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            output: Vec::new(),
        }
    }

    pub fn interpret(&mut self, p: &Program) {
        for stmt in p.program() {
            self.interpret_statement(stmt)
        }
    }

    fn interpret_statement(&mut self, s: &Stmt) {
        match s {
            Stmt::Let(x, e) => {
                self.symbol_table.insert(x.to_string(), e.clone());
            }
            Stmt::ExprStmt(e) => self.output.push(self.evaluate(e)),
        };
    }

    fn rewrite_expr(&self, e: &Expr) -> Expr {
        match e {
            Expr::Name(x) => match self.symbol_table.get(x) {
                Some(x) => self.rewrite_expr(x),
                None => panic!("interpret error. cannot find variable {{{}}}", x),
            },
            Expr::IntLit(_) => e.clone(),
            Expr::FloatLit(_) => e.clone(),
            Expr::Complex(_, _) => e.clone(),
            Expr::Add(l, r) => Expr::Add(
                Box::new(self.rewrite_expr(l)),
                Box::new(self.rewrite_expr(r)),
            ),
            Expr::Sub(l, r) => Expr::Sub(
                Box::new(self.rewrite_expr(l)),
                Box::new(self.rewrite_expr(r)),
            ),
            Expr::Mul(l, r) => Expr::Mul(
                Box::new(self.rewrite_expr(l)),
                Box::new(self.rewrite_expr(r)),
            ),
            Expr::Div(l, r) => Expr::Div(
                Box::new(self.rewrite_expr(l)),
                Box::new(self.rewrite_expr(r)),
            ),
            Expr::Mod(l) => Expr::Mod(Box::new(self.rewrite_expr(l))),
            Expr::Sqrt(l) => Expr::Sqrt(Box::new(self.rewrite_expr(l))),
            Expr::Pow(l, r) => Expr::Div(
                Box::new(self.rewrite_expr(l)),
                Box::new(self.rewrite_expr(r)),
            ),
            Expr::Lambda(_, _) => e.clone(),
            Expr::LambdaCall(_, _) => e.clone(),
        }
    }

    fn evaluate(&self, e: &Expr) -> Complex {
        let expr = self.rewrite_expr(e);
        self.evaluate_expr(&expr)
    }

    fn evaluate_expr(&self, e: &Expr) -> Complex {
        match e {
            Expr::Name(_) => unreachable!(),
            Expr::IntLit(x) => Complex::from_real(*x as f64),
            Expr::FloatLit(x) => Complex::from_real(*x),
            Expr::Complex(r, i) => Complex::complex(*r, *i),
            Expr::Add(l, r) => self.evaluate_expr(l) + self.evaluate_expr(r),
            Expr::Sub(l, r) => self.evaluate_expr(l) + self.evaluate_expr(r),
            Expr::Mul(l, r) => self.evaluate_expr(l) + self.evaluate_expr(r),
            Expr::Div(l, r) => self.evaluate_expr(l) + self.evaluate_expr(r),
            Expr::Mod(l) => {
                let x = self.evaluate_expr(l);
                Complex::from_real(f64::sqrt(x.real() * x.real() + x.img() * x.img()))
            }
            Expr::Sqrt(l) => Complex::sqrt(self.evaluate_expr(l)),
            Expr::Pow(l, r) => self.evaluate_expr(l) + self.evaluate_expr(r),
            Expr::Lambda(_, _) => unreachable!(),
            Expr::LambdaCall(_, _) => unreachable!(),
        }
    }

    pub fn output(self) -> Vec<Complex> {
        self.output
    }
}

#[cfg(test)]
mod tests {
    use crate::parser;

    use super::*;

    #[test]
    fn lazy_eval_1() {
        let x = parser::parse("let x=5; let y=7; let z=x+y; z+x").unwrap();
        println!("{:?}", x);
        let mut interpreter = LazyInterpreter::new();
        interpreter.interpret(&x);
        let result = interpreter.output();
        assert_eq!(result, vec![Complex::from_real(17.0)])
    }

    #[test]
    fn lazy_eval_2() {
        let x = parser::parse("let x=9; let y=10; let z=x+5; z").unwrap();
        println!("{:?}", x);
        let mut interpreter = LazyInterpreter::new();
        interpreter.interpret(&x);
        let result = interpreter.output();
        assert_eq!(result, vec![Complex::from_real(14.0)])
    }
}

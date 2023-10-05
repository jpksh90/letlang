use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

use crate::{
    ast::Expr::Name,
    ast::{Expr, Program, Stmt},
    complex::Complex,
    visit::Visitor,
};

pub trait Interpreter {
    fn get_lambda_function(&self, id: &str) -> Option<&Expr>;
    fn get_symbol_table(&self, id: &str) -> Option<&Complex>;
}

pub struct EagerInterpreter {
    symbol_table: HashMap<String, Complex>,
    lambda_functions: HashMap<String, Expr>,
}

impl EagerInterpreter {
    pub fn new() -> Self {
        EagerInterpreter {
            symbol_table: HashMap::new(),
            lambda_functions: HashMap::new(),
        }
    }
}

impl Interpreter for EagerInterpreter {
    fn get_lambda_function(&self, id: &str) -> Option<&Expr> {
        self.lambda_functions.get(id)
    }

    fn get_symbol_table(&self, id: &str) -> Option<&Complex> {
        self.symbol_table.get(id)
    }
}

#[derive(PartialEq, Debug, Error)]
pub enum InterpretErrors {
    #[error("Divide by zero in {}", .0)]
    DivideByZero(String),
    #[error("Failed to find variable {}", .0)]
    FailedToFindVar(String),
    #[error("Negative value of sqrt {}", .0)]
    NegSqrt(String),
    #[error("Unknown Error")]
    Unknown,
}

pub(crate) fn evaluate(
    interpreter: &mut impl Interpreter,
    e: &Expr,
) -> Result<Complex, InterpretErrors> {
    match e {
        Expr::IntLit(n) => Ok(Complex::complex(*n as f64, 0 as f64)),
        Expr::Complex(x, y) => Ok(Complex::complex(*x, *y)),
        Expr::Add(ref lhs, ref rhs) => arithmetic_op(
            evaluate(interpreter, lhs),
            evaluate(interpreter, rhs),
            |a, b| Ok(a + b),
        ),
        Expr::Sub(ref lhs, ref rhs) => arithmetic_op(
            evaluate(interpreter, lhs),
            evaluate(interpreter, rhs),
            |a, b| Ok(a - b),
        ),
        Expr::Mul(ref lhs, ref rhs) => arithmetic_op(
            evaluate(interpreter, lhs),
            evaluate(interpreter, rhs),
            |a, b| Ok(a * b),
        ),
        Expr::Div(ref lhs, ref rhs) => arithmetic_op(
            evaluate(interpreter, lhs),
            evaluate(interpreter, rhs),
            |a, b| {
                if b == Complex::zero() {
                    Err(InterpretErrors::DivideByZero(e.to_string()))
                } else {
                    Ok(a / b)
                }
            },
        ),
        Expr::Name(x) => {
            let v = interpreter.get_symbol_table(x);
            match v {
                Some(_) => Ok(*v.unwrap()),
                None => Err(InterpretErrors::FailedToFindVar(x.to_string())),
            }
        }
        Expr::FloatLit(x) => Ok(Complex::complex(*x, 0.0)),
        Expr::Mod(c) => evaluate(interpreter, c),
        Expr::Sqrt(x) => match evaluate(interpreter, x) {
            Err(t) => Err(t),
            Ok(t) => {
                if t.real() < 0.0 {
                    Err(InterpretErrors::NegSqrt(x.to_string()))
                } else {
                    Ok(t.sqrt())
                }
            }
        },
        Expr::Pow(x, p) => arithmetic_op(
            evaluate(interpreter, x),
            evaluate(interpreter, p),
            |_x, _p| {
                Ok(Complex::zero()) // FIXME
            },
        ),
        Expr::Lambda(_vars, _expr) => unreachable!("cannot reach here"),
        Expr::LambdaCall(f, args) => {
            let function = interpreter.get_lambda_function(f);
            match function {
                Some(e) => {
                    let formal_args = extract_variables_lambda(e);
                    let actual_args = args;
                    let map: HashMap<String, Expr> = formal_args
                        .iter()
                        .zip(actual_args)
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect();
                    let eval_expr = rewrite_args_in_lambda_expr(e, &map);
                    evaluate(interpreter, &eval_expr)
                }
                None => Err(InterpretErrors::FailedToFindVar(f.to_string())),
            }
        }
    }
}

type InterpreterResult = Result<Complex, InterpretErrors>;

fn arithmetic_op(
    l: InterpreterResult,
    r: InterpreterResult,
    op: impl Fn(Complex, Complex) -> InterpreterResult,
) -> InterpreterResult {
    let lval = l?;
    let rval = r?;
    op(lval, rval)
}

pub(crate) fn rewrite_args_in_lambda_expr(
    expr: &Expr,
    formal_actual_arg_map: &HashMap<String, Expr>,
) -> Expr {
    match expr {
        Expr::Name(x) => {
            if formal_actual_arg_map.contains_key(x) {
                formal_actual_arg_map.get(x).unwrap().clone()
            } else {
                Name(x.to_string())
            }
        }
        Expr::IntLit(_) => expr.clone(),
        Expr::FloatLit(_) => expr.clone(),
        Expr::Complex(_, _) => expr.clone(),
        Expr::Add(l, r) => Expr::Add(
            Box::new(rewrite_args_in_lambda_expr(l, formal_actual_arg_map)),
            Box::new(rewrite_args_in_lambda_expr(r, formal_actual_arg_map)),
        ),
        Expr::Sub(l, r) => Expr::Sub(
            Box::new(rewrite_args_in_lambda_expr(l, formal_actual_arg_map)),
            Box::new(rewrite_args_in_lambda_expr(r, formal_actual_arg_map)),
        ),
        Expr::Mul(l, r) => Expr::Mul(
            Box::new(rewrite_args_in_lambda_expr(l, formal_actual_arg_map)),
            Box::new(rewrite_args_in_lambda_expr(r, formal_actual_arg_map)),
        ),
        Expr::Div(l, r) => Expr::Div(
            Box::new(rewrite_args_in_lambda_expr(l, formal_actual_arg_map)),
            Box::new(rewrite_args_in_lambda_expr(r, formal_actual_arg_map)),
        ),
        Expr::Pow(l, r) => Expr::Pow(
            Box::new(rewrite_args_in_lambda_expr(l, formal_actual_arg_map)),
            Box::new(rewrite_args_in_lambda_expr(r, formal_actual_arg_map)),
        ),
        Expr::Mod(l) => Expr::Mod(Box::new(rewrite_args_in_lambda_expr(
            l,
            formal_actual_arg_map,
        ))),
        Expr::Sqrt(l) => Expr::Sqrt(Box::new(rewrite_args_in_lambda_expr(
            l,
            formal_actual_arg_map,
        ))),
        Expr::Lambda(_, _) => unimplemented!(),
        Expr::LambdaCall(_, _) => unimplemented!(),
    }
}

fn extract_variables_lambda(l: &Expr) -> Vec<String> {
    match l {
        Expr::Lambda(v, _) => v.to_vec(),
        _ => [].to_vec(),
    }
}

impl Visitor<InterpreterResult> for EagerInterpreter {
    fn visit_stmt(&mut self, s: &Stmt) -> InterpreterResult {
        match s {
            Stmt::Let(x, ref e) => match e {
                Expr::Lambda(_vars, _expr) => {
                    self.lambda_functions.insert(x.to_string(), e.clone());
                    Ok(Complex::zero())
                }
                _ => {
                    let v = self.visit_expr(e);
                    match v {
                        Ok(value) => self.symbol_table.insert(x.to_string(), value),
                        Err(_e) => unreachable!("this"),
                    };
                    Ok(Complex::zero())
                }
            },
            Stmt::ExprStmt(x) => self.visit_expr(x),
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> InterpreterResult {
        evaluate(self, e)
    }

    fn visit_prog(&mut self, p: &Program) -> InterpreterResult {
        let mut val = Complex::zero();
        let stmts = p.program();

        for s in stmts {
            let result = self.visit_stmt(s);
            val = result?;
        }
        Ok(val)
    }
}

#[cfg(test)]
mod tests {
    use super::Complex;
    use crate::{
        ast::{Expr::*, Stmt},
        eager_interpreter::{InterpretErrors, InterpreterResult},
        printer::PrettyPrinter,
        walk_program, EagerInterpreter, Program,
    };

    #[test]
    fn test_expr() {
        let expr = Add(
            Box::new(Name("x".to_string())),
            Box::new(Name("y".to_string())),
        );

        let program = vec![
            Stmt::Let("x".to_string(), IntLit(5)),
            Stmt::Let("y".to_string(), IntLit(6)),
            Stmt::Let("z".to_string(), expr),
            Stmt::ExprStmt(Add(Box::new(Name("z".to_string())), Box::new(IntLit(5)))),
        ];
        let p = Program::new(program);
        check_program(&p, Ok(Complex::from_real(16 as f64)));
    }

    #[test]
    fn test_expr_2() {
        let expr = Add(
            Box::new(Name("x".to_string())),
            Box::new(Name("y".to_string())),
        );

        let program = vec![
            Stmt::Let("x".to_string(), IntLit(5)),
            Stmt::Let("y".to_string(), IntLit(6)),
            Stmt::Let("z".to_string(), expr),
            Stmt::ExprStmt(Mul(Box::new(Name("k".to_string())), Box::new(IntLit(5)))),
        ];
        let p = Program::new(program);
        check_program(&p, Err(InterpretErrors::FailedToFindVar(String::from("k"))));
    }

    #[test]
    fn test_expr_3() {
        let program = Program::new(vec![
            Stmt::Let(
                "x".to_string(),
                Add(
                    Box::new(FloatLit(5.3)),
                    Box::new(Sub(Box::new(FloatLit(6.0)), Box::new(IntLit(5)))),
                ),
            ),
            Stmt::ExprStmt(Name("x".to_string())),
        ]);
        check_program(&program, Ok(Complex::from_real(6.3)));
    }

    #[test]
    fn divide_by_zero() {
        let program = Program::new(vec![
            Stmt::Let("x".to_string(), IntLit(5)),
            Stmt::Let(
                "y".to_string(),
                Sub(
                    Box::new(Name("x".to_string())),
                    Box::new(Name("x".to_string())),
                ),
            ),
            Stmt::ExprStmt(Div(
                Box::new(Name("x".to_string())),
                Box::new(Name("y".to_string())),
            )),
        ]);
        check_program(
            &program,
            Err(InterpretErrors::DivideByZero(String::from("x/y"))),
        );
    }

    fn check_program(p: &Program, e: InterpreterResult) {
        let val = walk_program(&p, &mut EagerInterpreter::new());
        println!("-------------");
        match &val {
            Ok(x) => println!("result={}", x),
            Err(e) => {
                walk_program(&p, &mut PrettyPrinter {});
                println!("{}", e);
            }
        };
        assert_eq!(val, e);
    }
}

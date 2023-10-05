use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::borrow::Cow;
use std::iter::Peekable;
use std::{process::id, vec};
use thiserror::Error;

use crate::{
    ast::{
        Expr::{self, Add, Div, FloatLit, IntLit, Mul, Name, Sub},
        Program,
        Stmt::{self, ExprStmt, Let},
    },
    complex::Complex,
    parser::ParseError::MalformedLetExpr,
};

#[allow(dead_code)]
#[derive(Debug, Error, PartialEq)]
pub enum ParseError<'a> {
    #[error("variable not found {}", .0)]
    VarNotFound(&'a str),
    #[error("malformed let expression {}", .0)]
    MalformedLetExpr(&'a str),
    #[error("duplicate variables")]
    DuplicateVars(&'a str),
    #[error("invalid complex number")]
    InvalidComplex(&'a str),
}

pub enum Token {
    Integer(String),
    Real(String),
    Complex(String),
    Operator(String),
}

#[derive(FromPrimitive)]
pub enum Operator {
    Plus = 1,
}

pub fn parse(program: &str) -> Result<Program, ParseError> {
    let p = parse_program(program);
    p
}

pub fn parse_program(program: &str) -> Result<Program, ParseError> {
    let stmts = program.split(';');
    let mut pgm_vec = Vec::new();
    for stmt in stmts {
        let t = parse_stmt(stmt.trim())?;
        pgm_vec.push(t);
    }
    Ok(Program::new(pgm_vec))
}

pub(crate) fn parse_stmt(program: &str) -> Result<Stmt, ParseError> {
    if program.starts_with("let") {
        let p = program.find("let");
        parse_let(&program[p.unwrap() + 3..])
    } else {
        parse_expr_stmt(program)
    }
}

pub(crate) fn parse_let(program: &str) -> Result<Stmt, ParseError> {
    let index_eq = program.find('=');
    match index_eq {
        None => Err(MalformedLetExpr(program)),
        Some(idx) => {
            let name: String = program[0..idx].trim().to_string();
            let expr = parse_expr(&program[idx + 1..]).unwrap();
            Ok(Let(name.to_string(), expr))
        }
    }
}

fn parse_expr_stmt(program: &str) -> Result<Stmt, ParseError> {
    let e = parse_expr(program)?;
    Ok(ExprStmt(e))
}

fn parse_complex_nb(program: &str) -> Result<Expr, ParseError> {
    let s = program.split(',').collect::<Vec<&str>>();
    if s.len() != 2 {
        Err(ParseError::InvalidComplex("invalid complex number"))
    } else {
        Ok(Expr::Complex(1.0, 1.0))
    }
}

fn parse_expr(program: &str) -> Result<Expr, ParseError> {
    if program.trim().starts_with("lambda") {
        parse_lambda_expr(program.trim())
    } else if program.trim().starts_with("complex(") {
        parse_complex(program)
    } else if is_id(program) {
        Ok(Name(program.trim().to_string()))
    } else if program.trim().chars().all(|x| x.is_numeric()) {
        match program.trim().parse::<i64>() {
            Ok(x) => Ok(IntLit(x)),
            Err(e) => Err(ParseError::MalformedLetExpr("Invalid integer")),
        }
    } else if is_floating_point(program) {
        if program.trim().chars().filter(|x| x == &'.').count() != 1 {
            Err(MalformedLetExpr("invalid float expression"))
        } else {
            Ok(FloatLit(program.trim().parse::<f64>().unwrap()))
        }
    } else {
        let index = program.find(|c: char| ['+', '-', '*', '/', '\n', ';'].contains(&c));
        match index {
            None => Err(MalformedLetExpr("cannot find operator")),
            Some(i) => {
                let lhs = parse_expr(program[..i].trim()).unwrap();
                let rhs = parse_expr(program[i + 1..].trim()).unwrap();
                match program.chars().nth(i).unwrap() {
                    '+' => Ok(Add(Box::new(lhs), Box::new(rhs))),
                    '-' => Ok(Sub(Box::new(lhs), Box::new(rhs))),
                    '*' => Ok(Mul(Box::new(lhs), Box::new(rhs))),
                    '/' => Ok(Div(Box::new(lhs), Box::new(rhs))),
                    _ => unimplemented!("unreachable statement"),
                }
            }
        }
    }
}

fn is_floating_point(program: &str) -> bool {
    program.trim().chars().all(|x| x.is_numeric() || x == '.')
}

fn is_id(program: &str) -> bool {
    program.trim().chars().all(char::is_alphabetic)
}

fn parse_complex(program: &str) -> Result<Expr, ParseError<'_>> {
    let begin = program.find('(');
    let end = program.find(')');
    match (begin, end) {
        (Some(b), Some(e)) => parse_complex_nb(&program[b..e]),
        _ => Err(ParseError::InvalidComplex("invalid expr")),
    }
}

fn parse_lambda_expr(program: &str) -> Result<Expr, ParseError<'_>> {
    let p = program[7..].trim();
    let open_paren_pos = p.find('(');
    if p.starts_with('(') {
        let index_close_paren = p.find(')');
        let args = parse_lambda_expr_args(
            p[open_paren_pos.unwrap() + 1..index_close_paren.unwrap()].trim(),
        );
        let expr = parse_expr(p[index_close_paren.unwrap() + 1..].trim());
        match expr {
            Ok(e) => Ok(Expr::Lambda(args, Box::new(e))),
            Err(_) => expr,
        }
    } else {
        Err(MalformedLetExpr("invalid lambda expr"))
    }
}

fn parse_lambda_expr_args(args: &str) -> Vec<String> {
    let arguments = args.split(',').map(|x| x.to_string());
    arguments.collect()
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr;
    use crate::ast::Expr::{Add, Mul, Sub};
    use crate::ast::Expr::{IntLit, Name};
    use crate::ast::Program;
    use crate::ast::Stmt::{ExprStmt, Let};
    use crate::parser::{parse_expr, parse_let, parse_program, parse_stmt};

    use super::{parse_lambda_expr, Tokenizer};

    #[test]
    fn test_let_ok_1() {
        assert_eq!(
            parse_let("x = 3 + 4"),
            Ok(Let(
                "x".to_string(), //expression
                Expr::Add(Box::new(IntLit(3)), Box::new(IntLit(4)),),
            ))
        );
    }

    #[test]
    fn test_stmt_let_ok_1() {
        assert_eq!(
            parse_stmt("let x = 3 + 4"),
            Ok(Let(
                "x".to_string(), //expression
                Expr::Add(Box::new(IntLit(3)), Box::new(IntLit(4)),),
            ))
        );
    }

    #[test]
    fn test_expr_stmt_ok_1() {
        assert_eq!(
            parse_stmt("3+4"),
            Ok(ExprStmt(Expr::Add(
                Box::new(IntLit(3)),
                Box::new(IntLit(4)),
            ),))
        );
    }

    #[test]
    fn test_let_ok_2() {
        assert_eq!(
            parse_stmt("let x = y - 5"),
            Ok(Let(
                "x".to_string(),
                Expr::Sub(Box::new(Name("y".to_string())), Box::new(IntLit(5)),),
            ))
        )
    }

    #[test]
    fn test_expr_1() {
        assert_eq!(parse_expr("x "), Ok(Name("x".to_string())));
    }

    #[test]
    fn text_expr_2() {
        assert_eq!(
            parse_expr("x + y"),
            Ok(Add(
                Box::new(Name("x".to_string())),
                Box::new(Name("y".to_string()))
            ))
        );
    }

    #[test]
    fn test_expr_3() {
        assert_eq!(
            parse_expr("x + y - z"),
            Ok(Add(
                Box::new(Name("x".to_string())),
                Box::new(Sub(
                    Box::new(Name("y".to_string())),
                    Box::new(Name("z".to_string()))
                ))
            ))
        );
    }

    #[test]
    fn test_program() {
        let expr = Add(
            Box::new(Name("x".to_string())),
            Box::new(Name("y".to_string())),
        );

        let mut program = Vec::new();
        program.push(Let("x".to_string(), IntLit(5)));
        program.push(Let("y".to_string(), IntLit(6)));
        program.push(Let("z".to_string(), expr));
        program.push(ExprStmt(Mul(
            Box::new(Name("k".to_string())),
            Box::new(IntLit(5)),
        )));

        let expr = "let x = 5;
        let y = 6;
        let z = x+y;
        k*5";

        assert_eq!(parse_program(expr), Ok(Program::new(program)))
    }

    #[test]
    fn test_lambda_expr() {
        let expr = Expr::Lambda(
            vec!["a", "b"].iter().map(|x| x.to_string()).collect(),
            Box::new(Add(
                Box::new(Name("a".to_string())),
                Box::new(Name("b".to_string())),
            )),
        );
        assert_eq!(parse_lambda_expr("lambda (a,b) a + b"), Ok(expr))
    }

    #[test]
    fn test_lambda_expr_1() {
        let le = Expr::Lambda(
            vec!["x".to_string()],
            Box::new(Add(Box::new(Name("x".to_string())), Box::new(IntLit(1)))),
        );
        let expr = Let("x".to_string(), le);
        assert_eq!(parse_stmt("let x = lambda (x) x+1"), Ok(expr))
    }
}

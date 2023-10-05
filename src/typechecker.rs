use std::collections::HashMap;

use thiserror::Error;

use visit::Visitor;

use crate::ast::{Expr, Stmt};
use crate::{visit, Program};

#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub(crate) enum Type {
    // all let expression and statements are of type void
    Int,
    Float,
    Complex,
    Void,
}

pub(crate) struct TypeChecker {
    pub(crate) symbol_table: HashMap<String, Type>,
}

pub(crate) type TypeCheckerResult = Result<Type, TypeInterpretErrors>;

impl TypeChecker {
    pub(crate) fn check_valid_compatible_types(l: Type, r: Type) -> TypeCheckerResult {
        match (l, r) {
            (Type::Complex, Type::Complex) => Ok(Type::Complex),
            (Type::Complex, _) => Err(TypeInterpretErrors::OneIsComplex),
            (_, Type::Complex) => Err(TypeInterpretErrors::OneIsComplex),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (_, Type::Float) => Ok(Type::Float),
            (Type::Float, _) => Ok(Type::Float),
            (_, Type::Void) => unreachable!("impossible"),
            (Type::Void, _) => unreachable!("impossible"),
        }
    }

    pub fn new() -> Self {
        TypeChecker {
            symbol_table: HashMap::new(),
        }
    }
}

#[derive(PartialEq, Error, Debug)]
pub(crate) enum TypeInterpretErrors {
    #[error("ERROR: failed to find {}", .0)]
    FailToSearchName(String),
    #[error("ERROR: type mismatch; only one of the operands is complex")]
    OneIsComplex,
    #[error("ERROR: mod operation expects a complex operand")]
    ModOperandNotComplex,
    #[error("ERROR: one or many InterpretErrors detected")]
    InterpretErrorsDetected,
}

impl Visitor<TypeCheckerResult> for TypeChecker {
    fn visit_stmt(&mut self, s: &Stmt) -> TypeCheckerResult {
        match s {
            Stmt::Let(n, e) => match self.visit_expr(e) {
                Ok(t) => {
                    self.symbol_table.insert(n.to_string(), t);
                    Ok(Type::Void)
                }
                Err(err) => Err(err),
            },
            Stmt::ExprStmt(e) => match self.visit_expr(e) {
                Ok(_) => Ok(Type::Void),
                Err(e) => {
                    println!("ERROR: {}", e);
                    Err(e)
                }
            },
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> TypeCheckerResult {
        let check_type_results = |ltype, rtype| match (ltype, rtype) {
            (Ok(t1), Ok(t2)) => TypeChecker::check_valid_compatible_types(t1, t2),
            (Ok(_), Err(e)) => Err(e),
            (Err(e), Ok(_)) => Err(e),
            (Err(e), Err(v)) => {
                println!("ERROR: {}", e);
                println!("ERROR: {}", v);
                Err(TypeInterpretErrors::InterpretErrorsDetected)
            }
        };

        match e {
            Expr::Name(ref x) => {
                if self.symbol_table.contains_key(x) {
                    Ok(*self.symbol_table.get(x).unwrap())
                } else {
                    Err(TypeInterpretErrors::FailToSearchName(x.to_string()))
                }
            }
            Expr::IntLit(_) => Ok(Type::Int),
            Expr::FloatLit(_) => Ok(Type::Float),
            Expr::Complex(_, _) => Ok(Type::Complex),
            Expr::Add(l, r) | Expr::Sub(l, r) | Expr::Mul(l, r) | Expr::Div(l, r) => {
                check_type_results(self.visit_expr(l), self.visit_expr(r))
            }
            Expr::Mod(t) => match self.visit_expr(t) {
                Ok(Type::Complex) => Ok(Type::Float),
                Ok(_) => Err(TypeInterpretErrors::ModOperandNotComplex),
                Err(e) => Err(e),
            },
            Expr::Sqrt(x) => match self.visit_expr(x) {
                Ok(Type::Float) | Ok(Type::Int) => self.visit_expr(x),
                _ => Err(TypeInterpretErrors::InterpretErrorsDetected),
            },
            Expr::Pow(x, p) => check_type_results(self.visit_expr(x), self.visit_expr(p)),
            Expr::Lambda(_vars, _expr) => todo!(),
            Expr::LambdaCall(_, _) => todo!(),
        }
    }

    fn visit_prog(&mut self, p: &Program) -> Result<Type, TypeInterpretErrors> {
        let mut t = p.program().iter().map(|s| self.visit_stmt(s));
        if t.all(|s| matches!(s, Ok(Type::Void))) {
            Ok(Type::Void)
        } else {
            Err(TypeInterpretErrors::InterpretErrorsDetected)
        }
    }
}

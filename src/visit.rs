use crate::ast::{Expr, Program, Stmt};

pub trait Visitor<T> {
    fn visit_stmt(&mut self, s: &Stmt) -> T;
    fn visit_expr(&mut self, e: &Expr) -> T;
    fn visit_prog(&mut self, p: &Program) -> T;
}

pub fn walk_program<T>(p: &Program, visitor: &mut impl Visitor<T>) -> T {
    visitor.visit_prog(p)
}

#[allow(unused)]
pub fn walk_expression<T>(e: &Expr, visitor: &mut impl Visitor<T>) -> T {
    visitor.visit_expr(e)
}

#[allow(unused)]
pub fn walk_statement<T>(s: &Stmt, visitor: &mut impl Visitor<T>) -> T {
    visitor.visit_stmt(s)
}

use crate::{
    ast::{Expr, Program, Stmt},
    visit::{walk_program, Visitor},
};

pub struct PrettyPrinter;

impl Visitor<()> for PrettyPrinter {
    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Let(x, e) => print!("let {} = {}; ", x, e),
            Stmt::ExprStmt(e) => print!("{}; ", e),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        print!("{}", e)
    }

    fn visit_prog(&mut self, p: &Program) {
        for s in p.program() {
            self.visit_stmt(s);
        }
    }
}

pub fn pretty_print(p: &Program) {
    walk_program(p, &mut PrettyPrinter {})
}

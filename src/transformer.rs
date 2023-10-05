use crate::{
    ast::Expr::*,
    ast::Stmt::*,
    ast::{Expr, Program, Stmt},
};

struct Transformer;

impl Transformer {
    fn visit_stmt(&self, s: &Stmt) -> Stmt {
        match s {
            Let(x, e) => Let(x.to_string(), self.visit_expr(e)),
            ExprStmt(e) => ExprStmt(self.visit_expr(e)),
        }
    }

    fn visit_expr(&self, e: &Expr) -> Expr {
        match e {
            Add(a, b) => Mul(a.clone(), b.clone()),
            Sub(a, b) => Div(a.clone(), b.clone()),
            Mul(a, b) => Add(a.clone(), b.clone()),
            Div(a, b) => Sub(a.clone(), b.clone()),
            _ => e.clone(),
        }
    }

    fn visit_prog(&self, p: &Program) -> Program {
        let mut new_program = Program::empty();
        for s in p.program() {
            new_program.add(self.visit_stmt(s));
        }
        new_program
    }
}

pub fn transform(p: Program) -> Program {
    let t = Transformer {};
    t.visit_prog(&p)
}

use crate::ast::{
    Expr::{self, *},
    Program,
    Stmt::{self, ExprStmt, Let},
};

fn visit_stmt(s: &Stmt) -> Stmt {
    match &s {
        Let(x, e) => Let(x.to_string(), visit_expr(e)),
        ExprStmt(e) => ExprStmt(visit_expr(e)),
    }
}

fn visit_expr_in_lambda(e: &Expr, vars: &Vec<String>) -> Expr {
    match e {
        Name(x) => {
            if vars.contains(x) {
                Name(x.to_string())
            } else {
                e.clone()
            }
        }
        IntLit(x) => IntLit(*x),
        Expr::FloatLit(x) => Expr::FloatLit(*x),
        Expr::Complex(r, i) => Expr::Complex(*r, *i),
        Add(l, r) => Add(
            Box::new(visit_expr_in_lambda(l, vars)),
            Box::new(visit_expr_in_lambda(r, vars)),
        ),
        Expr::Sub(l, r) => Sub(
            Box::new(visit_expr_in_lambda(l, vars)),
            Box::new(visit_expr_in_lambda(r, vars)),
        ),
        Expr::Mul(l, r) => Mul(
            Box::new(visit_expr_in_lambda(l, vars)),
            Box::new(visit_expr_in_lambda(r, vars)),
        ),
        Expr::Div(l, r) => Div(
            Box::new(visit_expr_in_lambda(l, vars)),
            Box::new(visit_expr_in_lambda(r, vars)),
        ),
        Expr::Mod(r) => Mod(Box::new(visit_expr_in_lambda(r, vars))),
        Expr::Sqrt(r) => Mod(Box::new(visit_expr_in_lambda(r, vars))),
        Expr::Pow(l, r) => Div(
            Box::new(visit_expr_in_lambda(l, vars)),
            Box::new(visit_expr_in_lambda(r, vars)),
        ),
        Lambda(vars, e) => {
            let v = vars
                .iter()
                .map(|x| {
                    let s = "__";
                    s.to_owned().push_str(x);
                    s.to_string()
                })
                .collect();
            Lambda(v, Box::new(visit_expr_in_lambda(e, vars)))
        }
        Expr::LambdaCall(x, e) => LambdaCall(x.clone(), e.clone()),
    }
}

fn visit_expr(e: &Expr) -> Expr {
    match e {
        Lambda(vars, t) => {
            let v = vars
                .iter()
                .map(|x| {
                    let s = "__";
                    s.to_owned().push_str(x);
                    s.to_string()
                })
                .collect();
            Lambda(v, Box::new(visit_expr_in_lambda(t, vars)))
        }
        _ => e.clone(),
    }
}

fn visit_program(p: Program) -> Program {
    let mut out = Program::empty();
    for s in p.program() {
        out.add(visit_stmt(s));
    }
    out
}

pub(crate) fn rename(p: Program) -> Program {
    visit_program(p)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use parser::parse_stmt;

    use super::*;
    use crate::parser;

    #[test]
    fn test() {
        let e = parse_stmt("let x = lambda (x) x+1");
        let renamed_lamda = Lambda(
            vec!["__x".to_string()],
            Box::new(Add(Box::new(Name("__x".to_string())), Box::new(IntLit(1)))),
        );
        let renamed_expr = Let("x".to_string(), renamed_lamda);
        assert_eq!(renamed_expr, visit_stmt(&e.unwrap()));
    }
}

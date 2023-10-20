use std::io;
use std::process::exit;

use ast::Program;
use eager_interpreter::EagerInterpreter;
use typechecker::TypeChecker;
use visit::walk_program;

#[macro_use]
extern crate fix_hidden_lifetime_bug;

use crate::complex::Complex;
use crate::printer::pretty_print;
use crate::transformer::transform;

mod ast;
mod complex;
mod eager_interpreter;
mod lambda_renamer;
mod lazy_interpreter;
mod parser;
mod printer;
mod tokenizer;
mod transformer;
mod typechecker;
mod visit;

fn main() {
    let mut buffer = String::new();
    loop {
        let _ = io::stdin().read_line(&mut buffer);

        if buffer.eq("quit") {
            exit(0);
        }

        let mut program = match parser::parse(&buffer) {
            Ok(x) => x,
            Err(_e) => Program::empty(),
        };

        let _ = match walk_program(&program, &mut TypeChecker::new()) {
            Ok(_) => Ok(()),
            Err(t) => Err(println!("errors {}", t)),
        };

        program = lambda_renamer::rename(program);
        if rand::random() {
            print!("I will compute something beddar (courtesy: shelbys.ca) ==> ");
            program = transform(program);
            pretty_print(&program);
            println!()
        }

        let mut interpreter: EagerInterpreter = EagerInterpreter::new();
        let res: Result<Complex, eager_interpreter::InterpretErrors> =
            walk_program(&program, &mut interpreter);
        match res {
            Ok(r) => println!("{}", r),
            Err(t) => println!("{}", t),
        }
        buffer.clear();
    }
}

// let expr = Add(
//     Box::new(Name(String::from("x"))),
//     Box::new(Name(String::from("y"))),
// );

// let program = vec![
//     Let(String::from("x"), IntLit(5)),
//     Let(String::from("y"), IntLit(6)),
//     Let(String::from("z"), expr),
//     Stmt::ExprStmt(crate::Expr::Mul(
//         Box::new(Name(String::from("z"))),
//         Box::new(IntLit(5)),
//     )),
// ];

// let p = Program::new(program);
// walk_program(&p, &mut PrettyPrinter {});

// match walk_program(&p, &mut Interpreter::new()) {
//     Ok(x) => println!("{}", x),
//     Err(e) => println!("ERROR: {}", e.to_string()),
// };

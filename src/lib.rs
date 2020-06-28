pub mod scanner;
pub mod token;
pub mod ast;
pub mod parser;
pub mod interpreter;

use std::fs;
use std::io;
use std::io::prelude::*;

pub fn print_ast_dummy() {
    let expr_kind = ast::ExprKind::LitExpr(ast::Lit::Double(4.2));
    let expr = Box::new(ast::Expr{ expr_kind });

    let expr_kind = ast::ExprKind::ParenExpr(expr);
    let expr = Box::new(ast::Expr{expr_kind});
    print_ast(&expr);
}

pub fn print_ast(root: & ast::Expr) {
    let mut printer = ast::AstPrinter::new(root);
    printer.print();
    println!("{}", &printer.ast_print);
}

pub enum Error {
    ScannerError,
    ParserError,
    RuntimeError,
}

pub fn run_file(fname: &str) -> Result<(), Error> {
    let content = fs::read_to_string(fname).expect("error reading the file");
    run(content)
}

pub fn run_prompt() -> Result<(), io::Error> {
    loop {
        let mut line = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        run(line).unwrap_or(());
    }
}

fn run(source: String) -> Result<(), Error> {
    let mut scanner = scanner::Scanner::new(&source);
    scanner.scan_tokens();
    if scanner.has_error {
	return Err(Error::ScannerError);
    }

    let mut parser = parser::Parser::new(&scanner.tokens);
    match parser.parse() {
	Some(ast) => {
	    let mut interpreter = interpreter::Interpreter::new();
	    interpreter.run(&ast);
	    if interpreter.has_error {
		return Err(Error::RuntimeError);
	    }
	    return Ok(());
	}
	None => {
	    return Err(Error::ParserError);
	}
    }
}

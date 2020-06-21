use loxi::scanner;
use loxi::parser;
use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        print_help();
        process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt().unwrap_or_else(|error| {
            println!("error reading input: {}", error);
            process::exit(1);
        });
    }
}

fn print_help() {
    let help_msg = r#"
Usage: loxi [script]
"#;
    println!("{}", help_msg);
}

fn run_file(fname: &str) {
    let content = fs::read_to_string(fname).expect("error reading the file");
    run(content);
}

fn run_prompt() -> Result<(), io::Error> {
    loop {
        let mut line = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        run(line);
    }
}

fn run(source: String) {
    let mut scanner = scanner::Scanner::new(&source);
    scanner.scan_tokens();
    println!( "{:?}", scanner.tokens );
    let mut parser = parser::Parser::new(&scanner.tokens);
    if let Some(ast) = parser.parse() {
	loxi::print_ast(&ast);
    }
}

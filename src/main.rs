use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        print_help();
        process::exit(64);
    } else if args.len() == 2 {
        loxi::run_file(&args[1]).unwrap_or_else(|error| {
	    match error {
		loxi::Error::ScannerError |
		loxi::Error::ParserError => process::exit(65),
		loxi::Error::RuntimeError => process::exit(70),
	    }
	});
    } else {
        loxi::run_prompt().unwrap_or_else(|error| {
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

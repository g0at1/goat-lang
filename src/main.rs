mod ast;
mod lexer;
mod parser;
mod runtime;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

use parser::Parser;
use runtime::{eval, Value};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.goat>", args[0]);
        process::exit(1);
    }

    let src = fs::read_to_string(&args[1]).unwrap_or_else(|e| {
        eprintln!("Error: {}", e);
        process::exit(1)
    });

    let mut parser = Parser::new(&src);
    let stmts = parser.parse();

    let mut env: HashMap<String, Value> = HashMap::new();
    eval(&stmts, &mut env);
}

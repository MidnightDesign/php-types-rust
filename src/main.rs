use clap::{Parser, Subcommand};

use crate::parser::parse_type;
use crate::scope::Scope;

mod compatibility;
mod lexer;
mod parser;
mod scope;
mod r#type;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command()]
    Validate {
        #[arg()]
        input: String,
    },
    #[command()]
    Check {
        #[arg()]
        super_: String,
        #[arg()]
        sub: String,
    },
    #[command()]
    Normalize {
        #[arg()]
        input: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Validate { input }) => validate(input),
        Some(Commands::Check { super_, sub }) => check(super_, sub),
        Some(Commands::Normalize { input }) => normalize(input),
        None => {
            println!("No command specified");
            std::process::exit(1);
        }
    }
}

fn validate(input: &str) {
    let scope = Scope::global();
    let result = parse_type(input, &scope);
    if let Err(e) = result {
        println!("{}", e);
        std::process::exit(1);
    }
    std::process::exit(0);
}

fn check(super_: &str, sub: &str) {
    let scope = Scope::global();
    let super_ = parse_type(super_, &scope).unwrap();
    let sub = parse_type(sub, &scope).unwrap();
    let result = sub.is_subtype_of(&super_);
    if result {
        std::process::exit(0);
    } else {
        println!("{} is not a subtype of {}", sub, super_);
        std::process::exit(1);
    }
}

fn normalize(input: &str) {
    let scope = Scope::global();
    let result = parse_type(input, &scope);
    if let Err(e) = result {
        println!("{}", e);
        std::process::exit(1);
    }
    let ty = result.unwrap();
    println!("{}", ty);
    std::process::exit(0);
}

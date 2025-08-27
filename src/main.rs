use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};

use lox_interpreter_rust::{lexer, parser};

#[derive(Parser)]
struct App {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Tokenize { filepath: PathBuf },
    Parse { filepath: PathBuf },
}

fn main() {
    let app = App::parse();

    match app.command {
        Commands::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath).unwrap();

            let mut found_err = false;
            for maybe_token in lexer::Lexer::new(&file_contents) {
                match maybe_token {
                    Ok(token) => println!("{token}"),
                    Err(error) => {
                        eprint!("{error}");
                        found_err = true;
                    }
                }
            }
            if found_err {
                std::process::exit(65);
            }
        }

        Commands::Parse { filepath } => {
            let file_contents = fs::read_to_string(&filepath).unwrap();

            let expression: parser::Expression = parser::Parse::parse_str(&file_contents)
                .unwrap_or_else(|error| {
                    eprint!("{error}");
                    std::process::exit(65)
                });

            println!("{expression:?}");
        }
    }
}

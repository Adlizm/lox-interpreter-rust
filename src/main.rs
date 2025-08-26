use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};

use lox_interpreter_rust::lexer;

#[derive(Parser)]
struct App {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Tokenize { filepath: PathBuf },
}

fn main() {
    let app = App::parse();

    match app.command {
        Commands::Tokenize { filepath } => {
            let file_contents = fs::read_to_string(&filepath).unwrap_or_else(|_| {
                eprint!("Failed to read file {:?}", filepath);
                String::new()
            });

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
            println!("EOF  null");
        }
    }
}

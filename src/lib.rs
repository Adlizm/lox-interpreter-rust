use thiserror::Error;

pub mod lexer;
pub mod parser;

pub type Result<'a, T> = std::result::Result<T, LoxError<'a>>;

#[derive(Debug)]
pub struct LoxError<'a> {
    line_source: &'a str,
    line: usize,
    col: usize,

    kind: LoxErrorKind,
}

impl<'a> std::error::Error for LoxError<'a> {}
impl<'a> std::fmt::Display for LoxError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let LoxError {
            line_source,
            line,
            col,
            kind,
        } = self;

        writeln!(f, "[line {}] Error: {kind}", line + 1)?;
        writeln!(f, "[{}, {}]: {line_source}", line + 1, col)
    }
}

#[derive(Debug, Error)]
pub enum LoxErrorKind {
    #[error(transparent)]
    Lexer(#[from] lexer::LexerError),

    #[error(transparent)]
    Parser(#[from] parser::ParserError),
}

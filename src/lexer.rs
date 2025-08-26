use thiserror::Error;

use super::LoxError;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unxpected EOF")]
    UnxpectedEof,

    #[error("Unexpected character: {0}")]
    UnxpectedChar(char),
}

pub struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,

    bytes: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn with_error(&self, error: LexerError) -> LoxError<'a> {
        LoxError {
            line_source: self
                .source
                .lines()
                .skip(self.line)
                .next()
                .unwrap_or_default(),
            line: self.line,
            col: self.col,
            kind: error.into(),
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            rest: source,
            bytes: 0,
            line: 0,
            col: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER _EQUAL >= null"),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LoxError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars_iter = self.rest.chars();

        loop {
            let c = chars_iter.next()?;

            self.col += 1;
            self.bytes += c.len_utf8();
            self.rest = chars_iter.as_str();

            match c {
                '(' => return Some(Ok(Token::LeftParen)),
                ')' => return Some(Ok(Token::RightParen)),
                '{' => return Some(Ok(Token::LeftBrace)),
                '}' => return Some(Ok(Token::RightBrace)),
                ';' => return Some(Ok(Token::Semicolon)),
                ',' => return Some(Ok(Token::Comma)),
                '+' => return Some(Ok(Token::Plus)),
                '-' => return Some(Ok(Token::Minus)),
                '*' => return Some(Ok(Token::Star)),
                '/' => return Some(Ok(Token::Slash)),
                '.' => return Some(Ok(Token::Dot)),
                '!' => todo!(),
                '=' => todo!(),
                '<' => todo!(),
                '>' => todo!(),
                '0'..='9' => todo!(),
                '_' | 'a'..='z' | 'A'..='Z' => todo!(),
                c if c.is_whitespace() => {
                    if c == '\n' {
                        self.line += 1;
                        self.col = 0;
                    }
                    continue;
                }
                c => return Some(Err(self.with_error(LexerError::UnxpectedChar(c)))),
            }
        }
    }
}

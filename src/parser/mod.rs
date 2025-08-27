use thiserror::Error;

use crate::{
    LoxError, LoxErrorKind, Result,
    lexer::{Lexer, Token, TokenKind},
};

mod expression;
pub use expression::Expression;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected EOF")]
    UnxpectedEof,

    #[error("Unexpected token: {0}")]
    UnexpectedToken(TokenKind),

    #[error("Expected for token ( {0} ) but found a ( {1} )")]
    ExpectTokenButNotFound(TokenKind, TokenKind),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek: Option<Token<'a>>,
}
pub trait Parse<'a>: Sized + 'a {
    type State: Default + Copy;

    fn parse_within(parser: &mut Parser<'a>, state: Self::State) -> Result<'a, Self>;

    fn parse_str(source: &'a str) -> Result<'a, Self> {
        let mut parser = Parser::new(source);
        let ast = Self::parse_within(&mut parser, Self::State::default())?;

        if let Some(token) = parser.peek()? {
            let token = ParserError::UnexpectedToken(token.kind());
            return Err(parser.with_error(token));
        }

        Ok(ast)
    }

    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        Self::parse_within(parser, Self::State::default())
    }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            peek: None,
        }
    }

    pub fn peek(&mut self) -> Result<'a, Option<&Token<'a>>> {
        match self.peek {
            Some(ref token) => Ok(Some(token)),
            None => {
                let token = self.lexer.next().transpose()?;
                self.peek = token;
                Ok(self.peek.as_ref())
            }
        }
    }

    pub fn next(&mut self) -> Result<'a, Option<Token<'a>>> {
        match self.peek.take() {
            Some(token) => Ok(Some(token)),
            None => self.lexer.next().transpose(),
        }
    }

    pub fn expect(&mut self, kind: TokenKind) -> Result<'a, Token<'a>> {
        let token = self
            .next()?
            .ok_or_else(|| self.with_error(ParserError::UnxpectedEof))?;

        if token.kind() != kind {
            let err = ParserError::UnexpectedToken(token.kind());
            return Err(self.with_error(err));
        }

        Ok(token)
    }

    #[inline]
    pub fn with_error(&self, error: impl Into<LoxErrorKind>) -> LoxError<'a> {
        self.lexer.with_error(error)
    }
}

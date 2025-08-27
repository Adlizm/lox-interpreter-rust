use super::{Parse, Parser};
use crate::{Result, lexer::TokenKind, parser::ParserError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpInfix {
    Add,
    Mul,
    Div,
    Sub,
    Or,
    And,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    FieldAccess,
}
impl OpInfix {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Star => Some(Self::Mul),
            TokenKind::Slash => Some(Self::Div),
            TokenKind::Minus => Some(Self::Sub),
            TokenKind::Or => Some(Self::Or),
            TokenKind::And => Some(Self::And),
            TokenKind::Less => Some(Self::Less),
            TokenKind::LessEqual => Some(Self::LessEqual),
            TokenKind::Equal => Some(Self::Equal),
            TokenKind::BangEqual => Some(Self::NotEqual),
            TokenKind::Greater => Some(Self::Greater),
            TokenKind::GreaterEqual => Some(Self::GreaterEqual),
            TokenKind::Dot => Some(Self::FieldAccess),
            _ => None,
        }
    }

    fn binding_power(self) -> (usize, usize) {
        match self {
            Self::Equal
            | Self::NotEqual
            | Self::Less
            | Self::Greater
            | Self::LessEqual
            | Self::GreaterEqual => (1, 2),
            Self::Or => (3, 4),
            Self::And => (5, 6),
            Self::Add | Self::Sub => (7, 8),
            Self::Mul | Self::Div => (9, 10),
            Self::FieldAccess => (15, 16),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpPrefix {
    Not,
    Neg,
}
impl OpPrefix {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Bang => Some(Self::Not),
            TokenKind::Minus => Some(Self::Neg),
            _ => None,
        }
    }

    fn binding_power(self) -> ((), usize) {
        match self {
            OpPrefix::Neg => ((), 11),
            OpPrefix::Not => ((), 11),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpSufix {
    FunCall,
}
impl OpSufix {
    fn from_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::LeftParen => Some(Self::FunCall),
            _ => None,
        }
    }
    fn binding_power(self) -> (usize, ()) {
        match self {
            Self::FunCall => (13, ()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Operation(Box<Expression<'a>>, OpInfix, Box<Expression<'a>>),
    Neg(Box<Expression<'a>>),
    Not(Box<Expression<'a>>),

    FieldAccess(Box<Expression<'a>>, &'a str),
    FunCall(Box<Expression<'a>>, Vec<Expression<'a>>),

    Super,
    This,
    Ident(&'a str),

    Nil,
    Bool(bool),
    Number(f64),
    String(&'a str),
}

impl<'a> Parse<'a> for Expression<'a> {
    type State = usize;

    fn parse_within(parser: &mut Parser<'a>, min_bp: usize) -> Result<'a, Self> {
        let token = parser
            .next()?
            .ok_or_else(|| parser.with_error(ParserError::UnxpectedEof))?;

        let mut lhs = match token.kind() {
            TokenKind::Nil => Expression::Nil,
            TokenKind::This => Expression::This,
            TokenKind::Super => Expression::Super,
            TokenKind::True => Expression::Bool(true),
            TokenKind::False => Expression::Bool(false),
            TokenKind::Ident => Expression::Ident(token.ident().expect("must be ident")),
            TokenKind::Number => Expression::Number(token.number().expect("must be number")),
            TokenKind::String => Expression::String(token.string().expect("must be string")),

            TokenKind::LeftParen => {
                let exp = Self::parse_within(parser, 0)?;
                parser.expect(TokenKind::RightParen)?;
                exp
            }
            kind => {
                if let Some(op) = OpPrefix::from_kind(kind) {
                    let (_, right_bp) = op.binding_power();
                    let rhs = Self::parse_within(parser, right_bp)?;

                    match op {
                        OpPrefix::Neg => Expression::Neg(Box::new(rhs)),
                        OpPrefix::Not => Expression::Not(Box::new(rhs)),
                    }
                } else {
                    let err = ParserError::UnexpectedToken(token.kind());
                    let err = parser.with_error(err);
                    return Err(err);
                }
            }
        };

        loop {
            let Some(token) = parser.peek()? else {
                break;
            };

            if let Some(op) = OpSufix::from_kind(token.kind()) {
                let (left_bp, _) = op.binding_power();
                if left_bp < min_bp {
                    break;
                }
                let _ = parser.next(); // Consume operator

                lhs = match op {
                    OpSufix::FunCall => {
                        let mut arguments = vec![];
                        loop {
                            let token = match parser.peek()? {
                                Some(token) => token,
                                None => return Err(parser.with_error(ParserError::UnxpectedEof)),
                            };

                            match (arguments.len(), token.kind()) {
                                (0.., TokenKind::RightParen) => break,
                                (1.., TokenKind::Comma) => {
                                    let _ = parser.next(); // Consume ,
                                }
                                (0, _) => {}
                                _ => {
                                    let err = ParserError::UnexpectedToken(token.kind());
                                    return Err(parser.with_error(err));
                                }
                            }

                            let arg = Expression::parse(parser)?;
                            arguments.push(arg);
                        }
                        parser.expect(TokenKind::RightParen)?;

                        Expression::FunCall(Box::new(lhs), arguments)
                    }
                };
                continue;
            }

            if let Some(op) = OpInfix::from_kind(token.kind()) {
                let (left_bp, right_bp) = op.binding_power();
                if left_bp < min_bp {
                    break;
                }
                let _ = parser.next(); // Consume operator

                if op == OpInfix::FieldAccess {
                    let token = parser.expect(TokenKind::Ident)?;
                    let ident = token.ident().unwrap();

                    lhs = Expression::FieldAccess(Box::new(lhs), ident);
                } else {
                    let rhs = Self::parse_within(parser, right_bp)?;
                    lhs = Expression::Operation(Box::new(lhs), op, Box::new(rhs));
                }
            }
        }

        Ok(lhs)
    }
}

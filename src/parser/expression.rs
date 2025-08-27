use super::{Parse, Parser};
use crate::Result;

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Mul(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Neg(Box<Expression<'a>>),

    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Not(Box<Expression<'a>>),

    Less(Box<Expression<'a>>, Box<Expression<'a>>),
    Equals(Box<Expression<'a>>, Box<Expression<'a>>),
    Greater(Box<Expression<'a>>, Box<Expression<'a>>),
    NotEquals(Box<Expression<'a>>, Box<Expression<'a>>),
    LessEquals(Box<Expression<'a>>, Box<Expression<'a>>),
    GreaterEquals(Box<Expression<'a>>, Box<Expression<'a>>),

    Field(Box<Expression<'a>>, &'a str),

    FunCall(Box<Expression<'a>>, Vec<Expression<'a>>),

    Ternary {
        condtion: Box<Expression<'a>>,
        yes: Box<Expression<'a>>,
        no: Box<Expression<'a>>,
    },

    Ident(&'a str),
    Super,
    This,

    Nil,
    Bool(bool),
    Number(f64),
    String(&'a str),
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse_within(parser: &mut Parser<'a>, pos: usize) -> Result<'a, Self> {
        todo!()
    }
}

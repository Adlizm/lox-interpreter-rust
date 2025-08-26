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

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
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
    And,
    Or,
    Nil,
    True,
    False,
    Number(f64),
    Ident(&'a str),
    String(&'a str),
    If,
    Else,
    Var,
    Fun,
    For,
    While,
    Print,
    Return,
    Class,
    Super,
    This,
}

impl<'a> std::fmt::Display for Token<'a> {
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
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::And => todo!(),
            Token::Or => todo!(),
            Token::Nil => todo!(),
            Token::True => todo!(),
            Token::False => todo!(),
            Token::Ident(_) => todo!(),
            Token::String(_) => todo!(),
            Token::Number(n) => {
                if n.trunc() == *n {
                    write!(f, "NUMBER {n} {n}.0")
                } else {
                    write!(f, "NUMBER {n} {n}")
                }
            }
            Token::If => todo!(),
            Token::Else => todo!(),
            Token::Var => todo!(),
            Token::Fun => todo!(),
            Token::For => todo!(),
            Token::While => todo!(),
            Token::Print => todo!(),
            Token::Return => todo!(),
            Token::Class => todo!(),
            Token::Super => todo!(),
            Token::This => todo!(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LoxError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars_iter = self.rest.chars();

        loop {
            let rest_with_c = self.rest;
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
                '!' | '=' | '<' | '>' => {
                    let (no, yes) = match c {
                        '!' => (Token::Bang, Token::BangEqual),
                        '=' => (Token::Equal, Token::EqualEqual),
                        '<' => (Token::Less, Token::LessEqual),
                        '>' => (Token::Greater, Token::GreaterEqual),
                        _ => unreachable!(),
                    };
                    if let Some('=') = chars_iter.next() {
                        self.bytes += 1;
                        self.col += 1;
                        self.rest = &self.rest[1..];
                        return Some(Ok(yes));
                    } else {
                        return Some(Ok(no));
                    }
                }
                '0'..='9' => {
                    let mut len = 1;
                    let find = loop {
                        if let Some(c) = chars_iter.next() {
                            if matches!(c, '0'..='9') {
                                len += 1;
                            } else {
                                break Some(c);
                            }
                        } else {
                            break None;
                        }
                    };

                    // NOTE: for numbers literals 123abc return:
                    //   -  Tokens: Number(123) and Literal("abc") (choiced)
                    //   -  Err(UnxpectedChar('a'))
                    if let Some('.') = find {
                        let mut decimals = 0;
                        loop {
                            if chars_iter.next().is_some_and(|c| matches!(c, '0'..='9')) {
                                decimals += 1;
                            } else {
                                break;
                            }
                        }
                        if decimals > 0 {
                            len += decimals + 1
                        }
                    }

                    self.bytes += len;
                    self.rest = &rest_with_c[len..];
                    let number = &rest_with_c[0..len];
                    let number = number
                        .parse::<f64>()
                        .expect("this string not contains any nondigit or double '.'");

                    return Some(Ok(Token::Number(number)));
                }
                '_' | 'a'..='z' | 'A'..='Z' => {
                    let literal = match chars_iter
                        .find(|c| !matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                    {
                        Some(c) => {
                            let len = rest_with_c.len() - chars_iter.as_str().len() - c.len_utf8();
                            let literal = &rest_with_c[0..len];
                            literal
                        }
                        None => chars_iter.as_str(),
                    };

                    self.rest = &rest_with_c[literal.len()..];
                    self.bytes += literal.len();
                    self.col += literal.len();

                    let token = match literal {
                        "nil" => Token::Nil,
                        "true" => Token::True,
                        "false" => Token::False,
                        "and" => Token::And,
                        "or" => Token::Or,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "var" => Token::Var,
                        "for" => Token::For,
                        "while" => Token::While,
                        "fun" => Token::Fun,
                        "class" => Token::Class,
                        "super" => Token::Super,
                        "this" => Token::This,
                        "print" => Token::Print,
                        "return" => Token::Return,
                        ident => Token::Ident(ident),
                    };
                    return Some(Ok(token));
                }
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

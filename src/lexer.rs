use thiserror::Error;

use super::{LoxError, LoxErrorKind};

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unexpected character: {0}")]
    UnxpectedChar(char),

    #[error("String not terminated, character '\"' not found")]
    UnterminatedString,
}

pub struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,

    bytes: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn with_error(&self, error: impl Into<LoxErrorKind>) -> LoxError<'a> {
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

#[derive(Debug, Clone)]
pub struct Token<'a> {
    kind: TokenKind,
    lit: Option<&'a str>,
    value: Option<TokenValue<'a>>,
}

impl<'a> Token<'a> {
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn ident(&self) -> Option<&'a str> {
        match self.kind {
            TokenKind::Ident => {
                return Some(self.lit.expect("Ident must have literal"));
            }
            _ => None,
        }
    }

    pub fn bool(&self) -> Option<bool> {
        match self.kind {
            TokenKind::True => Some(true),
            TokenKind::False => Some(false),
            _ => None,
        }
    }

    pub fn number(&self) -> Option<f64> {
        match self.kind {
            TokenKind::Number => match self.value {
                Some(TokenValue::Number(n)) => Some(n),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    pub fn string(&self) -> Option<&'a str> {
        match self.kind {
            TokenKind::String => match self.value {
                Some(TokenValue::String(s)) => Some(s),
                _ => unreachable!(),
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenValue<'a> {
    Number(f64),
    String(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
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
    Number,
    Ident,
    String,
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

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lit = match self {
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Dot => ".",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Nil => "nil",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Var => "var",
            TokenKind::Fun => "fun",
            TokenKind::For => "for",
            TokenKind::While => "while",
            TokenKind::Print => "print",
            TokenKind::Return => "return",
            TokenKind::Class => "class",
            TokenKind::Super => "super",
            TokenKind::This => "this",
            kind => return write!(f, "{kind:?}"),
        };

        write!(f, "{lit}")
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Ident => {
                let lit = self.lit.expect("Indent kind contains lit");
                write!(f, "{lit}")
            }
            TokenKind::String => {
                let string = self.string().expect("String kind contains string value");
                write!(f, "\"{string}\"")
            }
            TokenKind::Number => {
                let n = self.number().expect("Number kind contains number value");
                if n.trunc() == n {
                    write!(f, "NUMBER {n} {n}.0")
                } else {
                    write!(f, "NUMBER {n} {n}")
                }
            }
            kind => write!(f, "{kind}"),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LoxError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars_iter = self.rest.chars();

        let just_kind = |kind: TokenKind| {
            Some(Ok(Token {
                kind,
                lit: None,
                value: None,
            }))
        };

        loop {
            let rest_with_c = self.rest;
            let c = chars_iter.next()?;

            self.col += 1;
            self.bytes += c.len_utf8();
            self.rest = chars_iter.as_str();

            match c {
                '(' => return just_kind(TokenKind::LeftParen),
                ')' => return just_kind(TokenKind::RightParen),
                '{' => return just_kind(TokenKind::LeftBrace),
                '}' => return just_kind(TokenKind::RightBrace),
                ';' => return just_kind(TokenKind::Semicolon),
                ',' => return just_kind(TokenKind::Comma),
                '.' => return just_kind(TokenKind::Dot),
                '+' => return just_kind(TokenKind::Plus),
                '-' => return just_kind(TokenKind::Minus),
                '*' => return just_kind(TokenKind::Star),
                '/' => {
                    if let Some('/') = chars_iter.next() {
                        // comments
                        let _ = chars_iter.find(|c| *c == '\n');
                        self.rest = chars_iter.as_str();
                        self.col = 0;
                        self.line += 1;
                        self.bytes += rest_with_c.len() - self.rest.len() - 1;

                        continue;
                    } else {
                        return just_kind(TokenKind::Slash);
                    }
                }
                '!' | '=' | '<' | '>' => {
                    let (no, yes) = match c {
                        '!' => (TokenKind::Bang, TokenKind::BangEqual),
                        '=' => (TokenKind::Equal, TokenKind::EqualEqual),
                        '<' => (TokenKind::Less, TokenKind::LessEqual),
                        '>' => (TokenKind::Greater, TokenKind::GreaterEqual),
                        _ => unreachable!(),
                    };
                    if let Some('=') = chars_iter.next() {
                        self.bytes += 1;
                        self.col += 1;
                        self.rest = &self.rest[1..];
                        return just_kind(yes);
                    } else {
                        return just_kind(no);
                    }
                }

                // strings
                '"' => {
                    let literal = loop {
                        if let Some(c) = chars_iter.next() {
                            self.bytes += c.len_utf8();
                            match c {
                                '\n' => {
                                    self.line += 1;
                                    self.col = 0;
                                }
                                '"' => {
                                    self.col += 1;
                                    self.rest = chars_iter.as_str();

                                    let len = rest_with_c.len() - self.rest.len();
                                    break &rest_with_c[0..len];
                                }
                                _ => self.col += 1,
                            }
                        } else {
                            return Some(Err(self.with_error(LexerError::UnterminatedString)));
                        }
                    };
                    return Some(Ok(Token {
                        kind: TokenKind::String,
                        lit: Some(literal),
                        value: Some(TokenValue::String(&literal[1..literal.len() - 1])),
                    }));
                }

                // numbers
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
                    let number_lit = &rest_with_c[0..len];
                    let number = number_lit
                        .parse::<f64>()
                        .expect("this string not contains any nondigit or double '.'");

                    return Some(Ok(Token {
                        kind: TokenKind::Number,
                        lit: Some(number_lit),
                        value: Some(TokenValue::Number(number)),
                    }));
                }

                // indentifiers and reserved words
                '_' | 'a'..='z' | 'A'..='Z' => {
                    let literal = match chars_iter
                        .find(|c| !matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                    {
                        Some(c) => {
                            let len = rest_with_c.len() - chars_iter.as_str().len() - c.len_utf8();
                            let literal = &rest_with_c[0..len];
                            literal
                        }
                        None => rest_with_c,
                    };

                    self.rest = &rest_with_c[literal.len()..];
                    self.bytes += literal.len();
                    self.col += literal.len();

                    match literal {
                        "false" => return just_kind(TokenKind::False),
                        "true" => return just_kind(TokenKind::True),
                        "nil" => return just_kind(TokenKind::Nil),
                        "and" => return just_kind(TokenKind::And),
                        "or" => return just_kind(TokenKind::Or),
                        "if" => return just_kind(TokenKind::If),
                        "else" => return just_kind(TokenKind::Else),
                        "var" => return just_kind(TokenKind::Var),
                        "for" => return just_kind(TokenKind::For),
                        "while" => return just_kind(TokenKind::While),
                        "fun" => return just_kind(TokenKind::Fun),
                        "class" => return just_kind(TokenKind::Class),
                        "super" => return just_kind(TokenKind::Super),
                        "this" => return just_kind(TokenKind::This),
                        "print" => return just_kind(TokenKind::Print),
                        "return" => return just_kind(TokenKind::Return),
                        ident => {
                            return Some(Ok(Token {
                                kind: TokenKind::Ident,
                                lit: Some(ident),
                                value: None,
                            }));
                        }
                    };
                }

                // ignoring spaces
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

#[cfg(test)]
mod test {
    use super::*;

    fn collect_token_vec<'a>(content: &'a str) -> Vec<Token<'a>> {
        Lexer::new(content)
            .into_iter()
            .map(|token| token.unwrap())
            .collect()
    }

    #[test]
    fn tokenize_numbers() {
        let tokens = collect_token_vec("123.4234 0234 24 0.13");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].number(), Some(123.4234));
        assert_eq!(tokens[1].number(), Some(234.0));
        assert_eq!(tokens[2].number(), Some(24.0));
        assert_eq!(tokens[3].number(), Some(0.13));
    }

    #[test]
    fn tokenize_strings() {
        let tokens = collect_token_vec(r#""""hello world""#);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].string(), Some(""));
        assert_eq!(tokens[1].string(), Some("hello world"));
    }

    #[test]
    fn tokenize_puctuators() {
        let tokens = collect_token_vec("(){};,+-*!===<=>=!<>/.=");

        assert_eq!(tokens.len(), 19);
        assert_eq!(tokens[0].kind(), TokenKind::LeftParen);
        assert_eq!(tokens[1].kind(), TokenKind::RightParen);
        assert_eq!(tokens[2].kind(), TokenKind::LeftBrace);
        assert_eq!(tokens[3].kind(), TokenKind::RightBrace);
        assert_eq!(tokens[4].kind(), TokenKind::Semicolon);
        assert_eq!(tokens[5].kind(), TokenKind::Comma);
        assert_eq!(tokens[6].kind(), TokenKind::Plus);
        assert_eq!(tokens[7].kind(), TokenKind::Minus);
        assert_eq!(tokens[8].kind(), TokenKind::Star);
        assert_eq!(tokens[9].kind(), TokenKind::BangEqual);
        assert_eq!(tokens[10].kind(), TokenKind::EqualEqual);
        assert_eq!(tokens[11].kind(), TokenKind::LessEqual);
        assert_eq!(tokens[12].kind(), TokenKind::GreaterEqual);
        assert_eq!(tokens[13].kind(), TokenKind::Bang);
        assert_eq!(tokens[14].kind(), TokenKind::Less);
        assert_eq!(tokens[15].kind(), TokenKind::Greater);
        assert_eq!(tokens[16].kind(), TokenKind::Slash);
        assert_eq!(tokens[17].kind(), TokenKind::Dot);
        assert_eq!(tokens[18].kind(), TokenKind::Equal);
    }

    #[test]
    fn tokenize_reserved() {
        let tokens = collect_token_vec(
            "and class else false for fun if nil or return super this true var while",
        );

        assert_eq!(tokens.len(), 15);
        assert_eq!(tokens[0].kind(), TokenKind::And);
        assert_eq!(tokens[1].kind(), TokenKind::Class);
        assert_eq!(tokens[2].kind(), TokenKind::Else);

        assert_eq!(tokens[3].kind(), TokenKind::False);
        assert_eq!(tokens[3].bool(), Some(false));

        assert_eq!(tokens[4].kind(), TokenKind::For);
        assert_eq!(tokens[5].kind(), TokenKind::Fun);
        assert_eq!(tokens[6].kind(), TokenKind::If);
        assert_eq!(tokens[7].kind(), TokenKind::Nil);
        assert_eq!(tokens[8].kind(), TokenKind::Or);
        assert_eq!(tokens[9].kind(), TokenKind::Return);
        assert_eq!(tokens[10].kind(), TokenKind::Super);
        assert_eq!(tokens[11].kind(), TokenKind::This);

        assert_eq!(tokens[12].kind(), TokenKind::True);
        assert_eq!(tokens[12].bool(), Some(true));

        assert_eq!(tokens[13].kind(), TokenKind::Var);
        assert_eq!(tokens[14].kind(), TokenKind::While);
    }

    #[test]
    fn tokenize_idents() {
        let tokens = collect_token_vec(
            "andy formless fo _ _123 _abc ab123
            abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_
            ",
        );

        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].ident(), Some("andy"));
        assert_eq!(tokens[1].ident(), Some("formless"));
        assert_eq!(tokens[2].ident(), Some("fo"));
        assert_eq!(tokens[3].ident(), Some("_"));
        assert_eq!(tokens[4].ident(), Some("_123"));
        assert_eq!(tokens[5].ident(), Some("_abc"));
        assert_eq!(tokens[6].ident(), Some("ab123"));
        assert_eq!(
            tokens[7].ident(),
            Some("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_")
        );
    }

    #[test]
    fn test_whitespaces_and_comments() {
        let tokens = collect_token_vec(
            "space    tabs				newlines

            // some comment 
            // other comment

            end
            // final comment",
        );

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].ident(), Some("space"));
        assert_eq!(tokens[1].ident(), Some("tabs"));
        assert_eq!(tokens[2].ident(), Some("newlines"));
        assert_eq!(tokens[3].ident(), Some("end"));
    }
}

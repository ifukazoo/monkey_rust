use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;

#[derive(Debug, Clone, Default)]
pub struct Token {
    pub t_type: TokenType,
    pub literal: String,
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type:{},literal:{}", self.t_type, self.literal)
    }
}

#[derive(PartialEq, Copy, Clone, Debug, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    SLASH,
    ASTERISK,
    LT,
    GT,
    EQ,
    NOTEQ,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,

    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    IF,
    ELSE,
    TRUE,
    FALSE,
    RETURN,
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", *self)
    }
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::EOF
    }
}

lazy_static! {
    static ref KEY_WORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("let", TokenType::LET);
        map.insert("fn", TokenType::FUNCTION);
        map.insert("if", TokenType::IF);
        map.insert("else", TokenType::ELSE);
        map.insert("true", TokenType::TRUE);
        map.insert("false", TokenType::FALSE);
        map.insert("return", TokenType::RETURN);
        map
    };
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match KEY_WORDS.get(ident) {
        Some(t) => *t,
        None => TokenType::IDENT,
    }
}

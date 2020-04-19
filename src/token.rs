use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Token {
    pub t_type: TokenType,
    pub literal: String,
}

#[derive(PartialEq, Copy, Clone, Debug)]
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

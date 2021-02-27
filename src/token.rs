use std::fmt;

/// Token種別
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    ILLEGAL,
    IDENT,
    INT,
    STRING,

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

/// 位置情報
/// * (開始位置, 終了位置+1) を表す
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location(usize, usize);

impl fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

/// トークン
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    /// 種別
    pub kind: TokenKind,
    /// トークンの開始リテラル
    pub literal: String,
    /// 入力文字中の位置
    pub loc: Location,
}

impl Token {
    pub fn new(kind: TokenKind, literal: String, start: usize, end: usize) -> Self {
        Self {
            kind,
            literal,
            loc: Location(start, end),
        }
    }
    pub fn location(&self) -> (usize, usize) {
        (*&self.loc.0, *&self.loc.1)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{{:?},{}}}", self.kind, self.loc)
    }
}

#[test]
fn test_disp() {
    assert_eq!(
        r#"{IF,0-1}"#,
        format!("{}", Token::new(TokenKind::IF, "if".to_string(), 0, 1))
    );
}

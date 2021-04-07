use crate::token::Token;
use crate::token::TokenKind;
use std::fmt;

/// プログラム
pub type Program = Vec<Statement>;

/// 文
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// 空の文
    Empty,
    /// Let文
    Let(LetStatement),
    /// 式文
    Exp(Expression),
    /// return文
    Return(Expression),
    /// if文
    If(IfStatement),
}

/// 式
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    /// bool式
    Bool(BoolLiteral),
    /// 整数式
    Int(IntegerLiteral),
    /// 文字列式
    Str(StringLiteral),
    /// 変数式
    Ident(Identifier),
    /// 配列式
    Array(ArrayLiteral),
    /// インデックス式
    Index(ArrayIndex),
    /// ハッシュ式
    Hash(HashLiteral),
    /// 関数式
    Function(FunctionLiteral),
    /// 関数呼び出し式
    Call(CallFunction),
    /// 前置演算子式
    Prefix(PrefixExpression),
    /// 中置演算子式
    Infix(InfixExpression),
}

/// 二項演算
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    /// 'exp + exp'
    Add,
    /// 'exp - exp'
    Sub,
    /// 'exp * exp'
    Mul,
    /// 'exp / exp'
    Div,
    /// 'exp > exp'
    Gt,
    /// 'exp < exp'
    Lt,
    /// 'exp == exp'
    Eq,
    /// 'exp != exp'
    NotEq,
}
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
        }
    }
}

/// 単項演算
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    /// '+ exp'
    Positive,
    /// '- exp'
    Negative,
    /// '! exp'
    Not,
}
impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Positive => write!(f, "+"),
            Self::Negative => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

/// if文
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    token: Token,
    pub cond: Box<Expression>,
    pub cons: Vec<Statement>,
    pub alt: Option<Vec<Statement>>,
}
impl IfStatement {
    pub fn new(
        token: Token,
        cond: Expression,
        cons: Vec<Statement>,
        alt: Option<Vec<Statement>>,
    ) -> Self {
        Self {
            token,
            cond: Box::new(cond),
            cons,
            alt,
        }
    }
}

/// Let文
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    token: Token,
    pub id: Identifier,
    pub exp: Box<Expression>,
}
impl LetStatement {
    pub fn new(token: Token, id: Identifier, exp: Expression) -> Self {
        Self {
            token,
            id,
            exp: Box::new(exp),
        }
    }
}

/// 整数リテラル
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}
impl IntegerLiteral {
    pub fn new(token: Token) -> Self {
        Self {
            value: token.literal.parse::<i64>().unwrap(),
            token,
        }
    }
}

/// ブールリテラル "true" or "false"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoolLiteral {
    token: Token,
    pub value: bool,
}
impl BoolLiteral {
    pub fn new(token: Token) -> Self {
        Self {
            value: token.literal.parse::<bool>().unwrap(),
            token,
        }
    }
}

/// 文字列リテラル
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    token: Token,
    pub value: String,
}
impl StringLiteral {
    pub fn new(token: Token) -> Self {
        // 両端のダブルクオートを外す
        // "hello" => hello
        // "\"hello\"" => "hello"
        let s = &token.literal[1..token.literal.len() - 1];
        Self {
            value: String::from(s),
            token,
        }
    }
}

/// 変数名に使用する識別子
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    token: Token,
}
impl Identifier {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
    pub fn symbol(&self) -> String {
        self.token.literal.clone()
    }
}

/// 関数定義
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLiteral {
    token: Token,
    pub params: Vec<Identifier>,
    pub block: Vec<Statement>,
}
impl FunctionLiteral {
    pub fn new(token: Token, params: Vec<Identifier>, block: Vec<Statement>) -> Self {
        Self {
            token,
            params,
            block,
        }
    }
}
impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (s, e) = self.token.location();
        write!(
            f,
            "(block:{}-{}, params:{}, stmts:{})",
            s,
            e,
            self.params.len(),
            self.block.len()
        )
    }
}

// 配列定義
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayLiteral {
    token: Token,
    pub elements: Vec<Expression>,
}
impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> Self {
        Self { token, elements }
    }
}

/// 配列インデックス
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayIndex {
    token: Token,
    pub arr: Box<Expression>,
    pub index: Box<Expression>,
}
impl ArrayIndex {
    pub fn new(token: Token, arr: Expression, index: Expression) -> Self {
        Self {
            token,
            arr: Box::new(arr),
            index: Box::new(index),
        }
    }
}

// ハッシュ定義
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashLiteral {
    token: Token,
    pub keyvals: Vec<(Expression, Expression)>,
}
impl HashLiteral {
    pub fn new(token: Token, keyvals: Vec<(Expression, Expression)>) -> Self {
        Self { token, keyvals }
    }
}

/// 前置演算子式
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrefixExpression {
    token: Token,
    /// 演算
    pub operator: UnOp,
    /// 右辺
    pub right: Box<Expression>,
}
impl PrefixExpression {
    pub fn new(token: Token, right: Expression) -> Self {
        Self {
            operator: match token.kind {
                TokenKind::PLUS => UnOp::Positive,
                TokenKind::MINUS => UnOp::Negative,
                TokenKind::BANG => UnOp::Not,
                _ => unreachable!("illegal kind[{:?}]", token.kind),
            },
            token,
            right: Box::new(right),
        }
    }
}

/// 中置演算子式
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InfixExpression {
    token: Token,
    /// 左辺
    pub left: Box<Expression>,
    /// 演算
    pub operator: BinOp,
    /// 右辺
    pub right: Box<Expression>,
}
impl InfixExpression {
    pub fn new(token: Token, left: Expression, right: Expression) -> Self {
        Self {
            operator: match token.kind {
                TokenKind::PLUS => BinOp::Add,
                TokenKind::MINUS => BinOp::Sub,
                TokenKind::ASTERISK => BinOp::Mul,
                TokenKind::SLASH => BinOp::Div,
                TokenKind::LT => BinOp::Lt,
                TokenKind::GT => BinOp::Gt,
                TokenKind::EQ => BinOp::Eq,
                TokenKind::NOTEQ => BinOp::NotEq,
                _ => unreachable!("illegal kind[{:?}]", token.kind),
            },
            token,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

/// 関数呼び出し式
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallFunction {
    token: Token,
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}
impl CallFunction {
    pub fn new(token: Token, func: Expression, args: Vec<Expression>) -> Self {
        Self {
            token,
            func: Box::new(func),
            args,
        }
    }
}

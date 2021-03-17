use crate::ast::*;
use crate::env::*;
use std::fmt;

/// オブジェクト (monkey言語interpreterでの値)
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// 整数
    Int(i64),
    /// ブール
    Bool(bool),
    /// 文字列
    Str(String),
    /// クロージャー
    Closure(ClosureValue),
    /// リターン値
    Return(Box<Object>),
    /// ビルトイン関数
    Builtin(String),
    /// NULL
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Str(s) => write!(f, "{}", s),
            Self::Null => write!(f, "null"),
            Self::Return(r) => write!(f, "return({})", *r),
            Self::Closure(c) => write!(f, "{}", c.to_string()),
            Self::Builtin(s) => {
                write!(f, "builtin({})", s)
            }
        }
    }
}

/// 関数定義
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureValue {
    print: String,
    pub params: Vec<Identifier>,
    pub block: Vec<Statement>,
    pub env: RefEnvironment,
}
impl ClosureValue {
    pub fn new(
        print: &str,
        params: Vec<Identifier>,
        block: Vec<Statement>,
        env: RefEnvironment,
    ) -> Self {
        Self {
            print: print.to_string(),
            params,
            block,
            env,
        }
    }
}
impl fmt::Display for ClosureValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "closure{{fn:{}}}", self.print)
    }
}

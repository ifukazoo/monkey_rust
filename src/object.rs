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
    /// クロージャー
    Closure(FunctionLiteral, RefEnvironment),
    /// リターン値
    Return(Box<Object>),
    /// NULL
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Return(r) => write!(f, "return({})", *r),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Null => write!(f, "null"),
            Self::Closure(func, _) => {
                write!(f, "closure{{")?;
                write!(f, "fn:{}", func)?;
                write!(f, "}}")
            }
        }
    }
}

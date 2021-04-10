use crate::ast::*;
use crate::env::*;
use std::collections::HashMap;
use std::fmt;

/// ハッシュのキーとして許容する型
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum HashKey {
    Int(i64),
    Bool(bool),
    Str(String),
}
impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            HashKey::Int(i) => write!(f, "{}", i),
            HashKey::Bool(b) => write!(f, "{}", b),
            HashKey::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

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
    /// 配列
    Array(Vec<Object>),
    /// ハッシュ
    Hash(HashMap<HashKey, Object>),
    /// リターン値
    Return(Box<Object>),
    /// ビルトイン関数
    Builtin(&'static str),
    /// NULL
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Null => write!(f, "null"),
            Self::Return(r) => write!(f, "return({})", *r),
            Self::Closure(c) => write!(f, "{}", c.to_string()),
            Self::Array(elems) => {
                let mut sep = "";
                write!(f, "[")?;
                for e in elems.iter() {
                    write!(f, "{}{}", sep, e)?;
                    sep = ",";
                }
                write!(f, "]")
            }
            Self::Hash(hash) => {
                let mut sep = "";
                write!(f, "{{")?;
                for (k, v) in hash.iter() {
                    write!(f, "{}{}:{}", sep, k, v)?;
                    sep = ",";
                }
                write!(f, "}}")
            }
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

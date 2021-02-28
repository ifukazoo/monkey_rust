use crate::eval::EvalError;
use crate::parser::ParseError;
use std::fmt;
use std::io;

/// 評価時エラー
#[derive(Debug)]
pub enum MonkeyError {
    // 評価時のエラー
    Eval(EvalError),
    // 構文解析時のエラー
    Parser(ParseError),
    // IOエラー
    Io(io::Error),
}

impl From<EvalError> for MonkeyError {
    fn from(e: EvalError) -> Self {
        Self::Eval(e)
    }
}
impl From<ParseError> for MonkeyError {
    fn from(e: ParseError) -> Self {
        Self::Parser(e)
    }
}
impl From<io::Error> for MonkeyError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl fmt::Display for MonkeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eval(e) => write!(f, "eval error:{}", e),
            Self::Parser(e) => write!(f, "parser error:{}", e),
            Self::Io(e) => write!(f, "IO error:{}", e),
        }
    }
}

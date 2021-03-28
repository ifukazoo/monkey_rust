use crate::eval::EvalError;
use crate::object::Object;

const FUNCTION_LEN: &str = "len";
const FUNCTION_FIRST: &str = "first";
const FUNCTION_LAST: &str = "last";
const FUNCTION_REST: &str = "rest";

/// ビルトイン関数の名前を返す
pub fn get(key: &str) -> Option<Object> {
    match key {
        FUNCTION_LEN => Some(Object::Builtin(FUNCTION_LEN)),
        FUNCTION_FIRST => Some(Object::Builtin(FUNCTION_FIRST)),
        FUNCTION_LAST => Some(Object::Builtin(FUNCTION_LAST)),
        FUNCTION_REST => Some(Object::Builtin(FUNCTION_REST)),
        _ => None,
    }
}

/// getで返した名前を渡す．名前に応じた処理を施す．
pub fn exec(name: &str, args: Vec<Object>) -> Result<Object, EvalError> {
    match name {
        FUNCTION_LEN => len(args),
        FUNCTION_FIRST => first(args),
        FUNCTION_LAST => last(args),
        FUNCTION_REST => rest(args),
        _ => Err(EvalError::NameError(String::from(name))),
    }
}

fn len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "len() requires 1 arg. but {} args.",
            args.len()
        )));
    }

    match args.first().unwrap() {
        Object::Str(s) => Ok(Object::Int(s.chars().count() as i64)),
        Object::Array(a) => Ok(Object::Int(a.len() as i64)),
        _ => Err(EvalError::IllegalSyntax("len() requires Str.".to_string())),
    }
}
fn first(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "first() requires 1 arg. but {} args.",
            args.len()
        )));
    }
    match args.first().unwrap() {
        Object::Array(a) => {
            if a.len() == 0 {
                Err(EvalError::IndexOutOfRange((0, 0)))
            } else {
                Ok(a.first().unwrap().clone())
            }
        }
        _ => Err(EvalError::IllegalSyntax(
            "first() requires Array.".to_string(),
        )),
    }
}
fn last(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "last() requires 1 arg. but {} args.",
            args.len()
        )));
    }
    match args.first().unwrap() {
        Object::Array(a) => {
            if a.len() == 0 {
                Err(EvalError::IndexOutOfRange((0, 0)))
            } else {
                Ok(a.last().unwrap().clone())
            }
        }
        _ => Err(EvalError::IllegalSyntax(
            "first() requires Array.".to_string(),
        )),
    }
}
fn rest(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "rest() requires 1 arg. but {} args.",
            args.len()
        )));
    }
    match args.first().unwrap() {
        Object::Array(a) => {
            if a.len() == 0 {
                Err(EvalError::IllegalSyntax(
                    "rest() applied empty array.".to_string(),
                ))
            } else {
                let slice = &a[1..];
                Ok(Object::Array(slice.to_vec()))
            }
        }
        _ => Err(EvalError::IllegalSyntax(
            "rest() requires Array.".to_string(),
        )),
    }
}

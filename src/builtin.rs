use crate::eval::EvalError;
use crate::object::Object;

const FUNCTION_LEN: &str = "len";

/// ビルトイン関数の名前を返す
pub fn get(key: &str) -> Option<Object> {
    match key {
        FUNCTION_LEN => Some(Object::Builtin(String::from(FUNCTION_LEN))),
        _ => None,
    }
}

/// getで返した名前を渡す．名前に応じた処理を施す．
pub fn exec(name: &str, args: Vec<Object>) -> Result<Object, EvalError> {
    match name {
        FUNCTION_LEN => len(args),
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
        _ => Err(EvalError::IllegalSyntax(String::from(
            "len() requires Str.",
        ))),
    }
}

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

fn array_function_with_one_param(
    name: &str,
    args: Vec<Object>,
    exec: fn(&Vec<Object>) -> Result<Object, EvalError>,
) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "{} requires 1 arg. but {} args.",
            name,
            args.len()
        )));
    }
    match args.first().unwrap() {
        Object::Array(a) => exec(a),
        _ => Err(EvalError::IllegalSyntax(format!(
            "{} requires Array.",
            name
        ))),
    }
}
fn first(args: Vec<Object>) -> Result<Object, EvalError> {
    let f: fn(&Vec<Object>) -> Result<Object, EvalError> = |a| {
        if a.len() == 0 {
            Err(EvalError::IndexOutOfRange((0, 0)))
        } else {
            Ok(a.first().unwrap().clone())
        }
    };
    array_function_with_one_param("first()", args, f)
}
fn last(args: Vec<Object>) -> Result<Object, EvalError> {
    let f: fn(&Vec<Object>) -> Result<Object, EvalError> = |a| {
        if a.len() == 0 {
            Err(EvalError::IndexOutOfRange((0, 0)))
        } else {
            Ok(a.last().unwrap().clone())
        }
    };
    array_function_with_one_param("last()", args, f)
}

fn rest(args: Vec<Object>) -> Result<Object, EvalError> {
    let f: fn(&Vec<Object>) -> Result<Object, EvalError> = |a| {
        if a.len() == 0 {
            Err(EvalError::IllegalSyntax(
                "rest() applied empty array.".to_string(),
            ))
        } else {
            let slice = &a[1..];
            Ok(Object::Array(slice.to_vec()))
        }
    };
    array_function_with_one_param("rest()", args, f)
}

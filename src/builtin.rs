use crate::eval::EvalError;
use crate::object::Object;

const FUNCTION_LEN: &str = "len";
const FUNCTION_FIRST: &str = "first";
const FUNCTION_LAST: &str = "last";
const FUNCTION_REST: &str = "rest";
const FUNCTION_PUSH: &str = "push";

/// ビルトイン関数の名前を返す
pub fn get(key: &str) -> Option<Object> {
    match key {
        FUNCTION_LEN => Some(Object::Builtin(FUNCTION_LEN)),
        FUNCTION_FIRST => Some(Object::Builtin(FUNCTION_FIRST)),
        FUNCTION_LAST => Some(Object::Builtin(FUNCTION_LAST)),
        FUNCTION_REST => Some(Object::Builtin(FUNCTION_REST)),
        FUNCTION_PUSH => Some(Object::Builtin(FUNCTION_PUSH)),
        _ => None,
    }
}

/// getで返した名前を渡すと名前に応じた処理を実施する．
pub fn exec(name: &str, args: Vec<Object>) -> Result<Object, EvalError> {
    match name {
        FUNCTION_LEN => len(args),
        FUNCTION_FIRST => first(args),
        FUNCTION_LAST => last(args),
        FUNCTION_REST => rest(args),
        FUNCTION_PUSH => push(args),
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

fn array_function(
    name: &str,
    args: Vec<Object>,
    require_num: usize,
    exec: fn(&Vec<Object>) -> Result<Object, EvalError>,
) -> Result<Object, EvalError> {
    if args.len() != require_num {
        return Err(EvalError::IllegalSyntax(format!(
            "{} requires {} args. but {} args.",
            name,
            require_num,
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
    array_function("first()", args, 1, f)
}
fn last(args: Vec<Object>) -> Result<Object, EvalError> {
    let f: fn(&Vec<Object>) -> Result<Object, EvalError> = |a| {
        if a.len() == 0 {
            Err(EvalError::IndexOutOfRange((0, 0)))
        } else {
            Ok(a.last().unwrap().clone())
        }
    };
    array_function("last()", args, 1, f)
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
    array_function("rest()", args, 1, f)
}
fn push(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::IllegalSyntax(format!(
            "push requires 2 arg. but {} args.",
            args.len()
        )));
    }
    match args.get(0).unwrap() {
        Object::Array(a) => {
            let mut new = a.to_vec();
            let addend = args.get(1).unwrap().clone();
            new.push(addend);
            Ok(Object::Array(new))
        }
        _ => Err(EvalError::IllegalSyntax("push requires Array.".to_string())),
    }
}

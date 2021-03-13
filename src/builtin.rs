use crate::eval::EvalError;
use crate::object::Object;

pub fn get(key: &str) -> Option<Object> {
    match key.as_ref() {
        "len" => Some(Object::Builtin(String::from(key))),
        _ => None,
    }
}

pub fn exec(name: &str, args: Vec<Object>) -> Result<Object, EvalError> {
    match name {
        "len" => len(args),
        _ => unreachable!(format!("builtin name is already checked. but[{}]", name)),
    }
}

fn len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::IllegalSyntax(format!(
            "len() requires 1 arg. but {} arg.",
            args.len()
        )));
    }

    match args.first().unwrap() {
        Object::Str(s) => Ok(Object::Int(s.chars().count() as i64)),
        _ => Err(EvalError::IllegalSyntax(format!("len() requires Str.",))),
    }
}

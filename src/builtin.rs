use crate::eval::EvalError;
use crate::object::Object;

/// ビルトイン関数
const FUNCTION_LEN: &str = "len";
const FUNCTION_FIRST: &str = "first";
const FUNCTION_LAST: &str = "last";
const FUNCTION_REST: &str = "rest";
const FUNCTION_PUSH: &str = "push";
const FUNCTION_PUTS: &str = "puts";

/// ビルトイン関数の名前を返す
pub fn get(key: &str) -> Option<Object> {
    match key {
        FUNCTION_LEN => Some(Object::Builtin(FUNCTION_LEN)),
        FUNCTION_FIRST => Some(Object::Builtin(FUNCTION_FIRST)),
        FUNCTION_LAST => Some(Object::Builtin(FUNCTION_LAST)),
        FUNCTION_REST => Some(Object::Builtin(FUNCTION_REST)),
        FUNCTION_PUSH => Some(Object::Builtin(FUNCTION_PUSH)),
        FUNCTION_PUTS => Some(Object::Builtin(FUNCTION_PUTS)),
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
        FUNCTION_PUTS => puts(args),
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

// 配列へのビルトイン関数適用
fn apply_arr_func<Func>(
    name: &str,
    args: &[Object],      // ビルトイン関数に与えられた引数
    req_param_num: usize, // 必要な引数の数
    apply: Func,          // 配列に適用する関数
) -> Result<Object, EvalError>
where
    Func: Fn(&Vec<Object>) -> Result<Object, EvalError>,
{
    if args.len() != req_param_num {
        return Err(EvalError::IllegalSyntax(format!(
            "{} requires {} args. but {} args.",
            name,
            req_param_num,
            args.len()
        )));
    }
    // ビルトイン関数の第一引数は配列とする．
    // eg. first(arr), push(arr, elem), ...
    match args.first().unwrap() {
        Object::Array(arr) => apply(arr),
        _ => Err(EvalError::IllegalSyntax(format!(
            "{} requires Array.",
            name
        ))),
    }
}
fn first(args: Vec<Object>) -> Result<Object, EvalError> {
    let req_param_num = 1;
    apply_arr_func("first()", &args, req_param_num, |arr| {
        if arr.is_empty() {
            Err(EvalError::IndexOutOfRange((0, 0)))
        } else {
            Ok(arr.first().unwrap().clone())
        }
    })
}
fn last(args: Vec<Object>) -> Result<Object, EvalError> {
    let req_param_num = 1;
    apply_arr_func("last()", &args, req_param_num, |arr| {
        if arr.is_empty() {
            Err(EvalError::IndexOutOfRange((0, 0)))
        } else {
            Ok(arr.last().unwrap().clone())
        }
    })
}
fn rest(args: Vec<Object>) -> Result<Object, EvalError> {
    let req_param_num = 1;
    apply_arr_func("rest()", &args, req_param_num, |arr| {
        if arr.is_empty() {
            Err(EvalError::IllegalSyntax(
                "rest() applied empty array.".to_string(),
            ))
        } else {
            let slice = &arr[1..];
            Ok(Object::Array(slice.to_vec()))
        }
    })
}

fn push(args: Vec<Object>) -> Result<Object, EvalError> {
    let req_param_num = 2;
    apply_arr_func("push()", &args, req_param_num, |arr| {
        let mut new_arr = arr.clone();
        // 追加する要素を第2引数から抜く
        let addend = args.get(1).unwrap().clone();
        new_arr.push(addend);
        Ok(Object::Array(new_arr))
    })
}

fn puts(args: Vec<Object>) -> Result<Object, EvalError> {
    for o in args.iter() {
        println!("{}", o);
    }
    Ok(Object::Null)
}

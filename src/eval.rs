use crate::ast::Expression::*;
use crate::ast::Statement::*;
use crate::ast::*;
use crate::builtin;
use crate::env;
use crate::env::RefEnvironment;
use crate::object::ClosureValue;
use crate::object::Object;
use std::collections::HashMap;
use std::fmt;

/// 評価時エラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// 単項演算エラー (Operator, Operand)
    IllegalUnaryOperation(String, String),
    /// 二項演算エラー (Left, Operator, Right)
    IllegalBinaryOperation(String, String, String),
    /// 文字列エスケープ不正
    IllegalStringEscape(String),
    /// 不正な構文 (エラーの理由)
    IllegalSyntax(String),
    /// ゼロ除算 (diviend)
    ZeroDivision(i64),
    /// 不明な変数
    NameError(String),
    /// 配列範囲外
    IndexOutOfRange((usize, i64)),
}
impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IllegalUnaryOperation(operator, operand) => {
                write!(f, "illegal unary operation:`{} {}`", operator, operand)
            }
            Self::IllegalBinaryOperation(left, operator, right) => {
                write!(
                    f,
                    "illegal binary operation:`{} {} {}`",
                    left, operator, right
                )
            }
            Self::IllegalStringEscape(literal) => write!(f, "illegal string escape:{}", literal),
            Self::IllegalSyntax(cause) => write!(f, "illegal syntax:{}", cause),
            Self::ZeroDivision(diviend) => write!(f, "zero division:{}/0", diviend),
            Self::NameError(s) => write!(f, "undefined name:`{}`", s),
            Self::IndexOutOfRange((length, index)) => {
                write!(f, "index out of range [{}] with length {}", index, length)
            }
        }
    }
}

/// プログラムの評価
pub fn eval_program(program: Program) -> Result<Object, EvalError> {
    let env = env::new_env(HashMap::new());
    eval_statements(program, &env)
}

// 文の評価
fn eval_statement(stmt: Statement, env: &RefEnvironment) -> Result<Object, EvalError> {
    match stmt {
        Block(block) => eval_statements(block.statements, env),
        Let(letstmt) => eval_letstatement(letstmt, env),
        Return(exp) => {
            let val = eval_exp(exp, env)?;
            Ok(Object::Return(Box::new(val)))
        }
        If(ifstmt) => eval_ifstatement(ifstmt, env),
        Exp(exp) => eval_exp(exp, env),
        Empty => Ok(Object::Null),
    }
}

// 複数文の評価
pub fn eval_statements(block: Vec<Statement>, env: &RefEnvironment) -> Result<Object, EvalError> {
    let mut result = Object::Null;
    for s in block {
        result = eval_statement(s, env)?;
        match result {
            Object::Return(_) => return Ok(result),
            _ => continue,
        }
    }
    Ok(result)
}

fn eval_letstatement(letstmt: LetStatement, env: &RefEnvironment) -> Result<Object, EvalError> {
    let val = eval_exp(*letstmt.exp, env)?;
    env::set_value(env, &letstmt.id.symbol(), val);
    Ok(Object::Null)
}

fn eval_ifstatement(ifstmt: IfStatement, env: &RefEnvironment) -> Result<Object, EvalError> {
    let cond = eval_exp(*ifstmt.cond, env)?;
    match cond {
        Object::Bool(b) => {
            if b {
                Ok(eval_statements(ifstmt.cons, env)?)
            } else if let Some(alt) = ifstmt.alt {
                Ok(eval_statements(alt, env)?)
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(EvalError::IllegalSyntax(format!(
            "non-bool [{}] used as if condition",
            cond
        ))),
    }
}

// 式の評価
fn eval_exp(exp: Expression, env: &RefEnvironment) -> Result<Object, EvalError> {
    match exp {
        Int(n) => Ok(Object::Int(n.value)),
        Bool(b) => Ok(Object::Bool(b.value)),
        Str(s) => match create_string(&s.value) {
            Ok(s) => Ok(Object::Str(s)),
            Err(e) => Err(e),
        },
        Array(l) => {
            let mut elems = vec![];
            for e in l.elements.into_iter() {
                let ev = eval_exp(e, &env)?;
                elems.push(ev);
            }
            Ok(Object::Array(elems))
        }
        Index(i) => match eval_exp(*i.arr, &env)? {
            Object::Array(arr) => match eval_exp(*i.index, &env)? {
                Object::Int(num) => {
                    if num < 0 || num as usize > (arr.len() - 1) {
                        Err(EvalError::IndexOutOfRange((arr.len(), num)))
                    } else {
                        let elem = arr.get(num as usize).unwrap();
                        Ok(elem.clone())
                    }
                }
                _ => Err(EvalError::IllegalSyntax(
                    "index should be integer".to_string(),
                )),
            },
            _ => Err(EvalError::IllegalSyntax(
                "index was applied to non-array".to_string(),
            )),
        },
        Ident(i) => match env::get_value(&env, &i.symbol()) {
            Some(v) => Ok(v),
            None => Err(EvalError::NameError(i.symbol())),
        },
        Prefix(pexp) => eval_prefix(pexp, env),
        Infix(exp) => eval_infix(exp, env),

        Function(f) => Ok(Object::Closure(ClosureValue::new(
            &f.to_string(),
            f.params,
            f.block,
            env.clone(),
        ))),
        Call(c) => eval_calling_function(c, env),
    }
}

fn eval_prefix(pexp: PrefixExpression, env: &RefEnvironment) -> Result<Object, EvalError> {
    use crate::ast::UnOp::*;
    let ope_str: String = pexp.operator.to_string();
    let val = eval_exp(*pexp.right, env)?;
    match val {
        Object::Int(n) => match pexp.operator {
            Positive => Ok(val),
            Negative => Ok(Object::Int(-n)),
            Not => Err(EvalError::IllegalUnaryOperation(ope_str, n.to_string())),
        },
        Object::Bool(b) => match pexp.operator {
            Positive | Negative => Err(EvalError::IllegalUnaryOperation(ope_str, b.to_string())),
            Not => Ok(Object::Bool(!b)),
        },
        _ => Err(EvalError::IllegalUnaryOperation(ope_str, val.to_string())),
    }
}

fn eval_calling_function(call: CallFunction, env: &RefEnvironment) -> Result<Object, EvalError> {
    // クロージャー or ビルトイン関数を取り出す
    match eval_exp(*call.func, env)? {
        Object::Closure(closure) => eval_closure(closure, call.args, &env),
        Object::Return(r) => match *r {
            Object::Closure(closure) => eval_closure(closure, call.args, &env),
            _ => Err(EvalError::IllegalSyntax(String::from(
                "prev exp is not a function",
            ))),
        },
        Object::Builtin(s) => eval_builtin(&s, call.args, &env),
        _ => Err(EvalError::IllegalSyntax(String::from(
            "prev exp is not a function",
        ))),
    }
}

fn eval_infix(exp: InfixExpression, env: &RefEnvironment) -> Result<Object, EvalError> {
    use crate::ast::BinOp::*;
    let left = eval_exp(*exp.left, env)?;
    let right = eval_exp(*exp.right, env)?;

    // エラーを作っておく
    let err = EvalError::IllegalBinaryOperation(
        left.to_string(),
        exp.operator.to_string(),
        right.to_string(),
    );
    match left {
        Object::Int(l) => match right {
            Object::Int(r) => match exp.operator {
                Add => Ok(Object::Int(l + r)),
                Sub => Ok(Object::Int(l - r)),
                Mul => Ok(Object::Int(l * r)),
                Div => {
                    if r == 0 {
                        Err(EvalError::ZeroDivision(l))
                    } else {
                        Ok(Object::Int(l / r))
                    }
                }
                Gt => Ok(Object::Bool(l > r)),
                Lt => Ok(Object::Bool(l < r)),
                Eq => Ok(Object::Bool(l == r)),
                NotEq => Ok(Object::Bool(l != r)),
            },
            _ => Err(err),
        },
        Object::Bool(l) => match right {
            Object::Bool(r) => match exp.operator {
                Eq => Ok(Object::Bool(l == r)),
                NotEq => Ok(Object::Bool(l != r)),
                _ => Err(err),
            },
            _ => Err(err),
        },
        Object::Str(l) => match right {
            Object::Str(r) => match exp.operator {
                Add => Ok(Object::Str(format!("{}{}", l, r))),
                Gt => Ok(Object::Bool(l > r)),
                Lt => Ok(Object::Bool(l < r)),
                Eq => Ok(Object::Bool(l == r)),
                NotEq => Ok(Object::Bool(l != r)),
                _ => Err(err),
            },
            _ => Err(err),
        },
        _ => Err(err),
    }
}

fn eval_closure(
    closure: ClosureValue,
    args: Vec<Expression>,
    env: &RefEnvironment,
) -> Result<Object, EvalError> {
    // 引数のチェック
    if closure.params.len() != args.len() {
        return Err(EvalError::IllegalSyntax(format!(
            "closure requires {} args. but {} given",
            closure.params.len(),
            args.len()
        )));
    }

    // 実引数の評価
    let mut hash = HashMap::new();
    for (param, arg) in closure.params.into_iter().zip(args.into_iter()) {
        let obj = eval_exp(arg, env)?;
        hash.insert(param.symbol(), obj);
    }

    // 実引数を評価した環境にクロージャーの定義環境を追加
    let new_env = env::new_env(hash);
    env::add_outer(&new_env, &closure.env);

    // クロージャーブロックを評価
    eval_statements(closure.block, &new_env)
}

fn eval_builtin(
    name: &str,
    args: Vec<Expression>,
    env: &RefEnvironment,
) -> Result<Object, EvalError> {
    // 実引数の評価
    let mut evaluated_args = Vec::new();
    for arg in args.into_iter() {
        let obj = eval_exp(arg, env)?;
        evaluated_args.push(obj);
    }
    builtin::exec(name, evaluated_args)
}

fn create_string(literal: &str) -> Result<String, EvalError> {
    let mut chars = literal.chars().peekable();
    let mut s = String::new();
    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.next() {
                Some('\\') => s.push('\\'),
                Some('\"') => s.push('"'),
                Some('n') => s.push('\n'),
                Some('r') => s.push('\r'),
                Some('t') => s.push('\t'),
                Some(_) => return Err(EvalError::IllegalStringEscape(literal.to_string())),
                None => return Err(EvalError::IllegalStringEscape(literal.to_string())),
            },
            _ => s.push(c),
        }
    }
    Ok(s)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;
    use crate::object::*;
    use crate::parser;

    #[test]
    fn test_eval_int() {
        let tests = vec![
            ("0;", Object::Int(0)),
            ("1;", Object::Int(1)),
            ("1234567890;", Object::Int(1234567890)),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_boolean() {
        let tests = vec![
            ("true;", Object::Bool(true)),
            ("false;", Object::Bool(false)),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }
    #[test]
    fn test_eval_string() {
        let tests = vec![
            ("\"hello\";", Object::Str(String::from("hello"))),
            (r#" "hello"; "#, Object::Str(String::from("hello"))),
            (
                r#" "hello \"world\""; "#,
                Object::Str(String::from("hello \"world\"")),
            ),
            (r#" "\\" ; "#, Object::Str(String::from("\\"))),
            (r#""\\\\";"#, Object::Str(String::from("\\\\"))),
            (r#""\n";"#, Object::Str(String::from("\n"))),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }
    #[test]
    fn test_eval_string_err() {
        let tests = vec![
            r#" "\a"; "#, //
            r#" "\\\  "; "#,
        ];
        for input in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            if let Ok(obj) = eval_program(ast) {
                panic!("should panic. but {}", obj);
            }
        }
    }

    #[test]
    fn test_eval_null() {
        let tests = vec![
            (";", Object::Null),
            (";;", Object::Null),
            ("{;;}", Object::Null),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_prefix() {
        let tests = vec![
            ("!true;", Object::Bool(false)),
            ("!false;", Object::Bool(true)),
            ("+0;", Object::Int(0)),
            ("-0;", Object::Int(0)),
            ("+1;", Object::Int(1)),
            ("-1;", Object::Int(-1)),
            ("-fn(x) {x;}(3);", Object::Int(-3)),
            ("!fn() {true;}();", Object::Bool(false)),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_infix() {
        let tests = vec![
            ("5 + 5;", Object::Int(10)),
            ("5 - 5;", Object::Int(0)),
            ("5 * 5;", Object::Int(25)),
            ("5 / 5;", Object::Int(1)),
            ("5 > 5;", Object::Bool(false)),
            ("5 < 5;", Object::Bool(false)),
            ("5 == 5;", Object::Bool(true)),
            ("5 != 5;", Object::Bool(false)),
            ("1 + (2 + 3) + 4;", Object::Int(10)),
            ("(5 + 5) * 2;", Object::Int(20)),
            ("20 / (5 + 5);", Object::Int(2)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + - 10;", Object::Int(50)),
            ("-(5 + 5);", Object::Int(-10)),
            ("!(true == true);", Object::Bool(false)),
            ("!(true == true);", Object::Bool(false)),
            ("(1 < 2) == true;", Object::Bool(true)),
            ("(1 < 2) == false;", Object::Bool(false)),
            ("(1 > 2) == true;", Object::Bool(false)),
            ("(1 > 2) == false;", Object::Bool(true)),
            (
                r#" "hello" + "world"; "#,
                Object::Str(String::from("helloworld")),
            ),
            (
                r#" "hello" + "," + "world"; "#,
                Object::Str(String::from("hello,world")),
            ),
            (r#" "abc" == "abc"; "#, Object::Bool(true)),
            (r#" "abc" != "abc"; "#, Object::Bool(false)),
            (r#" "abc" < "bca"; "#, Object::Bool(true)),
            (r#" "abc" > "bca"; "#, Object::Bool(false)),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }
    #[test]

    fn test_eval_if() {
        let tests = vec![
            (r#" if (true) { 10; } "#, Object::Int(10)),
            (r#" if (false) { 10; } "#, Object::Null),
            (r#" if (1 < 2) { 10; } "#, Object::Int(10)),
            (r#" if (1 > 2) { 10; } "#, Object::Null),
            (r#" if (1 > 2) { 10; } else { 20;} "#, Object::Int(20)),
            (r#" if (1 < 2) { 10; } else { 20;} "#, Object::Int(10)),
            (
                r#"
            if (5 != 5) {
            } else {
                1;
                2;
            }
            "#,
                Object::Int(2),
            ),
            (
                r#"
            if (5 != 5) {
            }
            "#,
                Object::Null,
            ),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_return() {
        let tests = vec![
            ("return 10;", Object::Return(Box::new(Object::Int(10)))),
            ("return 10;9;", Object::Return(Box::new(Object::Int(10)))),
            ("9; return 10;9;", Object::Return(Box::new(Object::Int(10)))),
            (
                r#"
            fn() {
              if (10 > 1) {
                  return 10;
              }
              return 1;
            }();
            "#,
                Object::Return(Box::new(Object::Int(10))),
            ),
            (
                r#"
            fn() {
              if (10 < 1) {
                  return 10;
              }
              return 1;
            }();
            "#,
                Object::Return(Box::new(Object::Int(1))),
            ),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_let() {
        let tests = vec![
            ("let a = 5; a;", Object::Int(5)),
            ("let a = 5 * 5; a;", Object::Int(25)),
            ("let a = true; a;", Object::Bool(true)),
            ("let a = 5; let b = a; b;", Object::Int(5)),
            ("let a=5; let b=a; let c=a+b+5; c;", Object::Int(15)),
            ("let f = fn(x) { x * x; }; f(2);", Object::Int(4)),
        ];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_block() {
        let tests = vec![("{1;}", Object::Int(1)), ("{1;2;}", Object::Int(2))];
        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_function() {
        let tests = vec![
            (
                "let add = fn(a,b,c,d) { a + b + c + d; }; add(1,2,3,4);",
                Object::Int(10),
            ),
            (
                "let max = fn(x, y) { if (x > y) { x; } else { y; } }; max(5, 10);",
                Object::Int(10),
            ),
            (
                "let factorical = fn(n) { if (n == 0) { 1; } else { n * factorical(n - 1); } };factorical(5);",
                Object::Int(120),
            ),
            (
                r#"
        let z = 10;
        let newadder = fn(x) {return fn(y) {x + y + z;};};
        let addtwo = newadder(2);
        addtwo(3);
        "#,
                Object::Int(15),
            ),
            (
                r#"
        let z = 10;
        let newadder = fn(x) {fn(y) {x + y + z;};};
        let addtwo = newadder(2);
        addtwo(3);
        "#,
                Object::Int(15),
            ),
            (
                r#"
        let add = fn(a,b) { a + b ;};
        let apply = fn(a, b, func) { func(a,b); };
        apply(2, 3, add);
        "#,
                Object::Int(5),
            ),
            (
                r#"
        let sub = fn(a,b) { a - b ;};
        let apply = fn(a, b, func) { func(a,b); };
        apply(10, 2, sub);
        "#,
                Object::Int(8),
            ),
        ];

        for (input, expected) in tests {
            let tokens = lexer::lex(&input);
            let ast = parser::parse_program(tokens).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_eq!(expected, obj);
        }
    }

    #[test]
    fn test_eval_error() {
        use lexer;
        use parser;
        let tests = vec![(
            r#"5 + true;"#,
            EvalError::IllegalBinaryOperation("5".to_string(), "+".to_string(), "true".to_string()),
        )];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            let obj = eval_program(ast);
            match obj {
                Ok(_) => assert!(false, "error"),
                Err(err) => assert_eq!(expected, err),
            }
        }
    }
    #[test]
    fn test_eval_closure() {
        use lexer;
        use parser;
        let tests = vec![(
            r#"
            let f = fn(){;}; f;
             "#,
            Object::Null,
        )];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            let obj = eval_program(ast).unwrap();
            assert_ne!(expected, obj)
        }
    }
    #[test]
    fn test_closure_print() {
        use lexer;
        use parser;
        let tests = vec![(
            r#"
            let f = fn(){;}; f;
             "#,
            Object::Null,
        )];
        for (input, _) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            let obj = eval_program(ast).unwrap();
            println!("{}", obj);
        }
    }

    #[test]
    fn test_eval_builtin_ok() {
        use lexer;
        use parser;
        let tests = vec![
            (
                r#"
            len("hello");
             "#,
                Object::Int(5),
            ),
            (
                r#"
            len("");
             "#,
                Object::Int(0),
            ),
            (
                r#"
            len("こんにちは，世界");
             "#,
                Object::Int(8),
            ),
            (
                r#"
            len("hello" + "," + "世界");
             "#,
                Object::Int(8),
            ),
            (
                r#"
            let a = [];
            len(a);
             "#,
                Object::Int(0),
            ),
            (
                r#"
            let a = [1];
            len(a);
             "#,
                Object::Int(1),
            ),
        ];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            match eval_program(ast) {
                Ok(obj) => assert_eq!(expected, obj),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_eval_builtin_ng() {
        use lexer;
        use parser;
        let tests = vec![
            (
                r#"
            len(1);
             "#,
                EvalError::IllegalSyntax("".to_string()),
            ),
            (
                r#"
            len(true);
             "#,
                EvalError::IllegalSyntax("".to_string()),
            ),
            (
                r#"
            len();
             "#,
                EvalError::IllegalSyntax("".to_string()),
            ),
        ];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            match eval_program(ast) {
                Ok(_) => panic!("expects failure, but success."),
                Err(e) => match e {
                    EvalError::IllegalSyntax(_) => {}
                    _ => panic!("expects {} but [{}].", expected, e),
                },
            }
        }
    }

    #[test]
    fn test_eval_array_literal() {
        use lexer;
        use parser;
        let tests = vec![
            (
                r#"
            [1,"a", true];
             "#,
                Object::Array(vec![
                    Object::Int(1),
                    Object::Str("a".to_string()),
                    Object::Bool(true),
                ]),
            ),
            (
                r#"
            [];
             "#,
                Object::Array(vec![]),
            ),
            (
                r#"
            [[1]];
             "#,
                Object::Array(vec![Object::Array(vec![Object::Int(1)])]),
            ),
            (
                r#"
            [2*2, 3+ 3];
             "#,
                Object::Array(vec![Object::Int(4), Object::Int(6)]),
            ),
        ];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            match eval_program(ast) {
                Ok(obj) => assert_eq!(expected, obj),
                _ => panic!(),
            }
        }
    }
    #[test]
    fn test_eval_array_index() {
        use lexer;
        use parser;
        let tests = vec![
            (
                r#"
            let a = [1];
            a[0];
             "#,
                Object::Int(1),
            ),
            (
                r#"
            let a = [1, "str" ];
            a[1];
             "#,
                Object::Str("str".to_string()),
            ),
            (
                r#"
            let a = [2];
            let f = fn(x) { x * 2;};
            f(a[0]);
             "#,
                Object::Int(4),
            ),
            (
                r#"
            let a = [0,1];
            let f = fn(x) { x;};
            a[f(1)];
             "#,
                Object::Int(1),
            ),
        ];
        for (input, expected) in tests.into_iter() {
            let ast = parser::parse_program(lexer::lex(&input)).unwrap();
            match eval_program(ast) {
                Ok(obj) => assert_eq!(expected, obj),
                _ => panic!(),
            }
        }
    }
}

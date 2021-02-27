use super::ast::Expression::*;
use super::ast::Statement::*;
use super::ast::*;
use super::env;
use super::env::RefEnvironment;
use super::object::Object;
use std::collections::HashMap;
use std::fmt;

/// 評価時エラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// 単項演算エラー (Operator, Operand)
    IllegalUnaryOperation(String, String),
    /// 二項演算エラー (Left, Operator, Right)
    IllegalBinaryOperation(String, String, String),
    /// 不正な構文 (エラーの理由)
    IllegalSyntax(String),
    /// ゼロ除算 (diviend)
    ZeroDivision(i64),
    /// 不明な変数
    NameError(String),
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
            Self::IllegalSyntax(cause) => write!(f, "illegal syntax:{}", cause),
            Self::ZeroDivision(diviend) => write!(f, "zero division:{}/0", diviend),
            Self::NameError(s) => write!(f, "undefined name:`{}`", s),
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
            } else {
                if let Some(alt) = ifstmt.alt {
                    Ok(eval_statements(alt, env)?)
                } else {
                    Ok(Object::Null)
                }
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
        Ident(i) => match env::get_value(&env, &i.symbol()) {
            Some(v) => Ok(v),
            None => Err(EvalError::NameError(i.symbol())),
        },
        Prefix(pexp) => eval_prefix(pexp, env),
        Infix(exp) => eval_infix(exp, env),
        Function(f) => Ok(Object::Closure(f, env.clone())),
        Call(c) => eval_calling_function(c, env),
    }
}

fn eval_prefix(pexp: PrefixExpression, env: &RefEnvironment) -> Result<Object, EvalError> {
    use super::ast::UnOp::*;
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

fn eval_calling_function(c: CallFunction, env: &RefEnvironment) -> Result<Object, EvalError> {
    // クロージャーを取り出す
    let (func, closed_env) = match eval_exp(*c.func, env)? {
        Object::Closure(f, e) => (f, e),
        Object::Return(r) => match *r {
            Object::Closure(f, e) => (f, e),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    // 実引数の評価
    let mut hash = HashMap::new();
    for (param, arg) in (func.params).into_iter().zip(c.args.into_iter()) {
        let obj = eval_exp(arg, env)?;
        hash.insert(param.symbol(), obj);
    }

    // 実引数を評価した環境にクロージャーの定義環境を追加
    let new_env = env::new_env(hash);
    env::add_outer(&new_env, &closed_env);

    // クロージャーブロックを評価
    eval_statements(func.block, &new_env)
}

fn eval_infix(exp: InfixExpression, env: &RefEnvironment) -> Result<Object, EvalError> {
    use super::ast::BinOp::*;
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
        _ => Err(err),
    }
}

#[cfg(test)]
mod test {
    use super::super::lexer;
    use super::super::parser;
    use super::*;

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
    // TODO 実行時エラーのテスト.
    // zero divideとか, 束縛してない,とか
}
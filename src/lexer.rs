use super::token::*;
use std::iter::Peekable;

/// 字句解析結果
pub type LexResult = Vec<Token>;

/// 入力からTokenの配列を返す.
/// * char単位だと簡単に複数バイト文字が扱えるのでcharのsliceベースで処理することにする.
pub fn lex(input: &str) -> LexResult {
    use TokenKind::*;

    // 字句解析に便利なpeekableに変換
    let mut peekable = input.chars().peekable();
    let mut tokens = Vec::new();
    let mut pos = 0;

    // 1トークン(専用関数)解析
    macro_rules! lex_a_token {
        ($do_lexing:expr) => {
            let (token, after_pos) = $do_lexing;
            tokens.push(token);
            pos = after_pos;
        };
    }

    // 1文字解析
    macro_rules! lex_a_char {
        ($ch:expr, $kind:expr) => {{
            tokens.push(Token::new($kind, $ch.to_string(), pos, pos + 1));
            next_char!();
        }};
    }

    macro_rules! next_char {
        () => {{
            peekable.next().unwrap();
            pos = pos + 1;
        }};
    }

    while let Some(&c) = peekable.peek() {
        if c.is_digit(10) {
            lex_a_token!(lex_int(&mut peekable, pos));
        } else if c.is_alphabetic() {
            lex_a_token!(lex_alpha(&mut peekable, pos));
        } else if c == '=' {
            lex_a_token!(lex_prefix_equal(&mut peekable, pos));
        } else if c == '!' {
            lex_a_token!(lex_prefix_exc(&mut peekable, pos));
        } else {
            match c {
                '+' => lex_a_char!(c, PLUS),
                '-' => lex_a_char!(c, MINUS),
                '*' => lex_a_char!(c, ASTERISK),
                '/' => lex_a_char!(c, SLASH),
                '<' => lex_a_char!(c, LT),
                '>' => lex_a_char!(c, GT),
                ';' => lex_a_char!(c, SEMICOLON),
                ',' => lex_a_char!(c, COMMA),
                '{' => lex_a_char!(c, LBRACE),
                '}' => lex_a_char!(c, RBRACE),
                '(' => lex_a_char!(c, LPAREN),
                ')' => lex_a_char!(c, RPAREN),
                ' ' | '\n' | '\r' | '\t' => next_char!(),
                _ => lex_a_char!(c, ILLEGAL),
            };
        }
    }
    tokens
}

// 数列
// 0, 1, 12, 01, ...
fn lex_int<'a, Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    use TokenKind::*;

    let mut pos = start;
    let mut s = String::new();
    while let Some(c) = input.peek() {
        if c.is_digit(10) {
            s.push(*c);
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    (Token::new(INT, s, start, pos), pos)
}

// アルファベットの列
// a, A, aB, ABC
fn lex_alpha<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    use TokenKind::*;

    let mut pos = start;
    let mut s = String::new();
    while let Some(&c) = input.peek() {
        if c.is_alphabetic() {
            s.push(c);
            input.next().unwrap();
            pos += 1;
        } else {
            break;
        }
    }
    match s.as_str() {
        "let" => (Token::new(LET, s, start, pos), pos),
        "fn" => (Token::new(FUNCTION, s, start, pos), pos),
        "if" => (Token::new(IF, s, start, pos), pos),
        "else" => (Token::new(ELSE, s, start, pos), pos),
        "true" => (Token::new(TRUE, s, start, pos), pos),
        "false" => (Token::new(FALSE, s, start, pos), pos),
        "return" => (Token::new(RETURN, s, start, pos), pos),
        _ => (Token::new(IDENT, s, start, pos), pos),
    }
}

// =で始まる
fn lex_prefix_equal<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    use TokenKind::*;
    let mut pos = start;

    // 1文字目の = を刈り取り
    input.next().unwrap();
    pos += 1;
    match input.peek() {
        Some(&c) => {
            if c == '=' {
                input.next().unwrap();
                pos += 1;
                (Token::new(EQ, String::from("=="), start, pos), pos)
            } else {
                (Token::new(ASSIGN, String::from("="), start, pos), pos)
            }
        }
        None => (Token::new(ASSIGN, String::from("="), start, pos), pos),
    }
}

// !で始まる
fn lex_prefix_exc<Tokens>(input: &mut Peekable<Tokens>, start: usize) -> (Token, usize)
where
    Tokens: Iterator<Item = char>,
{
    use TokenKind::*;
    let mut pos = start;

    // 1文字目の ! を刈り取り
    input.next().unwrap();
    pos += 1;
    match input.peek() {
        Some(&c) => {
            if c == '=' {
                input.next().unwrap();
                pos += 1;
                (Token::new(NOTEQ, String::from("!="), start, pos), pos)
            } else {
                (Token::new(BANG, String::from("!"), start, pos), pos)
            }
        }
        None => (Token::new(BANG, String::from("!"), start, pos), pos),
    }
}

#[test]

fn test_lex() {
    use TokenKind::*;
    let inputs = vec!["ab = 1", "a == 10", "bca = !true"];
    let expected = vec![
        vec![
            Token::new(IDENT, String::from("ab"), 0, 2),
            Token::new(ASSIGN, String::from("="), 3, 4),
            Token::new(INT, String::from("1"), 5, 6),
        ],
        vec![
            Token::new(IDENT, String::from("a"), 0, 1),
            Token::new(EQ, String::from("=="), 2, 4),
            Token::new(INT, String::from("10"), 5, 7),
        ],
        vec![
            Token::new(IDENT, String::from("bca"), 0, 3),
            Token::new(ASSIGN, String::from("="), 4, 5),
            Token::new(BANG, String::from("!"), 6, 7),
            Token::new(TRUE, String::from("true"), 7, 11),
        ],
    ];
    for (idx, input) in inputs.iter().enumerate() {
        assert_eq!(expected[idx], lex(input));
    }
}

#[test]
fn test_lex_int() {
    use TokenKind::*;
    let tests = vec![
        ("1", Token::new(INT, String::from("1"), 0, 1)),
        ("0", Token::new(INT, String::from("0"), 0, 1)),
        ("12", Token::new(INT, String::from("12"), 0, 2)),
    ];
    for (input, expected) in tests {
        let mut p = input.chars().peekable();
        assert_eq!(expected, lex_int(&mut p, 0).0);
    }
}

#[test]
fn test_lex_alpha() {
    use TokenKind::*;

    let tests = vec![
        ("let", Token::new(LET, String::from("let"), 0, 3)),
        ("fn", Token::new(FUNCTION, String::from("fn"), 0, 2)),
        ("if", Token::new(IF, String::from("if"), 0, 2)),
        ("else", Token::new(ELSE, String::from("else"), 0, 4)),
        ("true", Token::new(TRUE, String::from("true"), 0, 4)),
        ("false", Token::new(FALSE, String::from("false"), 0, 5)),
        ("return", Token::new(RETURN, String::from("return"), 0, 6)),
        ("var", Token::new(IDENT, String::from("var"), 0, 3)),
    ];
    for (input, expected) in tests {
        let mut p = input.chars().peekable();
        assert_eq!(expected, lex_alpha(&mut p, 0).0);
    }
}

use crate::ast::Expression::*;
use crate::ast::Program;
use crate::ast::Statement::*;
use crate::ast::*;
use crate::lexer::LexResult;
use crate::token::{Token, TokenKind, TokenKind::*};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;

/// 演算子優先度
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
enum Priority {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

// トークンの優先度表
lazy_static! {
    static ref PRIORITY: HashMap<TokenKind, Priority> = {
        let mut priority = HashMap::new();
        // =, !=
        priority.insert(TokenKind::EQ, Priority::EQUALS);
        priority.insert(TokenKind::NOTEQ, Priority::EQUALS);

        // >, <
        priority.insert(TokenKind::GT, Priority::LESSGREATER);
        priority.insert(TokenKind::LT, Priority::LESSGREATER);

        // -, +
        priority.insert(TokenKind::PLUS, Priority::SUM);
        priority.insert(TokenKind::MINUS, Priority::SUM);

        // *, /
        priority.insert(TokenKind::ASTERISK, Priority::PRODUCT);
        priority.insert(TokenKind::SLASH, Priority::PRODUCT);

        // xxx(
        priority.insert(TokenKind::LPAREN, Priority::CALL);

        priority
    };
}

/// パースエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// 不正なトークン
    IllegalToken(Token),
    /// 予期していないトークン
    UnexpectedToken(Token),
    /// 予期せぬ入力終了
    UnexpectedEOF,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IllegalToken(t) => write!(f, "illegal token `{}`({})", t.literal, t.loc),
            Self::UnexpectedToken(t) => write!(f, "illegal token `{}`({})", t.literal, t.loc),
            Self::UnexpectedEOF => write!(f, "unexpected eof"),
        }
    }
}

pub fn parse_program(lex_result: LexResult) -> Result<Program, ParseError> {
    let tokens = lex_result;
    let mut tokens = tokens.into_iter().peekable();

    let mut program = vec![];
    while tokens.peek().is_some() {
        let statement = parse_statement(&mut tokens)?;
        match statement {
            Statement::Empty => {}
            _ => program.push(statement),
        }
    }
    Ok(program)
}

fn parse_statement<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Statement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match peek_kind(tokens) {
        None => Err(ParseError::UnexpectedEOF),
        Some(LET) => parse_let_stmt(tokens),
        Some(RETURN) => parse_return_stmt(tokens),
        Some(IF) => parse_if_stmt(tokens),
        Some(LBRACE) => parse_block_stmt(tokens),
        Some(SEMICOLON) => {
            tokens.next().unwrap();
            Ok(Empty)
        }
        Some(_) => {
            let exp = parse_exp(tokens, Priority::LOWEST)?;
            expect_next(tokens, TokenKind::SEMICOLON)?;
            Ok(Exp(exp))
        }
    }
}

fn parse_let_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Statement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let let_token = expect_next(tokens, LET)?;
    let ident_token = expect_next(tokens, IDENT)?;
    let ident = Identifier::new(ident_token);
    expect_next(tokens, ASSIGN)?;
    let exp = parse_exp(tokens, Priority::LOWEST)?;
    expect_next(tokens, TokenKind::SEMICOLON)?;

    Ok(Let(LetStatement::new(let_token, ident, exp)))
}

fn parse_return_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Statement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    expect_next(tokens, RETURN)?;
    let exp = parse_exp(tokens, Priority::LOWEST)?;
    expect_next(tokens, TokenKind::SEMICOLON)?;

    Ok(Return(exp))
}

fn parse_if_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Statement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let iftoken = expect_next(tokens, IF)?;

    expect_next(tokens, LPAREN)?;
    let cond = parse_exp(tokens, Priority::LOWEST)?;
    expect_next(tokens, RPAREN)?;

    // then
    let cons_block = parse_block(tokens)?;

    // alt
    if let Some(TokenKind::ELSE) = peek_kind(tokens) {
        expect_next(tokens, ELSE)?;
        let alt_block = parse_block(tokens)?;
        Ok(If(IfStatement::new(
            iftoken,
            cond,
            cons_block.statements,
            Some(alt_block.statements),
        )))
    } else {
        Ok(If(IfStatement::new(
            iftoken,
            cond,
            cons_block.statements,
            None,
        )))
    }
}

fn parse_block_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Statement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let block = parse_block(tokens)?;
    Ok(Block(block))
}

fn parse_exp<Tokens>(
    tokens: &mut Peekable<Tokens>,
    cur_pri: Priority,
) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut left = match peek_kind(tokens) {
        Some(PLUS) | Some(MINUS) | Some(BANG) => parse_prefix_exp(tokens),
        Some(INT) => parse_int_exp(tokens),
        Some(TRUE) => parse_bool_exp(tokens, TRUE),
        Some(FALSE) => parse_bool_exp(tokens, FALSE),
        Some(STRING) => parse_str_exp(tokens),
        Some(IDENT) => parse_ident_exp(tokens),
        Some(LPAREN) => parse_group_exp(tokens),
        Some(FUNCTION) => parse_function_exp(tokens),
        Some(ILLEGAL) => return Err(ParseError::IllegalToken(tokens.next().unwrap())),
        Some(_) => return Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
        None => return Err(ParseError::UnexpectedEOF),
    }?;

    while let Some(next_kind) = peek_kind(tokens) {
        // 文の終わり
        if next_kind == SEMICOLON {
            break;
        }
        // 次のトークンとの結合度をとって, 次のトークンとの結合度が高い場合は先に進む.
        let next_pri = get_priority(next_kind);
        if next_pri <= cur_pri {
            break;
        }
        left = match next_kind {
            LPAREN => parse_call_exp(tokens, left)?,
            PLUS | MINUS | ASTERISK | SLASH | GT | LT | EQ | NOTEQ => {
                parse_infix_exp(tokens, left, next_pri)?
            }
            _ => return Err(ParseError::UnexpectedToken(tokens.next().unwrap())),
        }
    }
    Ok(left)
}

fn parse_prefix_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // +, -, !
    let operator = tokens.next().unwrap();
    // 式を読む
    let right = parse_exp(tokens, Priority::PREFIX)?;

    Ok(Prefix(PrefixExpression::new(operator, right)))
}

fn parse_int_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let token = expect_next(tokens, INT)?;
    Ok(Int(IntegerLiteral::new(token)))
}

fn parse_str_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let token = expect_next(tokens, STRING)?;
    Ok(Str(StringLiteral::new(token)))
}

fn parse_bool_exp<Tokens>(
    tokens: &mut Peekable<Tokens>,
    expect: TokenKind,
) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `true` or `false`
    let token = expect_next(tokens, expect)?;
    Ok(Bool(BoolLiteral::new(token)))
}

fn parse_ident_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let token = expect_next(tokens, IDENT)?;
    Ok(Ident(Identifier::new(token)))
}

fn parse_group_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    expect_next(tokens, LPAREN)?;
    let exp = parse_exp(tokens, Priority::LOWEST)?;
    expect_next(tokens, RPAREN)?;
    Ok(exp)
}

fn parse_function_exp<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `fn`の刈り取り
    let token = expect_next(tokens, FUNCTION)?;

    // `(`の刈り取り
    expect_next(tokens, LPAREN)?;

    let params = parse_function_params(tokens)?;

    // `)`の刈り取り
    expect_next(tokens, RPAREN)?;

    let block = parse_block(tokens)?;

    Ok(Expression::Function(FunctionLiteral::new(
        token,
        params,
        block.statements,
    )))
}

fn parse_function_params<Tokens>(
    tokens: &mut Peekable<Tokens>,
) -> Result<Vec<Identifier>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut params = Vec::new();

    while is_expected_peek(tokens, IDENT) {
        let token = expect_next(tokens, IDENT)?;
        let ident = Identifier::new(token);
        params.push(ident);
        if let Some(COMMA) = peek_kind(tokens) {
            tokens.next().unwrap();
        } else {
            break;
        }
    }
    Ok(params)
}

fn parse_call_exp<Tokens>(
    tokens: &mut Peekable<Tokens>,
    left: Expression,
) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `(`の刈り取り
    let token = expect_next(tokens, LPAREN)?;

    // 実引数の刈り取り
    let args = parse_args(tokens)?;

    // `)`の刈り取り
    expect_next(tokens, RPAREN)?;

    // left は identifier or 関数リテラル式
    Ok(Call(CallFunction::new(token, left, args)))
}

fn parse_infix_exp<Tokens>(
    tokens: &mut Peekable<Tokens>,
    left: Expression,
    pri: Priority,
) -> Result<Expression, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let operator = tokens.next().unwrap();
    let right = parse_exp(tokens, pri)?;
    Ok(Infix(InfixExpression::new(operator, left, right)))
}

fn parse_block<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BlockStatement, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `{`の刈り取り
    let token = expect_next(tokens, LBRACE)?;

    let mut statements = Vec::new();

    while !is_expected_peek(tokens, RBRACE) {
        let stmt = parse_statement(tokens)?;
        statements.push(stmt);
    }

    // `}`を刈り取る
    expect_next(tokens, RBRACE)?;

    Ok(BlockStatement::new(token, statements))
}

fn parse_args<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Expression>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut args = Vec::new();

    while !is_expected_peek(tokens, RPAREN) {
        let exp = parse_exp(tokens, Priority::LOWEST)?;
        args.push(exp);
        if let Some(COMMA) = peek_kind(tokens) {
            tokens.next().unwrap();
        } else {
            break;
        }
    }
    Ok(args)
}

fn peek_kind<Tokens>(tokens: &mut Peekable<Tokens>) -> Option<TokenKind>
where
    Tokens: Iterator<Item = Token>,
{
    tokens.peek().map(|token| token.kind)
}

// 次にくる予定のトークン種別を待ち受け
fn expect_next<Tokens>(
    tokens: &mut Peekable<Tokens>,
    expected: TokenKind,
) -> Result<Token, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match peek_kind(tokens) {
        None => Err(ParseError::UnexpectedEOF),
        Some(kind) => {
            if kind == expected {
                Ok(tokens.next().unwrap())
            } else {
                Err(ParseError::UnexpectedToken(tokens.next().unwrap()))
            }
        }
    }
}

fn is_expected_peek<Tokens>(tokens: &mut Peekable<Tokens>, expected: TokenKind) -> bool
where
    Tokens: Iterator<Item = Token>,
{
    if let Some(kind) = peek_kind(tokens) {
        kind == expected
    } else {
        false
    }
}

fn get_priority(kind: TokenKind) -> Priority {
    match PRIORITY.get(&kind) {
        Some(pri) => *pri,
        None => Priority::LOWEST,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;
    use crate::token::*;

    #[test]
    fn test_parse_ident() {
        let inputs = vec![String::from("x"), String::from("result")];
        let expected_tokens = inputs
            .iter()
            .map(|s| Token::new(TokenKind::IDENT, s, 0, s.len()))
            .collect::<Vec<Token>>();
        for (i, _) in inputs.iter().enumerate() {
            let expected_ast = Identifier::new(expected_tokens[i].clone());
            let result = lexer::lex(&inputs[i]);
            let ast = parse_ident_exp(&mut result.into_iter().peekable()).unwrap();
            assert_eq!(ast, Ident(expected_ast));
        }
    }

    #[test]
    fn test_parse_int() {
        let inputs = vec![String::from("0"), String::from("12")];
        let expected_tokens = inputs
            .iter()
            .map(|s| Token::new(TokenKind::INT, s, 0, s.len()))
            .collect::<Vec<Token>>();
        for (i, _) in inputs.iter().enumerate() {
            let expected_ast = IntegerLiteral::new(expected_tokens[i].clone());
            let result = lexer::lex(&inputs[i]);
            let ast = parse_int_exp(&mut result.into_iter().peekable()).unwrap();
            assert_eq!(ast, Int(expected_ast));
        }
    }

    #[test]
    fn test_parse_str() {
        let inputs = vec![
            String::from("\"hello\""),
            String::from("\"a\""),
            String::from("\"\\\\\""),
        ];
        let expected_tokens = inputs
            .iter()
            .map(|s| Token::new(TokenKind::STRING, s, 0, s.len()))
            .collect::<Vec<Token>>();
        for (i, _) in inputs.iter().enumerate() {
            let expected_ast = StringLiteral::new(expected_tokens[i].clone());
            let result = lexer::lex(&inputs[i]);
            let ast = parse_str_exp(&mut result.into_iter().peekable()).unwrap();
            assert_eq!(ast, Str(expected_ast));
        }
    }

    #[test]
    fn test_parse_function() {
        let input = String::from("fn(x, y) { x; } ");
        let result = lexer::lex(&input);
        let a = parse_function_exp(&mut result.into_iter().peekable()).unwrap();
        match a {
            Function(_) => {}
            err => panic!(format!("test failed. reason [{:?}]", err)),
        }
    }

    #[test]
    fn test_parse_call() {
        let id_token = Token::new(TokenKind::IDENT, "function", 0, 8);
        let id_exp = Ident(Identifier::new(id_token));
        let input = String::from("(5,6)");
        let result = lexer::lex(&input);
        let a = parse_call_exp(&mut result.into_iter().peekable(), id_exp).unwrap();
        match a {
            Call(_) => {}
            err => panic!(format!("test failed. reason [{:?}]", err)),
        }
    }

    #[test]
    fn test_parse() {
        let input = String::from(
            r#"
    fn(x,y) { x; }(2,3);
    "#,
        );
        parse_program(lexer::lex(&input)).unwrap();
    }

    #[test]
    fn test_parse_block() {
        let input = String::from("{y; x; }");
        let result = lexer::lex(&input);
        parse_block(&mut result.into_iter().peekable()).unwrap();
    }

    #[test]
    fn test_parse_let() {
        let input = String::from("let x = 0;");
        let result = lexer::lex(&input);
        parse_let_stmt(&mut result.into_iter().peekable()).unwrap();
        parse_program(lexer::lex(&input)).unwrap();
    }

    #[test]
    fn test_parse_return() {
        let input = String::from("return 1;");
        let result = lexer::lex(&input);
        parse_return_stmt(&mut result.into_iter().peekable()).unwrap();

        parse_program(lexer::lex(&input)).unwrap();
    }

    #[test]
    fn test_parse_if() {
        let input = String::from("if (true) { 1; } else { 2; }");
        let result = lexer::lex(&input);
        parse_program(result).unwrap();
    }

    #[test]
    fn test_parse_blockstmt() {
        let input = String::from("{1;}");
        let result = lexer::lex(&input);
        parse_program(result).unwrap();
    }

    #[test]
    fn test_parse_empty() {
        let input = String::from(";;;;");
        let result = lexer::lex(&input);
        let ast = parse_program(result).unwrap();
        assert_eq!(0, ast.len());
    }
}

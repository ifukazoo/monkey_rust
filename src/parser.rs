use super::ast::*;
use super::lexer::*;
use super::token::*;
use std::collections::HashMap;

#[derive(PartialEq, PartialOrd, Copy, Clone, Debug, Eq)]
enum Precedence {
    LOWEST,
    // ==
    EQUALS,
    // > or <
    LESSGREATER,
    // +
    SUM,
    // *
    PRODUCT,
    // -X or !X
    PREFIX,
    // myFunction(X)
    CALL,
}

type PrefixParser<'a> = fn(selff: &mut Parser<'a>) -> Option<Box<dyn Expression>>;
type InfixParser<'a> =
    fn(selff: &mut Parser<'a>, Box<dyn Expression>) -> Option<Box<dyn Expression>>;

pub struct Parser<'a> {
    l: &'a mut Lexer<'a>,
    // カーソル地点のトークン
    cur_token: Token,
    // カーソル地点の次のトークン
    peek_token: Token,
    errors: Vec<String>,
    prefix_parsers: HashMap<TokenType, PrefixParser<'a>>,
    infix_parsers: HashMap<TokenType, InfixParser<'a>>,
    precedences: HashMap<TokenType, Precedence>,
}

impl<'a> Parser<'a> {
    pub fn new(lex: &'a mut Lexer<'a>) -> Parser<'a> {
        let mut p = Parser {
            l: lex,
            cur_token: Default::default(),
            peek_token: Default::default(),
            errors: Default::default(),
            prefix_parsers: HashMap::new(),
            infix_parsers: HashMap::new(),
            precedences: HashMap::new(),
        };
        p.register_prefix();
        p.register_infix();
        p.register_precedence();

        p.next_token();
        p.next_token();
        p
    }
    pub fn errors(&self) -> &[String] {
        &self.errors
    }
    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token().clone();
    }
    pub fn parse_program(&mut self) -> Box<Program> {
        let mut statements = vec![];
        while !self.cur_token_is(TokenType::EOF) {
            match self.parse_statement() {
                Some(s) => statements.push(s),
                None => {}
            }
            self.next_token();
        }
        Box::new(Program { statements })
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.t_type {
            TokenType::LET => self.parse_letstatement(),
            TokenType::RETURN => self.parse_returnstatement(),
            _ => self.parse_expression_statement(),
        }
    }
    // let 文
    fn parse_letstatement(&mut self) -> Option<Box<dyn Statement>> {
        // let x = 5;
        // ^
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }
        // let x = 5;
        //     ^
        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.to_string(),
        };
        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        // let x = 5;
        //       ^
        self.next_token();
        // let x = 5;
        //         ^
        let value = self.parse_expression(Precedence::LOWEST);
        // セミコロンまで刈り取り
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        match value {
            Some(value) => Some(Box::new(LetStatement { token, name, value })),
            None => None,
        }
    }
    // return 文
    fn parse_returnstatement(&mut self) -> Option<Box<dyn Statement>> {
        // return 5;
        // ^
        let token = self.cur_token.clone();
        self.next_token();
        // return 5;
        //        ^
        let value = self.parse_expression(Precedence::LOWEST);
        // セミコロンまで刈り取り
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        match value {
            Some(value) => Some(Box::new(ReturnStatement { token, value })),
            None => None,
        }
    }
    // 式文
    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        let exp = self.parse_expression(Precedence::LOWEST);

        // セミコロンまで刈り取り
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        match exp {
            Some(exp) => Some(Box::new(ExpressionStatement { token, exp })),
            None => None,
        }
    }

    // 式
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix_parser = self.get_prefix_parser()?;
        let mut leftexp = prefix_parser(self)?;

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            self.next_token();
            let parser = self.get_infix_parser()?;
            leftexp = parser(self, leftexp)?;
        }
        Some(leftexp)
    }

    // グループ化された式
    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        // (exp)
        // ^
        self.next_token();
        // (exp)
        //  ^
        let exp = self.parse_expression(Precedence::LOWEST)?;

        if self.expect_peek(TokenType::RPAREN) {
            Some(exp)
        } else {
            None
        }
    }

    // ブロック文
    fn parse_block_statement(&mut self) -> Option<Box<BlockStatement>> {
        // { exp }
        // ^
        let token = self.cur_token.clone();
        self.next_token();
        // { exp }
        //   ^
        let mut statements: Vec<Box<dyn Statement>> = vec![];
        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Some(Box::new(BlockStatement { token, statements }))
    }

    // if 式
    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        // if (x < y) { x }
        // ^
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        // if (x < y) { x }
        //    ^
        self.next_token();
        // if (x < y) { x }
        //     ^
        let condition = self.parse_expression(Precedence::LOWEST)?;
        // if (x < y) { x }
        //         ^
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        // if (x < y) { x }
        //          ^
        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }
        // if (x < y) { x }
        //            ^
        let consequence = self.parse_block_statement()?;
        // if (x < y) { x } else { y }
        //                ^
        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            // if (x < y) { x } else { y }
            //                  ^
            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }
            // if (x < y) { x } else { y }
            //                       ^
            let alter = self.parse_block_statement()?;
            Some(Box::new(IfExpression {
                token,
                condition,
                consequence,
                alternarive: Some(alter),
            }))
        } else {
            Some(Box::new(IfExpression {
                token,
                condition,
                consequence,
                alternarive: None,
            }))
        }
    }

    // 関数リテラル 式
    fn parse_funtionliteral_expression(&mut self) -> Option<Box<dyn Expression>> {
        // fn (x , y) { x+y; }
        // ^
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        // fn (x , y) { x+y; }
        //    ^
        self.next_token();
        // fn (x , y) { x+y; }
        //     ^
        let params = self.parse_function_params();
        // fn (x , y) { x+y; }
        //          ^

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement()?;
        Some(Box::new(FunctionLiteral {
            token,
            params,
            body,
        }))
    }

    // ブーリアン
    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let value = match self.cur_token.literal.as_str() {
            "true" => true,
            "false" => false,
            _ => {
                let msg = format!("could not parse {} boolean", token);
                self.errors.push(msg);
                return None;
            }
        };
        Some(Box::new(Boolean { token, value }))
    }

    // 識別子 変数
    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    // 仮引数
    fn parse_function_params(&mut self) -> Vec<Box<Identifier>> {
        let mut params: Vec<Box<Identifier>> = vec![];
        while !self.cur_token_is(TokenType::RPAREN) {
            params.push(Box::new(Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            }));
            if self.peek_token_is(TokenType::COMMA) {
                self.next_token();
            }
            self.next_token();
        }
        params
    }

    // 整数リテラル
    fn parse_integerliteral(&mut self) -> Option<Box<dyn Expression>> {
        match self.cur_token.literal.parse::<i64>() {
            Ok(n) => Some(Box::new(IntegerLiteral {
                token: self.cur_token.clone(),
                value: n,
            })),
            Err(_) => {
                let msg = format!("could not parse {} integer", self.cur_token.literal);
                self.errors.push(msg);
                None
            }
        }
    }
    // 前置演算子 ! or -
    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.to_string();
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX)?;
        Some(Box::new(PrefixExpression {
            token,
            operator,
            right,
        }))
    }
    // 中間演算子式
    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        // exp op exp;
        //     ^
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.to_string();
        // operatorの優先度を取る
        let precedence = self.cur_precedence();

        self.next_token();
        // exp op exp;
        //        ^
        let right = self.parse_expression(precedence)?;
        Some(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        }))
    }

    // 関数呼び出し式
    fn parse_call_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        // left はもうparse済みで呼ばれる
        // (identifier or func literal) (arg , arg);
        //                              ^
        let token = self.cur_token.clone();
        let func = left;
        let mut args = vec![];
        self.next_token();
        while !self.cur_token_is(TokenType::RPAREN) {
            let arg = self.parse_expression(Precedence::LOWEST)?;
            args.push(arg);
            // (identifier or func literal) (arg , arg);
            // (identifier or func literal) (arg);
            //                                 ^
            if self.peek_token_is(TokenType::COMMA) {
                self.next_token();
                // (identifier or func literal) (arg , arg);
                //                                   ^
            }
            self.next_token();
            // (identifier or func literal) (arg , arg);
            // (identifier or func literal) (arg   );
            //                                     ^
        }
        Some(Box::new(CallExpression { token, func, args }))
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.t_type == t
    }
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.t_type == t
    }
    // 期待値だった場合, トークンを進める.
    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }
    fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "expected next token to be {}, got {} insted",
            t, self.peek_token.t_type
        );
        self.errors.push(msg);
    }
    // 次の演算子の優先度
    fn peek_precedence(&mut self) -> Precedence {
        match self.precedences.get(&self.peek_token.t_type) {
            Some(p) => *p,
            None => Precedence::LOWEST,
        }
    }
    // 直下の演算子の優先度
    fn cur_precedence(&mut self) -> Precedence {
        match self.precedences.get(&self.cur_token.t_type) {
            Some(p) => *p,
            None => Precedence::LOWEST,
        }
    }
    fn register_prefix(&mut self) {
        self.prefix_parsers
            .insert(TokenType::IDENT, Parser::parse_identifier);
        self.prefix_parsers
            .insert(TokenType::INT, Parser::parse_integerliteral);
        self.prefix_parsers
            .insert(TokenType::BANG, Parser::parse_prefix_expression);
        self.prefix_parsers
            .insert(TokenType::MINUS, Parser::parse_prefix_expression);
        self.prefix_parsers
            .insert(TokenType::TRUE, Parser::parse_boolean);
        self.prefix_parsers
            .insert(TokenType::FALSE, Parser::parse_boolean);
        self.prefix_parsers
            .insert(TokenType::LPAREN, Parser::parse_grouped_expression);
        self.prefix_parsers
            .insert(TokenType::IF, Parser::parse_if_expression);
        self.prefix_parsers
            .insert(TokenType::FUNCTION, Parser::parse_funtionliteral_expression);
    }
    fn get_prefix_parser(&mut self) -> Option<PrefixParser<'a>> {
        match self.prefix_parsers.get(&self.cur_token.t_type) {
            Some(p) => Some(*p),
            None => {
                let msg = format!(
                    "no prefix parse function for {} found",
                    self.cur_token.t_type
                );
                self.errors.push(msg);
                None
            }
        }
    }
    fn register_infix(&mut self) {
        self.infix_parsers
            .insert(TokenType::PLUS, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::MINUS, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::ASTERISK, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::SLASH, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::GT, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::LT, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::EQ, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::NOTEQ, Parser::parse_infix_expression);
        self.infix_parsers
            .insert(TokenType::LPAREN, Parser::parse_call_expression);
    }
    fn get_infix_parser(&mut self) -> Option<InfixParser<'a>> {
        match self.infix_parsers.get(&self.cur_token.t_type) {
            Some(p) => Some(*p),
            None => {
                let msg = format!(
                    "no infix parse function for {} found",
                    self.cur_token.t_type
                );
                self.errors.push(msg);
                None
            }
        }
    }
    fn register_precedence(&mut self) {
        self.precedences.insert(TokenType::EQ, Precedence::EQUALS);
        self.precedences
            .insert(TokenType::NOTEQ, Precedence::EQUALS);
        self.precedences
            .insert(TokenType::LT, Precedence::LESSGREATER);
        self.precedences
            .insert(TokenType::GT, Precedence::LESSGREATER);
        self.precedences.insert(TokenType::PLUS, Precedence::SUM);
        self.precedences.insert(TokenType::MINUS, Precedence::SUM);
        self.precedences
            .insert(TokenType::ASTERISK, Precedence::PRODUCT);
        self.precedences
            .insert(TokenType::SLASH, Precedence::PRODUCT);
        self.precedences.insert(TokenType::LPAREN, Precedence::CALL);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_call_expression() {
        let input = String::from(
            r#"
             add(x + y, 2)
             print()
             show(abc)
             fn (x, y) { x + y; }(x + y, 2)
             "#,
        );
        let expects = vec![
            "add((x+y),2)",
            "print()",
            "show(abc)",
            "fn(x,y){(x+y)}((x+y),2)",
        ];
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(4, program.statements.len());

        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(expects[i], statement.to_string());
        }
    }
    #[test]
    fn test_funcliteral_expression() {
        let input = String::from(
            r#"
             fn(x, y) { x + y; }
             fn() { x + y; }
             "#,
        );
        let expects = vec!["fn(x,y){(x+y)}", "fn(){(x+y)}"];
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(expects[i], statement.to_string());
        }
    }
    #[test]
    fn test_if_expression() {
        let input = String::from(
            r#"
             if (x <y) { x }
             if ( x < y ) { x }else {y}
             "#,
        );
        let expects = vec!["if(x<y){x}", "if(x<y){x}else{y}"];
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(expects[i], statement.to_string());
        }
    }
    #[test]
    fn test_grouped_expression() {
        let input = String::from(
            r#"
             (1 + (2 + 3) + 4);
             ( 5 +5) * 2;
            2 / (5 + 5);
            -(5+  5);
            !(true ==  true);
             "#,
        );
        let expects = vec![
            "((1+(2+3))+4)",
            "((5+5)*2)",
            "(2/(5+5))",
            "(-(5+5))",
            "(!(true==true))",
        ];
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(5, program.statements.len());

        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(expects[i], statement.to_string());
        }
    }
    #[test]
    fn test_boolean_expression() {
        let input = String::from(
            r#"
             true;
             false;
             "#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        let tests = vec!["true", "false"];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(tests[i], statement.to_string());
        }
    }
    #[test]
    fn test_infix_expression() {
        let input = String::from(
            r#"
             5 + 1 ;
             5-5 ;
             5*5;
             5 /5;
             5> 5;
             5 < 5;
             5 == 5;
             5 != 5;
             "#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(8, program.statements.len());

        let tests = vec![
            "(5+1)", "(5-5)", "(5*5)", "(5/5)", "(5>5)", "(5<5)", "(5==5)", "(5!=5)",
        ];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(tests[i], statement.to_string());
        }
    }
    #[test]
    fn test_prefix_expression() {
        let input = String::from(
            r#"
             - 1 ;
             !foobar;"#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        let tests = vec!["(-1)", "(!foobar)"];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(tests[i], statement.to_string());
        }
    }
    #[test]
    fn test_identifier_expression() {
        let input = String::from(
            r#"
             foobar ;
             piyopiyo;"#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        let tests = vec!["foobar", "piyopiyo"];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(tests[i], statement.to_string());
        }
    }
    #[test]
    fn test_integerliteral_expression() {
        let input = String::from(
            r#"
             1 ;
             4192;
             "#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        let tests = vec!["1", "4192"];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(tests[i], statement.to_string());
        }
    }
    #[test]
    fn test_letstatements() {
        let input = String::from(
            r#"
             let x = 5;
             let y = 15;
             let foobar = 838383;
                         "#,
            // peek_errorのテスト
            //             r#"
            //  let x 5;
            //  let = 15;
            //  let 838383;
            //              "#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(3, program.statements.len());

        let tests = vec!["let x = 5;", "let y = 15;", "let foobar = 838383;"];
        for (i, statement) in program.statements.iter().enumerate() {
            test_letstatements_(statement, tests[i]);
        }
    }
    #[test]
    fn test_returnstatements() {
        let input = String::from(
            r#"
             return 5;
             return  15;
                         "#,
        );
        let mut l = Lexer::new(&input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(2, program.statements.len());

        let tests = vec!["return 5;", "return 15;"];
        for (i, statement) in program.statements.iter().enumerate() {
            test_letstatements_(statement, tests[i]);
        }
    }
    //--------------------------------------------------------------------------------
    fn test_letstatements_(stmt: &Box<dyn Statement>, string: &str) {
        assert_eq!(string, stmt.to_string());
    }

    fn check_parser_errors(p: &Parser) {
        if p.errors().len() == 0 {
            return;
        }
        eprintln!("parser has {} errors", p.errors().len());
        for msg in p.errors() {
            eprintln!("parser error: {}", msg);
        }
    }
}

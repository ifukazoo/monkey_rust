use super::token::*;
use std::fmt;

//----------------------------------------
pub trait Node {
    fn token_literal(&self) -> &str;
    fn to_string(&self) -> String;
}
//----------------------------------------
pub trait Statement: Node {
    fn statement_node(&self);
}
//----------------------------------------
pub trait Expression: Node {
    fn expression_node(&self);
}
impl fmt::Display for dyn Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

//----------------------------------------
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
    fn to_string(&self) -> String {
        let mut sb = vec![];
        for s in &self.statements {
            sb.push(s.to_string());
        }
        sb.join("")
    }
}

//----------------------------------------
pub struct LetStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
    pub name: Identifier,
}
impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("{} {} = {};", self.token_literal(), self.name, self.value,)
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}
//----------------------------------------
pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("{} {};", self.token_literal(), self.value)
    }
}
impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

//----------------------------------------
pub struct ExpressionStatement {
    pub token: Token,
    pub exp: Box<dyn Expression>,
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        self.exp.to_string()
    }
}
impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}
//----------------------------------------
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

//----------------------------------------
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}
impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}
//----------------------------------------
// - と !
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}
impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right)
    }
}
impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

//----------------------------------------
// 中間演算式 右辺と左辺を持つ式
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}
impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("({}{}{})", self.left, self.operator, self.right)
    }
}
impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

//----------------------------------------
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}
impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}
impl Expression for Boolean {
    fn expression_node(&self) {}
}

//----------------------------------------
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternarive: Option<Box<BlockStatement>>,
}
impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        let alt = match &self.alternarive {
            None => "".to_string(),
            Some(a) => format!("else{{{}}}", a.to_string()),
        };
        format!(
            "if{}{{{}}}{}",
            self.condition.to_string(),
            self.consequence.to_string(),
            alt
        )
    }
}
impl Expression for IfExpression {
    fn expression_node(&self) {}
}

//----------------------------------------
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}
impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        let mut sb = vec![];
        for s in &self.statements {
            sb.push(s.to_string());
        }
        sb.join("")
    }
}
impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

//----------------------------------------
pub struct FunctionLiteral {
    pub token: Token,
    pub params: Vec<Box<Identifier>>,
    pub body: Box<BlockStatement>,
}
impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        let mut params = vec![];
        for p in &self.params {
            params.push(p.to_string());
        }
        format!("fn({}){{{}}}", params.join(","), self.body.to_string())
    }
}
impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

// 関数呼び出し式
//----------------------------------------
pub struct CallExpression {
    pub token: Token,
    pub func: Box<dyn Expression>, // 関数リテラルか, Identifier
    pub args: Vec<Box<dyn Expression>>,
}
impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
    fn to_string(&self) -> String {
        let mut args = vec![];
        for p in &self.args {
            args.push(p.to_string());
        }
        format!("{}({})", self.func.to_string(), args.join(","))
    }
}
impl Expression for CallExpression {
    fn expression_node(&self) {}
}

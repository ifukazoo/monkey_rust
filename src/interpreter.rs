use super::error::*;
use super::object::Object;
use super::*;
use std::io::BufReader;
use std::io::Read;

pub fn run<R: Read>(mut reader: BufReader<R>) -> Result<Object, MonkeyError> {
    let mut buf = String::new();
    reader.read_to_string(&mut buf)?;
    let token = lexer::lex(&buf);
    let ast = parser::parse_program(token)?;
    let obj = eval::eval_program(ast)?;
    Ok(obj)
}

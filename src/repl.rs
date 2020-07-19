use super::ast::*;
use super::lexer::*;
use super::parser::*;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Result;
use std::io::Write;

pub fn start<R: Read, W: Write>(mut reader: BufReader<R>, mut writer: BufWriter<W>) -> Result<()> {
    let prompt = ">>";
    loop {
        writer.write(prompt.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        let size = reader.read_line(&mut line)?;
        if size == 0 {
            // EOF
            break;
        }
        let mut l = Lexer::new(&line);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        if p.errors().len() != 0 {
            eprintln!("parser has {} errors", p.errors().len());
            for msg in p.errors() {
                eprintln!("parser error: {}", msg);
            }
            continue;
        }
        writer.write(program.to_string().as_bytes())?;
    }
    Ok(())
}

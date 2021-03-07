use crate::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;

pub fn start<R: Read, W: Write>(
    mut reader: BufReader<R>,
    mut writer: BufWriter<W>,
) -> std::io::Result<()> {
    let prompt = ">>";
    let env = env::new_env(HashMap::new());
    loop {
        writer.write_all(prompt.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        let size = reader.read_line(&mut line)?;
        if size == 0 {
            // EOF
            break;
        }
        let lex_result = lexer::lex(&line);
        writer.write_all(b"success lex\n")?;
        for token in &lex_result {
            writer.write_all(format!("{}\n", token).as_bytes())?;
        }
        match parser::parse_program(lex_result) {
            Ok(ast) => {
                writer.write_all(b"success parse\n")?;
                writer.write_all(format!("{:?}\n", ast).as_bytes())?;
                match eval::eval_statements(ast, &env) {
                    Ok(obj) => {
                        writer.write_all(b"success eval\n")?;
                        writer.write_all(format!("{}\n", obj).as_bytes())?;
                    }
                    Err(e) => {
                        writer.write_all(format!("{:?}\n", e).as_bytes())?;
                    }
                }
            }
            Err(e) => {
                writer.write_all(format!("{:?}\n", e).as_bytes())?;
            }
        }
    }
    Ok(())
}

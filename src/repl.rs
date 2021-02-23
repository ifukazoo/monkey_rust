use super::*;
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
        writer.write(prompt.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        let size = reader.read_line(&mut line)?;
        if size == 0 {
            // EOF
            break;
        }
        let lex_result = lexer::lex(&line);
        writer.write("success lex\n".as_bytes())?;
        for token in &lex_result {
            writer.write(format!("{}\n", token).as_bytes())?;
        }
        match parser::parse_program(lex_result) {
            Ok(ast) => {
                writer.write("success parse\n".as_bytes())?;
                writer.write(format!("{:?}\n", ast).as_bytes())?;
                match eval::eval_statements(ast, &env) {
                    Ok(obj) => {
                        writer.write("success eval\n".as_bytes())?;
                        writer.write(format!("{}\n", obj).as_bytes())?;
                    }
                    Err(e) => {
                        writer.write(format!("{:?}\n", e).as_bytes())?;
                    }
                }
            }
            Err(e) => {
                writer.write(format!("{:?}\n", e).as_bytes())?;
            }
        }
    }
    Ok(())
}

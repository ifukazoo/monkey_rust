use super::lexer::*;
use super::token::*;
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
        loop {
            let t = l.next_token();
            if t.t_type == TokenType::EOF {
                break;
            }
            writer.write_fmt(format_args!("{:?}\n", t))?;
        }
    }
    Ok(())
}

use super::token::*;
use std::fmt;
use std::iter::FromIterator;
use std::iter::Iterator;

pub struct Lexer<'a> {
    // 入力文
    input: &'a str,
    // 現在のpos
    position: usize,
    // 次のPos
    read_position: usize,
    // 現在のch
    ch: char,

    chs: Vec<char>,
}

impl<'a> fmt::Display for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let remain = &self.input[self.position..];
        // let mut sl = sl.iter().skip(self.position - 1);
        write!(
            f,
            "{{\ninput:\"{}\", \nremain:\"{}\", position:{}, readPosition:{}, ch:\"{}\"}}",
            self.input, remain, self.position, self.read_position, self.ch
        )
    }
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input: s,
            position: 0,
            read_position: 0,
            ch: 0 as char,
            chs: s.chars().collect(),
        };
        lexer.read_char();
        lexer
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char;
        } else {
            self.ch = self.chs[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            self.chs[self.read_position]
        }
    }
    fn back_char(&mut self) {
        self.read_position = self.position;
        if self.position == 0 {
            self.ch = 0 as char;
        } else {
            self.position -= 1;
            self.ch = self.chs[self.position];
        }
    }
    fn read_number(&mut self) -> String {
        let mut buf: Vec<char> = Vec::new();
        while is_digit(self.ch) {
            buf.push(self.ch);
            self.read_char();
        }
        self.back_char();
        String::from_iter(buf)
    }
    fn read_identifier(&mut self) -> String {
        let mut buf: Vec<char> = Vec::new();
        while is_letter(self.ch) {
            buf.push(self.ch);
            self.read_char();
        }
        self.back_char();
        String::from_iter(buf)
    }
    fn skip_white_space(&mut self) {
        while self.ch == ' ' || self.ch == '\n' || self.ch == '\r' || self.ch == '\t' {
            self.read_char();
        }
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_white_space();

        let token = match self.ch {
            '=' => {
                // 先読み
                match self.peek_char() {
                    '=' => {
                        // 読み進める
                        self.read_char();
                        new_token(TokenType::EQ, "==")
                    }
                    _ => new_token(TokenType::ASSIGN, &self.ch.to_string()),
                }
            }
            '+' => new_token(TokenType::PLUS, &self.ch.to_string()),
            '-' => new_token(TokenType::MINUS, &self.ch.to_string()),
            '*' => new_token(TokenType::ASTERISK, &self.ch.to_string()),
            '/' => new_token(TokenType::SLASH, &self.ch.to_string()),
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    new_token(TokenType::NOTEQ, "!=")
                }
                _ => new_token(TokenType::BANG, &self.ch.to_string()),
            },
            '<' => new_token(TokenType::LT, &self.ch.to_string()),
            '>' => new_token(TokenType::GT, &self.ch.to_string()),
            ';' => new_token(TokenType::SEMICOLON, &self.ch.to_string()),
            ',' => new_token(TokenType::COMMA, &self.ch.to_string()),
            '{' => new_token(TokenType::LBRACE, &self.ch.to_string()),
            '}' => new_token(TokenType::RBRACE, &self.ch.to_string()),
            '(' => new_token(TokenType::LPAREN, &self.ch.to_string()),
            ')' => new_token(TokenType::RPAREN, &self.ch.to_string()),
            '\0' => Token {
                t_type: TokenType::EOF,
                literal: "".to_string(),
            },
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    let t_type = lookup_ident(&ident);
                    new_token(t_type, &ident)
                } else if is_digit(self.ch) {
                    new_token(TokenType::INT, &self.read_number())
                } else {
                    new_token(TokenType::ILLEGAL, &self.ch.to_string())
                }
            }
        };
        self.read_char();
        token
    }
}
fn new_token(t: TokenType, l: &str) -> Token {
    Token {
        t_type: t,
        literal: String::from(l),
    }
}
fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}
fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        // let input = String::from(r#"let five = 5;"#);
        let input = String::from(
            r#"
 let five = 5;
                          let ten = 10;

             let add = fn(x , y) {
                 x + y;
             };
             let result = add(five, ten);
             !-/*5;
             5 < 10 > 5;

             if (5 < 10) {
                 return true;
             } else {
                 return false;
             }
             10 == 10;
             10 != 9;

             "#,
        );
        let expects = vec![
            Token {
                t_type: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "five".to_string(),
            },
            Token {
                t_type: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "ten".to_string(),
            },
            Token {
                t_type: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "add".to_string(),
            },
            Token {
                t_type: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                t_type: TokenType::FUNCTION,
                literal: "fn".to_string(),
            },
            Token {
                t_type: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "x".to_string(),
            },
            Token {
                t_type: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "y".to_string(),
            },
            Token {
                t_type: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                t_type: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "x".to_string(),
            },
            Token {
                t_type: TokenType::PLUS,
                literal: "+".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "y".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::LET,
                literal: "let".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "result".to_string(),
            },
            Token {
                t_type: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "add".to_string(),
            },
            Token {
                t_type: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "five".to_string(),
            },
            Token {
                t_type: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                t_type: TokenType::IDENT,
                literal: "ten".to_string(),
            },
            Token {
                t_type: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::BANG,
                literal: "!".to_string(),
            },
            Token {
                t_type: TokenType::MINUS,
                literal: "-".to_string(),
            },
            Token {
                t_type: TokenType::SLASH,
                literal: "/".to_string(),
            },
            Token {
                t_type: TokenType::ASTERISK,
                literal: "*".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                t_type: TokenType::LT,
                literal: "<".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::GT,
                literal: ">".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::IF,
                literal: "if".to_string(),
            },
            Token {
                t_type: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "5".to_string(),
            },
            Token {
                t_type: TokenType::LT,
                literal: "<".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                t_type: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                t_type: TokenType::RETURN,
                literal: "return".to_string(),
            },
            Token {
                t_type: TokenType::TRUE,
                literal: "true".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                t_type: TokenType::ELSE,
                literal: "else".to_string(),
            },
            Token {
                t_type: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                t_type: TokenType::RETURN,
                literal: "return".to_string(),
            },
            Token {
                t_type: TokenType::FALSE,
                literal: "false".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::EQ,
                literal: "==".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "10".to_string(),
            },
            Token {
                t_type: TokenType::NOTEQ,
                literal: "!=".to_string(),
            },
            Token {
                t_type: TokenType::INT,
                literal: "9".to_string(),
            },
            Token {
                t_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
        ];

        let mut l = Lexer::new(&input);
        for expect in expects.iter() {
            let t = l.next_token();
            assert!(expect.t_type == t.t_type);
            assert!(expect.literal == t.literal);
        }
    }
}

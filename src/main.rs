extern crate monkey_rust;
extern crate whoami;

use monkey_rust::repl;
use std::io;
use std::io::{BufReader, BufWriter};

fn main() {
    let user_name = whoami::username();
    println!(
        "Hello {}! This is the Monkey programming language!",
        user_name
    );
    println!("Feel free to type in commands");
    let reader = BufReader::new(io::stdin());
    let writer = BufWriter::new(io::stdout());
    repl::start(reader, writer).unwrap();
}

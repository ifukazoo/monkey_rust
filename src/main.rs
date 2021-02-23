use monkey_rust::interpreter;
use std::io;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let reader = io::BufReader::new(stdin);
    match interpreter::run(reader) {
        Ok(obj) => println!("{}", obj),
        Err(e) => eprintln!("{}", e),
    }
}

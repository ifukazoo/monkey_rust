use monkey_rust::interpreter;
use std::io;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let reader = io::BufReader::new(stdin);
    if let Err(e) = interpreter::run(reader) {
        eprintln!("Error:{}", e);
    }
}

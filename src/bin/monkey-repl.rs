use monkey_rust::repl;
use std::io;
use std::io::BufWriter;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let reader = io::BufReader::new(stdin);
    let writer = BufWriter::new(io::stdout());
    repl::start(reader, writer).unwrap();
}

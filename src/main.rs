mod lexer;
mod ast;
mod parser;

use std::fs;

fn main() {

    let lex: lexer::Lexer = lexer::Lexer::new(fs::read_to_string("main.rstini").expect("Cannot open main.rstini"));

    let mut par: parser::Parser = parser::Parser::new(lex);

    par.start();
}

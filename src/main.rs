#[macro_use]
#[cfg(test)]
extern crate itertools;

extern crate term;

mod tokenizer;
mod parser;
mod interpreter;
mod output;

use tokenizer::Tokenizer;
use parser::Parser;
use interpreter::evaluate;

fn main() {

    // Set up inputs
    let mut tok = Tokenizer::new("fg: blue bg: green { \"bluegreen \" style: underline \"bgu \" fg: yellow (yellow fg) bg: red [red bg]} \" neutral\"");
    let par = Parser::new(&mut tok);

    // Set up terminal outputs
    let mut t = term::stdout().unwrap();

    // Loop through units, evaluate them, and display them
    for unit_result in par {
        match unit_result {
            Ok(unit) => {
                let content = evaluate(unit.content).unwrap();
                output::write_format(&mut t, &unit.format, &content);
            },
            Err(_) => println!("Error"),
        }
    }
}

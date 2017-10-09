#[macro_use]
#[cfg(test)]
extern crate itertools;

mod tokenizer;
mod parser;

fn main() {
    let mut tok = tokenizer::Tokenizer::new("fg: blue ? bg: green { \"bluegreen\" style: underline \"bgu\" fg: yellow (yellow_fg) bg: red [red_bg]} \"neutral\"");
    let par = parser::Parser::new(&mut tok);

    for unit in par {
        println!("{:?}", unit);
    }
}

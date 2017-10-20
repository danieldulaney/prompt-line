extern crate term;

use std::marker::Sized;
use std::boxed::Box;

use term::Attr::Underline;
use term::Attr::Italic;

use parser;

pub fn write_format<T: term::Terminal + ?Sized>(t: &mut Box<T>, format: &parser::Format, content: &str) {

    t.reset().unwrap();

    match format.foreground {
        Some(color) => or_log(t.fg(color), "setting foreground"),
        None => {},
    };

    match format.background {
        Some(color) => or_log(t.bg(color), "setting background"),
        None => {},
    };

    match format.italic {
        Some(i) => or_log(t.attr(Italic(i)), "setting italic"),
        None => {},
    };

    match format.underline {
        Some(i) => or_log(t.attr(Underline(i)), "setting underline"),
        None => {},
    };

    write!(t, "{}", content).unwrap();

}

pub fn or_log(r: term::Result<()>, action: &str) {
    match r {
        Ok(_) => {},
        Err(e) => eprintln!("Error {}: {:?}", action, e),
    }
}


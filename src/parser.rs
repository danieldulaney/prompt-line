extern crate term;

use self::term::color;
use self::term::color::Color;

use std;
use std::vec::Vec;
use std::result::Result;
use std::error::Error;
use std::fmt::Display;

use tokenizer;
use tokenizer::Token;
use tokenizer::TokenizerResult;
use tokenizer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct ParserError {
    location: usize,
    description: String,
    cause: Option<tokenizer::TokenizerError>,
}

impl ParserError {

    fn from_tokenizer_error(err: tokenizer::TokenizerError) -> ParserError {
        ParserError {
            location: err.location,
            description: String::from(err.description()),
            cause: Some(err),
        }
    }

    fn at_token(token: &Token, description: String) -> ParserError {
        ParserError {
            location: token.location,
            description,
            cause: None
        }
    }

}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.cause {
            None => write!(f, "Parse error at byte {}: {}", self.location, self.description),
            Some(ref cause) => cause.fmt(f),
        }
    }
}

impl Error for ParserError {
    fn description(&self) -> &str {
        match self.cause {
            None => &self.description,
            Some(ref cause) => cause.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        panic!("Not yet implemented");
    }
}

pub type ParserResult<'s> = Result<Unit<'s>, ParserError>;

#[derive(Debug, Clone, Copy)]
pub struct Format {
    foreground: Option<Color>,
    background: Option<Color>,
    italic: Option<bool>,
    underline: Option<bool>,
}

impl Format {

    fn empty() -> Format {
        return Format {
            foreground: None,
            background: None,
            italic: None,
            underline: None,
        }
    }

    fn rollup(list: &[Format]) -> Format {
        let result = list.iter().rev().fold(Format::empty(), Format::fill);
        return result;
    }

    fn fill(self, fill: &Format) -> Format {
        Format {
            foreground: self.foreground.or(fill.foreground),
            background: self.background.or(fill.background),
            italic: self.italic.or(fill.italic),
            underline: self.underline.or(fill.underline),
        }
    }
}

#[derive(Debug)]
pub enum Content<'s> {
    Literal(&'s str),
    Command(&'s str),
    Builtin(&'s str),
}

#[derive(Debug)]
pub struct Unit<'s> {
    format: Format,
    content: Content<'s>,
}

trait OptionTokenizerResultExt<'s> {
    fn is_kind(&self, kind: TokenKind) -> bool;
    fn token(&self) -> Option<&Token>;
    fn lexeme(&self) -> Option<&'s str>;
    fn error(&self, description: String) -> ParserError;
}

impl<'s> OptionTokenizerResultExt<'s> for Option<TokenizerResult<'s>> {
    fn is_kind(&self, kind: TokenKind) -> bool {
        match self {
            &Some(Ok(ref token)) => token.kind == kind,
            _ => false,
        }
    }

    fn token(&self) -> Option<&Token> {
        match self {
            &Some(Ok(ref token)) => Some(token),
            _ => None,
        }
    }

    fn lexeme(&self) -> Option<&'s str> {
        match self {
            &Some(Ok(ref token)) => Some(token.lexeme),
            _ => None,
        }
    }

    fn error(&self, description: String) -> ParserError {
        match self {
            &Some(Ok(ref token)) => ParserError::at_token(token, description),
            &Some(Err(ref err)) => ParserError::from_tokenizer_error(err.clone()),
            // TODO Investigate what situations could cause this scenario.
            &None => panic!("OptionTokenizerResultExt::error not implemented when self is None"),
        }
    }
}

pub struct Parser<'s> {

    format_stack: Vec<Format>,
    source: &'s mut Iterator<Item=TokenizerResult<'s>>,
    current: Option<TokenizerResult<'s>>,
    next: Option<TokenizerResult<'s>>,

}


impl<'s> Parser<'s> {

    pub fn new(source: &'s mut Iterator<Item=TokenizerResult<'s>>) -> Parser<'s> {

        let next = source.next();

        Parser {
            format_stack: vec![Format::empty()],
            source,
            current: None,
            next,
        }

    }

    fn advance(&mut self) {
        // Move next value into current value
        // Also moves current value into next value, but that gets clobbered on the next line
        std::mem::swap(&mut self.current, &mut self.next);
        self.next = self.source.next();
    }

    fn advance_onto(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if self.next.is_kind(kind) {
            self.advance();
            Ok(())
        } else {
            Err(self.next.error(format!("Expected {:?} but got {:?}", kind, self.next)))
        }
    }

    fn clear_current_format(&mut self) -> Result<(), ParserError> {
        self.format_stack.pop();
        self.format_stack.push(Format::empty());

        Ok(())
    }

    fn finalize_unit(&mut self, content: Content<'s>) -> Result<Option<Unit<'s>>, ParserError> {
        let result = Ok(Some(Unit {
            format: Format::rollup(&self.format_stack),
            content,
        }));

        self.clear_current_format()?;

        result
    }

    fn finish_color(&mut self) -> Result<Color, ParserError> {
        self.advance_onto(TokenKind::Colon)?;
        self.advance_onto(TokenKind::Identifier)?;

        match self.current.lexeme().expect("Just used advance_onto(); current token is valid") {
            "black" => Ok(color::BLACK),
            "blue" => Ok(color::BLUE),
            "brightblack" => Ok(color::BRIGHT_BLACK),
            "brightblue" => Ok(color::BRIGHT_BLUE),
            "brightcyan" => Ok(color::BRIGHT_CYAN),
            "brightgreen" => Ok(color::BRIGHT_GREEN),
            "brightmagenta" => Ok(color::BRIGHT_MAGENTA),
            "brightred" => Ok(color::BRIGHT_RED),
            "brightwhite" => Ok(color::BRIGHT_WHITE),
            "brightyellow" => Ok(color::BRIGHT_YELLOW),
            "cyan" => Ok(color::CYAN),
            "green" => Ok(color::GREEN),
            "magenta" => Ok(color::MAGENTA),
            "red" => Ok(color::RED),
            "white" => Ok(color::WHITE),
            "yellow" => Ok(color::YELLOW),
            color_name => Err(self.current.error(format!("Unrecognized color identifier \"{}\"", color_name))),
        }
    }

    fn finish_style(&mut self) -> Result<(), ParserError> {
        self.advance_onto(TokenKind::Colon)?;
        self.advance_onto(TokenKind::Identifier)?;

        match self.current.lexeme().expect("Just used advance_onto(); current token is valid") {
            "italic" => {
                self.format_stack.last_mut().unwrap().italic = Some(true);
                Ok(())
            },
            "underline" => {
                self.format_stack.last_mut().unwrap().underline = Some(true);
                Ok(())
            },
            "noitalic" => {
                self.format_stack.last_mut().unwrap().italic = Some(false);
                Ok(())
            },
            "nounderline" => {
                self.format_stack.last_mut().unwrap().underline = Some(false);
                Ok(())
            }
            style_name => Err(self.current.error(format!("Unrecognized color identifier \"{}\"", style_name))),
        }
    }

    // Consumption functions return one of:
    // - Success, returning a unit
    // - Success, but no unit
    // - Failure
    //
    // Start consuming from the NEXT token
    // Leave the last consumed token as the CURRENT token
    // Guaranteed to consume at least 1 token

    fn consume(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        self.consume_literal()
    }

    fn consume_literal(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::String) {
            Ok(_) => {
                let lexeme = self.current.lexeme().expect("advance_onto worked");

                self.finalize_unit(Content::Literal(&lexeme[1..lexeme.len()-1]))
            },
            Err(_) => self.consume_builtin(),
        }
    }

    fn consume_builtin(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::LeftParen) {
            Ok(_) => {
                self.advance_onto(TokenKind::Identifier)?;
                let identifier = self.current.lexeme().expect("advance_onto worked");
                self.advance_onto(TokenKind::RightParen)?;

                self.finalize_unit(Content::Builtin(identifier))
            },
            Err(_) => self.consume_command(),
        }
    }

    fn consume_command(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::LeftSquare) {
            Ok(_) => {
                self.advance_onto(TokenKind::Identifier)?;
                let identifier = self.current.lexeme().expect("advance_onto worked");
                self.advance_onto(TokenKind::RightSquare)?;

                self.finalize_unit(Content::Command(identifier))
            },
            Err(_) => self.consume_foreground(),
        }
    }

    fn consume_foreground(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::Foreground) {
            Ok(_) => {
                self.format_stack.last_mut().unwrap().foreground = Some(self.finish_color()?);
                Ok(None)
            },
            Err(_) => self.consume_background(),
        }
    }

    fn consume_background(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::Background) {
            Ok(_) => {
                self.format_stack.last_mut().unwrap().background = Some(self.finish_color()?);
                Ok(None)
            },
            Err(_) => self.consume_style(),
        }
    }

    fn consume_style(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::Style) {
            Ok(_) => {
                self.finish_style()?;
                Ok(None)
            },
            Err(_) => self.consume_open_group(),
        }
    }

    fn consume_open_group(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::LeftCurly) {
            Ok(_) => {
                self.format_stack.push(Format::empty());
                Ok(None)
            },
            Err(_) => self.consume_close_group(),
        }
    }

    fn consume_close_group(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::RightCurly) {
            Ok(_) => {
                let result = match self.format_stack.pop() {
                    Some(_) => Ok(None),
                    None => Err(self.current.error(format!("Unexpected group closing"))),
                };

                self.clear_current_format()?;

                result
            },
            Err(_) => self.consume_unknown(),
        }
    }

    fn consume_unknown(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        self.advance();

        match &self.current {
            &Some(Ok(ref token)) => Err(ParserError::at_token(token, format!("Unexpected token: '{}'", token.lexeme))),
            &Some(Err(ref err)) => Err(ParserError::from_tokenizer_error(err.clone())),
            &None => Ok(None),
        }
    }


}

impl<'s> Iterator for Parser<'s> {
    type Item = ParserResult<'s>;

    fn next(&mut self) -> Option<ParserResult<'s>> {

        loop {
            // Check to see if it's done (next token is EOF or nonexistant)
            if self.next.is_kind(TokenKind::EOF) || self.next == None {
                break None;
            }

            // Consume the next chunk
            // If it's a unit or an error, return it
            // Otherwise, consume another chunk
            match self.consume() {
                Err(error) => break Some(Err(error)),
                Ok(Some(unit)) => break Some(Ok(unit)),
                Ok(None) => {},
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // Given a number, generates a unique format
    // There are currently:
    //    17 foreground
    //    17 background
    //     3 italic
    //     3 underline
    //     =
    //  2601 values
    const FORMAT_COUNT: u32 = 2601;
    fn format(index: u32) -> (Vec<Token<'static>>, Format) {
        let mut v = Vec::new();
        let mut format = Format::empty();

        let fg = color(index % 17);
        let bg = color((index / 17) % 17);
        let italic = (index / (17 * 17)) % 3;
        let underline = (index / (17 * 17 * 3)) % 3;

        // Foreground
        match fg {
            Some(color) => {
                v.push(Token {location: 0, lexeme: "fg", kind: TokenKind::Foreground});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: color.0, kind: TokenKind::Identifier});
                format.foreground = Some(color.1);
            },
            _ => {},
        }

        // Background
        match bg {
            Some(color) => {
                v.push(Token {location: 0, lexeme: "bg", kind: TokenKind::Background});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: color.0, kind: TokenKind::Identifier});
                format.background = Some(color.1);
            }
            None => {},
        }

        // Italic
        match italic {
            0 => {
                v.push(Token {location: 0, lexeme: "style", kind: TokenKind::Style});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: "italic", kind: TokenKind::Identifier});
                format.italic = Some(true);
            },
            1 => {
                v.push(Token {location: 0, lexeme: "style", kind: TokenKind::Style});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: "noitalic", kind: TokenKind::Identifier});
                format.italic = Some(false);
            },
            _ => {},
        }

        // Underline
        match underline {
            0 => {
                v.push(Token {location: 0, lexeme: "style", kind: TokenKind::Style});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: "underline", kind: TokenKind::Identifier});
                format.underline = Some(true);
            },
            1 => {
                v.push(Token {location: 0, lexeme: "style", kind: TokenKind::Style});
                v.push(Token {location: 0, lexeme: ":", kind: TokenKind::Colon});
                v.push(Token {location: 0, lexeme: "nounderline", kind: TokenKind::Identifier});
                format.underline = Some(false);
            },
            _ => {},
        }

        return (v, format);

    }

    fn color(index: u32) -> Option<(&'static str, Color)> {
        match index {
            0  => Some(("black", 0)),
            1  => Some(("red", 1)),
            2  => Some(("green", 2)),
            3  => Some(("yellow", 3)),
            4  => Some(("blue", 4)),
            5  => Some(("magenta", 5)),
            6  => Some(("cyan", 6)),
            7  => Some(("white", 7)),
            8  => Some(("brightblack", 8)),
            9  => Some(("brightred", 9)),
            10 => Some(("brightgreen", 10)),
            11 => Some(("brightyellow", 11)),
            12 => Some(("brightblue", 12)),
            13 => Some(("brightmagenta", 13)),
            14 => Some(("brightcyan", 14)),
            15 => Some(("brightwhite", 15)),
            _ => None,
        }
    }

    fn create_content() -> Vec<(Vec<Token<'static>>, Content<'static>)> {
        vec![
            (
                vec![Token {location: 0, lexeme: "\"test string\"", kind: TokenKind::String}],
                Content::Literal("\"test string\""),
            ),
            (
                vec![
                    Token {location: 0, lexeme: "(", kind: TokenKind::LeftParen},
                    Token {location: 0, lexeme: "test_builtin", kind: TokenKind::Identifier},
                    Token {location: 0, lexeme: ")", kind: TokenKind::RightParen},
                ],
                Content::Builtin("test_builtin"),
            ),
            (
                vec![
                    Token {location: 0, lexeme: "[", kind: TokenKind::LeftSquare},
                    Token {location: 0, lexeme: "test_command", kind: TokenKind::Identifier},
                    Token {location: 0, lexeme: "]", kind: TokenKind::RightSquare},
                ],
                Content::Command("test_command"),
            ),
        ]
    }

    #[test]
    fn test_parse_formatted_content() {
        for (format_tokens, correct_format) in (0..FORMAT_COUNT).map(format) {
            for &(ref content_tokens, ref correct_content) in create_content().into_iter() {
                let mut p = Parser::new(&mut
                    format_tokens.iter()
                        .chain(content_tokens.iter())
                        .map(Result::Ok));
            }
        }
    }
}

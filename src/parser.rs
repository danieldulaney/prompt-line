extern crate term;

use self::term::color;
use self::term::color::Color;

use std;
use std::vec::Vec;
use std::result::Result;
use std::error::Error;
use std::fmt::Display;

use tokenizer::Token;
use tokenizer::TokenizerResult;
use tokenizer::TokenKind;
use tokenizer::TokenizerError;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {

    // Caused by an underlying TokenizerError
    FromTokenizer(TokenizerError),

    // Wrong token kind
    // location, actual kind, expected kind
    BadToken(Option<(usize, TokenKind)>, Option<TokenKind>),

    // Extra closed group
    MismatchedGroup(Option<usize>),

    // Unrecognized color
    // location, color
    UnknownColor(usize, String),
    UnknownStyle(usize, String),

}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            ParserError::FromTokenizer(ref cause) =>
                write!(f, "{}", cause),
            ParserError::BadToken(Some((location, actual)), Some(expected)) =>
                write!(f, "expected {} but got {} at byte {}", expected, actual, location),
            ParserError::BadToken(Some((location, actual)), None) =>
                write!(f, "unexpected {} at byte {}", actual, location),
            ParserError::BadToken(None, Some(actual)) =>
                write!(f, "expected {} but could not find another token", actual),
            ParserError::BadToken(None, None) =>
                write!(f, "unexpected token found"),
            ParserError::UnknownColor(location, ref color) =>
                write!(f, "unrecognized color \"{}\" at byte {}", color, location),
            ParserError::UnknownStyle(location, ref style) =>
                write!(f, "unrecognized style \"{}\" at byte {}", style, location),
            ParserError::MismatchedGroup(Some(location)) =>
                write!(f, "unexpected group closing at byte {}", location),
            ParserError::MismatchedGroup(None) =>
                write!(f, "unexpected group closing"),
        }
    }
}

impl Error for ParserError {
    fn description(&self) -> &str {
        match *self {
            ParserError::FromTokenizer(ref cause) => cause.description(),
            ParserError::BadToken(_, _) => "unexpected token",
            ParserError::UnknownColor(_, _) => "unrecognized color",
            ParserError::UnknownStyle(_, _) => "unrecognized style",
            ParserError::MismatchedGroup(_) => "unexpected group closing"
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            ParserError::FromTokenizer(ref cause) => Some(cause),
            _ => None,
        }
    }
}

pub type ParserResult<'s> = Result<Unit<'s>, ParserError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Format {
    pub foreground: Option<Color>,
    pub background: Option<Color>,
    pub italic: Option<bool>,
    pub underline: Option<bool>,
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
        let mut current = Format::empty();

        // .rev() because the stack needs to get iterated from top down (last added to first).
        // Fill gives priority if a value is already set, so you want to start with the highest
        // priority, then go down the list.
        for f in list.iter().rev() {
            current = current.fill(f)
        }

        return current;
    }

    // Given two `Format`s, returns a new Format that combines them, giving priority to the first
    // format.
    fn fill(&self, fill: &Format) -> Format {
        Format {
            foreground: self.foreground.or(fill.foreground),
            background: self.background.or(fill.background),
            italic: self.italic.or(fill.italic),
            underline: self.underline.or(fill.underline),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Content<'s> {
    Literal(&'s str),
    Command(&'s str),
    Builtin(&'s str),
}

#[derive(Debug, PartialEq)]
pub struct Unit<'s> {
    pub format: Format,
    pub content: Content<'s>,
    pub location: usize,
}

trait OptionTokenizerResultExt<'s> {
    fn is_kind(&self, kind: TokenKind) -> bool;
    fn token(&self) -> Option<&'s Token>;
    fn lexeme(&self) -> Option<&'s str>;
    fn location(&self) -> Option<usize>;
    fn bad_token(&'s self, expected: Option<TokenKind>) -> ParserError;
}

impl<'s> OptionTokenizerResultExt<'s> for Option<TokenizerResult<'s>> {
    fn is_kind(&self, kind: TokenKind) -> bool {
        match self {
            &Some(Ok(ref token)) => token.kind == kind,
            _ => false,
        }
    }

    fn token(&self) -> Option<&'s Token> {
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

    fn location(&self) -> Option<usize> {
        match self {
            &Some(Ok(ref token)) => Some(token.location),
            _ => None,
        }
    }

    fn bad_token(&'s self, expected: Option<TokenKind>) -> ParserError {
        match *self {
            Some(Ok(ref token)) => ParserError::BadToken(Some((token.location, token.kind)), expected),
            Some(Err(ref err)) => ParserError::FromTokenizer(err.clone()),
            None => ParserError::BadToken(None, expected),
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
            Err(self.next.bad_token(Some(kind)))
        }
    }

    fn clear_current_format(&mut self) -> Result<(), ParserError> {
        match self.format_stack.pop() {
            Some(_) => {
                self.format_stack.push(Format::empty());
                Ok(())
            },
            None => {
                self.format_stack.push(Format::empty());
                Err(ParserError::MismatchedGroup(self.current.location()))
            }
        }

    }

    fn finalize_unit(&mut self, content: Content<'s>, location: usize) -> Result<Option<Unit<'s>>, ParserError> {
        let result = Ok(Some(Unit {
            format: Format::rollup(&self.format_stack),
            content,
            location,
        }));

        self.clear_current_format()?;

        result
    }

    fn finish_color(&mut self) -> Result<Color, ParserError> {
        self.advance_onto(TokenKind::Colon)?;
        self.advance_onto(TokenKind::Identifier)?;

        let token = self.current.token().expect("advance_onto worked");

        match token.lexeme {
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
            color => Err(ParserError::UnknownColor(token.location, String::from(color))),
        }
    }

    fn finish_style(&mut self) -> Result<(), ParserError> {
        self.advance_onto(TokenKind::Colon)?;
        self.advance_onto(TokenKind::Identifier)?;

        let token = self.current.token().expect("advance_onto worked");

        match token.lexeme {
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
            style => Err(ParserError::UnknownStyle(token.location, String::from(style))),
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
                let location = self.current.location().expect("advance_onto worked");

                self.finalize_unit(Content::Literal(&lexeme[1..lexeme.len()-1]), location)
            },
            Err(_) => self.consume_builtin(),
        }
    }

    fn consume_builtin(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::ParenSurround) {
            Ok(_) => {
                let lexeme = self.current.lexeme().expect("advance_onto worked");
                let location = self.current.location().expect("advance_onto worked");

                self.finalize_unit(Content::Builtin(&lexeme[1..lexeme.len()-1]), location)
            },
            Err(_) => self.consume_command(),
        }
    }

    fn consume_command(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        match self.advance_onto(TokenKind::SquareSurround) {
            Ok(_) => {
                let lexeme = self.current.lexeme().expect("advance_onto worked");
                let location = self.current.location().expect("advance_onto worked");

                self.finalize_unit(Content::Command(&lexeme[1..lexeme.len()-1]), location)
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
                    None => Err(ParserError::MismatchedGroup(self.current.location()))
                };

                self.clear_current_format()?;

                result
            },
            Err(_) => self.consume_unknown(),
        }
    }

    fn consume_unknown(&mut self) -> Result<Option<Unit<'s>>, ParserError> {
        self.advance();

        Err(self.current.bad_token(None))
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

    // Given an index, return a format and the tokens to produce it
    // Indices are taken mod FORMAT_COUNT
    const FORMAT_COUNT: i32 = 16 * 2 * 2;
    fn format(index: i32) -> (Vec<Token<'static>>, Format){
        let phase = index % 16;
        let style = index / 16 % 2;
        let color = color(index / 32 % 16);

        let mut tokens = Vec::with_capacity(12);

        let format = Format {
            background: match phase % 2 {
                0 => None,
                _ => {
                    tokens.push(Token { location: 0, kind: TokenKind::Background, lexeme: "bg" });
                    tokens.push(Token { location: 0, kind: TokenKind::Colon, lexeme: ":" });
                    tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: color.0 });
                    Some(color.1)
                }
            },
            foreground: match (phase / 2) % 2 {
                0 => None,
                _ => {
                    tokens.push(Token { location: 0, kind: TokenKind::Foreground, lexeme: "fg" });
                    tokens.push(Token { location: 0, kind: TokenKind::Colon, lexeme: ":" });
                    tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: color.0 });
                    Some(color.1)
                },
            },
            italic: match (phase / 4) % 2 {
                0 => None,
                _ => {
                    tokens.push(Token { location: 0, kind: TokenKind::Style, lexeme: "style" });
                    tokens.push(Token { location: 0, kind: TokenKind::Colon, lexeme: ":" });
                    if style != 0 {tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: "italic" });}
                        else {tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: "noitalic"});}
                    Some(style != 0)
                },
            },
            underline: match (phase / 8) % 2 {
                0 => None,
                _ => {
                    tokens.push(Token { location: 0, kind: TokenKind::Style, lexeme: "style" });
                    tokens.push(Token { location: 0, kind: TokenKind::Colon, lexeme: ":" });
                    if style != 0 {tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: "underline" });}
                        else {tokens.push(Token { location: 0, kind: TokenKind::Identifier, lexeme: "nounderline"});}
                    Some(style != 0)
                },
            },
        };

        return (tokens, format)
    }

    // Given an index, return a color name and value
    // Indices are taken mod 16
    fn color(index: i32) -> (&'static str, Color) {
        match index % 16 {
            0  => ("black", 0),
            1  => ("red", 1),
            2  => ("green", 2),
            3  => ("yellow", 3),
            4  => ("blue", 4),
            5  => ("magenta", 5),
            6  => ("cyan", 6),
            7  => ("white", 7),
            8  => ("brightblack", 8),
            9  => ("brightred", 9),
            10 => ("brightgreen", 10),
            11 => ("brightyellow", 11),
            12 => ("brightblue", 12),
            13 => ("brightmagenta", 13),
            14 => ("brightcyan", 14),
            15 => ("brightwhite", 15),
            _ => panic!("Somehow, index % 16 was outside the range [0, 15]"),
        }
    }

    // Given an index, return a chunk of content and the tokens to produce it
    // Indices are taken mod CONTENT_COUNT
    const CONTENT_COUNT: i32 = 3;
    fn content(index: i32) -> (Vec<Token<'static>>, Content<'static>) {
        match index % CONTENT_COUNT {
            0 => (
                vec![Token { location: 0, lexeme: "\"test string\"", kind: TokenKind::String }],
                Content::Literal("test string"),
            ),
            1 => (
                vec![
                    Token { location: 0, lexeme: "(test_builtin)", kind: TokenKind::ParenSurround},
                ],
                Content::Builtin("test_builtin"),
            ),
            2 => (
                vec![
                    Token { location: 0, lexeme: "[test_command]", kind: TokenKind::SquareSurround},
                ],
                Content::Command("test_command"),
            ),
            _ => panic!("Test programmer error: CONTENT_COUNT doesn't match content")
        }
    }

    fn one_token(kind: TokenKind, lexeme: &str) -> std::iter::Once<Token> {
        std::iter::once(Token {
            kind,
            lexeme,
            location: 0,
        })
    }

    fn some_tokens<'s>(kind: TokenKind, lexeme: &'s str, count: usize) -> std::iter::Take<std::iter::Repeat<Token<'s>>> {
        std::iter::repeat(Token {
            kind,
            lexeme,
            location: 0,
        }).take(count)
    }

    //fn color_errors()

    #[test]
    fn test_format_rollup() {
        for ((_, format1), (_, format2), (_, format3)) in iproduct!(0..FORMAT_COUNT, 0..FORMAT_COUNT, 0..FORMAT_COUNT).map(|(a,b,c)| (format(a), format(b), format(c))) {

            let correct_format = Format {
                foreground: format3.foreground.or(format2.foreground).or(format1.foreground),
                background: format3.background.or(format2.background).or(format1.background),
                italic: format3.italic.or(format2.italic).or(format1.italic),
                underline: format3.underline.or(format2.underline).or(format1.underline),
            };

            let stack = vec![format1, format2, format3];

            let actual_format = Format::rollup(&stack);

            assert_eq!(correct_format, actual_format, "Expected:\n{:?}\nbut got\n{:?}\nwhen rolling up stack\n{:?}", correct_format, actual_format, stack);

        }
    }

    #[test]
    fn test_parse_formatted_content() {
        for (f, c) in iproduct!(0..FORMAT_COUNT, 0..CONTENT_COUNT).map(|(a,b)| (format(a), content(b))) {

            let mut all_tokens = f.0.into_iter()
                .chain(c.0.into_iter())
                .map(Result::Ok);

            let mut parser = Parser::new(&mut all_tokens);

            let unit = parser.next().unwrap().unwrap();

            assert_eq!(unit.format, f.1);
            assert_eq!(unit.content, c.1);

            assert_eq!(parser.next(), None);
        }
    }

    // Tests token sequences of the form:
    // f_a { c_a } c_0
    // Confirms that:
    // Group formats are applied within the group
    // Group formats are not applied after the group
    #[test]
    fn test_parse_single_group() {
        for (f_a, c_a, c_0) in iproduct!(0..FORMAT_COUNT, 0..CONTENT_COUNT, 0..CONTENT_COUNT).map(|(a,b,c)| (format(a), content(b), content(c))) {

            let mut tokens = f_a.0.into_iter()
                .chain(one_token(TokenKind::LeftCurly, "{"))
                .chain(c_a.0.into_iter())
                .chain(one_token(TokenKind::RightCurly, "}"))
                .chain(c_0.0.into_iter())
                .map(|t| Ok::<_, TokenizerError>(t));

            let mut parser = Parser::new(&mut tokens);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, f_a.1);
            assert_eq!(unit.content, c_a.1);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, Format::empty());
            assert_eq!(unit.content, c_0.1);

            assert_eq!(parser.next(), None);
        }
    }

    // Tests token sequences of the form
    // f_a { f_b { c_ba } c_a } c_0
    // Confirms that:
    // Group formats roll up within nested groups
    // Group formats are cleared outside of nested group
    #[test]
    fn test_parse_nested_group() {
        for (f_a, f_b, c_ba, c_a, c_0) in
            iproduct!(0..FORMAT_COUNT, 0..FORMAT_COUNT, 0..CONTENT_COUNT, 0..CONTENT_COUNT, 0..CONTENT_COUNT)
                .map(|(a,b,c,d,e)| (format(a), format(b), content(c), content(d), content(e))) {

            let mut tokens = f_a.0.into_iter()
                .chain(one_token(TokenKind::LeftCurly, "{"))
                .chain(f_b.0.into_iter())
                .chain(one_token(TokenKind::LeftCurly, "{"))
                .chain(c_ba.0.into_iter())
                .chain(one_token(TokenKind::RightCurly, "}"))
                .chain(c_a.0.into_iter())
                .chain(one_token(TokenKind::RightCurly, "}"))
                .chain(c_0.0.into_iter())
                .map(|t| Ok(t));

            let mut parser = Parser::new(&mut tokens);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, f_b.1.fill(&f_a.1));
            assert_eq!(unit.content, c_ba.1);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, f_a.1);
            assert_eq!(unit.content, c_a.1);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, Format::empty());
            assert_eq!(unit.content, c_0.1);
        }
    }

    // Tests deep nesting
    // f_a { { { { { { f_b { { { c_ba } } } } } } } } } c_0
    #[test]
    fn test_deeply_nested_group() {
        for (f_a, f_b, c_ba, c_0) in
            iproduct!(0..FORMAT_COUNT, 0..FORMAT_COUNT, 0..CONTENT_COUNT, 0..CONTENT_COUNT)
                .map(|(a,b,c,d)| (format(a), format(b), content(c), content(d))) {

            let mut tokens = f_a.0.into_iter()
                .chain(some_tokens(TokenKind::LeftCurly, "{", 6))
                .chain(f_b.0.into_iter())
                .chain(some_tokens(TokenKind::LeftCurly, "{", 3))
                .chain(c_ba.0.into_iter())
                .chain(some_tokens(TokenKind::RightCurly, "}", 9))
                .chain(c_0.0.into_iter())
                .map(|t| Ok::<_, TokenizerError>(t));

            let mut parser = Parser::new(&mut tokens);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, f_b.1.fill(&f_a.1));
            assert_eq!(unit.content, c_ba.1);

            let unit = parser.next().unwrap().unwrap();
            assert_eq!(unit.format, Format::empty());
            assert_eq!(unit.content, c_0.1);

            assert_eq!(parser.next(), None);

        }
    }

    #[test]
    fn test_failures() {

        // Token sequence, expected error, successful units
        let errors: [(&[Token<'static>], ParserError, u32); 17] = [

            // Issues with foreground structure
            ( // Foreground colon is nothing
              &[Token {location: 0, kind: TokenKind::Foreground, lexeme: "fg"}],
              ParserError::BadToken(None, Some(TokenKind::Colon)),
              0,
            ),
            ( // Foreground colon is wrong
              &[
                  Token {location: 0, kind: TokenKind::Foreground, lexeme: "fg"},
                  Token {location: 10, kind: TokenKind::Identifier, lexeme: "foobar"}
              ],
              ParserError::BadToken(Some((10, TokenKind::Identifier)), Some(TokenKind::Colon)),
              0,
            ),
            ( // Foreground color is nothing
              &[
                  Token {location: 0, kind: TokenKind::Foreground, lexeme: "fg"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
              ],
              ParserError::BadToken(None, Some(TokenKind::Identifier)),
              0,
            ),
            ( // Foreground color is wrong
              &[
                  Token {location: 0, kind: TokenKind::Foreground, lexeme: "fg"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                  Token {location: 10, kind: TokenKind::LeftCurly, lexeme: "{"},
              ],
              ParserError::BadToken(Some((10, TokenKind::LeftCurly)), Some(TokenKind::Identifier)),
              0
            ),

            // Issues with background structure
            ( // Background colon is nothing
              &[Token {location: 0, kind: TokenKind::Background, lexeme: "bg"}],
              ParserError::BadToken(None, Some(TokenKind::Colon)),
              0,
            ),
            ( // Background colon is wrong
              &[
                  Token {location: 0, kind: TokenKind::Background, lexeme: "bg"},
                  Token {location: 10, kind: TokenKind::Identifier, lexeme: "foobar"}
              ],
              ParserError::BadToken(Some((10, TokenKind::Identifier)), Some(TokenKind::Colon)),
              0,
            ),
            ( // Background color is nothing
              &[
                  Token {location: 0, kind: TokenKind::Background, lexeme: "bg"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
              ],
              ParserError::BadToken(None, Some(TokenKind::Identifier)),
              0,
            ),
            ( // Background color is wrong
              &[
                  Token {location: 0, kind: TokenKind::Background, lexeme: "bg"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                  Token {location: 10, kind: TokenKind::LeftCurly, lexeme: "{"},
              ],
              ParserError::BadToken(Some((10, TokenKind::LeftCurly)), Some(TokenKind::Identifier)),
              0
            ),

            // Issues with style structure
            ( // Style colon is nothing
              &[Token {location: 0, kind: TokenKind::Style, lexeme: "style"}],
              ParserError::BadToken(None, Some(TokenKind::Colon)),
              0,
            ),
            ( // Style colon is wrong
              &[
                  Token {location: 0, kind: TokenKind::Style, lexeme: "style"},
                  Token {location: 10, kind: TokenKind::Identifier, lexeme: "foobar"}
              ],
              ParserError::BadToken(Some((10, TokenKind::Identifier)), Some(TokenKind::Colon)),
              0,
            ),
            ( // Style color is nothing
              &[
                  Token {location: 0, kind: TokenKind::Style, lexeme: "style"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
              ],
              ParserError::BadToken(None, Some(TokenKind::Identifier)),
              0,
            ),
            ( // Style color is wrong
              &[
                  Token {location: 0, kind: TokenKind::Style, lexeme: "style"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                  Token {location: 10, kind: TokenKind::LeftCurly, lexeme: "{"},
              ],
              ParserError::BadToken(Some((10, TokenKind::LeftCurly)), Some(TokenKind::Identifier)),
              0,
            ),

            // Mismatched groups
            ( // Simple case: Single close group
              &[
                  Token {location: 1, kind: TokenKind::RightCurly, lexeme: "}"},
              ],
              ParserError::MismatchedGroup(Some(1)),
              0,
            ),

            ( // Complex case: {{}{{}}}}{ or LLRLLRRRRL
              &[
                  Token {location: 1, kind: TokenKind::LeftCurly, lexeme: "{"},
                  Token {location: 2, kind: TokenKind::LeftCurly, lexeme: "{"},
                  Token {location: 3, kind: TokenKind::RightCurly, lexeme: "}"},
                  Token {location: 4, kind: TokenKind::LeftCurly, lexeme: "{"},
                  Token {location: 5, kind: TokenKind::LeftCurly, lexeme: "{"},
                  Token {location: 6, kind: TokenKind::RightCurly, lexeme: "}"},
                  Token {location: 7, kind: TokenKind::RightCurly, lexeme: "}"},
                  Token {location: 8, kind: TokenKind::RightCurly, lexeme: "}"},
                  Token {location: 9, kind: TokenKind::RightCurly, lexeme: "}"},
                  Token {location: 10, kind: TokenKind::LeftCurly, lexeme: "{"},
              ],
              ParserError::MismatchedGroup(Some(9)),
              0,
            ),

            // Bad identifiers
            ( // Foreground
                &[
                    Token {location: 0, kind: TokenKind::Foreground, lexeme: "fg"},
                    Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                    Token {location: 10, kind: TokenKind::Identifier, lexeme: "badfgcolor"},
                ],
                ParserError::UnknownColor(10, String::from("badfgcolor")),
                0,
            ),

            ( // Background
              &[
                  Token {location: 0, kind: TokenKind::Background, lexeme: "bg"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                  Token {location: 10, kind: TokenKind::Identifier, lexeme: "badbgcolor"},
              ],
              ParserError::UnknownColor(10, String::from("badbgcolor")),
              0,
            ),

            ( // Style
              &[
                  Token {location: 0, kind: TokenKind::Style, lexeme: "style"},
                  Token {location: 5, kind: TokenKind::Colon, lexeme: ":"},
                  Token {location: 10, kind: TokenKind::Identifier, lexeme: "badstyle"},
              ],
              ParserError::UnknownStyle(10, String::from("badstyle")),
              0,
            )
        ];

        for &(tokens, ref error, skips) in errors.iter() {
            let mut tokens = tokens.iter()
                .map(Token::clone)
                .map(Result::Ok);

            let mut parser = Parser::new(&mut tokens);

            for _ in 0..skips {
                parser.next();
            }

            assert_eq!(parser.next().unwrap().unwrap_err(), *error);
        }
    }
}


//! A Tokenizer for prompter code.
//!
//! # Note on lifetimes
//!
//! The `'s` lifetime is the lifetime of the underlying source code.
//!
//! `Tokenizer` and `Token` only hold slices into the source, but they do not copy it.

use std;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizerError {

    /// Unexpected character
    /// location, actual, expected
    BadChar(usize, Option<char>, Option<char>),

    /// Bad identifier
    /// Generally caused when an identifier consumer is called on a non-identifier character
    /// location, character
    NonIdentifier(usize, Option<char>),


}

impl Display for TokenizerError {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            TokenizerError::BadChar(location, Some(actual), Some(expected)) =>
                write!(f, "expected '{}' but got '{}' at byte {}", expected, actual, location),
            TokenizerError::BadChar(location, Some(actual), None) =>
                write!(f, "unexpected character '{}' at byte {}", actual, location),
            TokenizerError::BadChar(location, None, Some(expected)) =>
                write!(f, "expected character '{}' at byte {}", expected, location),
            TokenizerError::BadChar(location, None, None) =>
                write!(f, "bad character at byte {}", location),

            TokenizerError::NonIdentifier(location, None) =>
                write!(f, "tried and failed to parse identifier at byte {}", location),
            TokenizerError::NonIdentifier(location, Some(actual)) =>
                write!(f, "tried and failed to parse '{}' as the start of an identifier at byte {}", actual, location),
        }
    }
}

impl Error for TokenizerError {
    fn description(&self) -> &str {
        match *self {
            TokenizerError::BadChar(_, _, _) => "bad character",
            TokenizerError::NonIdentifier(_, _) => "tried and failed to parse identifier",
        }
    }

    fn cause(&self) -> Option<&Error> { None }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'s> {

    pub location: usize,
    pub kind: TokenKind,
    pub lexeme: &'s str,

}

/// A dedicated `Result` type for the `Tokenizer`.
pub type TokenizerResult<'s> = std::result::Result<Token<'s>, TokenizerError>;

/// The kinds of `Token`s available.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {

    LeftCurly,
    RightCurly,
    Colon,
    Foreground,
    Background,
    Style,
    Identifier,
    String,
    ParenSurround,
    SquareSurround,
    EOF, // End of file

}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// An `Iterator` that reads through a source string and yields `Token`s as it goes.
#[derive(Debug)]
pub struct Tokenizer<'s> {

    source: &'s str,
    chars: std::str::CharIndices<'s>,
    no_more: bool, // False in normal operation
                   // True after EOF is sent or if there's an unrecoverable error

    current_pos: usize,
    current_char: Option<char>,

}

impl<'s> Tokenizer<'s> {

    /// Creates a new `Tokenizer` over the given source.
    pub fn new(source: &'s str) -> Tokenizer<'s> {

        let mut chars = source.char_indices();

        let current = chars.next();

        Tokenizer {
            source,
            chars,
            no_more: false,

            current_pos: current.unwrap_or((0, '\0')).0, // The '\0' doesn't do anything
            current_char: current.map(|tup| tup.1),
        }
    }

    /// A utility function that determines if a given `char` can be part of an identifier.
    ///
    /// Valid characters include all alphanumeric characters (as defined by `char::is_alphanumeric`
    /// and underscore (`_`).
    fn is_identifier_char(c: char) -> bool {
        c.is_alphanumeric()
            || c == '_'
    }

    /// Advance to the next `char`.
    fn step(&mut self) {
        match self.chars.next() {
            None => {
                self.current_pos = self.source.len();
                self.current_char = None;
            },
            Some((pos, char)) => {
                self.current_pos = pos;
                self.current_char = Some(char);
            },
        }
    }

    /// Advance to the next `char`, returning `true` if it matches the target.
    fn consume(&mut self, target: char) -> bool {
        let current = self.current_char;

        self.step();

        match current {
            None => false,
            Some(c) if c == target => true,
            Some(_) => false,
        }
    }

    /// Determine if the `Tokenizer` has hit the end of the string.
    fn is_finished(&self) -> bool {
        self.current_char.is_none()
    }

    /// Determine if the current `char` matches a given rule.
    fn check_current(&self, predicate: &Fn(char) -> bool) -> bool {
        match self.current_char {
            None => false,
            Some(c) => predicate(c),
        }
    }

    /// Get a `&str` that runs from the given byte index to the `Tokenizer`'s current position.
    ///
    /// It includes the start position, but excludes the current position.
    ///
    /// # Panics
    ///
    /// Panics if `start` is not on a character boundary (as defined by `str::is_char_boundary`, or
    /// if `start > self.current_pos`.
    ///
    /// Because `step` is guaranteed to leave `current_pos` on a character boundary and never
    /// decrease it, it is always safe to call `preceding_chunk` with some previous value of
    /// `current_pos`, assuming `current_pos` is only ever updated by `step`.
    fn preceding_chunk(&self, start: usize) -> &'s str {
        &self.source[start .. self.current_pos]
    }

    /// Produces an error result for a bad character
    ///
    /// Optionally takes the character that was expected
    fn bad_char(&mut self, expected: Option<char>) -> TokenizerResult<'s> {
        self.no_more = true;

        Err(TokenizerError::BadChar(self.current_pos, self.current_char, expected))
    }

    /// Produces and error result for a bad identifier
    fn non_identifier(&mut self) -> TokenizerResult<'s> {
        self.no_more = true;

        Err(TokenizerError::NonIdentifier(self.current_pos, self.current_char))
    }

    /// Consumes a single-character token, producing a success result.
    ///
    /// The `Tokenizer` should be on the token's first byte.
    fn single_token(&mut self, kind: TokenKind) -> TokenizerResult<'s> {

        // Save off the current location (so that we can step ahead later)
        let location = self.current_pos;

        // Actually consume the thing
        self.step();

        Ok(Token {
            location,
            kind,
            lexeme: self.preceding_chunk(location),
        })

    }

    /// Consumes an identifier, producing a token of the correct kind or an error.
    ///
    /// The `Tokenizer` should be on the identifier's first byte. If the first character is not a
    /// valid identifier character (as defined by `is_identifier_char`), an error is returned.
    ///
    /// If the identifier is a known multi-character token, the `Token::kind` field is set
    /// appropriately. If not, it defaults to `TokenKind::Identifier`.
    fn identifier(&mut self) -> TokenizerResult<'s> {

        let start = self.current_pos;

        while self.check_current(&Tokenizer::is_identifier_char) {
            self.step();
        }

        // Figure out what the lexeme is
        // The slice starts wherever we started at
        // It ends just before the end of the source or our current position
        let lexeme = self.preceding_chunk(start);

        if start == self.current_pos {
            self.non_identifier()
        } else {
            // Figure out what type it is
            // Default to a generic Identifier
            Ok(Token {
                location: start,
                lexeme,
                kind: match lexeme {
                    "fg" => TokenKind::Foreground,
                    "bg" => TokenKind::Background,
                    "style" => TokenKind::Style,
                    _ => TokenKind::Identifier,
                }
            })
        }
    }

    /// Consumes a token that is surrounded by another token.
    ///
    /// A surround token has a leading character, zero or more body characters, and a trailing
    /// character. For example, a string has both leading and trailing characters of `"`.
    ///
    /// The `Tokenizer` should be on the leading character's first byte. It will consume the leading
    /// character, body, and trailing character, and be left at byte immediately after the trailing
    /// character's last byte.
    ///
    /// If the `first` cannot be consumed, an error is returned at the leading character. If `last`
    /// cannot be found, an error is returned at the end of the input. Otherwise, a token of the
    /// specified `kind` is returned, with the location at the first byte of the input character.
    fn surround(&mut self, first: char, last: char, kind: TokenKind) -> TokenizerResult<'s> {

        // Remember where the start of the surround is
        let start = self.current_pos;

        // Consume the opening character
        if !self.consume(first) {
            return self.bad_char(Some(first));
        }

        // Consume the body
        while self.check_current(&|c| c != last) {
            self.step();
        }

        // Consume the closing character
        if !self.consume(last) {
            return self.bad_char(Some(last));
        }

        Ok(Token {
            location: start,
            kind,
            lexeme: self.preceding_chunk(start),
        })

    }
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = TokenizerResult<'s>;

    fn next(&mut self) -> Option<TokenizerResult<'s>> {

        // Consume all waiting whitespace
        while self.check_current(&char::is_whitespace) {
            self.step();
        }

        // If it's finished, send the EOF token once then stop iterating
        if self.no_more {
            None
        } else if self.is_finished() {

            self.no_more = true;

            Some(Ok(Token {
                location: self.current_pos,
                kind: TokenKind::EOF,
                lexeme: "",
            }))

        } else {

            Some(match self.current_char.expect("Checked for is_finished()") {
                '{' => self.single_token(TokenKind::LeftCurly),
                '}' => self.single_token(TokenKind::RightCurly),
                ':' => self.single_token(TokenKind::Colon),
                '"' => self.surround('"', '"', TokenKind::String),
                '(' => self.surround('(', ')', TokenKind::ParenSurround),
                '[' => self.surround('[', ']', TokenKind::SquareSurround),
                c if Tokenizer::is_identifier_char(c) => self.identifier(),
                _ => self.bad_char(None),

            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // Some tokens to try. Tuples hold the lexeme and the type
    // Note: Making this longer increases test run time pretty drastically. test_two_tokens runs in
    // O(n^2) where n is TOKENS.len().
    const TOKENS: [(&'static str, TokenKind); 16] = [
        ("{", TokenKind::LeftCurly),
        ("}", TokenKind::RightCurly),
        (":", TokenKind::Colon),
        ("\"\"", TokenKind::String), // Empty string
        ("\"foobar\"", TokenKind::String), // Non-whitespace string
        ("\"space: tab:\tnewline:\ncrlf:\r\n\"", TokenKind::String), // Single whitespace string
        ("\"spaces:      tabs:\t\t\t\tnewlines:\n\n\ncrlfs:\r\n\r\n\r\n\"", TokenKind::String), // Multi-whitespace string
        ("\"This will be poop. 💩 This has been poop.\"", TokenKind::String), // Unicode-containing string
        ("fg", TokenKind::Foreground),
        ("bg", TokenKind::Background),
        ("style", TokenKind::Style),
        ("s", TokenKind::Identifier), // One-char identifier
        ("foobar", TokenKind::Identifier), // Multi-char identifier
        ("underscore_here", TokenKind::Identifier), // Underscored identifier
        ("(test builtin)", TokenKind::ParenSurround),
        ("[test command]", TokenKind::SquareSurround),
    ];

    // Tuple is:
    // Source that causes the error
    // Successful token parses before the error
    // Expected error object
    const FAILURES: [(&'static str, u32, TokenizerError); 5] = [
        // String bad first and last
        ("\"", 0, TokenizerError::BadChar(1, None, Some('\"'))),
        ("fg: blue bg: red { \"some_unfinished_literal ", 7, TokenizerError::BadChar(44, None, Some('"'))),

        // Unterminated paren surround
        ("style: underline (unfinished paren surround", 3, TokenizerError::BadChar(43, None, Some(')'))),

        // Unterminated square surround
        ("style: italic [unfinished square surround", 3, TokenizerError::BadChar(41, None, Some(']'))),

        // Unrecognized character
        ("style :green { \" this literal is OK?\" ?", 5, TokenizerError::BadChar(38, Some('?'), None)),
    ];

    // When generating sources, some tokens need spaces around them
    // Example: if "bg" and "fg" are next to each other, "bgfg" gets parsed as a single identifier
    // Contrapositive: "{}[](),:" gets parsed correctly
    // You only need a space if both neighboring tokens need space
    // Example: "(hey)" is parsed correctly
    fn needs_space(kind: TokenKind) -> bool {
        match kind {
            TokenKind::LeftCurly => false,
            TokenKind::RightCurly => false,
            TokenKind::Colon => false,
            TokenKind::String => false,
            TokenKind::SquareSurround => false,
            TokenKind::ParenSurround => false,
            TokenKind::Foreground => true,
            TokenKind::Background => true,
            TokenKind::Style => true,
            TokenKind::Identifier => true,
            TokenKind::EOF => false,
        }
    }

    // Picks an arbitrary (pseudo-random) token out of TOKENS
    // Deterministic based on both of the given seeds
    // If TOKENS gets bigger than a few dozen, might be worth increasing the primes used
    fn arbitrary_token(seed_1: usize, seed_2: usize) -> (&'static str, TokenKind) {
        // World's worst hash algorithm
        // Takes 2 seeds, multiplies each by a largish primes, then brings it down under the length
        TOKENS[(691 * seed_1 + 701 * seed_2) % TOKENS.len()]
    }

    // Try an empty string.
    //
    // Yields EOF immediately.
    #[test]
    fn test_no_tokens() {
        let mut tok = Tokenizer::new("");

        assert_eq!(tok.next(), Some(Ok(Token {
            location: 0,
            kind: TokenKind::EOF,
            lexeme: "",
        })));

        assert_eq!(tok.next(), None);

        // Whitespace should have the same behavior, but report the correct location
        let mut tok = Tokenizer::new(" \n\t\r");

        assert_eq!(tok.next(), Some(Ok(Token {
            location: 4,
            kind: TokenKind::EOF,
            lexeme: "",
        })));

        assert_eq!(tok.next(), None);
    }

    // Try a single token.
    //
    // Yields that token, then EOF.
    #[test]
    fn test_single_token() {

        for &(s, kind) in TOKENS.iter() {
            let mut tok = Tokenizer::new(s);

            assert_eq!(tok.next(), Some(Ok(Token {
                location: 0,
                kind,
                lexeme: s,
            })));

            assert_eq!(tok.next(), Some(Ok(Token {
                location: s.len(),
                kind: TokenKind::EOF,
                lexeme: "",
            })));

            assert_eq!(tok.next(), None);
        }

    }

    // Try each pair of two tokens.
    //
    // Yields those tokens in order, then EOF.
    #[test]
    fn test_two_tokens() {

        for &(s1, kind1) in TOKENS.iter() {
            for &(s2, kind2) in TOKENS.iter() {

                let offset: usize;
                let s: String;

                if needs_space(kind1) && needs_space(kind2) {
                    offset = 1;
                    s = format!("{} {}", s1, s2);
                } else {
                    offset = 0;
                    s = format!("{}{}", s1, s2);
                }

                let mut tok = Tokenizer::new(&s);

                assert_eq!(tok.next(), Some(Ok(Token {
                    location: 0,
                    kind: kind1,
                    lexeme: s1,
                })));

                assert_eq!(tok.next(), Some(Ok(Token {
                    location: s1.len() + offset,
                    kind: kind2,
                    lexeme: s2,
                })));

                assert_eq!(tok.next(), Some(Ok(Token {
                    location: s1.len() + offset + s2.len(),
                    kind: TokenKind::EOF,
                    lexeme: "",
                })));

                assert_eq!(tok.next(), None);

            }
        }

    }

    // Try a bunch of longer sources.
    //
    // Mess with TRIALS and LENGTH so it runs in an appropriate length of time. TRIALS is the number
    // of test source strings generated, LENGTH is the number of tokens in each source string.
    #[test]
    fn fuzz_many_tokens() {

        static TRIALS: usize = 1000;
        static LENGTH: usize = 1000;

        for trial_index in 0..TRIALS {

            let mut source = String::from("");
            let mut expected: Vec<Token> = Vec::with_capacity(LENGTH);

            for token_index in 0..LENGTH {

                let (current_lexeme, current_kind) = arbitrary_token(trial_index, token_index);

                if expected.last().map_or(false, |t| needs_space(t.kind)) && needs_space(current_kind) {
                    source += " ";
                }

                expected.push(Token {
                    location: source.len(),
                    kind: current_kind,
                    lexeme: current_lexeme,
                });

                source += current_lexeme;

            }

            let mut tokenizer = Tokenizer::new(&source);

            for expected_token in expected {
                assert_eq!(tokenizer.next(), Some(Ok(expected_token)));
            }

            assert_eq!(tokenizer.next(), Some(Ok(Token {
                location: source.len(),
                kind: TokenKind::EOF,
                lexeme: "",
            })));

            assert_eq!(tokenizer.next(), None);

        }

    }

    // Try each of the failure cases in FAILURES.
    #[test]
    fn test_failures() {

        for &(source, leading_tokens, ref error) in FAILURES.iter() {
            let mut tokenizer = Tokenizer::new(source);

            for _ in 0..leading_tokens {
                tokenizer.next();
            }

            let actual_error = tokenizer.next().unwrap().unwrap_err();

            assert_eq!(&actual_error, error);
        }

    }

}

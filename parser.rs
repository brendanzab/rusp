// Copyright 2013 The Rusp Developers. For a full listing of the authors,
// refer to the AUTHORS file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use super::*;

///
/// A position in a source string
///
pub struct Position {
    line: uint,
    col: uint,
}

pub struct ParseFailure {
    description: ~str,
    position: Position,
}

impl ParseFailure {
    fn err<T>(descr: ~str, pos: Position) -> Result<T, ParseFailure> {
        Err(ParseFailure { description: descr, position: pos })
    }
    fn eof<T>(pos: Position) -> Result<T, ParseFailure> {
        ParseFailure::err(~"Unexpected EOF", pos)
    }
}

#[deriving(Eq)]
enum Tok<'self> {
    LPAREN, RPAREN,
    LIT(&'self str),
    STRING(&'self str),
    EOF
}

struct Token<'self> {
    val: Tok<'self>,
    position: Position,
}

impl<'self> Token<'self> {
    fn new<'r>(val: Tok<'r>, position: Position) -> Token<'r> {
        Token { val: val, position: position }
    }
}


pub struct Parser<'self> {
    token_start: uint,
    src_position: uint,
    position: Position,
    token: Result<Token<'self>, ParseFailure>,
    src: &'self str
}

pub impl<'self> Parser<'self> {
    fn new<'r>(src: &'r str) -> Parser<'r> {
        let pos = Position { line: 1, col: 0 };
        Parser {
            token_start: 0,
            src_position: 0,
            position: pos,
            token: ParseFailure::err(~"No input", pos),
            src: src
        }
    }

    // Tokenising

    /// Load the next token in to `self.token`. If it is currently
    /// `Ok(..)`, then it is preserved or overwritten depending on the
    /// value of `overwrite`.
    fn load_token(&mut self, overwrite: bool) {
        macro_rules! set_token (
            ($tok:expr) => {{
                self.token = Ok(Token::new($tok, self.position));
                self.token_start = self.src_position;
                return;
            }}
        );

        cond!(
            // already loaded
            (!overwrite && self.token.is_ok()) { return; }
            // a left-over token
            (self.token_start < self.src_position) {
                println("foo");
                self.src_position = self.token_start; // reparse the token
            }
            // finished
            (self.token_start >= self.src.len()) {
                set_token!(EOF);
            }
        );

        let mut seen_token = false;
        let mut inside_string = false;

        for str::each_char(self.src.slice(self.src_position, self.src.len())) |c| {
            match c {
                // TODO string escape sequences
                '"' => { // string
                    if seen_token { break }
                    if !inside_string {
                        inside_string = true;
                        self.token_start = self.src_position + 1;
                    } else {
                        self.src_position += 1; // skip the trailing " for the next token
                        set_token!(STRING(self.src.slice(self.token_start,
                                                         self.src_position - 1)))
                    }
                }
                // just collect characters while inside a string
                _ if inside_string => {}
                '(' | ')' => {
                    if !seen_token {
                        // we haven't seen a token yet, so use this character
                        self.position.col += 1; // always 1 column
                        self.src_position += 1; // always 1 byte
                        set_token!(if c == '(' {LPAREN} else {RPAREN})
                    } // we've already seen a token, so don't advance, and use that one
                    break;
                }
                // ignore (leading) whitespace
                _ if c.is_whitespace() => { if seen_token { break } }
                _ => {
                    if !seen_token { // start a new token here
                        self.token_start = self.src_position;
                        seen_token = true;
                    }
                }
            }
            if c == '\n' {
                self.position.line += 1;
                self.position.col = 0;
            } else {
                // technically incorrect, because we are iterating
                // over characters, not graphemes
                self.position.col += 1;
            }

            self.src_position += char::len_utf8_bytes(c);
        }

        if seen_token {
            set_token!(LIT(self.src.slice(self.token_start, self.src_position)))
        } else {
            set_token!(EOF);
        }
    }

    fn peek_token<'r>(&'r mut self) -> Result<Token<'self>, ParseFailure> {
        // this is does nothing if token is Some
        self.load_token(false);
        copy self.token
    }

    fn bump_token(&mut self) {
        self.load_token(false);
        self.load_token(true);
    }

    fn eat_token(&mut self, tok: Tok) -> bool {
        match self.peek_token() {
            Ok(Token { val, _ }) if val == tok => {
                self.bump_token();
                true
            }
            _ => false,
        }
    }

    fn expect_token(&mut self, tok: Tok) -> Result<(), ParseFailure> {
        if self.eat_token(tok) {
            Ok(())
        } else {
            ParseFailure::err(fmt!("Expecting %?", tok), self.position)
        }
    }

    fn is_eof(&mut self) -> bool {
        // eating is ok, since the token after EOF is also EOF
        self.eat_token(EOF)
    }

    // Parsing

    /// Parse an identifier
    fn parse_ident(&mut self) -> Result<Ident, ParseFailure> {
        do self.peek_token().chain |tok| {
            match tok.val {
                LIT(val) => {
                    self.bump_token();
                    Ok(val.to_owned())
                }
                EOF => {
                    ParseFailure::eof(tok.position)
                }
                _ => {
                    ParseFailure::err(~"invalid ident", tok.position)
                }
            }
        }
    }

    fn parse_value_from_token(&mut self, tok: Token)  -> Result<Value, ParseFailure> {
        macro_rules! ret(($val:expr) => {{ self.bump_token(); return Ok($val); }});
        match tok.val {
            LIT(string) => {
                match string {
                    "true" => ret!(Bool(true)),
                    "false" => ret!(Bool(false)),
                    _ => {}
                }
                match int::from_str(string) {
                    Some(i) => ret!(Int(i)),
                    None => {}
                }
                match float::from_str(string) {
                    Some(f) => ret!(Float(f)),
                    None => {}
                }
                // fall-through on failure
            }
            STRING(string) => ret!(Str(string.to_owned())),
            EOF => { return ParseFailure::eof(tok.position); }
            _ => {}
        }

        ParseFailure::err(~"Invalid value", tok.position)
    }

    fn parse_value(&mut self) -> Result<Value, ParseFailure> {
        do self.peek_token().chain |tok| { self.parse_value_from_token(tok) }
    }

    fn parse_value_or_ident(&mut self) -> Result<Value, ParseFailure> {
        match self.parse_value() {
            Ok(val) => Ok(val),
            Err(_) => self.parse_ident().map(|&ident| Symbol(ident))
        }
    }

    /// Parse the interior of an S-expr
    fn parse_parened(&mut self) -> Result<Value, ParseFailure> {
        Ok(List(
            do vec::build |push| {
                loop {
                    match self.parse() {
                        Ok(val) => push(@val),
                        Err(_) => break,
                    }
                }
            }
        ))
    }

    fn parse(&mut self) -> Result<Value, ParseFailure> {
        if self.eat_token(LPAREN) {
            do self.parse_parened().chain |parsed| {
                if self.eat_token(RPAREN) {
                    Ok(parsed)
                } else if self.eat_token(EOF) {
                    ParseFailure::eof(self.position)
                } else {
                    ParseFailure::err(~"Expecting ')'", self.position)
                }
            }
        } else {
            do self.peek_token().chain |tok| {
                if (tok.val == RPAREN) {
                    ParseFailure::err(~"Unexpected ')'", tok.position)
                } else {
                    self.parse_value_or_ident()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_ok() {
        macro_rules! test(
            ($s:expr) => ({
                let src = $s.clone();
                assert_eq!(Parser::new(src).parse().get().to_str(), src)
            })
        )

        test!(~"1");
        test!(~"(def a (+ 1 2))");
        test!(~"(do (def a 1) (def b 2) (+ a b))");
        test!(~"(list (1 2) 3 (+ 4 5))");

        test!(~"(if true (fn (a b) (+ 1 a b)) (quote (1 2 3)))");

        test!(~"(foo bar \"a b c d\")");

        test!(~"(ö ä å)");
    }

    #[test]
    fn test_parser_err() {
        macro_rules! test(
            ($s:expr) => ({
                let src = $s.clone();
                assert!(Parser::new(src).parse().is_err())
            })
        )

        test!(~"(");
        test!(~")");
        test!(~"\"no closing quote")
    }
}

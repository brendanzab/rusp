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

use std::str;
use std::int;
use std::float;

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

static EOF : char = 0 as char;

pub struct Parser<'self> {
    priv src_pos: uint,
    priv pos: Position,
    priv src: &'self str,
    priv src_len: uint,
}

pub type ParseResult = Result<@Value, ParseFailure>;

impl<'self> Parser<'self> {
    pub fn new<'r>(src: &'r str) -> Parser<'r> {
        Parser {
            src_pos: 0,
            pos: Position { line: 1, col: 1 },
            src: src,
            src_len: src.len()
        }
    }



    pub fn parse(&mut self) -> ParseResult {
        self.skip_whitespace();
        if self.eat('(') {
            self.parse_list()
        } else {
            match self.peek() {
                '.' if self.eat_str("..") => Ok(@Symbol(~"..")),
                '0'..'9' | '.' => self.parse_number(),
                't' if self.eat_str("true") => Ok(@Bool(true)),
                'f' if self.eat_str("false") => Ok(@Bool(false)),
                '"' => self.parse_string(),
                ')' => self.fail(~"Unexpected ')'"),
                _ => self.parse_ident()
            }
        }
    }

    pub fn eof(&self) -> bool {
        self.src_pos >= self.src_len
    }

    priv fn parse_list(&mut self) -> ParseResult {
        let mut vals = ~[];
        loop {
            if self.eof() {
                return self.fail(~"Unexpected EOF, expected ')'");
            }

            if self.eat(')') {
                return Ok(@List(vals));
            }

            match self.parse() {
                Ok(val) => vals.push(val),
                err => return err
            }

            self.skip_whitespace();
        }
    }

    priv fn parse_number(&mut self) -> ParseResult {
        let mut len = 0;
        let mut seen_point = self.peek() == '.';

        loop {
            match self.src.char_at(self.src_pos+len) {
                '.' if seen_point => return self.fail(~"Unexpected character '.' while parsing number"),
                '.' if !seen_point => {
                    len += 1; // I know that '.' is a single byte
                    seen_point = true;
                }
                '0'..'9' => len += 1, // Same here
                ')' => break,
                c if c.is_whitespace() => break,
                c => {
                    return self.fail(fmt!("Unexpected character '%c' while parsing number", c));
                }
            }
            if (self.src_pos+len) >= self.src_len {
                break
            }
        }

        let slice = self.slice_src(len);
        self.advance(slice.char_len());
        if seen_point {
            let n = float::from_str(slice).unwrap();
            Ok(@Float(n))
        } else {
            let n = int::from_str(slice).unwrap();
            Ok(@Int(n))
        }
    }

    priv fn parse_string(&mut self) -> ParseResult {
        if self.eat('"') {
            let mut s = ~"";

            loop {
                match self.pop() {
                    '"' => {
                        return Ok(@Str(s));
                    }
                    '\\' => {
                        match self.peek() {
                            // Minimal list of escape characters, most common ones
                            '0' => s.push_char(0x00 as char),
                            'a' => s.push_char(0x07 as char),
                            'b' => s.push_char(0x08 as char),
                            't' => s.push_char(0x09 as char),
                            'n' => s.push_char(0x0A as char),
                            'v' => s.push_char(0x0B as char),
                            'f' => s.push_char(0x0C as char),
                            'r' => s.push_char(0x0D as char),
                            '"' => s.push_char(0x22 as char),
                            '\''=> s.push_char(0x27 as char),
                            '\\'=> s.push_char(0x5C as char),

                            EOF => return self.fail(~"Unexpected EOF while parsing escape"),
                            _   => loop,
                        }
                        self.advance(1);
                    }
                    EOF => {
                        return self.fail(~"Unexpected EOF while parsing string");
                    }
                    a => {
                        s.push_char(a);
                    }
                }
            }

        } else {
            fail!("Parser Error, parse_string called at the wrong time");
        }
    }

    priv fn parse_ident(&mut self) -> ParseResult {
        let c = self.peek();

        if self.char_is_read_macro(c) {
            return self.eval_read_macro(c);
        }

        let mut ident = ~"";

        loop {
            match self.peek() {
                EOF => return self.fail(~"Unexpected EOF while parsing ident"),
                c if self.char_is_read_macro(c) => {
                    return self.fail(fmt!("Unexpected character '%c' while parsing ident", c));
                }
                ')' => return Ok(@Symbol(ident)),
                c if c.is_whitespace() => return Ok(@Symbol(ident)),
                c => ident.push_char(c),
            }
            self.advance(1);
        }
    }

    priv fn skip_whitespace(&mut self) {
        while self.peek().is_whitespace() {
            self.advance(1)
        }
    }

    priv fn fail(&self, msg: ~str) -> ParseResult {
        ParseFailure::err(msg, self.pos)
    }

    priv fn slice_src(&self, len: uint) -> &'self str {
        self.src.slice(self.src_pos, self.src_pos+len)
    }

    priv fn pop(&mut self) -> char {
        let c = self.peek();
        self.advance(1);
        c
    }

    priv fn peek(&self) -> char {
        if self.eof() {
            EOF
        } else {
            self.src.char_at(self.src_pos)
        }
    }

    priv fn eat(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance(1);
            true
        } else {
            false
        }
    }

    priv fn eat_str(&mut self, s: &str) -> bool {
        let slen = s.len();
        if (self.src_pos + slen) >= self.src_len {
            false
        } else {
            let slice = self.slice_src(slen);
            if slice == s {
                self.advance(s.char_len());
                true
            } else {
                false
            }
        }
    }

    priv fn advance(&mut self, n: uint) {
        if self.eof() { return; }
        let mut n = n;
        while n > 0 {
            let str::CharRange {ch:c, next:next} = str::char_range_at(self.src, self.src_pos);
            self.src_pos = next;
            n -= 1;
            self.pos.col += 1;
            if c == '\n' {
                self.pos.col = 1;
                self.pos.line += 1;
            }
        }
    }

    priv fn char_is_read_macro(&mut self, c: char) -> bool {
        match c {
            '\'' | '`' | '#' => true,
            _ => false
        }
    }

    priv fn eval_read_macro(&mut self, _: char) -> ParseResult {
        self.advance(1);
        self.parse()
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

        test!(~"\"new-line:\\n\"");
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

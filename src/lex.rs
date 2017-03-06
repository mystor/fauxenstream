use std::char;

use unicode_xid::UnicodeXID;

use {
    LexError,
    Span,
    Symbol,
    ByteSymbol,
    TokenStream,
    TokenTree,
    TokenKind,
    Delimiter,
    CommentKind,
    OpJoin,
    IntVal,
    FloatVal,
};

pub fn lex_str(input: &str, offset: usize) -> Result<TokenStream, LexError> {
    let mut lexer = Lexer::new(input, offset);
    lexer.parse_complete_stream()
}

/// Helper macro for returning lexer errors
macro_rules! lex_fail {
    ($lexer:expr, $($rest:expr),*) => {
        {
            use std::fmt::Write;
            let mut reason = format!($($rest),*);
            let off = $lexer.err_offset();
            write!(&mut reason, " at offset {}", off).unwrap();

            return Err(LexError(reason));
        }
    }
}

/// An internal type alias to reduce typing
type SResult<T> = Result<T, LexError>;

struct Lexer<'a> {
    input: &'a str,
    idx: usize,
    offset: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer, given the input string
    fn new(input: &'a str, offset: usize) -> Lexer<'a> {
        Lexer {
            input: input,
            idx: 0,
            offset: offset,
        }
    }

    fn byte(&self, idx: usize) -> u8 {
        if idx + self.idx < self.input.len() {
            self.input.as_bytes()[idx + self.idx]
        } else {
            0
        }
    }

    fn opt_byte(&self, idx: usize) -> Option<u8> {
        if idx + self.idx < self.input.len() {
            Some(self.byte(idx))
        } else {
            None
        }
    }

    fn rest(&self) -> &'a str {
        &self.input[self.idx..]
    }

    fn next_char(&self) -> char {
        self.rest().chars().next().unwrap_or('\0')
    }

    fn opt_next_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn span_from(&self, lo_idx: usize) -> Span {
        Span {
            lo: lo_idx + self.offset,
            hi: self.idx + self.offset,
        }
    }

    fn err_offset(&self) -> usize {
        self.idx + self.offset
    }

    fn skip_whitespace(&mut self) -> SResult<()> {
        while is_whitespace(self.next_char()) {
            self.idx += self.next_char().len_utf8();
        }
        Ok(())
    }

    fn block_comment(&mut self) -> SResult<()> {
        assert!(self.rest().starts_with("/*"));

        let mut depth = 0;
        while self.rest().len() >= 2 {
            if self.byte(0) == b'/' && self.byte(1) == b'*' {
                depth += 1;
                self.idx += 2; // eat '*'
                continue;
            }

            if self.byte(0) == b'*' && self.byte(1) == b'/' {
                depth -= 1;
                self.idx += 2;
                if depth == 0 {
                    return Ok(());
                }
                continue;
            }

            self.idx += 1;
        }

        lex_fail!(self, "Unexpected EOF while parsing block comment");
    }

    fn comment(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;
        if self.byte(0) != b'/' {
            return Ok(None);
        }

        let (block_byte, inner_byte) = (self.byte(1), self.byte(2));

        if block_byte == b'/' {
            if let Some(len) = self.rest().find('\n') {
                self.idx += len;
            } else {
                self.idx = self.input.len();
            }
        } else if block_byte == b'*' {
            self.block_comment()?
        } else {
            return Ok(None);
        };

        let comment_kind = if inner_byte == block_byte {
            CommentKind::OuterDoc
        } else if inner_byte == b'!' {
            CommentKind::InnerDoc
        } else {
            CommentKind::Regular
        };

        Ok(Some(TokenTree {
            kind: TokenKind::Comment(comment_kind),
            span: self.span_from(start),
        }))
    }

    fn symbol(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;

        let ch = self.next_char();
        if !is_symbolic(ch) {
            return Ok(None);
        }
        self.idx += ch.len_utf8();

        let join = if is_symbolic(self.next_char()) {
            OpJoin::Join
        } else {
            OpJoin::Alone
        };

        Ok(Some(TokenTree {
            kind: TokenKind::Op(join, ch),
            span: self.span_from(start),
        }))
    }

    fn word(&mut self) -> Option<&'a str> {
        let mut chars = self.rest().char_indices();
        match chars.next() {
            Some((_, ch)) if UnicodeXID::is_xid_start(ch) || ch == '_' => {}
            _ => return None,
        }

        while let Some((i, ch)) = chars.next() {
            if !UnicodeXID::is_xid_continue(ch) {
                let result = &self.rest()[..i];
                self.idx += i;
                return Some(result);
            }
        }

        let result = self.rest();
        self.idx = self.input.len();
        Some(result)
    }

    fn ident(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;

        Ok(self.word().map(|word| {
            TokenTree {
                kind: TokenKind::Word(word.into()),
                span: self.span_from(start),
            }
        }))
    }

    fn num(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;

        let base = match (self.byte(0), self.byte(1)) {
            (b'0', b'x') => {
                self.idx += 2;
                16
            }
            (b'0', b'o') => {
                self.idx += 2;
                8
            }
            (b'0', b'b') => {
                self.idx += 2;
                2
            }
            (b'0'...b'9', _) => 10,
            _ => return Ok(None),
        };

        let mut int_value = Some(0u64);
        let mut has_dot = false;
        let mut has_exp = false;
        loop {
            let b = self.byte(0);
            let digit = match b {
                b'e' | b'E' if base == 10 => {
                    self.idx += 1;
                    has_exp = true;
                    break;
                }
                b'0'...b'9' => (b - b'0') as u64,
                b'a'...b'f' if base > 10 => 10 + (b - b'a') as u64,
                b'A'...b'F' if base > 10 => 10 + (b - b'A') as u64,
                b'_' => {
                    self.idx += 1;
                    continue;
                }
                b'.' if base == 10 => {
                    self.idx += 1;
                    let ignorable =
                        has_dot ||
                        self.byte(0) == b'.' ||
                        UnicodeXID::is_xid_start(self.next_char());
                    if ignorable {
                        self.idx -= 1;
                        break;
                    }

                    has_dot = true;
                    continue;
                }
                _ => break,
            };

            if digit >= base {
                lex_fail!(self, "Unexpected digit {:x} out of base range", digit);
            }

            int_value = int_value
                .and_then(|v| v.checked_mul(base))
                .and_then(|v| v.checked_add(digit));
            self.idx += 1;
        }

        if has_exp {
            let mut has_value = false;
            loop {
                match self.byte(0) {
                    b'+' | b'-' if !has_value => {
                        self.idx += 1;
                    }
                    b'0'...b'9' => {
                        self.idx += 1;
                        has_value = true;
                    }
                    b'_' => {
                        self.idx += 1;
                    }
                    _ => if !has_value {
                        lex_fail!(self, "Unexpected end of float literal after \
                                        `E` character");
                    } else {
                        break;
                    }
                }
            }
        }

        let can_be_int = !has_dot && !has_exp;
        let can_be_float = base == 10;

        let value = self.input[start..self.idx].into();

        let suffix_start = self.idx;
        let is_int = match (self.byte(0), self.byte(1), self.byte(2), self.byte(3), self.byte(4)) {
            (b'f', b'3', b'2', ..) if can_be_float => {
                self.idx += 3;
                false
            }
            (b'f', b'6', b'4', ..) if can_be_float => {
                self.idx += 3;
                false
            }
            (b'u', b'8', ..) if can_be_int => {
                self.idx += 2;
                true
            }
            (b'i', b'8', ..) if can_be_int => {
                self.idx += 2;
                true
            }
            (b'u', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'i', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'u', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'i', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'u', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'i', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                true
            }
            (b'u', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 3;
                true
            }
            (b'i', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 3;
                true
            }
            _ => if can_be_int {
                true
            } else if can_be_float {
                false
            } else {
                unreachable!()
            }
        };

        let suffix = self.input[suffix_start..self.idx].into();

        Ok(Some(TokenTree {
            kind: if is_int {
                TokenKind::Int {
                    value: IntVal {
                        computed: int_value,
                        value: value,
                    },
                    suffix: suffix
                }
            } else {
                TokenKind::Float {
                    value: FloatVal {
                        computed: value.parse().ok(),
                        value: value,
                    },
                    suffix: suffix
                }
            },
            span: self.span_from(start),
        }))
    }

    fn raw_str(&mut self) -> SResult<(&str, usize)> {
        let mut pounds = 0;
        while self.byte(0) == b'#' {
            pounds += 1;
            self.idx += 1;
        }
        if self.byte(0) != b'"' {
            lex_fail!(self, "Unexpected character while parsing raw string literal");
        }
        self.idx += 1;
        let inner_start = self.idx;

        loop {
            match self.byte(0) {
                b'"' => {
                    let end_start = self.idx;
                    self.idx += 1;
                    let mut cl_pounds = 0;
                    while cl_pounds < pounds {
                        if self.byte(0) != b'#' {
                            break;
                        }
                        cl_pounds += 1;
                        self.idx += 1;
                    }
                    if cl_pounds == pounds {
                        return Ok((&self.input[inner_start..end_start], pounds));
                    }
                }
                _ => {}
            }
            self.idx += 1;
        }
    }

    fn backslash_x(&mut self) -> SResult<u8> {
        let mut ch = 0;
        let b0 = self.byte(0);
        let b1 = self.byte(1);
        ch += 0x10 * match b0 {
            b'0'...b'9' => b0 - b'0',
            b'a'...b'f' => 10 + (b0 - b'a'),
            b'A'...b'F' => 10 + (b0 - b'A'),
            _ => lex_fail!(self, "Unexpected non-hex character after \\x"),
        };
        ch += 0x1 * match b1 {
            b'0'...b'9' => b1 - b'0',
            b'a'...b'f' => 10 + (b1 - b'a'),
            b'A'...b'F' => 10 + (b1 - b'A'),
            _ => lex_fail!(self, "Unexpected non-hex character after \\x"),
        };
        self.idx += 2;
        Ok(ch)
    }

    fn backslash_u(&mut self) -> SResult<char> {
        if self.byte(0) != b'{' {
            lex_fail!(self, "Expected {{ after unicode hex after \\u");
        }
        self.idx += 1;
        let mut ch = 0;
        for _ in 0..6 {
            let b = self.byte(0);
            match b {
                b'0'...b'9' => {
                    ch *= 0x10;
                    ch += (b - b'0') as u32;
                    self.idx += 1;
                }
                b'a'...b'f' => {
                    ch *= 0x10;
                    ch += (10 + b - b'a') as u32;
                    self.idx += 1;
                }
                b'A'...b'F' => {
                    ch *= 0x10;
                    ch += (10 + b - b'A') as u32;
                    self.idx += 1;
                }
                b'}' => break,
                _ => lex_fail!(self, "Unexpected non-hex character after \\u"),
            }
        }
        if self.byte(0) != b'}' {
            lex_fail!(self, "Expected }} after unicode hex after \\u");
        }
        self.idx += 1;
        if let Some(ch) = char::from_u32(ch) {
            Ok(ch)
        } else {
            lex_fail!(self, "Character code {:x} is not a valid unicode character", ch);
        }
    }

    fn str_lit(&mut self) -> SResult<Symbol> {
        let mut s = String::new();
        loop {
            let ch = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s.into());
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            let byte = self.backslash_x()?;
                            if byte > 0x80 {
                                lex_fail!(self, "Invalid \\x byte {:x} in string literal", byte);
                            }
                            char::from_u32(byte as u32).unwrap()
                        }
                        Some(b'u') => {
                            self.idx += 2;
                            self.backslash_u()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            '\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            '\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            '\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            '\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            '\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            '\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            '"'
                        }
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => lex_fail!(self, "Unexpected byte {:?} after \\ character \
                                                   in byte literal", b),
                        None => lex_fail!(self, "Unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(_) => {
                    let ch = self.next_char();
                    self.idx += ch.len_utf8();
                    ch
                },
                None => lex_fail!(self, "Unexpected Eof while parsing byte literal")
            };
            s.push(ch);
        }
    }

    fn byte_str_lit(&mut self) -> SResult<ByteSymbol> {
        let mut s = Vec::new();
        loop {
            let byte = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s.into());
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            self.backslash_x()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            b'\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            b'\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            b'\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            b'\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            b'\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            b'\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            b'"'
                        }
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => lex_fail!(self, "Unexpected byte {:?} after \\ character \
                                                   in byte literal", b),
                        None => lex_fail!(self, "Unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(b) => {
                    self.idx += 1;
                    b
                },
                None => lex_fail!(self, "Unexpected Eof while parsing byte literal")
            };
            s.push(byte);
        }
    }

    fn char_lit(&mut self) -> SResult<Option<char>> {
        let start = self.idx;
        let ch = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        let byte = self.backslash_x()?;
                        assert!(byte <= 0x80);
                        char::from_u32(byte as u32).unwrap()
                    }
                    Some(b'u') => {
                        self.idx += 2;
                        self.backslash_u()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        '\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        '\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        '\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        '\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        '\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        '\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        '"'
                    }
                    Some(b) => lex_fail!(self, "Unexpected byte {:?} after \\ character \
                                               in byte literal", b),
                    None => lex_fail!(self, "Unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(_) => {
                let ch = self.next_char();
                self.idx += ch.len_utf8();
                ch
            },
            None => lex_fail!(self, "Unexpected Eof while parsing byte literal")
        };

        if self.byte(0) != b'\'' {
            self.idx = start;
            Ok(None)
        } else {
            self.idx += 1;
            Ok(Some(ch))
        }
    }

    fn byte_lit(&mut self) -> SResult<u8> {
        let byte = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        self.backslash_x()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        b'\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        b'\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        b'\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        b'\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        b'\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        b'\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        b'"'
                    }
                    Some(b) => lex_fail!(self, "Unexpected byte {:?} after \\ character \
                                               in byte literal", b),
                    None => lex_fail!(self, "Unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(b) => {
                // NOTE: At this point, self.idx doesn't necessarially point to
                // a byte boundary, however, the below assertion will assert
                // that we are.
                self.idx += 1;
                b
            },
            None => lex_fail!(self, "Unexpected Eof while parsing byte literal")
        };

        assert!(self.byte(0) == b'\'');
        self.idx += 1;
        Ok(byte)
    }

    fn stringlike(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;

        let kind = match (self.byte(0), self.byte(1), self.byte(2)) {
            (b'b', b'\'', ..) => {
                self.idx += 2;
                TokenKind::Byte(self.byte_lit()?)
            }
            (b'b', b'"', ..) => {
                self.idx += 2;
                TokenKind::ByteString {
                    text: self.byte_str_lit()?,
                    raw_markers: None
                }
            }
            (b'\'', ..) => {
                self.idx += 1;
                if let Some(ch) = self.char_lit()? {
                    TokenKind::Char(ch)
                } else {
                    TokenKind::Lifetime(self.word()
                                        .expect("Expected identifier while parsing lifetime")
                                        .into())
                }
            }
            (b'"', ..) => {
                self.idx += 1;
                TokenKind::String {
                    text: self.str_lit()?,
                    raw_markers: None,
                }
            }
            (b'b', b'r', b'#') => {
                self.idx += 2;
                let (text, markers) = self.raw_str()?;
                TokenKind::ByteString {
                    text: text.as_bytes().into(),
                    raw_markers: Some(markers),
                }
            }
            (b'r', b'#', ..) => {
                self.idx += 1;
                let (text, markers) = self.raw_str()?;
                TokenKind::String {
                    text: text.into(),
                    raw_markers: Some(markers),
                }
            }
            _ => return Ok(None),
        };

        Ok(Some(TokenTree {
            kind: kind,
            span: self.span_from(start)
        }))
    }

    fn delim(&mut self) -> SResult<Option<TokenTree>> {
        let start = self.idx;
        let delim = match self.byte(0) {
            b'{' => {
                self.idx += 1;
                Delimiter::Brace
            }
            b'[' => {
                self.idx += 1;
                Delimiter::Bracket
            }
            b'(' => {
                self.idx += 1;
                Delimiter::Parenthesis
            }
            _ => return Ok(None),
        };

        let body = self.parse_stream()?;

        let close_delim = match self.byte(0) {
            b'}' => {
                self.idx += 1;
                Delimiter::Brace
            }
            b']' => {
                self.idx += 1;
                Delimiter::Bracket
            }
            b')' => {
                self.idx += 1;
                Delimiter::Parenthesis
            }
            _ => {
                // NOTE: For parse_stream to have returned with success, we are
                // either looking at one of the closing delimiters or Eof.
                assert!(self.idx == self.input.len());
                lex_fail!(self, "Expected closing delimiter");
            }
        };

        if close_delim != delim {
            lex_fail!(self, "Mismatched delimiters. Expected {} instead got {}",
                      delim.close_delim(), close_delim.close_delim())
        }

        Ok(Some(TokenTree {
            kind: TokenKind::Sequence(delim, Box::new(body)),
            span: self.span_from(start),
        }))
    }

    /// Get the next token from the lexer
    fn parse_stream(&mut self) -> SResult<TokenStream> {
        let mut tts = Vec::new();
        loop {
            self.skip_whitespace()?;

            tts.push(if let Some(tt) = self.comment()? {
                tt
            } else if let Some(tt) = self.symbol()? {
                tt
            } else if let Some(tt) = self.num()? {
                tt
            } else if let Some(tt) = self.stringlike()? {
                tt
            } else if let Some(tt) = self.ident()? {
                tt
            } else if let Some(tt) = self.delim()? {
                tt
            } else {
                let b = self.byte(0);
                if b == b'}' || b == b']' || b == b')' || self.idx >= self.input.len() {
                    break;
                }

                lex_fail!(self, "Unexpected character {:?} does not start any token",
                          self.next_char());
            });
        }

        Ok(TokenStream::from(tts))
    }

    fn parse_complete_stream(&mut self) -> Result<TokenStream, LexError> {
        match self.parse_stream() {
            Ok(v) => {
                if self.idx < self.input.len() {
                    lex_fail!(self, "Unexpected mismatched closing delimiter {:?}",
                              self.next_char());
                }
                Ok(v)
            }
            Err(e) => Err(e)
        }
    }
}

/// Check if the passed in character is whitespace as far as rustc would be
/// concerned. Rustc considers left-to-right mark and right-to-left mark as
/// whitespace.
fn is_whitespace(c: char) -> bool {
    c.is_whitespace() || c == '\u{200e}' || c == '\u{200f}'
}

/// Check if a given character is symbolic.
///
/// A character is considered symbolic if:
///  * It is not a legal start to another non-comment token
///  * It is not whitespace
///  * It is not a control character
///  * It is not a delimiter
///
/// XXX: What is the correct implementation of this?
fn is_symbolic(c: char) -> bool {
    !(is_whitespace(c) ||
      c == '"' || c == '\'' ||
      c == '{' || c == '}' ||
      c == '[' || c == ']' ||
      c == '(' || c == ')' ||
      UnicodeXID::is_xid_start(c) ||
      c.is_digit(10) ||
      c.is_control())
}

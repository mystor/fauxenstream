use std::ascii;
use std::str;
use std::fmt::{self, Display, Write};

use {TokenStream, TokenTree, TokenKind, OpJoin};

impl Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TokenKind::Sequence(del, ref ts) => {
                f.write_char(del.open_delim())?;
                if !ts.is_empty() {
                    f.write_char(' ')?;
                    Display::fmt(ts, f)?;
                }
                f.write_char(' ')?;
                f.write_char(del.close_delim())?;
            }
            TokenKind::Comment(_) => {
                // XXX: Implement
            }

            TokenKind::String { ref text, raw_markers: Some(raw_markers) } => {
                f.write_char('r')?;
                for _ in 0..raw_markers {
                    f.write_char('#')?;
                }
                f.write_char('"')?;
                Display::fmt(text, f)?;
                f.write_char('"')?;
                for _ in 0..raw_markers {
                    f.write_char('#')?;
                }
            }
            TokenKind::String { ref text, .. } => {
                f.write_char('"')?;
                for chr in text.chars() {
                    for escaped_chr in chr.escape_default() {
                        f.write_char(escaped_chr)?;
                    }
                }
                f.write_char('"')?;
            }

            TokenKind::ByteString { ref text, raw_markers: Some(raw_markers) } => {
                f.write_char('r')?;
                for _ in 0..raw_markers {
                    f.write_char('#')?;
                }
                f.write_char('"')?;
                let utf8 = str::from_utf8(text).map_err(|_| fmt::Error)?;
                Display::fmt(utf8, f)?;
                f.write_char('"')?;
                for _ in 0..raw_markers {
                    f.write_char('#')?;
                }
            }
            TokenKind::ByteString { ref text, .. } => {
                f.write_char('b')?;
                f.write_char('"')?;
                for byte in text.iter().cloned() {
                    for esc_ch in ascii::escape_default(byte) {
                        f.write_char(esc_ch as char)?;
                    }
                }
                f.write_char('"')?;
            }

            TokenKind::Char(ch) => {
                f.write_char('\'')?;
                for esc_ch in ch.escape_default() {
                    f.write_char(esc_ch)?;
                }
                f.write_char('\'')?;
            }

            TokenKind::Byte(byte) => {
                f.write_char('b')?;
                f.write_char('\'')?;
                for esc_ch in ascii::escape_default(byte) {
                    f.write_char(esc_ch as char)?;
                }
                f.write_char('\'')?;
            }

            TokenKind::Int { ref value, ref suffix } => {
                Display::fmt(value.value(), f)?;
                Display::fmt(suffix, f)?;
            }

            TokenKind::Float { ref value, ref suffix } => {
                Display::fmt(value.value(), f)?;
                Display::fmt(suffix, f)?;
            }

            TokenKind::Lifetime(ref name) => {
                f.write_char('\'')?;
                Display::fmt(name, f)?;
            }

            TokenKind::Word(ref name) => {
                Display::fmt(name, f)?;
            }

            TokenKind::Op(_, ch) => {
                f.write_char(ch)?;
            }
        }
        Ok(())
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.trees().peekable();
        while let Some(tt) = iter.next() {
            match tt.kind {
                TokenKind::Op(OpJoin::Join, c) => {
                    // As the op is marked as OpJoin::Join, don't add the
                    // following whitespace
                    f.write_char(c)?;
                }
                _ => {
                    Display::fmt(&tt, f)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }
            }
        }
        Ok(())
    }
}

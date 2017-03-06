//! This crate is intended as a temporary stable implementation of the
//! `proc_macro` TokenStream APIs.
//!
//! When `proc_macro` is stabilized, this implementation will likely be replaced
//! by re-exports of the `proc_macro` APIs.

extern crate unicode_xid;

extern crate memchr;

use std::fmt;
use std::error::Error;
use std::str::{self, FromStr};
use std::mem;

use util::RcSlice;

mod util;
mod display;
mod symbol;
mod codemap;
mod lex;

pub use symbol::{Symbol, ByteSymbol};
pub use codemap::{
    file_to_span,
    string_to_span,
    span_to_stream,
    span_to_line_col,
    span_to_name,
};

/// An extent of source code.
/// The implementation of this is private.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    lo: usize,
    hi: usize,
}

#[derive(Debug)]
pub struct LexError(String);

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.description(), f)
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTree {
    pub kind: TokenKind,
    pub span: Span,
}

impl TokenTree {
    pub fn eq_unspanned(&self, other: &TokenTree) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    /// A sequence of tokens delimited by `{}`, `()`, or `[]`
    Sequence(Delimiter, Box<TokenStream>),

    /// A comment. The text of the comment must be extracted from the `span`.
    Comment(CommentKind),

    /// A utf-8 string. raw_markers is Some(num) where num is the number of `#`
    /// characters used to make it a raw string.
    String {
        text: Symbol,
        raw_markers: Option<usize>,
    },

    /// A byte string. raw_markers is Some(num) where num is the number of `#`
    /// characters used to make it a raw string.
    ByteString {
        text: ByteSymbol,
        raw_markers: Option<usize>,
    },

    /// A unicode character literal, like `'a'`
    Char(char),

    /// A byte literal, like `b'a'`
    Byte(u8),

    /// An integer literal, like `82` or `0xf312u32`
    Int {
        value: IntVal,
        suffix: Symbol,
    },

    /// A float literal, like `37.5` or `5E32`
    Float {
        value: FloatVal,
        suffix: Symbol,
    },

    /// A lifetime
    Lifetime(Symbol),

    Word(Symbol),
    Op(OpJoin, char),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Delimiter {
    /// { }
    Brace,
    /// ( )
    Parenthesis,
    /// [ ]
    Bracket,
}

impl Delimiter {
    pub fn open_delim(&self) -> char {
        match *self {
            Delimiter::Brace => '{',
            Delimiter::Parenthesis => '(',
            Delimiter::Bracket => '[',
        }
    }

    pub fn close_delim(&self) -> char {
        match *self {
            Delimiter::Brace => '}',
            Delimiter::Parenthesis => ')',
            Delimiter::Bracket => ']',
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CommentKind {
    Regular,
    InnerDoc,
    OuterDoc,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpJoin {
    /// The op is followed by a non-op character.
    Alone,
    /// The op is followed by an op-character, and may fuse with it to produce a
    /// compound operator.
    Join,
}

/// An integer value
///
/// XXX: Should provide a method to extract the value as a u{8,16,32,64,size} if
/// it fits.
#[derive(Debug, Clone)]
pub struct IntVal {
    value: Symbol,
    computed: Option<u64>,
}

impl IntVal {
    pub fn new(v: u64) -> IntVal {
        IntVal {
            value: v.to_string().into(),
            computed: Some(v),
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        self.computed.and_then(|v| if v > std::u8::MAX as u64 {
            None
        } else {
            Some(v as u8)
        })
    }

    pub fn as_u16(&self) -> Option<u16> {
        self.computed.and_then(|v| if v > std::u16::MAX as u64 {
            None
        } else {
            Some(v as u16)
        })
    }

    pub fn as_u32(&self) -> Option<u32> {
        self.computed.and_then(|v| if v > std::u32::MAX as u64 {
            None
        } else {
            Some(v as u32)
        })
    }

    pub fn as_u64(&self) -> Option<u64> {
        self.computed
    }

    /// Get the raw uninterpreted string value of the integer.
    pub fn value(&self) -> &Symbol {
        &self.value
    }
}

impl Eq for IntVal {}
impl PartialEq for IntVal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

/// A floating point value
///
/// XXX: Should provide a method to extract the value as an f{16,32,64}.
#[derive(Debug, Clone)]
pub struct FloatVal {
    value: Symbol,
    computed: Option<f64>,
}

impl FloatVal {
    pub fn new(v: f64) -> FloatVal {
        FloatVal {
            value: v.to_string().into(),
            computed: Some(v),
        }
    }

    pub fn as_f32(&self) -> Option<f32> {
        self.computed.map(|v| v as f32)
    }

    pub fn as_f64(&self) -> Option<f64> {
        self.computed
    }

    /// Get the raw uninterpreted string value of the float
    pub fn value(&self) -> &Symbol {
        &self.value
    }
}

impl Eq for FloatVal {}
impl PartialEq for FloatVal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream {
    // XXX: Should we use a space optimization for short streams like
    // syntax::TokenStream?
    kind: TokenStreamKind,
}

#[derive(Debug, Clone)]
enum TokenStreamKind {
    Empty,
    // NOTE: The following RcSlices must never be empty
    Tree(TokenTree),
    Stream(RcSlice<TokenStream>),
}

impl TokenStream {
    pub fn empty() -> TokenStream {
        TokenStream { kind: TokenStreamKind::Empty }
    }

    pub fn is_empty(&self) -> bool {
        match self.kind {
            TokenStreamKind::Empty => true,
            _ => false,
        }
    }

    pub fn concat(mut streams: Vec<TokenStream>) -> TokenStream {
        streams.retain(|ts| !ts.is_empty());
        match streams.len() {
            0 => TokenStream::empty(),
            1 => streams.pop().unwrap(),
            _ => TokenStream { kind: TokenStreamKind::Stream(RcSlice::new(streams)) },
        }
    }

    pub fn trees(&self) -> Cursor {
        self.clone().into_trees()
    }

    pub fn into_trees(self) -> Cursor {
        Cursor::new(self)
    }

    /// Compares two TokenStreams, checking equality without regarding span information.
    pub fn eq_unspanned(&self, other: &TokenStream) -> bool {
        for (t1, t2) in self.trees().zip(other.trees()) {
            if !t1.eq_unspanned(&t2) {
                return false;
            }
        }
        true
    }
}

impl From<TokenTree> for TokenStream {
    fn from(tt: TokenTree) -> TokenStream {
        TokenStream {
            kind: TokenStreamKind::Tree(tt)
        }
    }
}

impl From<Vec<TokenTree>> for TokenStream {
    fn from(tts: Vec<TokenTree>) -> TokenStream {
        TokenStream::concat(tts.into_iter().map(TokenStream::from).collect())
    }
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
        let span = string_to_span("<proc-macro source code>".to_owned(),
                                  src.to_owned());
        span_to_stream(span)
    }
}

impl Eq for TokenStream {}
impl PartialEq for TokenStream {
    fn eq(&self, other: &Self) -> bool {
        for (t1, t2) in self.trees().zip(other.trees()) {
            if t1 != t2 {
                return false;
            }
        }
        true
    }
}

pub struct Cursor(CursorKind);

enum CursorKind {
    Empty,
    Tree(TokenTree, bool /* consumed? */),
    Stream(StreamCursor),
}

struct StreamCursor {
    stream: RcSlice<TokenStream>,
    index: usize,
    stack: Vec<(RcSlice<TokenStream>, usize)>,
}

impl Iterator for Cursor {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        let cursor = match self.0 {
            CursorKind::Stream(ref mut cursor) => cursor,
            CursorKind::Tree(ref tree, ref mut consumed @ false) => {
                *consumed = true;
                return Some(tree.clone());
            }
            _ => return None,
        };

        loop {
            if cursor.index < cursor.stream.len() {
                match cursor.stream[cursor.index].kind.clone() {
                    TokenStreamKind::Tree(tree) => {
                        cursor.index += 1;
                        return Some(tree);
                    }
                    TokenStreamKind::Stream(stream) => {
                        cursor.stack.push((mem::replace(&mut cursor.stream, stream),
                                           mem::replace(&mut cursor.index, 0) + 1));
                    }
                    TokenStreamKind::Empty => {
                        cursor.index += 1;
                    }
                }
            } else if let Some((stream, index)) = cursor.stack.pop() {
                cursor.stream = stream;
                cursor.index = index;
            } else {
                return None;
            }
        }
    }
}

impl Cursor {
    fn new(stream: TokenStream) -> Self {
        Cursor(match stream.kind {
            TokenStreamKind::Empty => CursorKind::Empty,
            TokenStreamKind::Tree(tree) => CursorKind::Tree(tree, false),
            TokenStreamKind::Stream(stream) => {
                CursorKind::Stream(StreamCursor { stream: stream, index: 0, stack: Vec::new() })
            }
        })
    }

    pub fn original_stream(self) -> TokenStream {
        match self.0 {
            CursorKind::Empty => TokenStream::empty(),
            CursorKind::Tree(tree, _) => tree.into(),
            CursorKind::Stream(cursor) => TokenStream{
                kind: TokenStreamKind::Stream({
                    cursor.stack.get(0).cloned().map(|(stream, _)| stream).unwrap_or(cursor.stream)
                })
            },
        }
    }

    pub fn look_ahead(&self, n: usize) -> Option<TokenTree> {
        fn look_ahead(streams: &[TokenStream], mut n: usize) -> Result<TokenTree, usize> {
            for stream in streams {
                n = match stream.kind {
                    TokenStreamKind::Tree(ref tree) if n == 0 => return Ok(tree.clone()),
                    TokenStreamKind::Tree(..) => n - 1,
                    TokenStreamKind::Stream(ref stream) => match look_ahead(stream, n) {
                        Ok(tree) => return Ok(tree),
                        Err(n) => n,
                    },
                    _ => n,
                };
            }

            Err(n)
        }

        match self.0 {
            CursorKind::Empty | CursorKind::Tree(_, true) => Err(n),
            CursorKind::Tree(ref tree, false) => look_ahead(&[tree.clone().into()], n),
            CursorKind::Stream(ref cursor) => {
                look_ahead(&cursor.stream[cursor.index ..], n).or_else(|mut n| {
                    for &(ref stream, index) in cursor.stack.iter().rev() {
                        n = match look_ahead(&stream[index..], n) {
                            Ok(tree) => return Ok(tree),
                            Err(n) => n,
                        }
                    }

                    Err(n)
                })
            }
        }.ok()
    }
}

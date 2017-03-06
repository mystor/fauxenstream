use std::io::{self, Read};
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::cell::RefCell;

use memchr::memchr;

use {Span, TokenStream, LexError};
use lex::lex_str;

const FILE_PADDING_BYTES: usize = 1;

/// Information regarding the on-disk location of a span of code.
/// This type is produced by `SourceMap::locinfo`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SourceLoc<'a> {
    pub path: &'a Path,
    pub line: usize,
    pub col: usize,
}

impl<'a> fmt::Display for SourceLoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.path.display(), self.line, self.col)
    }
}

struct FileInfo {
    span: Span,
    name: String,
    src: String,
    lines: Vec<usize>,
}

struct CodeMap {
    files: Vec<FileInfo>,
    offset: usize,
}

thread_local! {
    static CODEMAP: RefCell<CodeMap> = RefCell::new(CodeMap {
        files: Vec::new(),
        offset: 0,
    });
}

/// NOTE: This produces line and column. Line is 1-indexed, column is 0-indexed
fn offset_line_col(lines: &Vec<usize>, off: usize) -> (usize, usize) {
    match lines.binary_search(&off) {
        Ok(found) => (found + 1, 0),
        Err(idx) => (idx, off - lines[idx - 1]),
    }
}

fn lines_offsets(s: &[u8]) -> Vec<usize> {
    let mut lines = vec![0];
    let mut prev = 0;
    while let Some(len) = memchr(b'\n', &s[prev..]) {
        prev += len + 1;
        lines.push(prev);
    }
    lines
}

fn with_fileinfo<F, R>(span: Span, f: F) -> Result<R, LexError>
    where F: FnOnce(&FileInfo, Span, &str) -> Result<R, LexError> {
    CODEMAP.with(|cm| {
        let cm = cm.borrow();

        if span.lo > span.hi {
            return Err(LexError("invalid span object with negative length".to_owned()));
        }
        for fi in &cm.files {
            if span.lo >= fi.span.lo && span.lo <= fi.span.hi {
                if span.hi < fi.span.lo || span.hi > fi.span.hi {
                    return Err(LexError("span spans multiple input files".to_owned()));
                }

                // Get the local span and the source string
                let loc_span = Span {
                    lo: span.lo - fi.span.lo,
                    hi: span.hi - fi.span.lo,
                };
                let src = &fi.src[loc_span.lo..loc_span.hi];

                // Set the path
                return f(fi, loc_span, src);
            }
        }

        Err(LexError("span is not part of any input file".to_owned()))
    })
}

pub fn file_to_span<P: AsRef<Path>>(path: P) -> io::Result<Span> {
    let path = path.as_ref();
    let mut source = String::new();
    File::open(&path)?.read_to_string(&mut source)?;

    Ok(string_to_span(path.to_string_lossy().into_owned(), source))
}

pub fn string_to_span(name: String, code: String) -> Span {
    CODEMAP.with(|cm| {
        let mut cm = cm.borrow_mut();

        let offset = cm.offset;
        cm.offset += code.len() + FILE_PADDING_BYTES;

        let span = Span {
            lo: offset,
            hi: offset + code.len(),
        };

        // Register the read-in file in the SourceMap
        cm.files.push(FileInfo {
            span: span,
            name: name,
            lines: lines_offsets(code.as_bytes()),
            src: code,
        });

        span
    })
}

pub fn span_to_stream(span: Span) -> Result<TokenStream, LexError> {
    with_fileinfo(span, |_, _, text| lex_str(text, span.lo))
}

pub fn span_to_name(span: Span) -> Result<String, LexError> {
    with_fileinfo(span, |fi, _, _| Ok(fi.name.clone()))
}

pub fn span_to_line_col(span: Span) -> Result<(usize, usize), LexError> {
    with_fileinfo(span, |fi, loc_span, _| {
        Ok(offset_line_col(&fi.lines, loc_span.lo))
    })
}

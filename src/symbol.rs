use std::fmt;
use std::ops;

/// NOTE: It would be cool if we could avoid copies when unnecessary.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Symbol(Box<str>);

impl Symbol {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl ops::Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> From<&'a str> for Symbol {
    fn from(s: &'a str) -> Symbol {
        Symbol(s.to_owned().into_boxed_str())
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Symbol {
        Symbol(s.into_boxed_str())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Debug>::fmt(&self.0, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Display>::fmt(&self.0, f)
    }
}

/// NOTE: It would be cool if we could avoid copies when unnecessary.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ByteSymbol(Box<[u8]>);

impl ByteSymbol {
    pub fn as_bytes(&self) -> &[u8]{
        &self.0
    }
}

impl ops::Deref for ByteSymbol {
    type Target = [u8];
    fn deref(&self) -> &[u8]{
        self.as_bytes()
    }
}

impl<'a> From<&'a [u8]> for ByteSymbol {
    fn from(s: &'a [u8]) -> ByteSymbol {
        ByteSymbol(s.to_owned().into_boxed_slice())
    }
}

impl From<Vec<u8>> for ByteSymbol {
    fn from(s: Vec<u8>) -> ByteSymbol {
        ByteSymbol(s.into_boxed_slice())
    }
}

impl fmt::Debug for ByteSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <[u8] as fmt::Debug>::fmt(&self.0, f)
    }
}

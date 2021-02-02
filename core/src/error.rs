use std::fmt;
use std::io;

use crate::ast::Pos;

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    DecompileError(String),
    SyntaxError(String),
    CompileError(String, Pos),
    FunctionResolutionError(String, Pos),
    PoolError(String),
    FormatError(fmt::Error),
}

impl Error {
    pub fn eof(hint: String) -> Error {
        Error::IOError(io::Error::new(io::ErrorKind::UnexpectedEof, hint))
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IOError(err)
    }
}

impl From<fmt::Error> for Error {
    fn from(err: fmt::Error) -> Self {
        Error::FormatError(err)
    }
}

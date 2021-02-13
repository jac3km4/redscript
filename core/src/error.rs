use std::{fmt, io};

use crate::ast::Pos;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    DecompileError(String),
    SyntaxError(String),
    CompileError(String, Pos),
    FunctionResolutionError(String, Pos),
    PoolError(String),
    FormatError(fmt::Error),
}

impl Error {
    pub fn eof(hint: String) -> Error {
        Error::IoError(io::Error::new(io::ErrorKind::UnexpectedEof, hint))
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<fmt::Error> for Error {
    fn from(err: fmt::Error) -> Self {
        Error::FormatError(err)
    }
}

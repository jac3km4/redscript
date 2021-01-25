use std::fmt;
use std::io;

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    DecompileError(String),
    CompileError(String),
    FunctionResolutionError(String),
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

use std::fmt;
use std::io;

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    DecompileError(String),
    SyntaxError(String),
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

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IOError(err) => write!(fmt, "IOError: {}", &err.to_string()),
            Error::FormatError(fmt_err) => write!(fmt, "FormatError: {}", &fmt_err.to_string()),
            Error::DecompileError(err_str) => write!(fmt, "DecompileError: {}", err_str),
            Error::SyntaxError(err_str) => write!(fmt, "SyntaxError: {}", err_str),
            Error::CompileError(err_str) => write!(fmt, "CompileError: {}", err_str),
            Error::FunctionResolutionError(err_str) => write!(fmt, "FunctionResolutionError: {}", err_str),
            Error::PoolError(err_str) => write!(fmt, "PoolError: {}", err_str),
        }
    }
}

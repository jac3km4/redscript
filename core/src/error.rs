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

    pub fn method_not_found(fun_name: &str, scope: &str, pos: Pos) -> Error {
        let error = format!("Method {} not found on {}", fun_name, scope);
        Error::CompileError(error, pos)
    }

    pub fn global_not_found(fun_name: &str, pos: Pos) -> Error {
        let error = format!("Function {} not found", fun_name);
        Error::CompileError(error, pos)
    }

    pub fn class_not_found(class_name: &str, pos: Pos) -> Error {
        let error = format!("Can't find class {}", class_name);
        Error::CompileError(error, pos)
    }

    pub fn invalid_annotation_args(pos: Pos) -> Error {
        Error::CompileError("Invalid arguments for annotation".to_owned(), pos)
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

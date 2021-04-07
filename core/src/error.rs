use std::fmt::{self, Display};
use std::{io, usize};

use crate::ast::Pos;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    DecompileError(String),
    SyntaxError(String, Pos),
    CompileError(String, Pos),
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

    pub fn type_annotation_required(pos: Pos) -> Error {
        Error::CompileError("Type annotation required".to_owned(), pos)
    }

    pub fn type_error<F: Display, T: Display>(from: F, to: T, pos: Pos) -> Error {
        let error = format!("Can't coerce {} to {}", from, to);
        Error::CompileError(error, pos)
    }

    pub fn invalid_context<N: Display>(type_: N, pos: Pos) -> Error {
        let error = format!("{} doesn't have members", type_);
        Error::CompileError(error, pos)
    }

    pub fn invalid_op<N: Display>(type_: N, op: &str, pos: Pos) -> Error {
        let error = format!("{} is not supported on {}", op, type_);
        Error::CompileError(error, pos)
    }

    pub fn invalid_arg_count<N: Display>(name: N, expected: usize, pos: Pos) -> Error {
        let error = format!("Expected {} parameters for {}", expected, name);
        Error::CompileError(error, pos)
    }

    pub fn void_cannot_be_used(pos: Pos) -> Error {
        Error::CompileError("Void value cannot be used".to_owned(), pos)
    }

    pub fn return_type_mismatch<N: Display>(type_: N, pos: Pos) -> Error {
        let error = format!("Function should return {}", type_);
        Error::CompileError(error, pos)
    }

    pub fn no_matching_overload<N: Display>(name: N, errors: &[FunctionResolutionError], pos: Pos) -> Error {
        let error = format!(
            "Arguments passed to {} do not match any of the overloads:{}",
            name,
            errors.iter().fold(String::new(), |acc, str| acc + "\n" + &str.0)
        );
        Error::CompileError(error, pos)
    }

    pub fn invalid_intrinsic<N: Display, T: Display>(name: N, type_: T, pos: Pos) -> Error {
        let err = format!("Invalid intrinsic {} call: unexpected {}", name, type_);
        Error::CompileError(err, pos)
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

#[derive(Debug)]
pub struct FunctionResolutionError(String);

impl FunctionResolutionError {
    pub fn parameter_mismatch(cause: &str, index: usize) -> FunctionResolutionError {
        let message = format!("Invalid parameter at position {}: {}", index, cause);
        FunctionResolutionError(message)
    }

    pub fn return_mismatch<N: Display>(expected: N, given: N) -> FunctionResolutionError {
        let message = format!("Return type {} does not match expected {}", expected, given);
        FunctionResolutionError(message)
    }

    pub fn too_many_args(expected: usize) -> FunctionResolutionError {
        let error = format!("Too many arguments, expected {}", expected);
        FunctionResolutionError(error)
    }

    pub fn invalid_arg_count(received: usize, min: usize, max: usize) -> FunctionResolutionError {
        let message = format!("Expected {}-{} parameters, given {}", min, max, received);
        FunctionResolutionError(message)
    }
}

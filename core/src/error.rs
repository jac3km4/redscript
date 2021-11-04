use std::fmt::{self, Display};
use std::{io, usize};

use thiserror::Error;

use crate::ast::Span;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("formatter error")]
    FormatError(#[from] fmt::Error),
    #[error("decompilation error: {0}")]
    DecompileError(String),
    #[error("syntax error: {0}")]
    SyntaxError(String, Span),
    #[error("compilation error: {0}")]
    CompileError(String, Span),
    #[error("type error: {0}")]
    TypeError(String, Span),
    #[error("function resolution error: {0}")]
    ResolutionError(String, Span),
    #[error("constant pool error: {0}")]
    PoolError(String),
    #[error("multiple errors")]
    MultipleErrors(Vec<Span>),
    #[error("compile-time eval error: {0}")]
    CteError(String, Span),
}

impl Error {
    pub fn eof(hint: String) -> Error {
        Error::IoError(io::Error::new(io::ErrorKind::UnexpectedEof, hint))
    }

    pub fn function_not_found<F: Display>(fun_name: F, pos: Span) -> Error {
        let error = format!("Function {} not found", fun_name);
        Error::CompileError(error, pos)
    }

    pub fn member_not_found<M: Display, C: Display>(member: M, context: C, pos: Span) -> Error {
        let error = format!("Member {} not found on {}", member, context);
        Error::CompileError(error, pos)
    }

    pub fn class_not_found<N: Display>(class_name: N, pos: Span) -> Error {
        let error = format!("Can't find class {}", class_name);
        Error::CompileError(error, pos)
    }

    pub fn class_is_abstract<N: Display>(class_name: N, pos: Span) -> Error {
        let error = format!("Cannot instantiate abstract class {}", class_name);
        Error::CompileError(error, pos)
    }

    pub fn unresolved_reference<N: Display>(name: N, pos: Span) -> Error {
        let error = format!("Unresolved reference {}", name);
        Error::CompileError(error, pos)
    }

    pub fn unresolved_type<N: Display>(name: N, pos: Span) -> Error {
        let error = format!("Unresolved type {}", name);
        Error::CompileError(error, pos)
    }

    pub fn unresolved_import<N: Display>(import: N, pos: Span) -> Error {
        Error::CompileError(format!("Unresolved import {}", import), pos)
    }

    pub fn unresolved_module<N: Display>(import: N, pos: Span) -> Error {
        Error::CompileError(format!("Module {} has no members or does not exist", import), pos)
    }

    pub fn invalid_annotation_args(pos: Span) -> Error {
        Error::CompileError("Invalid arguments for annotation".to_owned(), pos)
    }

    pub fn type_annotation_required(pos: Span) -> Error {
        Error::CompileError("Type annotation required".to_owned(), pos)
    }

    pub fn invalid_context<N: Display>(type_: N, pos: Span) -> Error {
        let error = format!("{} doesn't have members", type_);
        Error::CompileError(error, pos)
    }

    pub fn invalid_op<N: Display>(type_: N, op: &str, pos: Span) -> Error {
        let error = format!("{} is not supported on {}", op, type_);
        Error::CompileError(error, pos)
    }

    pub fn invalid_arg_count<N: Display>(name: N, expected: usize, pos: Span) -> Error {
        let error = format!("Expected {} parameters for {}", expected, name);
        Error::CompileError(error, pos)
    }

    pub fn void_cannot_be_used(pos: Span) -> Error {
        Error::CompileError("Void value cannot be used".to_owned(), pos)
    }

    pub fn value_expected<N: Display>(found: N, pos: Span) -> Error {
        Error::CompileError(format!("Expected a value, found {}", found), pos)
    }

    pub fn return_type_mismatch<N: Display>(type_: N, pos: Span) -> Error {
        let error = format!("Function should return {}", type_);
        Error::CompileError(error, pos)
    }

    pub fn type_error<F: Display, T: Display>(from: F, to: T, pos: Span) -> Error {
        let error = format!("Can't coerce {} to {}", from, to);
        Error::TypeError(error, pos)
    }

    pub fn no_matching_overload<N: Display>(name: N, errors: &[FunctionResolutionError], pos: Span) -> Error {
        let max_errors = 10;
        let messages = errors
            .iter()
            .take(max_errors)
            .fold(String::new(), |acc, str| acc + "\n " + &str.0);

        let detail = if errors.len() > max_errors {
            format!("{}\n...and more", messages)
        } else {
            messages
        };
        let error = format!(
            "Arguments passed to {} do not match any of the overloads:{}",
            name, detail
        );
        Error::ResolutionError(error, pos)
    }

    pub fn invalid_intrinsic<N: Display, T: Display>(name: N, type_: T, pos: Span) -> Error {
        let err = format!("Invalid intrinsic {} call: unexpected {}", name, type_);
        Error::CompileError(err, pos)
    }

    pub fn expected_static_method<N: Display>(name: N, pos: Span) -> Error {
        let err = format!("Method {} is not static", name);
        Error::CompileError(err, pos)
    }

    pub fn expected_non_static_method<N: Display>(name: N, pos: Span) -> Error {
        let err = format!("Method {} is static", name);
        Error::CompileError(err, pos)
    }

    pub fn no_this_in_static_context(pos: Span) -> Error {
        Error::CompileError("No 'this' in static context".to_owned(), pos)
    }

    pub fn unsupported<N: Display>(name: N, pos: Span) -> Error {
        let err = format!("{} is unsupported", name);
        Error::CompileError(err, pos)
    }

    pub fn class_redefinition(pos: Span) -> Error {
        let err = "Class with this name is already defined elsewhere".to_owned();
        Error::CompileError(err, pos)
    }

    pub fn expected_body(pos: Span) -> Error {
        let err = "This function must have a body".to_owned();
        Error::CompileError(err, pos)
    }

    pub fn native_with_body(pos: Span) -> Error {
        let err = "Native function cannot have a body".to_owned();
        Error::CompileError(err, pos)
    }

    pub fn unexpected_native(pos: Span) -> Error {
        let err = "Native member is not allowed on a non-native class".to_owned();
        Error::CompileError(err, pos)
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
        let message = format!("Return type {} does not match expected {}", given, expected);
        FunctionResolutionError(message)
    }

    pub fn too_many_args(expected: usize, got: usize) -> FunctionResolutionError {
        let error = format!("Too many arguments, expected {} but got {}", expected, got);
        FunctionResolutionError(error)
    }

    pub fn invalid_arg_count(received: usize, min: usize, max: usize) -> FunctionResolutionError {
        let message = format!("Expected {}-{} parameters, given {}", min, max, received);
        FunctionResolutionError(message)
    }
}

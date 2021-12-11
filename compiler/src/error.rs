use std::fmt::Display;
use std::{io, usize};

use itertools::Itertools;
use redscript::ast::Span;
use redscript::bundle::PoolError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("formatter error {0}")]
    SyntaxError(String, Span),
    #[error("compilation error: {0}")]
    CompileError(String, Span),
    #[error("function argument error: {0}")]
    ArgumentError(String, Span),
    #[error("function resolution error: {0}")]
    ResolutionError(String, Span),
    #[error("constant pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("multiple errors")]
    MultipleErrors(Vec<Span>),
    #[error("compile-time eval error: {0}")]
    CteError(String, Span),
}

impl Error {
    pub fn arg_type_error<F: Display, T: Display>(from: F, to: T, span: Span) -> Error {
        Error::ArgumentError(Cause::type_error(from, to).0, span)
    }

    pub fn no_matching_overload<N: Display>(name: N, errors: &[FunctionMatchError], span: Span) -> Error {
        let max_errors = 10;
        let messages = errors.iter().take(max_errors).join("\n ");

        let detail = if errors.len() > max_errors {
            format!("{}\n...and more", messages)
        } else {
            messages
        };
        let error = format!(
            "Arguments passed to {} do not match any of the overloads:\n {}",
            name, detail
        );
        Error::ResolutionError(error, span)
    }

    pub fn too_many_matching_overloads<N: Display>(name: N, span: Span) -> Error {
        let error = format!("Arguments passed to {} satisfy more than one overload", name);
        Error::ResolutionError(error, span)
    }
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct Cause(pub String);

impl From<PoolError> for Cause {
    #[inline]
    fn from(err: PoolError) -> Self {
        Cause(err.0)
    }
}

impl Cause {
    #[inline]
    pub fn with_span(self, span: Span) -> Error {
        Error::CompileError(self.0, span)
    }

    #[inline]
    pub fn pool_err(self) -> Error {
        Error::PoolError(PoolError(self.0))
    }

    pub fn type_error<F: Display, T: Display>(from: F, to: T) -> Cause {
        let error = format!("Can't coerce {} to {}", from, to);
        Cause(error)
    }

    pub fn function_not_found<F: Display>(fun_name: F) -> Cause {
        let error = format!("Function {} not found", fun_name);
        Cause(error)
    }

    pub fn member_not_found<M: Display, C: Display>(member: M, context: C) -> Cause {
        let error = format!("Member {} not found on {}", member, context);
        Cause(error)
    }

    pub fn class_not_found<N: Display>(class_name: N) -> Cause {
        let error = format!("Can't find class {}", class_name);
        Cause(error)
    }

    pub fn class_is_abstract<N: Display>(class_name: N) -> Cause {
        let error = format!("Cannot instantiate abstract class {}", class_name);
        Cause(error)
    }

    pub fn unresolved_reference<N: Display>(name: N) -> Cause {
        let error = format!("Unresolved reference {}", name);
        Cause(error)
    }

    pub fn unresolved_type<N: Display>(name: N) -> Cause {
        let error = format!("Unresolved type {}", name);
        Cause(error)
    }

    pub fn unresolved_import<N: Display>(import: N) -> Cause {
        Cause(format!("Unresolved import {}", import))
    }

    pub fn unresolved_module<N: Display>(import: N) -> Cause {
        Cause(format!("Module {} has no members or does not exist", import))
    }

    pub fn invalid_annotation_args() -> Cause {
        Cause("Invalid arguments for annotation".to_owned())
    }

    pub fn type_annotation_required() -> Cause {
        Cause("Type annotation required".to_owned())
    }

    pub fn invalid_context<N: Display>(type_: N) -> Cause {
        let error = format!("{} doesn't have members", type_);
        Cause(error)
    }

    pub fn invalid_op<N: Display>(type_: N, op: &str) -> Cause {
        let error = format!("{} is not supported on {}", op, type_);
        Cause(error)
    }

    pub fn invalid_arg_count<N: Display>(name: N, expected: usize) -> Cause {
        let error = format!("Expected {} parameters for {}", expected, name);
        Cause(error)
    }

    pub fn void_cannot_be_used() -> Cause {
        Cause("Void value cannot be used".to_owned())
    }

    pub fn value_expected<N: Display>(found: N) -> Cause {
        Cause(format!("Expected a value, found {}", found))
    }

    pub fn return_type_mismatch<N: Display>(type_: N) -> Cause {
        let error = format!("Function should return {}", type_);
        Cause(error)
    }

    pub fn invalid_intrinsic<N: Display, T: Display>(name: N, type_: T) -> Cause {
        let err = format!("Invalid intrinsic {} call: unexpected {}", name, type_);
        Cause(err)
    }

    pub fn expected_static_method<N: Display>(name: N) -> Cause {
        let err = format!("Method {} is not static", name);
        Cause(err)
    }

    pub fn expected_non_static_method<N: Display>(name: N) -> Cause {
        let err = format!("Method {} is static", name);
        Cause(err)
    }

    pub fn no_this_in_static_context() -> Cause {
        Cause("No 'this' in static context".to_owned())
    }

    pub fn unsupported<N: Display>(name: N) -> Cause {
        let err = format!("{} is unsupported", name);
        Cause(err)
    }

    pub fn class_redefinition() -> Cause {
        let err = "Class with this name is already defined elsewhere".to_owned();
        Cause(err)
    }

    pub fn expected_body() -> Cause {
        let err = "This function must have a body".to_owned();
        Cause(err)
    }

    pub fn native_with_body() -> Cause {
        let err = "Native function cannot have a body".to_owned();
        Cause(err)
    }

    pub fn unexpected_native() -> Cause {
        let err = "Native member is not allowed on a non-native class".to_owned();
        Cause(err)
    }

    pub fn unification_failed<A: Display, B: Display>(a: A, b: B) -> Cause {
        let err = format!("Cannot unify {} and {}", a, b);
        Cause(err)
    }
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct FunctionMatchError(String);

impl FunctionMatchError {
    pub fn parameter_mismatch<C: Display>(cause: C, index: usize) -> FunctionMatchError {
        let message = format!("Parameter at position {}: {}", index + 1, cause);
        FunctionMatchError(message)
    }

    pub fn return_mismatch<N: Display>(expected: N, given: N) -> FunctionMatchError {
        let message = format!("Return type {} does not match expected {}", given, expected);
        FunctionMatchError(message)
    }

    pub fn invalid_arg_count(received: usize, min: usize, max: usize) -> FunctionMatchError {
        let message = format!("Expected {}-{} parameters, given {}", min, max, received);
        FunctionMatchError(message)
    }
}

pub trait ResultSpan {
    type Output;

    fn with_span(self, span: Span) -> Self::Output;
}

impl<A> ResultSpan for std::result::Result<A, Cause> {
    type Output = std::result::Result<A, Error>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Output {
        self.map_err(|err| err.with_span(span))
    }
}

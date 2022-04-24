use std::fmt::{Display, Write};
use std::{io, usize};

use itertools::Itertools;
use peg::error::ExpectedSet;
use redscript::ast::{Ident, Span};
use redscript::bundle::PoolError;
use redscript::bytecode::IntrinsicOp;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("syntax error, expected {0}")]
    SyntaxError(ExpectedSet, Span),
    #[error("compilation error: {0}")]
    CompileError(Cause, Span),
    #[error("function resolution error: {0}")]
    ResolutionError(String, Span),
    #[error("constant pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("multiple errors")]
    MultipleErrors(Vec<Span>),
    #[error("compile-time eval error: {0}")]
    CteError(&'static str, Span),
}

impl Error {
    pub fn no_matching_overload<N: Display>(name: N, errors: &[FunctionMatchError], span: Span) -> Error {
        let max_errors = 10;
        let messages = errors.iter().take(max_errors).join("\n");

        let detail = if errors.len() > max_errors {
            format!("{messages}\n...and more")
        } else {
            messages
        };
        let error = format!("arguments passed to {name} do not match any of the overloads:\n{detail}",);
        Error::ResolutionError(error, span)
    }

    pub fn too_many_matching_overloads<N: Display>(name: N, span: Span) -> Error {
        let error = format!("arguments passed to {name} satisfy more than one overload");
        Error::ResolutionError(error, span)
    }
}

#[derive(Debug, Error)]
pub enum Cause {
    #[error("pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("can't coerce {0} into {1}")]
    TypeError(Ident, Ident),
    #[error("function {0} not found")]
    FunctionNotFound(Ident),
    #[error("member {0} not found on {1}")]
    MemberNotFound(Ident, Ident),
    #[error("class {0} not found")]
    ClassNotFound(Ident),
    #[error("cannot instantiate {0} because it's abstract")]
    InstantiatingAbstract(Ident),
    #[error("unresolved reference {0}")]
    UnresolvedReference(Ident),
    #[error("unresolved type {0}")]
    UnresolvedType(Ident),
    #[error("unresolved import {0}")]
    UnresolvedImport(Ident),
    #[error("module {0} has no members or does not exist")]
    UnresolvedModule(Ident),
    #[error("invalid arguments for annotation")]
    InvalidAnnotationArgs,
    #[error("type cannot be inferred here, try annotating the variable")]
    TypeAnnotationRequired,
    #[error("{0} has no members")]
    InvalidMemberAccess(Ident),
    #[error("{0} is not supported on {1}")]
    UnsupportedOperation(&'static str, Ident),
    #[error("expected {1} arguments for {0}")]
    InvalidArgCount(Ident, usize),
    #[error("void cannot be used as a value")]
    VoidCannotBeUsed,
    #[error("expected a value, found a {0}")]
    UnexpectedToken(&'static str),
    #[error("function should return {0}")]
    UnexpectedVoidReturn(Ident),
    #[error("function cannot return a value")]
    UnexpectedValueReturn,
    #[error("invalid use of {0}, unexpected {1}")]
    InvalidIntrinsicUse(IntrinsicOp, Ident),
    #[error("method {0} is static")]
    InvalidStaticMethodCall(Ident),
    #[error("method {0} is not static")]
    InvalidNonStaticMethodCall(Ident),
    #[error("no 'this' available in a static context")]
    UnexpectedThis,
    #[error("{0} is not supported")]
    UnsupportedFeature(&'static str),
    #[error("symbol with this name is already defined")]
    SymbolRedefinition,
    #[error("this function must have a body")]
    MissingBody,
    #[error("this function can't have have a body")]
    UnexpectedBody,
    #[error("native member is not allowed in a non-native context")]
    UnexpectedNative,
    #[error("cannot unify {0} and {1}")]
    UnificationFailed(Ident, Ident),
    #[error("{0} cannot be made persistent")]
    UnsupportedPersistent(Ident),
}

impl Cause {
    #[inline]
    pub fn with_span(self, span: Span) -> Error {
        Error::CompileError(self, span)
    }
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct FunctionMatchError(String);

impl FunctionMatchError {
    pub fn parameter_mismatch<C: Display>(cause: C, index: usize) -> FunctionMatchError {
        let mut output = String::new();
        match index {
            0 => write!(output, "1st").unwrap(),
            1 => write!(output, "2nd").unwrap(),
            2 => write!(output, "3rd").unwrap(),
            n => write!(output, "{}th", n + 1).unwrap(),
        };
        write!(output, " argument: {cause}").unwrap();

        FunctionMatchError(output)
    }

    pub fn return_mismatch<N: Display>(expected: N, given: N) -> FunctionMatchError {
        let message = format!("return type {given} does not match expected {expected}");
        FunctionMatchError(message)
    }

    pub fn invalid_arg_count(received: usize, min: usize, max: usize) -> FunctionMatchError {
        let message = format!("expected {min}-{max} arguments, given {received}");
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

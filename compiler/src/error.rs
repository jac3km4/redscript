use std::fmt::Display;
use std::{io, usize};

use itertools::Itertools;
use peg::error::ExpectedSet;
use redscript::ast::{Ident, Span};
use redscript::bundle::PoolError;
use redscript::bytecode::IntrinsicOp;
use thiserror::Error;

const MAX_RESOLUTION_ERRORS: usize = 6;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("syntax error, expected {0}")]
    SyntaxError(ExpectedSet, Span),
    #[error("compilation error: {0}")]
    CompileError(Cause, Span),
    #[error("constant pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("multiple errors")]
    MultipleErrors(Vec<Span>),
    #[error("compile-time eval error: {0}")]
    CteError(&'static str, Span),
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
    #[error("field with this name is already defined")]
    FieldRedefinition,
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
    #[error(r#"this value must be a constant (e.g. 1, "string")"#)]
    InvalidConstant,
    #[error(
        "arguments passed to {0} do not match any of the overloads:\n{}{}",
        .1.iter().take(MAX_RESOLUTION_ERRORS).format("\n"),
        if .1.len() > MAX_RESOLUTION_ERRORS {"\n...and more"} else {""}
    )]
    NoMatchingOverload(Ident, Box<[FunctionMatchError]>),
}

impl Cause {
    #[inline]
    pub fn with_span(self, span: Span) -> Error {
        Error::CompileError(self, span)
    }
}

#[derive(Debug, Error)]
pub enum FunctionMatchError {
    #[error("{} argument: expected {expected}, given {given}", NthArg(*index))]
    ParameterMismatch {
        given: Ident,
        expected: Ident,
        index: usize,
    },
    #[error("return type {expected} does not match {given}")]
    ReturnMismatch { given: Ident, expected: Ident },
    #[error("expected {min}-{max} arguments, given {given}")]
    ArgumentCountMismatch { given: usize, min: usize, max: usize },
}

struct NthArg(usize);

impl Display for NthArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "1st"),
            1 => write!(f, "2nd"),
            2 => write!(f, "3rd"),
            n => write!(f, "{}th", n + 1),
        }
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

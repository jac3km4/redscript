use redscript::ast::{Seq, Span};
use redscript::bundle::PoolIndex;
use redscript::definition::{Function, FunctionFlags};

use crate::error::Error;
use crate::typechecker::TypedAst;

pub mod return_val;
pub mod unused;

#[derive(Debug, PartialEq, Eq)]
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Span),
    Deprecation(String, Span),
    UnusedLocal(Span),
    MissingReturn(Span),
    CompileError(String, Span),
}

impl Diagnostic {
    pub fn from_error(error: Error) -> Result<Diagnostic, Error> {
        match error {
            Error::SyntaxError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::CompileError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::ResolutionError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::CteError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            other => Err(other),
        }
    }

    pub fn is_fatal(&self) -> bool {
        match self {
            Diagnostic::MethodConflict(_, _) => false,
            Diagnostic::Deprecation(_, _) => false,
            Diagnostic::UnusedLocal(_) => false,
            Diagnostic::MissingReturn(_) => false,
            Diagnostic::CompileError(_, _) => true,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Diagnostic::MethodConflict(_, span) => *span,
            Diagnostic::Deprecation(_, span) => *span,
            Diagnostic::UnusedLocal(span) => *span,
            Diagnostic::MissingReturn(span) => *span,
            Diagnostic::CompileError(_, span) => *span,
        }
    }

    pub fn unrelated_type_equals(span: Span) -> Self {
        let msg = "Comparing unrelated types, this is will not be allowed in the future".to_owned();
        Self::Deprecation(msg, span)
    }
}

pub trait DiagnosticPass {
    fn diagnose(&self, body: &Seq<TypedAst>, metadata: &FunctionMetadata) -> Vec<Diagnostic>;
}

pub struct FunctionMetadata {
    flags: FunctionFlags,
    was_callback: bool,
    span: Span,
}

impl FunctionMetadata {
    pub fn new(flags: FunctionFlags, was_callback: bool, span: Span) -> Self {
        Self {
            flags,
            was_callback,
            span,
        }
    }
}

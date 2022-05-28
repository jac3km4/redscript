use std::fmt;

use peg::error::ExpectedSet;
use redscript::ast::{Seq, Span};
use redscript::bundle::PoolIndex;
use redscript::definition::{Function, FunctionFlags};

use crate::error::{Cause, Error};
use crate::source_map::Files;
use crate::typechecker::TypedAst;

pub mod return_val;
pub mod unused;

#[derive(Debug)]
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Span),
    FieldConflict(Span),
    Deprecation(Deprecation, Span),
    UnusedLocal(Span),
    MissingReturn(Span),
    SyntaxError(ExpectedSet, Span),
    CompileError(Cause, Span),
    CteError(&'static str, Span),
    ResolutionError(String, Span),
}

impl Diagnostic {
    pub fn log(&self, files: &Files) {
        let mut output = String::new();
        self.display(files, &mut output).expect("Format failed");

        if self.is_fatal() {
            log::error!("{}", output);
        } else {
            log::warn!("{}", output);
        }
    }

    pub fn display<W: fmt::Write>(&self, files: &Files, out: &mut W) -> fmt::Result {
        let loc = files.lookup(self.span()).expect("Unknown file");
        let line = loc.enclosing_line().trim_end().replace('\t', " ");

        let padding = " ".repeat(loc.start.col);
        let underline_len = if loc.start.line == loc.end.line {
            (loc.end.col - loc.start.col).max(1)
        } else {
            3
        };
        let underline = "^".repeat(underline_len);

        writeln!(out, "At {loc}:")?;
        writeln!(out, "{line}")?;
        writeln!(out, "{padding}{underline}")?;
        writeln!(out, "{self}")
    }

    pub fn from_error(error: Error) -> Result<Diagnostic, Error> {
        match error {
            Error::SyntaxError(set, pos) => Ok(Diagnostic::SyntaxError(set, pos)),
            Error::CompileError(cause, pos) => Ok(Diagnostic::CompileError(cause, pos)),
            Error::ResolutionError(msg, pos) => Ok(Diagnostic::ResolutionError(msg, pos)),
            Error::CteError(msg, pos) => Ok(Diagnostic::CteError(msg, pos)),
            other => Err(other),
        }
    }

    #[inline]
    pub fn is_fatal(&self) -> bool {
        !matches!(
            self,
            Diagnostic::MethodConflict(_, _)
                | Diagnostic::FieldConflict(_)
                | Diagnostic::Deprecation(_, _)
                | Diagnostic::UnusedLocal(_)
                | Diagnostic::MissingReturn(_)
        )
    }

    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Diagnostic::MethodConflict(_, span) => *span,
            Diagnostic::FieldConflict(span) => *span,
            Diagnostic::Deprecation(_, span) => *span,
            Diagnostic::UnusedLocal(span) => *span,
            Diagnostic::MissingReturn(span) => *span,
            Diagnostic::CompileError(_, span) => *span,
            Diagnostic::SyntaxError(_, span) => *span,
            Diagnostic::CteError(_, span) => *span,
            Diagnostic::ResolutionError(_, span) => *span,
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Diagnostic::MethodConflict(_, _) => {
                f.write_str("this replacement overwrites a previous replacement on the same method")
            }
            Diagnostic::FieldConflict(_) => {
                f.write_str("field with this name is already defined in the class, this will have no effect")
            }
            Diagnostic::Deprecation(msg, _) => f.write_fmt(format_args!("{msg}")),
            Diagnostic::UnusedLocal(_) => f.write_str("unused variable"),
            Diagnostic::MissingReturn(_) => f.write_str("function might not return a value"),
            Diagnostic::SyntaxError(set, _) => f.write_fmt(format_args!("syntax error, expected {set}")),
            Diagnostic::CompileError(cause, _) => f.write_fmt(format_args!("{cause}")),
            Diagnostic::CteError(msg, _) => f.write_fmt(format_args!("compile-time expression error: {msg}")),
            Diagnostic::ResolutionError(msg, _) => f.write_fmt(format_args!("{msg}")),
        }
    }
}

#[derive(Debug)]
pub enum Deprecation {
    UnrelatedTypeEquals,
}

impl fmt::Display for Deprecation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Deprecation::UnrelatedTypeEquals => {
                f.write_str("comparing unrelated types, this is will not be allowed in the future")
            }
        }
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

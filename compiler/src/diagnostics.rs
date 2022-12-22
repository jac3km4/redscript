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

        if let Self::CompileError(cause, _) = self {
            write!(out, "[{}] ", cause.code())?;
        }

        writeln!(out, "At {loc}:",)?;
        writeln!(out, "{line}")?;
        writeln!(out, "{padding}{underline}")?;
        writeln!(out, "{self}")
    }

    pub fn from_error(error: Error) -> Result<Self, Error> {
        match error {
            Error::SyntaxError(set, pos) => Ok(Self::SyntaxError(set, pos)),
            Error::CompileError(cause, pos) => Ok(Self::CompileError(cause, pos)),
            Error::CteError(msg, pos) => Ok(Self::CteError(msg, pos)),
            other => Err(other),
        }
    }

    #[inline]
    pub fn is_fatal(&self) -> bool {
        !matches!(
            self,
            Self::MethodConflict(_, _)
                | Self::FieldConflict(_)
                | Self::Deprecation(_, _)
                | Self::UnusedLocal(_)
                | Self::MissingReturn(_)
        )
    }

    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Self::MethodConflict(_, span)
            | Self::FieldConflict(span)
            | Self::Deprecation(_, span)
            | Self::UnusedLocal(span)
            | Self::MissingReturn(span)
            | Self::CompileError(_, span)
            | Self::SyntaxError(_, span)
            | Self::CteError(_, span) => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::CompileError(cause, _) => cause.code(),
            _ => "OTHER",
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MethodConflict(_, _) => {
                f.write_str("this replacement overwrites a previous replacement of the same method")
            }
            Self::FieldConflict(_) => {
                f.write_str("field with this name is already defined in the class, this will have no effect")
            }
            Self::Deprecation(msg, _) => f.write_fmt(format_args!("{msg}")),
            Self::UnusedLocal(_) => f.write_str("unused variable"),
            Self::MissingReturn(_) => f.write_str("function might not return a value"),
            Self::SyntaxError(set, _) => f.write_fmt(format_args!("syntax error, expected {set}")),
            Self::CompileError(cause, _) => f.write_fmt(format_args!("{cause}")),
            Self::CteError(msg, _) => f.write_fmt(format_args!("compile-time expression error: {msg}")),
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
            Self::UnrelatedTypeEquals => {
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

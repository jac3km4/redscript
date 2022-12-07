use std::fmt;

use itertools::Itertools;
use peg::error::ExpectedSet;
use redscript::ast::Span;
use redscript::{str_fmt, Str};
use thiserror::Error;
use yansi::Paint;

use crate::source_map::{Files, SourceLoc};
use crate::type_repo::{OverloadEntry, TypeId};
use crate::typer::{Data, Mono};

pub type CompileResult<'id, A, E = CompileError<'id>> = Result<A, E>;

#[derive(Debug, Error)]
pub enum CompileError<'id> {
    #[error("{0}")]
    TypeError(TypeError<'id>, Span),
    #[error("{0} is not defined")]
    UnresolvedVar(Str, Span),
    #[error("member {1} not found in {0}")]
    UnresolvedMember(TypeId<'id>, Str, Span),
    #[error("function {0} not found")]
    UnresolvedFunction(Str, Span),
    #[error("method {0} not found")]
    UnresolvedMethod(Str, Span),
    #[error("no matching overload for {0}, available options are:\n{}", .1.iter().format("\n"))]
    NoMatchingOverload(Str, Box<[OverloadOption]>, Span),
    #[error("more than one matching overload for {0}, available options are:\n{}", .1.iter().format("\n"))]
    ManyMatchingOverloads(Str, Box<[OverloadOption]>, Span),
    #[error("insufficient type information available for member lookup")]
    CannotLookupMember(Span),
    #[error("import {} could not be resolved", .0.iter().format("."))]
    UnresolvedImport(Box<[Str]>, Span),
    #[error("virtual method {0} must be implemented")]
    UnimplementedMethod(Str, Span),
    #[error("{0} is not supported")]
    Unsupported(Unsupported, Span),
}

impl<'id> CompileError<'id> {
    pub fn span(&self) -> Span {
        match self {
            Self::TypeError(_, span)
            | Self::UnresolvedVar(_, span)
            | Self::UnresolvedMember(_, _, span)
            | Self::UnresolvedFunction(_, span)
            | Self::UnresolvedMethod(_, span)
            | Self::NoMatchingOverload(_, _, span)
            | Self::ManyMatchingOverloads(_, _, span)
            | Self::CannotLookupMember(span)
            | Self::UnresolvedImport(_, span)
            | Self::UnimplementedMethod(_, span)
            | Self::Unsupported(_, span) => *span,
        }
    }

    pub fn display(self, files: &Files) -> DisplayError<'_, 'id> {
        let location = files.lookup(self.span()).expect("Unknown file");
        DisplayError { location, error: self }
    }

    pub(crate) fn for_overloads<'a>(
        name: Str,
        matches: usize,
        candidates: impl IntoIterator<Item = &'a OverloadEntry<'a, 'id>>,
        span: Span,
    ) -> Self
    where
        'id: 'a,
    {
        let mut options = vec![];
        for candidate in candidates.into_iter().take(8) {
            let option = str_fmt!(
                "({})",
                candidate.function.typ.params.iter().map(|p| &p.typ).format(", ")
            );
            options.push(OverloadOption(option));
        }
        match (matches, options.len()) {
            (_, 0) => Self::UnresolvedMethod(name, span),
            (0, _) => Self::NoMatchingOverload(name, options.into(), span),
            _ => Self::ManyMatchingOverloads(name, options.into(), span),
        }
    }
}

#[derive(Debug)]
pub struct OverloadOption(Str);

impl fmt::Display for OverloadOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Error)]
pub enum TypeError<'id> {
    #[error("found {0}, expected {1}")]
    Mismatch(Mono<'id>, Mono<'id>),
    #[error("{0} does not satisfy {1} subtype constraint")]
    Unsatisfied(Data<'id>, Data<'id>),
    #[error("cannot unify {0} and {1}")]
    CannotUnify(Mono<'id>, Mono<'id>),
    #[error("type {0} not found")]
    UnresolvedType(Str),
    #[error("expected {1} type arguments")]
    InvalidNumberOfTypeArgs(usize, usize),
}

#[derive(Debug)]
pub struct DisplayError<'file, 'id> {
    location: SourceLoc<'file>,
    error: CompileError<'id>,
}

impl fmt::Display for DisplayError<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.location.enclosing_line().trim_end().replace('\t', " ");
        let underline_len = if self.location.start.line == self.location.end.line {
            (self.location.end.col - self.location.start.col).max(1)
        } else {
            3
        };
        const EMPTY: &str = "";
        writeln!(f, "At {}:", Paint::blue(&self.location).underline())?;
        writeln!(f, "{line}")?;
        writeln!(f, "{EMPTY:0$}{EMPTY:^<underline_len$}", self.location.start.col)?;
        writeln!(f, "{}", Paint::red(&self.error).bold())
    }
}

#[derive(Debug, Error)]
#[error("syntax error, expected {0}")]
pub struct ParseError(pub ExpectedSet, pub Span);

#[derive(Debug, Error)]
pub enum Unsupported {
    #[error("calling a custom class constructor")]
    CustomClassConstructor,
    #[error("defining a non-static member on a struct")]
    NonStaticStructMember,
    #[error("defining a method replacement without a body")]
    ReplacementWithoutBody,
    #[error("defining a final method without a body")]
    FinalWithoutBody,
    #[error("defining a native method with a body")]
    NativeWithBody,
    #[error("extending a final class")]
    ExtendingFinalClass,
    #[error("defining a native member in a non-native type")]
    NativeInNonNative,
}

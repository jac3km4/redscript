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

pub type TyperResult<'id, A, E = TyperError<'id>> = Result<A, E>;

#[derive(Debug, Error)]
#[error("syntax error, expected {0}")]
pub struct ParseError(pub ExpectedSet, pub Span);

#[derive(Debug, Error)]
pub enum TyperError<'id> {
    #[error("{0}")]
    TypeError(TypeError<'id>, Span),
    #[error("{0} is not defined")]
    UnresolvedVar(Str, Span),
    #[error("member {1} not found in {0}")]
    UnresolvedMember(TypeId<'id>, Str, Span),
    #[error("function {0} not found")]
    UnresolvedFunction(Str, Span),
    #[error("method not found")]
    UnresolvedMethod(Span),
    #[error("no matching overload, available options are:\n{}", .0.iter().format("\n"))]
    NoMatchingOverload(Box<[OverloadOption]>, Span),
    #[error("more than one matching overload, available options are:\n{}", .0.iter().format("\n"))]
    ManyMatchingOverloads(Box<[OverloadOption]>, Span),
    #[error("insufficient type information available for member lookup")]
    CannotLookupMember(Span),
    #[error("import {} could not be resolved", .0.iter().format("."))]
    UnresolvedImport(Box<[Str]>, Span),
}

impl<'id> TyperError<'id> {
    pub fn span(&self) -> Span {
        match self {
            TyperError::TypeError(_, span)
            | TyperError::UnresolvedVar(_, span)
            | TyperError::UnresolvedMember(_, _, span)
            | TyperError::UnresolvedFunction(_, span)
            | TyperError::UnresolvedMethod(span)
            | TyperError::NoMatchingOverload(_, span)
            | TyperError::ManyMatchingOverloads(_, span)
            | TyperError::CannotLookupMember(span)
            | TyperError::UnresolvedImport(_, span) => *span,
        }
    }

    pub fn display(self, files: &Files) -> DisplayError<'_, 'id> {
        let location = files.lookup(self.span()).expect("Unknown file");
        DisplayError { location, error: self }
    }

    pub(crate) fn for_overloads<'a>(
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
            (_, 0) => Self::UnresolvedMethod(span),
            (0, _) => Self::NoMatchingOverload(options.into(), span),
            _ => Self::ManyMatchingOverloads(options.into(), span),
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
    error: TyperError<'id>,
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

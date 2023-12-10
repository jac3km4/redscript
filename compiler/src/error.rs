use std::fmt;

use itertools::Itertools;
use peg::error::ExpectedSet;
use redscript::ast::Span;
use redscript::{str_fmt, Str};
use thiserror::Error;
#[cfg(feature = "pretty-errors")]
use yansi::Paint;

use crate::source_map::{Files, SourceLoc};
use crate::type_repo::{OverloadEntry, TypeId};
use crate::typer::{Data, Mono};

pub type CompileResult<'id, A, E = CompileError<'id>> = Result<A, E>;

#[derive(Debug, Error, PartialEq)]
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

    #[inline]
    pub fn code(&self) -> &'static str {
        match self {
            Self::TypeError(TypeError::UnresolvedType(_), _) => "UNRESOLVED_TYPE",
            Self::TypeError(_, _) => "TYPE_ERR",
            Self::UnresolvedFunction(_, _) => "UNRESOLVED_FN",
            Self::UnresolvedMethod(_, _) => "UNRESOLVED_METHOD",
            Self::UnresolvedMember(_, _, _) => "UNRESOLVED_MEMBER",
            Self::CannotLookupMember(_) => "TYPE_ANN_REQUIRED",
            Self::UnresolvedVar(_, _) => "UNRESOLVED_REF",
            Self::UnresolvedImport(_, _) => "UNRESOLVED_IMPORT",
            Self::NoMatchingOverload(_, _, _) | Self::ManyMatchingOverloads(_, _, _) => "NO_MATCHING_OVERLOAD",
            Self::UnimplementedMethod(_, _) => "UNIMPLEMENTED_METHOD",
            Self::Unsupported(feat, _) => match feat {
                Unsupported::AnnotatedFuncWithNoBody | Unsupported::FinalWithoutBody => "MISSING_BODY",
                Unsupported::NativeWithBody => "UNEXPECTED_BODY",
                Unsupported::NativeInNonNative => "UNEXPECTED_NATIVE",
                _ => "UNSUPPORTED",
            },
        }
    }

    pub fn display<'files>(&self, files: &'files Files) -> DisplayError<'files, &Self> {
        let location = files
            .lookup(self.span())
            .expect("span should point to a source map file");
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
pub struct DisplayError<'file, Err> {
    location: SourceLoc<'file>,
    error: Err,
}

impl<Err: fmt::Display> fmt::Display for DisplayError<'_, Err> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.location.enclosing_line().trim_end().replace('\t', " ");
        let underline_len = if self.location.start.line == self.location.end.line {
            (self.location.end.col - self.location.start.col).max(1)
        } else {
            3
        };
        const EMPTY: &str = "";

        #[cfg(feature = "pretty-errors")]
        writeln!(f, "At {}:", Paint::blue(&self.location).underline())?;
        #[cfg(not(feature = "pretty-errors"))]
        writeln!(f, "At {}:", self.location)?;
        writeln!(f, "{line}")?;
        writeln!(f, "{EMPTY:0$}{EMPTY:^<underline_len$}", self.location.start.col)?;
        #[cfg(feature = "pretty-errors")]
        writeln!(f, "{}", Paint::red(&self.error).bold())?;
        #[cfg(not(feature = "pretty-errors"))]
        writeln!(f, "{}", &self.error)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct OverloadOption(Str);

impl fmt::Display for OverloadOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum TypeError<'id> {
    #[error("found {0}, expected {1}")]
    Mismatch(Mono<'id>, Mono<'id>),
    #[error("{0} does not satisfy {1} subtype constraint")]
    Unsatisfied(Data<'id>, Data<'id>),
    #[error("cannot unify {0} and {1}")]
    CannotUnify(Mono<'id>, Mono<'id>),
    #[error("type {0} not found")]
    UnresolvedType(Str),
    #[error("provided {0} type arguments, but {2} expects {1}")]
    InvalidNumberOfTypeArgs(usize, usize, TypeId<'id>),
    #[error("provided {0} type arguments, but expected {1}")]
    InvalidNumberOfFunctionTypeArgs(usize, usize),
}

#[derive(Debug, Error)]
#[error("syntax error, expected {0}")]
pub struct ParseError(ExpectedSet, Span);

impl ParseError {
    #[inline]
    pub fn new(expected: ExpectedSet, span: Span) -> Self {
        Self(expected, span)
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.1
    }

    pub fn display<'files>(&self, files: &'files Files) -> DisplayError<'files, &Self> {
        let location = files.lookup(self.1).expect("span should point to a source map file");
        DisplayError { location, error: self }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum Unsupported {
    #[error("calling a custom class constructor")]
    CustomClassConstructor,
    #[error("defining a non-static member on a struct")]
    NonStaticStructMember,
    #[error("defining an annotated method without a body")]
    AnnotatedFuncWithNoBody,
    #[error("defining a final method without a body")]
    FinalWithoutBody,
    #[error("defining a native method with a body")]
    NativeWithBody,
    #[error("extending a final class")]
    ExtendingFinalClass,
    #[error("defining a native member in a non-native type")]
    NativeInNonNative,
    #[error("function type with more than 8 parameters")]
    FunctionMaxArityExceeded,
    #[error("this kind of annotation")]
    InvalidAnnotation,
    #[error("introducing global let bindings")]
    GlobalLetBinding,
}

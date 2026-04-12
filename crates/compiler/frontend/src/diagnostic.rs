use std::fmt;

use redscript_ast as ast;
use redscript_ast::Span;
use redscript_parser as parser;
use thiserror::Error;

use crate::lower::{LowerResult, Poly, TypeError};
use crate::stages::FunctionAnnotation;
use crate::symbols::Visibility;
use crate::utils::fmt::{DisplayFn, lowercase, sep_by, surrounded_by};
use crate::{CoalesceError, LowerError, Param, Type, TypeId, TypeKind, Variance, cte};

pub mod pass;

#[derive(Debug, Error)]
pub enum Diagnostic<'ctx> {
    #[error("{0}")]
    SyntaxError(#[from] parser::Error),
    #[error("{0}")]
    LoweringError(LowerError<'ctx>),
    #[error("{0}")]
    CoalesceError(Box<CoalesceError<'ctx>>, Span),
    #[error("duplicate variant name")]
    DuplicateVariantName(Span),
    #[error("duplicate variant value")]
    DuplicateVariantValue(Span),
    #[error("value overflow")]
    ValueOverflow(Span),
    #[error("this function must have a body")]
    MissingFunctionBody(Span),
    #[error("this function cannot have a body")]
    UnexpectedFunctionBody(Span),
    #[error("{} qualifiers have no effect on this item", sep_by(.0.iter_names().map(|(name,_)| surrounded_by(lowercase(name), "`", "`")), ", "))]
    UnusedItemQualifiers(ast::ItemQualifiers, Span),
    #[error("`{0}` is not a known intrinsic")]
    UnknownIntrinsic(&'ctx str, Span),
    #[error("items of this type are not allowed here")]
    UnexpectedItem(Span),
    #[error("base type must be a valid known type")]
    InvalidBaseType(Span),
    #[error("`{0}` is not a valid annotation in this context")]
    UnknownAnnotation(&'ctx str, Span),
    #[error("`{0}` could not be found")]
    ImportNotFound(&'ctx str, Span),
    #[error("`{0}` is private and cannot be imported")]
    ImportIsPrivate(&'ctx str, Span),
    #[error("this name is already defined in the scope")]
    NameRedefinition(Span),
    #[error("`{0}` is not a valid target for this annotation")]
    InvalidAnnotationType(&'ctx str, Span),
    #[error("could not find a method with a matching signature for the `{0}` annotation")]
    AnnotatedMethodNotFound(FunctionAnnotation<'ctx>, Span),
    #[error("the annotations on this function are incompatible with each other")]
    IncompatibleAnnotations(Span),
    #[error("this class is missing some required method implementation(s):\n{}", sep_by(.0, "\n"))]
    MissingMethodImpls(Box<[FunctionSignature<'ctx, Poly>]>, Span),
    #[error("this class contains a duplicated implementation of the `{0}` method")]
    DuplicateMethod(&'ctx str, Span),
    #[error("this class overrides a final method `{0}`")]
    FinalMethodOverride(&'ctx str, Span),
    #[error("this class circularly extends itself")]
    CircularInheritance(Span),
    #[error("type `{0}` expects {1} type arguments")]
    InvalidTypeArgCount(TypeId<'ctx>, usize, Span),
    #[error("type `{0}` does not satisfy expected bound `{1}`")]
    UnsastisfiedBound(Box<Type<'ctx>>, Box<Type<'ctx>>, Span),
    #[error("this annotation attempts to modify a user-defined symbol, which is not allowed")]
    UserSymbolAnnotation(Span),
    #[error("the type `{0}` appears in {1} position, which is incompatible with its declaration")]
    InvalidVariance(&'ctx str, Variance, Span),
    #[error("this type cannot inherit from {0}")]
    IncompatibleBaseType(&'static str, Span),
    #[error("this annotation duplicates an existing method with the same signature")]
    DuplicateMethodAnnotation(Span),
    #[error("annotated methods cannot be generic")]
    GenericMethodAnnotation(Span),
    #[error("non-data types cannot have variance")]
    NonDataVariance(Span),
    #[error("struct methods must be static")]
    NonStaticStructMethod(Span),
    #[error("scripted types are not allowed to have native members")]
    NativeMemberOfScriptedType(Span),
    #[error("strings, variants and resources cannot be marked as persistent")]
    InvalidPersistentField(Span),
    #[error("`@addField` cannot be used with native types that are known to be fully defined")]
    FullyDefinedNativeTypeFieldAddition(Span),
    #[error("`@addField` cannot be used with scripted structs")]
    ScriptedStructFieldAddition(Span),
    #[error("{0}")]
    Cte(#[from] cte::Error),
    #[error("the name of an implementation must be a valid identifier")]
    InvalidImplName(Span),
    #[error("the type of an implementation must have no free type variables")]
    InvalidImplType(Span),
    #[error("this implementation is a duplicate of a previous one")]
    DuplicateImpl(Span),
    #[error("unused variable")]
    UnusedLocal(Span),
    #[error(
        "this `@addField` conflicts with an existing field, this operation will have no effect"
    )]
    AddFieldConflict(Span),
    #[error("structures cannot have variance annotations")]
    InvalidStructVariance(Span),
    #[error("method overrides cannot be less visibile than the method they override ({0})")]
    LessVisibleOverride(Visibility, Span),
    #[error("{0}")]
    Other(Box<dyn std::error::Error + 'ctx>, DiagnosticLevel, Span),
}

impl<'ctx> Diagnostic<'ctx> {
    pub fn level(&self) -> DiagnosticLevel {
        match self {
            Self::LoweringError(err) => err.level(),
            Self::DuplicateVariantValue(_) | Self::FinalMethodOverride(_, _) => {
                DiagnosticLevel::Pedantic
            }
            Self::UnusedItemQualifiers(_, _)
            | Self::DuplicateVariantName(_)
            | Self::UnusedLocal(_)
            | Self::AddFieldConflict(_) => DiagnosticLevel::Warning,
            Self::ImportIsPrivate(_, _) | Self::LessVisibleOverride(_, _) => {
                DiagnosticLevel::ErrorAllowedAtRuntime
            }
            Self::Other(_, level, _) => *level,
            _ => DiagnosticLevel::Error,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(
            self.level(),
            DiagnosticLevel::Error | DiagnosticLevel::ErrorAllowedAtRuntime
        )
    }

    pub fn span(&self) -> Span {
        match self {
            Self::SyntaxError(err) => err.span(),
            Self::LoweringError(err) => err.span(),
            Self::Cte(err) => err.span(),
            Self::CoalesceError(_, span)
            | Self::DuplicateVariantName(span)
            | Self::DuplicateVariantValue(span)
            | Self::ValueOverflow(span)
            | Self::MissingFunctionBody(span)
            | Self::UnexpectedFunctionBody(span)
            | Self::UnusedItemQualifiers(_, span)
            | Self::UnknownIntrinsic(_, span)
            | Self::UnexpectedItem(span)
            | Self::InvalidBaseType(span)
            | Self::UnknownAnnotation(_, span)
            | Self::ImportNotFound(_, span)
            | Self::ImportIsPrivate(_, span)
            | Self::NameRedefinition(span)
            | Self::InvalidAnnotationType(_, span)
            | Self::AnnotatedMethodNotFound(_, span)
            | Self::IncompatibleAnnotations(span)
            | Self::MissingMethodImpls(_, span)
            | Self::DuplicateMethod(_, span)
            | Self::FinalMethodOverride(_, span)
            | Self::CircularInheritance(span)
            | Self::InvalidTypeArgCount(_, _, span)
            | Self::UnsastisfiedBound(_, _, span)
            | Self::UserSymbolAnnotation(span)
            | Self::InvalidVariance(_, _, span)
            | Self::IncompatibleBaseType(_, span)
            | Self::DuplicateMethodAnnotation(span)
            | Self::GenericMethodAnnotation(span)
            | Self::NonDataVariance(span)
            | Self::NonStaticStructMethod(span)
            | Self::NativeMemberOfScriptedType(span)
            | Self::InvalidPersistentField(span)
            | Self::FullyDefinedNativeTypeFieldAddition(span)
            | Self::ScriptedStructFieldAddition(span)
            | Self::InvalidImplName(span)
            | Self::InvalidImplType(span)
            | Self::DuplicateImpl(span)
            | Self::UnusedLocal(span)
            | Self::AddFieldConflict(span)
            | Self::InvalidStructVariance(span)
            | Self::LessVisibleOverride(_, span)
            | Self::Other(_, _, span) => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::SyntaxError(_) => "SYNTAX_ERR",
            Self::LoweringError(err) => err.code(),
            Self::CoalesceError(_, _) => "COALESCE_ERR",
            Self::DuplicateVariantName(_) => "DUP_VARIANT_NAME",
            Self::DuplicateVariantValue(_) => "DUP_VARIANT_VAL",
            Self::ValueOverflow(_) => "VAL_OVERFLOW",
            Self::MissingFunctionBody(_) => "MISSING_BODY",
            Self::UnexpectedFunctionBody(_) => "UNEXPECTED_BODY",
            Self::UnusedItemQualifiers(_, _) => "UNUSED_ITEM_QUALIFIERS",
            Self::UnknownIntrinsic(_, _) => "INVALID_INTRINSIC",
            Self::UnexpectedItem(_) => "UNEXPECTED_ITEM",
            Self::InvalidBaseType(_)
            | Self::CircularInheritance(_)
            | Self::IncompatibleBaseType(_, _) => "INVALID_BASE",
            Self::UnknownAnnotation(_, _)
            | Self::InvalidAnnotationType(_, _)
            | Self::AnnotatedMethodNotFound(_, _)
            | Self::IncompatibleAnnotations(_)
            | Self::UserSymbolAnnotation(_)
            | Self::GenericMethodAnnotation(_) => "INVALID_ANN_USE",
            Self::ImportNotFound(_, _) => "UNRESOLVED_IMPORT",
            Self::ImportIsPrivate(_, _) => "PRIVATE_IMPORT",
            Self::NameRedefinition(_) => "SYM_REDEFINITION",
            Self::MissingMethodImpls(_, _) => "MISSING_IMPL",
            Self::DuplicateMethod(_, _) => "DUP_METHOD",
            Self::FinalMethodOverride(_, _) => "FINAL_FN_OVERRIDE",
            Self::InvalidTypeArgCount(_, _, _) => "INVALID_TYPE_ARG_COUNT",
            Self::UnsastisfiedBound(_, _, _) => "UNSASTISFIED_BOUND",
            Self::InvalidVariance(_, _, _) => "INVALID_VARIANCE",
            Self::DuplicateMethodAnnotation(_) => "DUP_FN_ANN",
            Self::NonDataVariance(_) => "NON_DATA_VARIANCE",
            Self::NonStaticStructMethod(_) => "NON_STATIC_STRUCT_FN",
            Self::NativeMemberOfScriptedType(_) => "UNEXPECTED_NATIVE",
            Self::InvalidPersistentField(_) => "INVALID_PERSISTENT",
            Self::FullyDefinedNativeTypeFieldAddition(_) => "FULLY_DEFINED_NATIVE_FIELD_ADDITION",
            Self::ScriptedStructFieldAddition(_) => "SCRIPTED_STRUCT_FIELD_ADDITION",
            Self::Cte(_) => "CTE_ERR",
            Self::InvalidImplName(_) => "INVALID_IMPL_NAME",
            Self::InvalidImplType(_) => "INVALID_IMPL_TYPE",
            Self::DuplicateImpl(_) => "DUP_IMPL",
            Self::UnusedLocal(_) => "UNUSED_LOCAL",
            Self::AddFieldConflict(_) => "ADD_FIELD_CONFLICT",
            Self::InvalidStructVariance(_) => "INVALID_STRUCT_VARIANCE",
            Self::LessVisibleOverride(_, _) => "LESS_VISIBLE_OVERRIDE",
            Self::Other(_, _, _) => "OTHER",
        }
    }

    pub fn display<'a>(
        &'a self,
        sources: &'a ast::SourceMap,
    ) -> Result<impl fmt::Display + use<'a, 'ctx>, UnknownSource> {
        let span = self.span();
        let file = sources.get(span.file).ok_or(UnknownSource(span))?;
        let start = file.lookup(span.start);
        let end = file.lookup(span.end);
        let line = file.line_contents(start.line).ok_or(UnknownSource(span))?;

        Ok(DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            writeln!(
                f,
                "[{}] At {}:{}:{}",
                self.code(),
                file.path().display(),
                start.line + 1,
                start.col + 1
            )?;

            writeln!(f, "{}", line.trim_end())?;

            let pad = start.col;
            let underline_len = if start.line == end.line {
                (end.col - start.col).max(1)
            } else {
                3
            };
            writeln!(f, "{:>pad$}{:^>underline_len$}", "", "")?;
            writeln!(f, "{self}")
        }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticLevel {
    Pedantic = 0,
    Warning = 1,
    ErrorAllowedAtRuntime = 2,
    Error = 3,
}

impl<'ctx> From<LowerError<'ctx>> for Diagnostic<'ctx> {
    fn from(err: LowerError<'ctx>) -> Self {
        Self::LoweringError(err)
    }
}

#[derive(Debug)]
pub struct Reporter<A> {
    reported: Vec<A>,
}

impl<A> Reporter<A> {
    pub fn unwrap_err<A1, E1>(&mut self, res: Result<A1, E1>) -> Option<A1>
    where
        E1: Into<A>,
    {
        match res {
            Ok(res) => Some(res),
            Err(err) => {
                self.report(err.into());
                None
            }
        }
    }

    pub fn report(&mut self, error: impl Into<A>) {
        self.reported.push(error.into());
    }

    pub fn report_many(&mut self, errors: impl IntoIterator<Item = impl Into<A>>) {
        self.reported.extend(errors.into_iter().map(Into::into));
    }

    pub fn reported(&self) -> &[A] {
        &self.reported
    }

    pub fn into_reported(self) -> Vec<A> {
        self.reported
    }
}

impl<E> Default for Reporter<E> {
    fn default() -> Self {
        Self {
            reported: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionSignature<'ctx, K: TypeKind> {
    name: &'ctx str,
    params: Box<[Param<'ctx, K>]>,
    return_type: K::Type<'ctx>,
}

impl<'ctx, K: TypeKind> FunctionSignature<'ctx, K> {
    pub fn new(
        name: &'ctx str,
        params: impl Into<Box<[Param<'ctx, K>]>>,
        return_type: impl Into<K::Type<'ctx>>,
    ) -> Self {
        Self {
            name,
            params: params.into(),
            return_type: return_type.into(),
        }
    }
}

impl<K: TypeKind> fmt::Display for FunctionSignature<'_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "  func {}({}) -> {}",
            self.name,
            sep_by(&self.params, ", "),
            self.return_type
        )
    }
}

#[derive(Debug, Error)]
#[error("the source of a diagnostic could not be determined (span: {0})")]
pub struct UnknownSource(Span);

pub trait ErrorWithSpan {
    type Result;
    fn with_span(self, span: Span) -> Self::Result;
}

impl<'id, A> ErrorWithSpan for Result<A, TypeError<'id>> {
    type Result = LowerResult<'id, A>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Result {
        self.map_err(|e| LowerError::Type(Box::new(e), span))
    }
}

impl<'id, A> ErrorWithSpan for Result<A, CoalesceError<'id>> {
    type Result = Result<A, Diagnostic<'id>>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Result {
        self.map_err(|e| Diagnostic::CoalesceError(Box::new(e), span))
    }
}

use std::fmt;

use redscript_community_ast as ast;
use redscript_community_ast::Span;
use redscript_community_parser as parser;
use thiserror::Error;

use crate::lower::{LowerResult, Poly, TypeError};
use crate::stages::FunctionAnnotation;
use crate::symbols::Visibility;
use crate::types::Immutable;
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
    #[error("enum `{0}` has a duplicate variant name `{1}`")]
    DuplicateVariantName(&'ctx str, &'ctx str, Span),
    #[error("variant `{0}.{1}` has duplicate value {2}")]
    DuplicateVariantValue(&'ctx str, &'ctx str, i64, Span),
    #[error("implicit enum value exceeds the i64 range when incrementing the maximum value")]
    ValueOverflow(Span),
    #[error("enum storage must be Int8, Uint8, Int16, Uint16, Int32, Uint32, or Int64")]
    InvalidEnumStorage(Span),
    #[error("enum value {1} is outside the range of `{0}`")]
    EnumStorageOverflow(ast::EnumStorage, i64, Span),
    #[error("this function must have a body; define its implementation inside `{{ ... }}`")]
    MissingFunctionBody(Span),
    #[error("a body is not allowed for this function declaration")]
    UnexpectedFunctionBody(Span),
    #[error("{}", DisplayQualifiers(*.0))]
    UnusedItemQualifiers(ast::ItemQualifiers, Span),
    #[error("`{0}` is not a known intrinsic")]
    UnknownIntrinsic(&'ctx str, Span),
    #[error("{0}")]
    UnexpectedItem(&'static str, Span),
    #[error("base type must be a class or struct type")]
    InvalidBaseType(Span),
    #[error("`{0}` is not a valid annotation in this context")]
    UnknownAnnotation(&'ctx str, Span),
    #[error("imported name `{0}` could not be found")]
    ImportNotFound(&'ctx str, Span),
    #[error("`{0}` is private and cannot be imported")]
    ImportIsPrivate(&'ctx str, Span),
    #[error("`{0}` is already defined in this scope")]
    NameRedefinition(&'ctx str, Span),
    #[error("`{0}` is not a valid target for this annotation")]
    InvalidAnnotationType(&'ctx str, Span),
    #[error("could not find a method with a matching signature for the `{0}` annotation")]
    AnnotatedMethodNotFound(FunctionAnnotation<'ctx>, Span),
    #[error("the annotations on this function are incompatible with each other")]
    IncompatibleAnnotations(Span),
    #[error("this class is missing required method implementations:\n{}", sep_by(&*.0, "\n"))]
    MissingMethodImpls(Box<[FunctionSignature<'ctx, Poly>]>, Span),
    #[error("this class contains a duplicated implementation of the `{0}` method")]
    DuplicateMethod(&'ctx str, Span, Option<Span>),
    #[error("this method overrides the final method `{0}`")]
    FinalMethodOverride(&'ctx str, Span, Option<Span>),
    #[error(
        "inheritance cycle detected; a type cannot inherit from itself, directly or indirectly"
    )]
    CircularInheritance(Span),
    #[error("type `{0}` expects {1} type {noun}, but found {2}", noun = if *.1 == 1 { "argument" } else { "arguments" })]
    InvalidTypeArgCount(TypeId<'ctx>, usize, usize, Span),
    #[error("type `{0}` does not satisfy the requirement `{1}` declared for type parameter `{2}`")]
    UnsatisfiedBound(Box<Type<'ctx>>, Box<Type<'ctx>>, &'ctx str, Span),
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
    #[error("function type parameters cannot have variance annotations")]
    NonDataVariance(Span),
    #[error("struct methods must be static")]
    NonStaticStructMethod(Span),
    #[error("scripted types are not allowed to have native members")]
    NativeMemberOfScriptedType(Span),
    #[error("fields of type `{0}` cannot be marked as persistent")]
    InvalidPersistentField(Box<Type<'ctx>>, Span),
    #[error("`@addField` cannot be used with native types that are known to be fully defined")]
    FullyDefinedNativeTypeFieldAddition(Span),
    #[error("`@addField` cannot be used with scripted structs")]
    ScriptedStructFieldAddition(Span),
    #[error("{0}")]
    Cte(#[from] cte::Error),
    #[error("the name of an implementation must be a valid identifier")]
    InvalidImplName(Span),
    #[error("an implementation requires a concrete data type with no unresolved type parameters")]
    InvalidImplType(Span),
    #[error("this implementation is a duplicate of a previous one")]
    DuplicateImpl(Span),
    #[error("unused variable{}", .0.map(|n| format!(" `{n}`")).unwrap_or_default())]
    UnusedLocal(Option<&'ctx str>, Span),
    #[error(
        "this `@addField` conflicts with an existing field; this operation will have no effect"
    )]
    AddFieldConflict(Span),
    #[error(
        "replacement for `{0}.{name}({params}) -> {return_type}` overrides a previous replacement; this replacement takes precedence",
        name = .1.name,
        params = sep_by(&.1.params, ", "),
        return_type = .1.return_type
    )]
    ReplaceMethodConflict(
        TypeId<'ctx>,
        Box<FunctionSignature<'ctx, Immutable>>,
        Span,
        Span,
    ),
    #[error("structs cannot have variance annotations")]
    InvalidStructVariance(Span),
    #[error("method overrides cannot be less visible than the method they override ({0})")]
    LessVisibleOverride(Visibility, Span, Option<Span>),
    #[error(
        "override return type `{found}` is incompatible with the inherited signature{hint}",
        hint = ExpectedReturnHint(.expected.as_deref())
    )]
    OverrideReturnTypeMismatch {
        expected: Option<Box<Type<'ctx>>>,
        found: Box<Type<'ctx>>,
        span: Span,
        base_span: Option<Span>,
    },
    #[error("cannot write through a const parameter")]
    ConstParameterWrite(Span),
    #[error("cannot write through a const local")]
    ConstLocalWrite(Span),
    #[error("cannot write through a const_script_ref")]
    ConstRefWrite(Span),
    #[error("forwarding a const {root} to a {target} may allow mutation")]
    ConstForwarding {
        root: &'static str,
        target: &'static str,
        span: Span,
    },
    /// An extension diagnostic with a caller-defined stable code.
    #[error("{0}")]
    Custom(
        Box<dyn std::error::Error + 'ctx>,
        &'static str,
        DiagnosticLevel,
        Span,
    ),
}

impl<'ctx> Diagnostic<'ctx> {
    pub fn level(&self) -> DiagnosticLevel {
        match self {
            Self::LoweringError(err) => err.level(),
            Self::DuplicateVariantValue(_, _, _, _) | Self::FinalMethodOverride(..) => {
                DiagnosticLevel::Pedantic
            }
            Self::UnusedItemQualifiers(_, _)
            | Self::DuplicateVariantName(_, _, _)
            | Self::UnusedLocal(..)
            | Self::AddFieldConflict(_)
            | Self::ConstForwarding { .. }
            | Self::ReplaceMethodConflict(_, _, _, _) => DiagnosticLevel::Warning,
            Self::ImportIsPrivate(_, _)
            | Self::LessVisibleOverride(..)
            | Self::DuplicateMethodAnnotation(_) => DiagnosticLevel::ErrorAllowedAtRuntime,
            Self::Custom(_, _, level, _) => *level,
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
            | Self::DuplicateVariantName(_, _, span)
            | Self::DuplicateVariantValue(_, _, _, span)
            | Self::ValueOverflow(span)
            | Self::InvalidEnumStorage(span)
            | Self::EnumStorageOverflow(_, _, span)
            | Self::MissingFunctionBody(span)
            | Self::UnexpectedFunctionBody(span)
            | Self::UnusedItemQualifiers(_, span)
            | Self::UnknownIntrinsic(_, span)
            | Self::UnexpectedItem(_, span)
            | Self::InvalidBaseType(span)
            | Self::UnknownAnnotation(_, span)
            | Self::ImportNotFound(_, span)
            | Self::ImportIsPrivate(_, span)
            | Self::NameRedefinition(_, span)
            | Self::InvalidAnnotationType(_, span)
            | Self::AnnotatedMethodNotFound(_, span)
            | Self::IncompatibleAnnotations(span)
            | Self::MissingMethodImpls(_, span)
            | Self::DuplicateMethod(_, span, _)
            | Self::FinalMethodOverride(_, span, _)
            | Self::CircularInheritance(span)
            | Self::InvalidTypeArgCount(_, _, _, span)
            | Self::UnsatisfiedBound(_, _, _, span)
            | Self::UserSymbolAnnotation(span)
            | Self::InvalidVariance(_, _, span)
            | Self::IncompatibleBaseType(_, span)
            | Self::DuplicateMethodAnnotation(span)
            | Self::GenericMethodAnnotation(span)
            | Self::NonDataVariance(span)
            | Self::NonStaticStructMethod(span)
            | Self::NativeMemberOfScriptedType(span)
            | Self::InvalidPersistentField(_, span)
            | Self::FullyDefinedNativeTypeFieldAddition(span)
            | Self::ScriptedStructFieldAddition(span)
            | Self::InvalidImplName(span)
            | Self::InvalidImplType(span)
            | Self::DuplicateImpl(span)
            | Self::UnusedLocal(_, span)
            | Self::AddFieldConflict(span)
            | Self::ReplaceMethodConflict(_, _, span, _)
            | Self::InvalidStructVariance(span)
            | Self::LessVisibleOverride(_, span, _)
            | Self::OverrideReturnTypeMismatch { span, .. }
            | Self::ConstParameterWrite(span)
            | Self::ConstLocalWrite(span)
            | Self::ConstRefWrite(span)
            | Self::ConstForwarding { span, .. }
            | Self::Custom(_, _, _, span) => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::SyntaxError(_) => "SYNTAX_ERR",
            Self::LoweringError(err) => err.code(),
            Self::CoalesceError(_, _) => "COALESCE_ERR",
            Self::DuplicateVariantName(_, _, _) => "DUP_VARIANT_NAME",
            Self::DuplicateVariantValue(_, _, _, _) => "DUP_VARIANT_VAL",
            Self::ValueOverflow(_) | Self::EnumStorageOverflow(_, _, _) => "VAL_OVERFLOW",
            Self::InvalidEnumStorage(_) => "INVALID_ENUM_STORAGE",
            Self::MissingFunctionBody(_) => "MISSING_BODY",
            Self::UnexpectedFunctionBody(_) => "UNEXPECTED_BODY",
            Self::UnusedItemQualifiers(_, _) => "UNUSED_ITEM_QUALIFIERS",
            Self::UnknownIntrinsic(_, _) => "INVALID_INTRINSIC",
            Self::UnexpectedItem(_, _) => "UNEXPECTED_ITEM",
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
            Self::NameRedefinition(..) => "SYM_REDEFINITION",
            Self::MissingMethodImpls(_, _) => "MISSING_IMPL",
            Self::DuplicateMethod(..) => "DUP_METHOD",
            Self::FinalMethodOverride(..) => "FINAL_FN_OVERRIDE",
            Self::InvalidTypeArgCount(_, _, _, _) => "INVALID_TYPE_ARG_COUNT",
            Self::UnsatisfiedBound(_, _, _, _) => "UNSATISFIED_BOUND",
            Self::InvalidVariance(_, _, _) => "INVALID_VARIANCE",
            Self::DuplicateMethodAnnotation(_) => "DUP_FN_ANN",
            Self::NonDataVariance(_) => "NON_DATA_VARIANCE",
            Self::NonStaticStructMethod(_) => "NON_STATIC_STRUCT_FN",
            Self::NativeMemberOfScriptedType(_) => "UNEXPECTED_NATIVE",
            Self::InvalidPersistentField(_, _) => "INVALID_PERSISTENT",
            Self::FullyDefinedNativeTypeFieldAddition(_) => "FULLY_DEFINED_NATIVE_FIELD_ADDITION",
            Self::ScriptedStructFieldAddition(_) => "SCRIPTED_STRUCT_FIELD_ADDITION",
            Self::Cte(_) => "CTE_ERR",
            Self::InvalidImplName(_) => "INVALID_IMPL_NAME",
            Self::InvalidImplType(_) => "INVALID_IMPL_TYPE",
            Self::DuplicateImpl(_) => "DUP_IMPL",
            Self::UnusedLocal(..) => "UNUSED_LOCAL",
            Self::AddFieldConflict(_) => "ADD_FIELD_CONFLICT",
            Self::ReplaceMethodConflict(_, _, _, _) => "REPLACE_METHOD_CONFLICT",
            Self::InvalidStructVariance(_) => "INVALID_STRUCT_VARIANCE",
            Self::LessVisibleOverride(..) => "LESS_VISIBLE_OVERRIDE",
            Self::OverrideReturnTypeMismatch { .. } => "OVERRIDE_RETURN_TYPE_MISMATCH",
            Self::ConstParameterWrite(_) => "CONST_PARAMETER_WRITE",
            Self::ConstLocalWrite(_) => "CONST_LOCAL_WRITE",
            Self::ConstRefWrite(_) => "CONST_REF_WRITE",
            Self::ConstForwarding { .. } => "CONST_FORWARDING",
            Self::Custom(_, code, _, _) => code,
        }
    }

    pub fn related_locations(&self) -> impl Iterator<Item = (Span, &'static str)> {
        match self {
            Self::ReplaceMethodConflict(_, _, _, previous) => {
                Some((*previous, "previous replacement is overridden here"))
            }
            Self::DuplicateMethod(_, _, Some(previous)) => {
                Some((*previous, "previous implementation is here"))
            }
            Self::FinalMethodOverride(_, _, Some(base)) => {
                Some((*base, "overridden final method is declared here"))
            }
            Self::LessVisibleOverride(_, _, Some(base)) => {
                Some((*base, "overridden method is declared here"))
            }
            Self::OverrideReturnTypeMismatch {
                base_span: Some(base),
                ..
            } => Some((*base, "overridden method is declared here")),
            _ => None,
        }
        .into_iter()
    }

    pub fn display<'a>(
        &'a self,
        sources: &'a ast::SourceMap,
    ) -> Result<impl fmt::Display + use<'a, 'ctx>, UnknownSource> {
        let span = self.span();
        let file = sources.get(span.file).ok_or(UnknownSource(span))?;
        let snippet = ast::SourceSnippet::new(file, span).ok_or(UnknownSource(span))?;
        let start = snippet.start().clone();
        let related = self
            .related_locations()
            .map(|(span, message)| {
                let file = sources.get(span.file).ok_or(UnknownSource(span))?;
                let snippet = ast::SourceSnippet::new(file, span).ok_or(UnknownSource(span))?;
                Ok((file, snippet, message))
            })
            .collect::<Result<Vec<_>, UnknownSource>>()?;

        Ok(DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            writeln!(
                f,
                "{}[{}]: {self}",
                if self.is_error() { "error" } else { "warning" },
                self.code()
            )?;
            writeln!(
                f,
                " --> {}:{}:{}",
                file.path().display(),
                start.line + 1,
                start.col + 1
            )?;

            write!(f, "{snippet}")?;
            for (file, snippet, message) in &related {
                writeln!(f, "\nnote: {message}")?;
                writeln!(
                    f,
                    " --> {}:{}:{}",
                    file.path().display(),
                    snippet.start().line + 1,
                    snippet.start().col + 1
                )?;
                write!(f, "{snippet}")?;
            }
            Ok(())
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

struct DisplayQualifiers(ast::ItemQualifiers);

impl fmt::Display for DisplayQualifiers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let names = self
            .0
            .iter_names()
            .map(|(name, _)| surrounded_by(lowercase(name), "`", "`"))
            .collect::<Vec<_>>();
        let noun = if names.len() == 1 {
            "qualifier"
        } else {
            "qualifiers"
        };
        let verb = if names.len() == 1 { "has" } else { "have" };
        write!(
            f,
            "{} {noun} {verb} no effect on this item",
            sep_by(&names, ", ")
        )
    }
}

struct ExpectedReturnHint<'a, 'ctx>(Option<&'a Type<'ctx>>);

impl fmt::Display for ExpectedReturnHint<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expected) = self.0 {
            write!(f, " (expected `{expected}`)")
        } else {
            Ok(())
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_initialization_does_not_read_its_destination() {
        use redscript_compiler_api::SourceMapExt;

        use crate::pass::{DiagnosticPass, UnusedLocals};
        use crate::{Symbols, TypeInterner, infer_from_sources};

        let source = "func Test() {
            let element = 1;
            let unread = [element];
            let empty: [Int32] = [];
            let used = [2];
            ArraySize(used);
            let outer = [3];
            if true { let outer = [outer[0]]; }
            let nested = [[4]];
            let closure = () -> { let captured = [used[0]]; };
            closure();
        }";
        let sources = ast::SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back("arrays.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = crate::CompileErrorReporter::default();
        let (unit, _) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(reporter.reported().is_empty(), "{reporter:?}");

        for function in unit.all_functions() {
            UnusedLocals.run(function, &mut reporter);
        }
        let diagnostics = reporter.reported();
        assert_eq!(diagnostics.len(), 5, "{diagnostics:?}");
        for (diagnostic, declaration) in diagnostics.iter().zip([
            "unread =",
            "empty:",
            "outer = [outer",
            "nested =",
            "captured =",
        ]) {
            let start = source.find(declaration).unwrap() as u32;
            let name_len = declaration.find([' ', ':']).unwrap() as u32;
            assert_eq!(diagnostic.code(), "UNUSED_LOCAL");
            assert_eq!(diagnostic.span(), Span::new(start, start + name_len, file));
        }
    }

    #[test]
    fn declaration_diagnostics_preserve_codes_and_spans() {
        let file = ast::SourceMap::new().push_back("declarations.reds", "Example");
        let span = Span::new(0, 7, file);
        for (diagnostic, code) in [
            (Diagnostic::MissingFunctionBody(span), "MISSING_BODY"),
            (Diagnostic::UnexpectedFunctionBody(span), "UNEXPECTED_BODY"),
            (Diagnostic::CircularInheritance(span), "INVALID_BASE"),
            (
                Diagnostic::NameRedefinition("Example", span),
                "SYM_REDEFINITION",
            ),
            (Diagnostic::InvalidImplType(span), "INVALID_IMPL_TYPE"),
            (
                Diagnostic::ImportNotFound("Example", span),
                "UNRESOLVED_IMPORT",
            ),
        ] {
            assert_eq!(diagnostic.code(), code);
            assert_eq!(diagnostic.span(), span);
            assert!(diagnostic.related_locations().next().is_none());
        }
    }

    #[test]
    fn custom_diagnostics_preserve_metadata_and_message() {
        let sources = ast::SourceMap::new();
        let file = sources.push_back("custom.reds", "value");
        let span = Span::new(0, 5, file);
        let diagnostic = Diagnostic::Custom(
            Box::new(std::io::Error::other("custom diagnostic")),
            "CUSTOM_DIAGNOSTIC",
            DiagnosticLevel::Warning,
            span,
        );
        assert_eq!(diagnostic.code(), "CUSTOM_DIAGNOSTIC");
        assert_eq!(diagnostic.level(), DiagnosticLevel::Warning);
        assert_eq!(diagnostic.span(), span);
        assert_eq!(diagnostic.to_string(), "custom diagnostic");
        assert!(!diagnostic.is_error());
        assert!(
            diagnostic
                .display(&sources)
                .unwrap()
                .to_string()
                .contains("warning[CUSTOM_DIAGNOSTIC]")
        );
    }

    #[test]
    fn invalid_impl_type_message() {
        let file = ast::SourceMap::new().push_back("declarations.reds", "Example");
        let span = Span::new(0, 7, file);
        assert_eq!(
            Diagnostic::InvalidImplType(span).to_string(),
            "an implementation requires a concrete data type with no unresolved type parameters"
        );
    }
}

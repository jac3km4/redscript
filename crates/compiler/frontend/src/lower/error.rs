use std::borrow::Cow;
use std::fmt;
use std::ops::RangeInclusive;
use std::rc::Rc;

use redscript_ast::Span;
use thiserror::Error;

use crate::diagnostic::{DiagnosticLevel, FunctionSignature};
use crate::lower::types::{InferredType, InferredTypeApp};
use crate::types::{MAX_FN_ARITY, MAX_STATIC_ARRAY_SIZE, TypeId, predef};
use crate::utils::fmt::sep_by;
use crate::{FieldId, Immutable, MethodId, Visibility};

pub type InferResult<'ctx, A> = Result<A, TypeError<'ctx>>;
pub type LowerResult<'id, A, E = Error<'id>> = Result<A, E>;

#[derive(Debug, Clone, Error)]
pub enum Error<'ctx> {
    #[error("{0}")]
    Type(Box<TypeError<'ctx>>, Span),
    #[error("`{0}` is not defined")]
    UnresolvedVar(&'ctx str, Span),
    #[error("`{0}` is not a known type")]
    UnresolvedType(&'ctx str, Span),
    #[error("`{0}` has no member named `{1}`")]
    UnresolvedMember(TypeId<'ctx>, &'ctx str, Span),
    #[error("multiple overloads match the types of the provided arguments:\n{}", sep_by(.0.iter(), "\n"))]
    MultipleMatchingOverloads(Rc<[FunctionSignature<'ctx, Immutable>]>, Span),
    #[error("there's no matching `{0}` function")]
    UnresolvedFunction(&'ctx str, Span),
    #[error(
        "invalid number of arguments, expected {expected} {noun}, found {found}",
        expected = DisplayArgCounts(.0),
        noun = if .0.iter().all(|r| *r.start() == 1 && *r.end() == 1) { "argument" } else { "arguments" },
        found = .1
    )]
    InvalidArgCount(Vec<RangeInclusive<usize>>, usize, Span),
    #[error(
        "insufficient type information available for member lookup, consider adding \
         type annotations"
    )]
    InsufficientTypeInformation(Span),
    #[error("this type does not support the `new` operator")]
    InvalidNewType(Span),
    #[error("this type cannot be directly constructed")]
    InvalidConstructType(Span),
    #[error(
        "`{type_id}` cannot be constructed with the `new` operator; remove `new` and keep the constructor arguments"
    )]
    NewWithConstructible { type_id: TypeId<'ctx>, span: Span },
    #[error("this type cannot be used with the `as` operator, the target type must be a class")]
    InvalidDynCastType(Span),
    #[error("the target type of this cast is not known, consider specifying it")]
    UnknownStaticCastType(Span),
    #[error("invalid number of type arguments, expected {0}")]
    InvalidTypeArgCount(usize, Span),
    #[error("class constructors do not accept positional arguments")]
    ClassConstructorHasArguments(Span),
    #[error(
        "unsupported arity, functions can only have up to {} parameters",
        MAX_FN_ARITY
    )]
    UnsupportedArity(Span),
    #[error(
        "unsupported static array size, static arrays can only have up to {} elements",
        MAX_STATIC_ARRAY_SIZE
    )]
    UnsupportedStaticArraySize(Span),
    #[error("this expression cannot be used as a case label")]
    InvalidCaseLabel(Span),
    #[error("invalid cyclic type reference")]
    CyclicType(Span),
    #[error("this literal is out of range for `{0}`{hint}", hint = NumberTypeRangeHint(*.0))]
    LiteralOutOfRange(TypeId<'ctx>, Span),
    #[error(r#"expected `{0}`; use a `{0}` literal, e.g. `{1}"text"`"#)]
    WrongStringLiteral(TypeId<'ctx>, char, Span),
    #[error("`{0}` is an abstract class and cannot be instantiated")]
    InstantiatingAbstract(TypeId<'ctx>, Span),
    #[error("this type has no super type to refer to")]
    NonExistentSuperType(Span),
    #[error("cannot assign to this expression")]
    InvalidPlaceExpr(Span),
    #[error(
        "the highlighted temporary value cannot be used in this expression, consider extracting it \
        to a local"
    )]
    InvalidTemporary(Span),
    #[error("only constants can be used here")]
    UnexpectedNonConstant(Span),
    #[error("the `NameOf(Type)` syntax is deprecated, use `NameOf<Type>()` instead")]
    DeprecatedNameOf(Span),
    #[error(
        "`{type_id}` is a native struct with an incomplete script definition, passing arguments to its \
        constructor might result in undefined behavior, it can however still be safely \
        constructed without arguments: `{}()`"
    , .type_id.simple_name())]
    NonFullyDefinedNativeStructConstruction { type_id: TypeId<'ctx>, span: Span },
    #[error("`case let` block must end with a `break` or `return` statement")]
    MissingBreakInCaseLet(Span),
    #[error("this type cannot be wrapped with a ref")]
    RefOnNeverRefType(Span),
    #[error("this cast can never succeed, because the types are not related ({0})")]
    ImpossibleDynCast(Box<TypeError<'ctx>>, Span),
    #[error("this cast is redundant, you should remove it")]
    RedundantDynCast(Span),
    #[error("{0}")]
    InaccessibleMethod(Box<InaccessibleMember<'ctx, MethodId<'ctx>>>, Span),
    #[error("{0}")]
    InaccessibleField(Box<InaccessibleMember<'ctx, FieldId<'ctx>>>, Span),
    #[error("attempting to use a type with `ref` where none is expected")]
    UnexpectedRef(Span),
    #[error("attempting to use a type with no `ref` where one is expected")]
    UnexpectedNonRef(Span),
    #[error("calling a static method of `{type_id}` from its subclass, use `{}.{name}` instead", .type_id.simple_name())]
    CallingBaseStaticMethod {
        type_id: TypeId<'ctx>,
        name: &'ctx str,
        span: Span,
    },
    #[error("constructor of `{0}` is private and cannot be called here{help}", help = ConstructorHelp(.1))]
    PrivateConstructor(TypeId<'ctx>, Cow<'ctx, str>, Span),
}

impl Error<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Type(_, span)
            | Self::UnresolvedVar(_, span)
            | Self::UnresolvedType(_, span)
            | Self::UnresolvedMember(_, _, span)
            | Self::MultipleMatchingOverloads(_, span)
            | Self::UnresolvedFunction(_, span)
            | Self::InvalidArgCount(_, _, span)
            | Self::InsufficientTypeInformation(span)
            | Self::InvalidNewType(span)
            | Self::InvalidConstructType(span)
            | Self::NewWithConstructible { span, .. }
            | Self::InvalidDynCastType(span)
            | Self::UnknownStaticCastType(span)
            | Self::InvalidTypeArgCount(_, span)
            | Self::ClassConstructorHasArguments(span)
            | Self::UnsupportedArity(span)
            | Self::UnsupportedStaticArraySize(span)
            | Self::InvalidCaseLabel(span)
            | Self::CyclicType(span)
            | Self::LiteralOutOfRange(_, span)
            | Self::WrongStringLiteral(_, _, span)
            | Self::InstantiatingAbstract(_, span)
            | Self::NonExistentSuperType(span)
            | Self::InvalidPlaceExpr(span)
            | Self::InvalidTemporary(span)
            | Self::UnexpectedNonConstant(span)
            | Self::DeprecatedNameOf(span)
            | Self::NonFullyDefinedNativeStructConstruction { span, .. }
            | Self::MissingBreakInCaseLet(span)
            | Self::RefOnNeverRefType(span)
            | Self::ImpossibleDynCast(_, span)
            | Self::RedundantDynCast(span)
            | Self::InaccessibleMethod(_, span)
            | Self::InaccessibleField(_, span)
            | Self::UnexpectedRef(span)
            | Self::UnexpectedNonRef(span)
            | Self::CallingBaseStaticMethod { span, .. }
            | Self::PrivateConstructor(_, _, span) => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::Type(_, _) => "TYPE_ERR",
            Self::UnresolvedVar(_, _) => "UNRESOLVED_REF",
            Self::UnresolvedType(_, _) => "UNRESOLVED_TYPE",
            Self::UnresolvedMember(_, _, _) => "UNRESOLVED_MEMBER",
            Self::MultipleMatchingOverloads(_, _) => "MULTIPLE_MATCHING_OVERLOADS",
            Self::UnresolvedFunction(_, _) => "UNRESOLVED_FN",
            Self::InvalidArgCount(_, _, _) => "INVALID_ARG_COUNT",
            Self::InsufficientTypeInformation(_) => "CANNOT_LOOKUP_MEMBER",
            Self::InvalidNewType(_)
            | Self::ClassConstructorHasArguments(_)
            | Self::InstantiatingAbstract(_, _)
            | Self::NewWithConstructible { .. } => "INVALID_NEW_USE",
            Self::InvalidConstructType(_) => "INVALID_CONSTRUCT",
            Self::InvalidDynCastType(_) => "INVALID_DYN_CAST",
            Self::UnknownStaticCastType(_) => "INVALID_STATIC_CAST",
            Self::InvalidTypeArgCount(_, _) => "INVALID_TYPE_ARG_COUNT",
            Self::UnsupportedArity(_) => "UNSUPPORTED_ARITY",
            Self::UnsupportedStaticArraySize(_) => "UNSUPPORTED_ARRAY_SIZE",
            Self::InvalidCaseLabel(_) => "INVALID_CASE_LABEL",
            Self::CyclicType(_) => "CYCLIC_TYPE",
            Self::LiteralOutOfRange(_, _) => "LIT_OUT_OF_RANGE",
            Self::WrongStringLiteral(_, _, _) => "WRONG_STRING_LIT",
            Self::NonExistentSuperType(_) => "INVALID_BASE",
            Self::InvalidPlaceExpr(_) => "INVALID_PLACE",
            Self::InvalidTemporary(_) => "INVALID_TEMP",
            Self::UnexpectedNonConstant(_) => "INVALID_CONSTANT",
            Self::DeprecatedNameOf(_) => "DEPRECATED_SYNTAX",
            Self::NonFullyDefinedNativeStructConstruction { .. } => "NON_FULLY_DEFINED_CTR",
            Self::MissingBreakInCaseLet(_) => "MISSING_BREAK",
            Self::RefOnNeverRefType(_) => "REF_ON_NEVER_REF",
            Self::ImpossibleDynCast(_, _) => "IMPOSSIBLE_DYN_CAST",
            Self::RedundantDynCast(_) => "REDUNDANT_DYN_CAST",
            Self::InaccessibleMethod { .. } => "INACCESSIBLE_METHOD",
            Self::InaccessibleField { .. } => "INACCESSIBLE_FIELD",
            Self::UnexpectedRef(_) => "UNEXPECTED_REF",
            Self::UnexpectedNonRef(_) => "UNEXPECTED_NON_REF",
            Self::CallingBaseStaticMethod { .. } => "CALLING_BASE_STATIC_METHOD",
            Self::PrivateConstructor(_, _, _) => "PRIVATE_CONSTRUCTOR",
        }
    }

    pub fn level(&self) -> DiagnosticLevel {
        match self {
            Self::DeprecatedNameOf(_) | Self::CallingBaseStaticMethod { .. } => {
                DiagnosticLevel::Warning
            }
            Self::InvalidTemporary(_)
            | Self::NonFullyDefinedNativeStructConstruction { .. }
            | Self::InaccessibleMethod(_, _)
            | Self::InaccessibleField(_, _) => DiagnosticLevel::ErrorAllowedAtRuntime,
            _ => DiagnosticLevel::Error,
        }
    }

    pub fn is_fatal(&self) -> bool {
        !matches!(self, Self::DeprecatedNameOf(_))
    }
}

struct ConstructorHelp<'a>(&'a str);

impl fmt::Display for ConstructorHelp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            Ok(())
        } else {
            write!(f, "; help: {}", self.0)
        }
    }
}

struct NumberTypeRangeHint<'ctx>(TypeId<'ctx>);

impl fmt::Display for NumberTypeRangeHint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_range<T: fmt::Display>(f: &mut fmt::Formatter<'_>, min: T, max: T) -> fmt::Result {
            write!(
                f,
                "; provide a value in the inclusive range `{min}..={max}`"
            )
        }
        match self.0 {
            id if id == predef::UINT8 => write_range(f, u8::MIN, u8::MAX),
            id if id == predef::UINT16 => write_range(f, u16::MIN, u16::MAX),
            id if id == predef::UINT32 => write_range(f, u32::MIN, u32::MAX),
            id if id == predef::UINT64 => write_range(f, u64::MIN, u64::MAX),
            id if id == predef::INT8 => write_range(f, i8::MIN, i8::MAX),
            id if id == predef::INT16 => write_range(f, i16::MIN, i16::MAX),
            id if id == predef::INT32 => write_range(f, i32::MIN, i32::MAX),
            id if id == predef::INT64 => write_range(f, i64::MIN, i64::MAX),
            _ => Ok(()),
        }
    }
}

struct DisplayArgCounts<'a>(&'a [RangeInclusive<usize>]);

impl fmt::Display for DisplayArgCounts<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ranges = self.0.to_vec();
        ranges.sort_unstable_by_key(|range| (*range.start(), *range.end()));
        let mut merged: Vec<RangeInclusive<usize>> = Vec::new();
        for range in ranges {
            if let Some(last) = merged.last_mut()
                && *range.start() <= last.end().saturating_add(1)
            {
                *last = *last.start()..=*last.end().max(range.end());
            } else {
                merged.push(range);
            }
        }
        for (index, range) in merged.iter().enumerate() {
            if index > 0 {
                write!(f, " or ")?;
            }
            let start = range.start();
            let end = range.end();
            if start == end {
                write!(f, "{start}")?;
            } else {
                write!(f, "between {start} and {end}")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Error)]
pub enum TypeError<'ctx> {
    #[error("type mismatch: found `{0}` when expected `{1}`")]
    Mismatch(InferredType<'ctx>, InferredType<'ctx>),
    #[error("type error: `{0}` is not compatible with `{1}`")]
    Incompatible(InferredTypeApp<'ctx>, InferredTypeApp<'ctx>),
    #[error("type error: cannot unify `{0}` and `{1}`")]
    CannotUnify(InferredType<'ctx>, InferredType<'ctx>),
    #[error("{0}\n  when comparing `{1}` and `{2}`")]
    Nested(Box<Self>, InferredType<'ctx>, InferredType<'ctx>),
}

#[derive(Debug, Clone, Error)]
#[error("{visibility} member `{name}` of `{}` is not accessible here", .id.as_ref().as_str())]
pub struct InaccessibleMember<'ctx, K: AsRef<TypeId<'ctx>>> {
    id: K,
    visibility: Visibility,
    name: &'ctx str,
}

impl<'ctx, K: AsRef<TypeId<'ctx>>> InaccessibleMember<'ctx, K> {
    pub fn new(id: K, name: &'ctx str, visibility: Visibility) -> Self {
        Self {
            id,
            visibility,
            name,
        }
    }

    pub fn id(&self) -> &K {
        &self.id
    }
}

#[derive(Debug)]
pub enum CoalesceError<'ctx> {
    CannotCoalesce(InferredType<'ctx>, Option<InferredType<'ctx>>),
}

impl fmt::Display for CoalesceError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CannotCoalesce(lower, upper) => {
                write!(f, "type is too broad (`{lower}` <: T <: ")?;
                if let Some(upper) = upper {
                    write!(f, "`{upper}`")?;
                } else {
                    write!(f, "`Any`")?;
                }
                write!(f, ") for some type T; consider adding type annotations")
            }
        }
    }
}

impl std::error::Error for CoalesceError<'_> {}

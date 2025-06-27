use std::fmt;
use std::ops::RangeInclusive;

use redscript_ast::Span;
use thiserror::Error;

use crate::lower::types::{InferredType, InferredTypeApp};
use crate::types::{MAX_FN_ARITY, MAX_STATIC_ARRAY_SIZE, TypeId, predef};

pub type InferResult<'ctx, A> = Result<A, TypeError<'ctx>>;
pub type LowerResult<'id, A, E = Error<'id>> = Result<A, E>;

#[derive(Debug, Clone, Error)]
pub enum Error<'ctx> {
    #[error("{0}")]
    Type(Box<TypeError<'ctx>>, Span),
    #[error("'{0}' is not defined")]
    UnresolvedVar(&'ctx str, Span),
    #[error("'{0}' is not a known type")]
    UnresolvedType(&'ctx str, Span),
    #[error("'{0}' has no member named '{1}'")]
    UnresolvedMember(TypeId<'ctx>, &'ctx str, Span),
    #[error("{1} matching overloads found for '{0}'")]
    MultipleMatchingOverloads(&'ctx str, usize, Span),
    #[error("there's no matching '{0}' function")]
    UnresolvedFunction(&'ctx str, Span),
    #[error("invalid number of arguments, expected {}", DisplayRangeInclusive(.0))]
    InvalidArgCount(RangeInclusive<usize>, Span),
    #[error(
        "insufficient type information available for member lookup, consider adding \
         type annotations"
    )]
    InsufficientTypeInformation(Span),
    #[error("this type cannot be constructed with the 'new' operator")]
    InvalidNewType(Span),
    #[error("this type cannot be casted with the 'as' operator")]
    InvalidDynCastType(Span),
    #[error("the target type of this cast is not known, consider specifying it")]
    UnknownStaticCastType(Span),
    #[error("invalid number of type arguments, expected {0}")]
    InvalidTypeArgCount(usize, Span),
    #[error("class constructors do not accept arguments")]
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
    #[error("this literal is out of range for {0}{hint}", hint = NumberTypeRangeHint(*.0))]
    LiteralOutOfRange(TypeId<'ctx>, Span),
    #[error(
        r#"expected a {0} here, you should prefix your literal with '{1}', e.g. {1}"lorem ipsum""#
    )]
    WrongStringLiteral(TypeId<'ctx>, char, Span),
    #[error("'{0}' is an abstract class and cannot be instantiated")]
    InstantiatingAbstract(TypeId<'ctx>, Span),
    #[error("this type has no super type to refer to")]
    NonExistentSuperType(Span),
    #[error("this expression is not a place that can be written to")]
    InvalidPlaceExpr(Span),
    #[error("a temporary cannot be used here, consider storing this value in a variable")]
    InvalidTemporary(Span),
    #[error("only constants can be used here")]
    UnexpectedNonConstant(Span),
    #[error("the `NameOf(Type)` syntax is deprecated, use `NameOf<Type>()` instead")]
    DeprecatedNameOf(Span),
    #[error(
        "'{0}' is a native struct with an incomplete script definition, passing arguments to its \
        constructor might result in undefined behavior, it can however still be safely \
        constructed without arguments: 'new {0}()'"
    )]
    NonSealedStructConstruction(TypeId<'ctx>, Span),
    #[error("`case let` block must end with a `break` or `return` statement")]
    MissingBreakInCaseLet(Span),
}

impl Error<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Type(_, span)
            | Self::UnresolvedVar(_, span)
            | Self::UnresolvedType(_, span)
            | Self::UnresolvedMember(_, _, span)
            | Self::MultipleMatchingOverloads(_, _, span)
            | Self::UnresolvedFunction(_, span)
            | Self::InvalidArgCount(_, span)
            | Self::InsufficientTypeInformation(span)
            | Self::InvalidNewType(span)
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
            | Self::NonSealedStructConstruction(_, span)
            | Self::MissingBreakInCaseLet(span) => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::Type(_, _) => "TYPE_ERR",
            Self::UnresolvedVar(_, _) => "UNRESOLVED_REF",
            Self::UnresolvedType(_, _) => "UNRESOLVED_TYPE",
            Self::UnresolvedMember(_, _, _) => "UNRESOLVED_MEMBER",
            Self::MultipleMatchingOverloads(_, _, _) => "MULTIPLE_MATCHING_OVERLOADS",
            Self::UnresolvedFunction(_, _) => "UNRESOLVED_FN",
            Self::InvalidArgCount(_, _) => "INVALID_ARG_COUNT",
            Self::InsufficientTypeInformation(_) => "CANNOT_LOOKUP_MEMBER",
            Self::InvalidNewType(_)
            | Self::ClassConstructorHasArguments(_)
            | Self::InstantiatingAbstract(_, _) => "INVALID_NEW_USE",
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
            Self::NonSealedStructConstruction(_, _) => "NON_SEALED_CTR",
            Self::MissingBreakInCaseLet(_) => "MISSING_BREAK",
        }
    }

    pub fn is_fatal(&self) -> bool {
        !matches!(self, Self::DeprecatedNameOf(_))
    }
}

struct NumberTypeRangeHint<'ctx>(TypeId<'ctx>);

impl fmt::Display for NumberTypeRangeHint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_range<T: fmt::Display>(f: &mut fmt::Formatter<'_>, min: T, max: T) -> fmt::Result {
            write!(f, ", provide a value between {min} and {max}")
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

struct DisplayRangeInclusive<'a, T>(&'a RangeInclusive<T>);

impl<T: fmt::Display + PartialEq> fmt::Display for DisplayRangeInclusive<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.0.start();
        let end = self.0.end();
        if start == end {
            write!(f, "{start}")
        } else {
            write!(f, "between {start} and {end}")
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum TypeError<'ctx> {
    #[error("type mismatch: found {0} when expected {1}")]
    Mismatch(InferredType<'ctx>, InferredType<'ctx>),
    #[error("type error: {0} is not compatible with {1}")]
    Incompatible(InferredTypeApp<'ctx>, InferredTypeApp<'ctx>),
    #[error("type error: cannot unify {0} and {1}")]
    CannotUnify(InferredType<'ctx>, InferredType<'ctx>),
    #[error("{0}\n  when comparing {1} and {2}")]
    Nested(Box<Self>, InferredType<'ctx>, InferredType<'ctx>),
}

#[derive(Debug)]
pub enum CoalesceError<'ctx> {
    CannotCoalesce(InferredType<'ctx>, Option<InferredType<'ctx>>),
}

impl fmt::Display for CoalesceError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CannotCoalesce(lower, upper) => {
                write!(f, "type is too broad ({lower} <: T <: ")?;
                if let Some(upper) = upper {
                    write!(f, "{upper}")?;
                } else {
                    write!(f, "Any")?;
                }
                write!(f, " for some type T), consider adding type annotations")
            }
        }
    }
}

impl std::error::Error for CoalesceError<'_> {}

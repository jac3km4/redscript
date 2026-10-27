//! Source-to-source conversion from the community redscript dialect (`.reds`) to the REDmod
//! dialect (`.script`). The opposite direction is not supported.
//!
//! Most of the surface syntax differences are handled by the parser and the printer of each
//! dialect. This crate implements the rest: rewrites of constructs that have no direct
//! equivalent in the target dialect (each one a small, separately tested pass over the AST) and
//! diagnostics for the constructs that cannot be converted without type information or manual
//! review.
//!
//! Rewritten nodes keep the span of the node they replace, so the comments attached to them by
//! [`redscript_formatter::parse_document`] survive the conversion. Synthesized helper nodes get a
//! zero-length span, which never matches the span of an original node.
//!
//! # Generated names
//!
//! The AST borrows identifiers as `&'src str`. Names created by the passes (fresh locals,
//! renamed variables) are allocated in a [`NameArena`] owned by the caller, which has to live as
//! long as the source the document was parsed from. This keeps the AST unchanged and avoids
//! leaking memory.

use std::fmt;

use elsa::FrozenVec;
use redscript_ast::{Dialect, SourceModule, SourceType, Span, Type};
use redscript_formatter::Document;

mod diagnostics;
pub mod driver;
mod index;
mod to_redmod;
mod walk;

pub use to_redmod::fields::SourceFields;

#[cfg(test)]
mod test_util;

/// Storage for identifiers generated during a conversion.
///
/// Create it before parsing the document so that it outlives it.
#[derive(Default)]
pub struct NameArena(FrozenVec<String>);

impl NameArena {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&self, name: impl Into<String>) -> &str {
        self.0.push_get(name.into())
    }
}

impl fmt::Debug for NameArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NameArena")
            .field("len", &self.0.len())
            .finish()
    }
}

/// The kind of a reference type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefKind {
    /// `ref<T>`, implicit in REDmod.
    Strong,
    /// `wref<T>`.
    Weak,
    /// `script_ref<T>`.
    Script,
}

/// A description of the type of an expression, provided by a [`TypeQuery`]. It is independent
/// of the compiler so that the conversion does not depend on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeHint {
    /// A primitive type, e.g. `Int32`, `Float`, `String`, `CName`.
    Primitive(String),
    /// A class, by its qualified name.
    Class(String),
    /// A struct, by its qualified name.
    Struct(String),
    /// An enum, by its qualified name.
    Enum(String),
    Array(Box<TypeHint>),
    StaticArray(Box<TypeHint>, usize),
    Ref(RefKind, Box<TypeHint>),
    Variant,
    /// Any other type (functions, type variables...), by its name. Has no AST representation.
    Other(String),
}

impl TypeHint {
    /// The name of a primitive, class, struct or enum type, after stripping references.
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Primitive(name) | Self::Class(name) | Self::Struct(name) | Self::Enum(name) => {
                Some(name)
            }
            Self::Variant => Some("Variant"),
            Self::Ref(_, inner) => inner.name(),
            Self::Array(_) | Self::StaticArray(_, _) | Self::Other(_) => None,
        }
    }

    /// Like [`TypeHint::name`], without the module path.
    pub fn simple_name(&self) -> Option<&str> {
        self.name()
            .map(|name| name.rsplit_once('.').map_or(name, |(_, n)| n))
    }

    /// Converts the hint into a type as written in REDmod sources: strong references are
    /// implicit, `wref<T>` and `script_ref<T>` are kept, arrays are `array<T>` and named types use
    /// their unqualified name. Generated names are allocated in `arena`, nested types get a
    /// zero-length span at the start of `span`. Returns `None` for types with no representation
    /// ([`TypeHint::Other`]).
    pub fn to_ast_type<'src>(
        &self,
        arena: &'src NameArena,
        span: Span,
    ) -> Option<SourceType<'src>> {
        let synth = Span::new(span.start, span.start, span.file);
        let res = match self {
            Self::Primitive(name) | Self::Class(name) | Self::Struct(name) | Self::Enum(name) => {
                let simple = name.rsplit_once('.').map_or(name.as_str(), |(_, n)| n);
                Type::plain(arena.alloc(simple))
            }
            Self::Variant => Type::plain("Variant"),
            Self::Array(elem) => Type::Array(Box::new((elem.to_ast_type(arena, span)?, synth))),
            Self::StaticArray(elem, size) => {
                Type::StaticArray(Box::new((elem.to_ast_type(arena, span)?, synth)), *size)
            }
            Self::Ref(RefKind::Strong, inner) => inner.to_ast_type(arena, span)?,
            Self::Ref(kind, inner) => {
                let name = if *kind == RefKind::Weak {
                    "wref"
                } else {
                    "script_ref"
                };
                Type::Named {
                    name,
                    args: Box::new([(inner.to_ast_type(arena, span)?, synth)]),
                }
            }
            Self::Other(_) => return None,
        };
        Some(res)
    }
}

/// What the type information knows about the method of a base class that a method overrides.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BaseMethodInfo {
    /// Whether the overridden method is native (`import` in REDmod sources).
    pub is_native: bool,
    /// Whether the class declaring the overridden method is native or `importonly`.
    pub class_is_native: bool,
    /// Whether the overridden method takes a `TweakDBID` parameter.
    pub has_tweakdbid_param: bool,
    /// Whether the class declaring the overridden method is declared by the converted sources
    /// rather than by the game scripts.
    pub from_sources: bool,
}

/// Type information for the source being converted, keyed by the exact span of each expression
/// in the original source.
pub trait TypeQuery {
    /// The type of the expression at `span`.
    fn type_of(&self, span: Span) -> Option<TypeHint>;
    /// The type expected by the context of the expression at `span`, e.g. the type of the
    /// parameter an argument is passed to, of the variable a value is assigned to or the return
    /// type of the function a value is returned from.
    fn expected_type_of(&self, span: Span) -> Option<TypeHint>;
    /// Whether the enum `type_name` is declared by the converted sources rather than by the game
    /// scripts. Unknown enums are assumed to be, which only adds a note.
    fn is_declared_in_sources(&self, _type_name: &str) -> bool {
        true
    }
    /// Whether the struct `type_name` (as named by a [`TypeHint`]) declares a static method
    /// `method` taking one parameter of its own type and returning a `String`, e.g.
    /// `GameTime.ToString(self: GameTime) -> String`.
    fn has_static_method(&self, _type_name: &str, _method: &str) -> bool {
        false
    }
    /// Whether the class or struct `class` (as named in the sources, with or without its module
    /// path) or one of its bases has a field `field`, including fields added by other files or
    /// declared by the game scripts. `None` when the class is unknown.
    fn class_has_field(&self, _class: &str, _field: &str) -> Option<bool> {
        None
    }

    /// Whether a base class of the class `class` (qualified, e.g. `Mod.Module.Class`), direct or
    /// not, declares a non-static method `method`. `None` when the class is unknown.
    fn base_has_method(&self, _class: &str, _method: &str) -> Option<bool> {
        None
    }
    /// Whether a base class of the class `class` (qualified), direct or not, declares a `const`
    /// non-static method `method`, which REDmod requires the overrides to be `const` too. `None`
    /// when the class is unknown or the information is not available.
    fn base_method_is_const(&self, _class: &str, _method: &str) -> Option<bool> {
        None
    }
    /// Whether a class declared by the converted sources (in any file) that derives from `class`
    /// declares a non-static method `method`. `None` when the class is unknown.
    fn is_overridden_in_sources(&self, _class: &str, _method: &str) -> Option<bool> {
        None
    }
    /// The non-static method `method` of the nearest base class of the class `class`
    /// (qualified) that declares one, i.e. the method an override of `method` in `class`
    /// overrides. When the base class declares several overloads, the flags are set when one of
    /// them has them. `None` when the class is unknown or no base class declares the method.
    fn base_method_info(&self, _class: &str, _method: &str) -> Option<BaseMethodInfo> {
        None
    }
    /// Whether the class `class` (as named in the sources, with or without its module path) is
    /// `importonly`, which REDmod cannot add members to. `None` when the class is unknown.
    fn is_import_only(&self, _class: &str) -> Option<bool> {
        None
    }
}

#[derive(Default, Clone, Copy)]
pub struct ConvertOptions<'a> {
    /// Type information used in preference to the types recovered from the declarations of the
    /// module.
    pub types: Option<&'a dyn TypeQuery>,
    /// The classes declared by all the files converted together, whose fields with reserved
    /// names are renamed in every file. Only the classes of the module are known without it.
    pub fields: Option<&'a SourceFields>,
}

impl fmt::Debug for ConvertOptions<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ConvertOptions")
            .field("types", &self.types.is_some())
            .field("fields", &self.fields)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Info,
    Warning,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Info => write!(f, "info"),
            Self::Warning => write!(f, "warning"),
        }
    }
}

/// A note about a construct that was rewritten in a way worth reviewing, or that could not be
/// converted automatically.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConvertDiagnostic {
    /// Span of the construct in the source document.
    pub span: Span,
    pub severity: Severity,
    /// A short, stable identifier of the kind of diagnostic.
    pub code: &'static str,
    pub message: String,
}

impl fmt::Display for ConvertDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]: {}", self.severity, self.code, self.message)
    }
}

/// Shared state of the passes of a conversion.
pub(crate) struct Cx<'a, 'src> {
    pub arena: &'src NameArena,
    pub diagnostics: &'a mut Vec<ConvertDiagnostic>,
    pub opts: &'a ConvertOptions<'a>,
}

impl Cx<'_, '_> {
    pub fn warn(&mut self, span: Span, code: &'static str, message: impl Into<String>) {
        self.push(Severity::Warning, span, code, message);
    }

    pub fn info(&mut self, span: Span, code: &'static str, message: impl Into<String>) {
        self.push(Severity::Info, span, code, message);
    }

    fn push(&mut self, severity: Severity, span: Span, code: &'static str, msg: impl Into<String>) {
        self.diagnostics.push(ConvertDiagnostic {
            span,
            severity,
            code,
            message: msg.into(),
        });
    }
}

/// Converts a community document to the REDmod dialect. A document already in the REDmod
/// dialect is returned unchanged.
///
/// Returns the converted document, to be printed with
/// [`print_document`](redscript_formatter::print_document) using [`Dialect::Redmod`], and the
/// diagnostics sorted by position.
pub fn convert<'src>(
    mut doc: Document<'src>,
    opts: &ConvertOptions<'_>,
    arena: &'src NameArena,
) -> (Document<'src>, Vec<ConvertDiagnostic>) {
    let diagnostics = convert_module(&mut doc.module, opts, arena);
    (doc, diagnostics)
}

/// Converts a module in place, without comments. See [`convert`].
pub fn convert_module<'src>(
    module: &mut SourceModule<'src>,
    opts: &ConvertOptions<'_>,
    arena: &'src NameArena,
) -> Vec<ConvertDiagnostic> {
    if module.dialect == Dialect::Redmod {
        return vec![];
    }
    let mut diagnostics = vec![];
    let mut cx = Cx {
        arena,
        diagnostics: &mut diagnostics,
        opts,
    };
    to_redmod::run(module, &mut cx);
    module.dialect = Dialect::Redmod;
    diagnostics.sort_by_key(|d| (d.span.file, d.span.start, d.span.end));
    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, span_of};

    #[test]
    fn type_queries_match_exact_spans() {
        let src = "Scale(1)";
        let arg = span_of(src, "1", 0);
        let float = TypeHint::Primitive("Float".to_owned());
        let types = MapTypeQuery::new()
            .with_type(arg, TypeHint::Primitive("Int32".to_owned()))
            .with_expected(arg, float.clone());
        assert_eq!(types.expected_type_of(arg), Some(float));
        assert_eq!(
            types.type_of(arg).as_ref().and_then(TypeHint::name),
            Some("Int32")
        );
        let wider = Span::new(arg.start - 1, arg.end + 1, arg.file);
        assert_eq!(types.type_of(wider), None);
        assert_eq!(types.expected_type_of(wider), None);
    }

    #[test]
    fn type_hint_names_strip_references() {
        let hint = TypeHint::Ref(RefKind::Weak, TypeHint::Class("Foo".to_owned()).into());
        assert_eq!(hint.name(), Some("Foo"));
        assert_eq!(TypeHint::Variant.name(), Some("Variant"));
        let array = TypeHint::Array(TypeHint::Enum("E".to_owned()).into());
        assert_eq!(array.name(), None);
    }
}

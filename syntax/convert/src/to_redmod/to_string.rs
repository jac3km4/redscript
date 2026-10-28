//! How `ToString(x)` is converted for a value of a known type, shared by the typed intrinsics and
//! the string interpolation.
//!
//! - `String` is used as is, `Int32`, `Float`, `Bool` and `CName` use `IntToString`,
//!   `FloatToString`, `BoolToString` and `NameToString`
//! - `Int8`, `Int16`, `Uint8` and `Uint16` fit an `Int32`: `IntToString(Cast<Int32>(x))`
//! - enums: `EnumValueToString("<Enum>", Cast<Int64>((x as Int32)))`, with the simple name of the
//!   enum (the last segment of its qualified name), which is the name the game registers for its
//!   own enums. The compiler does not tell the name registered at runtime for enums declared by
//!   mods, so an `enum-to-string` note asks to verify it in game.
//! - structs with a static `ToString(self: S) -> String`: `S.ToString(x)`
//!
//! Everything else (`Uint32`, `Int64`, `Uint64` which have no lossless REDmod equivalent, objects,
//! other structs, arrays...) is left alone and reported with a message naming the type.

use std::borrow::Cow;

use redscript_ast::{Constant, Expr, Span};

use crate::walk::{SExpr, call, ident, plain_type, synth};
use crate::{NameArena, RefKind, TypeHint, TypeQuery};

/// What to do with a `ToString(x)` whose argument has a known type.
#[derive(Debug, PartialEq, Eq)]
pub enum Plan {
    /// `x`
    Identity,
    /// `Func(x)`
    Func(&'static str),
    /// `IntToString(Cast<Int32>(x))`
    SmallInt,
    /// `EnumValueToString("<name>", Cast<Int64>((x as Int32)))`, with whether the enum is declared
    /// by the converted sources, which warrants a note
    Enum(String, bool),
    /// `<name>.ToString(x)`
    Static(String),
    /// Not converted, with what to tell about the value.
    Warn(String),
}

fn simple(name: &str) -> &str {
    name.rsplit_once('.').map_or(name, |(_, n)| n)
}

/// The type as shown in messages.
fn describe(typ: &TypeHint) -> String {
    match typ {
        TypeHint::Primitive(n) | TypeHint::Class(n) | TypeHint::Struct(n) | TypeHint::Enum(n) => {
            simple(n).to_owned()
        }
        TypeHint::Variant => "Variant".to_owned(),
        TypeHint::Array(elem) => format!("array<{}>", describe(elem)),
        TypeHint::StaticArray(elem, size) => format!("[{}; {size}]", describe(elem)),
        TypeHint::Ref(kind, inner) => {
            let kind = match kind {
                RefKind::Strong => "ref",
                RefKind::Weak => "wref",
                RefKind::Script => "script_ref",
            };
            format!("{kind}<{}>", describe(inner))
        }
        TypeHint::Other(n) => n.clone(),
    }
}

/// Decides how to convert `ToString(x)` for a value of type `typ`.
pub fn plan(types: &dyn TypeQuery, typ: &TypeHint) -> Plan {
    let described = describe(typ);
    match typ {
        TypeHint::Primitive(name) => match simple(name) {
            "String" => Plan::Identity,
            "Int32" => Plan::Func("IntToString"),
            "Float" => Plan::Func("FloatToString"),
            "Bool" => Plan::Func("BoolToString"),
            "CName" => Plan::Func("NameToString"),
            "Int8" | "Int16" | "Uint8" | "Uint16" => Plan::SmallInt,
            "Uint32" | "Int64" | "Uint64" => Plan::Warn(format!(
                "this value has type `{described}`: there is no lossless REDmod equivalent \
                 (`IntToString(Cast<Int32>(x))` is wrong above 2^31), convert it manually"
            )),
            _ => Plan::Warn(generic(&described)),
        },
        TypeHint::Enum(name) => {
            Plan::Enum(simple(name).to_owned(), types.is_declared_in_sources(name))
        }
        TypeHint::Struct(name) if types.has_static_method(name, "ToString") => {
            Plan::Static(simple(name).to_owned())
        }
        TypeHint::Struct(_) => Plan::Warn(format!(
            "this value is a `{described}` struct with no static `ToString` in the game \
             scripts: print its fields with `IntToString`, `FloatToString`, etc."
        )),
        TypeHint::Class(_) | TypeHint::Ref(RefKind::Strong | RefKind::Weak, _) => {
            let class = match typ {
                TypeHint::Ref(_, inner) => describe(inner),
                _ => described,
            };
            Plan::Warn(format!(
                "this value is an object of type `{class}`, REDmod cannot print objects: \
                 use `NameToString(x.GetClassName())` for its class name"
            ))
        }
        _ => Plan::Warn(generic(&described)),
    }
}

fn generic(described: &str) -> String {
    format!(
        "this value is of type `{described}`: use the matching conversion function, e.g. \
         `IntToString`, `FloatToString`, `BoolToString` or `NameToString`"
    )
}

/// What to tell about a value whose type could not be determined by the compiler.
const UNTYPED: &str = "the type of this expression could not be determined (its statement \
     may have failed to type-check): use `IntToString`, `FloatToString`, `BoolToString`, \
     `NameToString`, etc. depending on the type";

/// The message of the note about the use of `EnumValueToString` for `name`.
pub fn enum_note(name: &str) -> String {
    format!(
        "relies on `{name}` being registered at runtime under this name; verify in game for \
         mod-declared enums (under REDmod it may be registered as `<Module>.{name}` when the \
         file has `@module`)"
    )
}

fn cast<'src>(target: &'src str, arg: SExpr<'src>, span: Span) -> SExpr<'src> {
    call(
        ident("Cast", synth(span)),
        vec![plain_type(target, synth(span))],
        vec![arg],
        synth(span),
    )
}

/// Builds the conversion of `arg` for a plan that rewrites, with `span` for the call. Gives `arg`
/// back for a plan that does not.
pub fn build<'src>(
    plan: Plan,
    arg: SExpr<'src>,
    span: Span,
    arena: &'src NameArena,
) -> Result<SExpr<'src>, SExpr<'src>> {
    let syn = synth(span);
    Ok(match plan {
        Plan::Identity => arg,
        Plan::Func(func) => call(ident(func, syn), vec![], vec![arg], span),
        Plan::SmallInt => call(
            ident("IntToString", syn),
            vec![],
            vec![cast("Int32", arg, span)],
            span,
        ),
        Plan::Enum(name, _) => {
            let value = (
                Expr::DynCast {
                    expr: Box::new(arg),
                    typ: Box::new(plain_type("Int32", syn)),
                },
                syn,
            );
            call(
                ident("EnumValueToString", syn),
                vec![],
                vec![
                    (Expr::Constant(Constant::String(Cow::Owned(name))), syn),
                    cast("Int64", value, span),
                ],
                span,
            )
        }
        Plan::Static(name) => {
            let name: &'src str = arena.alloc(name);
            let callee = (
                Expr::Member {
                    expr: Box::new(ident(name, syn)),
                    member: "ToString",
                },
                syn,
            );
            call(callee, vec![], vec![arg], span)
        }
        Plan::Warn(_) => return Err(arg),
    })
}

/// The warning about a `ToString(..)` call that was left alone. Without type information, or
/// with a call that is not `ToString(x)`, the message is the generic one.
pub fn call_warning(types: Option<&dyn TypeQuery>, arg: Option<Span>) -> String {
    const GENERIC: &str = "REDmod has no generic ToString(..): use `x.ToString()`, \
                           `IntToString`, `FloatToString`, `BoolToString`, etc. depending on the \
                           type";
    match (types, arg) {
        (Some(types), Some(arg)) => match detail(types, arg) {
            Some(detail) => format!("REDmod has no generic ToString(..); {detail}"),
            None => GENERIC.to_owned(),
        },
        _ => GENERIC.to_owned(),
    }
}

/// The warning about an interpolated value left behind a `ToString(..)` placeholder. Without type
/// information the message is the generic one.
pub fn interpolation_warning(types: Option<&dyn TypeQuery>, part: Span) -> String {
    const GENERIC: &str = "the type of this interpolated value is unknown, replace the \
                           `ToString(..)` placeholder with `IntToString`, `FloatToString`, \
                           `x.ToString()`, etc.";
    match types.and_then(|types| detail(types, part)) {
        Some(detail) => format!("replace the `ToString(..)` placeholder: {detail}"),
        None => GENERIC.to_owned(),
    }
}

/// What to tell about the value at `span`, when `types` can be asked about it.
fn detail(types: &dyn TypeQuery, span: Span) -> Option<String> {
    match types.type_of(span) {
        None => Some(UNTYPED.to_owned()),
        Some(typ) => match plan(types, &typ) {
            Plan::Warn(detail) => Some(detail),
            _ => None,
        },
    }
}

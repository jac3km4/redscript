//! Suffixed integer literals: `1u` -> `Cast<Uint32>(1)`, `1ul` -> `Cast<Uint64>(1)`,
//! `1l` -> `Cast<Int64>(1)`.
//!
//! REDmod field initializers only accept literals, so there the suffix is dropped instead
//! (`0u` -> `0`, a plain literal is accepted for any integer field), empty array initializers
//! are dropped (arrays start empty) and other initializers are reported.
//!
//! REDmod does not compile initializers on `@addField` fields at all: those with the default
//! value of their type are dropped, others are reported.

use redscript_ast::{Constant, Expr, Item, SourceExpr, SourceModule, UnOp};

use crate::Cx;
use crate::walk::{
    MutVisitor, SExpr, call, for_each_decl, for_each_function, ident, int, plain_type, synth,
    walk_body, walk_expr,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            walk_body(&mut Suffixes { cx: &mut *cx }, body);
        }
    });
    for_each_decl(module, |(decl, _)| {
        let added = decl
            .annotations
            .iter()
            .any(|(ann, _)| ann.name == "addField");
        if let Item::Let(field) = &mut decl.item
            && let Some(default) = &mut field.default
        {
            let keep = if added {
                added_field_default(cx, field.name.0, default)
            } else {
                field_default(cx, field.name.0, default)
            };
            if !keep {
                field.default = None;
            }
        }
    });
}

/// Handles the initializer of a field added with `@addField`, returns whether it should be kept.
///
/// REDmod fails to compile a mod with any initializer on an `@addField` field, and reports no
/// error for it (only `Compile mod scripts failed!`). A field starts with the default value of
/// its type, so an initializer with that value is dropped; others are reported.
fn added_field_default<'src>(cx: &mut Cx<'_, 'src>, name: &str, default: &mut SExpr<'src>) -> bool {
    if is_default_value(&default.0) {
        cx.info(
            default.1,
            "added-field-initializer",
            format!(
                "REDmod does not compile initializers on @addField fields, the initializer of \
                 `{name}` is the default value of its type and was dropped"
            ),
        );
        return false;
    }
    field_default(cx, name, default);
    cx.warn(
        default.1,
        "added-field-initializer",
        format!(
            "REDmod does not compile initializers on @addField fields (the compile fails with \
             no error message): remove it and initialize `{name}` in a method instead"
        ),
    );
    true
}

/// Whether `expr` is a literal of the default value of its type: a zero, `false`, `""` or `[]`.
fn is_default_value(expr: &SourceExpr<'_>) -> bool {
    let is_zero = |constant: &Constant<'_>| match constant {
        Constant::I32(v) => *v == 0,
        Constant::I64(v) => *v == 0,
        Constant::U32(v) => *v == 0,
        Constant::U64(v) => *v == 0,
        Constant::F32(v) => *v == 0.0,
        Constant::F64(v) => *v == 0.0,
        _ => false,
    };
    match expr {
        Expr::UnOp {
            op: UnOp::Neg,
            expr,
        } => matches!(&expr.0, Expr::Constant(constant) if is_zero(constant)),
        Expr::Constant(Constant::Bool(value)) => !value,
        Expr::Constant(Constant::String(value)) => value.is_empty(),
        Expr::Constant(constant) => is_zero(constant),
        Expr::ArrayLit(elems) => elems.is_empty(),
        _ => false,
    }
}

/// Rewrites the initializer of a field, returns whether it should be kept.
fn field_default<'src>(cx: &mut Cx<'_, 'src>, name: &str, (expr, span): &mut SExpr<'src>) -> bool {
    let literal = match expr {
        Expr::UnOp {
            op: UnOp::Neg,
            expr,
        } => &mut expr.0,
        other => other,
    };
    // the minimum values have no literal: the printer writes them as `(-2147483647 - 1)` and
    // `(-9223372036854775807l - 1l)`, which REDmod does not accept in a field initializer
    if matches!(
        literal,
        Expr::Constant(Constant::I32(i32::MIN) | Constant::I64(i64::MIN))
    ) {
        cx.warn(
            *span,
            "int-literal-range",
            format!(
                "the initializer of field `{name}` is the minimum integer value, which has no \
                 literal, and REDmod only accepts plain literals in field initializers: \
                 initialize `{name}` in a method instead"
            ),
        );
        return true;
    }
    let suffixed = match literal {
        Expr::Constant(Constant::U32(v)) => Some(i32::try_from(*v).ok()),
        Expr::Constant(Constant::U64(v)) => Some(i32::try_from(*v).ok()),
        Expr::Constant(Constant::I64(v)) => Some(i32::try_from(*v).ok()),
        _ => None,
    };
    match (suffixed, literal) {
        (Some(Some(value)), literal) => {
            *literal = Expr::Constant(Constant::I32(value));
            true
        }
        (Some(None), _) => {
            cx.warn(
                *span,
                "int-literal-range",
                format!(
                    "the initializer of field `{name}` does not fit in an Int32, and REDmod only \
                     accepts plain literals in field initializers"
                ),
            );
            true
        }
        (None, Expr::Constant(_)) => true,
        (None, Expr::ArrayLit(elems)) if elems.is_empty() => {
            cx.info(
                *span,
                "field-initializer",
                format!(
                    "REDmod only accepts literals in field initializers, the empty array \
                     initializer of `{name}` was dropped"
                ),
            );
            false
        }
        (None, _) => {
            cx.warn(
                *span,
                "field-initializer",
                format!(
                    "REDmod only accepts literals in field initializers, initialize `{name}` in \
                     a method instead"
                ),
            );
            true
        }
    }
}

struct Suffixes<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
}

impl<'src> MutVisitor<'src> for Suffixes<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let span = expr.1;
        let (typ, value) = match &expr.0 {
            Expr::Constant(Constant::U32(v)) => ("Uint32", i32::try_from(*v).ok()),
            Expr::Constant(Constant::U64(v)) => ("Uint64", i32::try_from(*v).ok()),
            Expr::Constant(Constant::I64(v)) => ("Int64", i32::try_from(*v).ok()),
            _ => return,
        };
        let Some(value) = value else {
            self.cx.warn(
                span,
                "int-literal-range",
                format!("this {typ} literal does not fit in an Int32 and cannot be cast"),
            );
            return;
        };
        let syn = synth(span);
        *expr = call(
            ident("Cast", syn),
            vec![plain_type(typ, syn)],
            vec![int(value, syn)],
            span,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn suffixed_literals_become_casts() {
        let (out, diags) = to_redmod(
            "func F() {\n  let a: Uint32 = 1u;\n  let b: Uint64 = 2ul;\n  let c: Int64 = 3l;\n  let d: Uint64 = 4294967296ul;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("let a: Uint32 = Cast<Uint32>(1);"), "{out}");
        assert!(out.contains("let b: Uint64 = Cast<Uint64>(2);"), "{out}");
        assert!(out.contains("let c: Int64 = Cast<Int64>(3);"), "{out}");
        assert_eq!(codes(&diags), ["int-literal-range"]);
    }

    #[test]
    fn field_initializers_keep_plain_literals() {
        let (out, diags) = to_redmod(
            "class C {\n  let a: Uint32 = 0u;\n  let b: Int64 = -2l;\n  let c: Uint64 = 4294967296ul;\n  let d: Bool = false;\n  let e: array<Int32> = [];\n  let f: E = E.A;\n  let g: Float = -1.0;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("let a: Uint32 = 0;"), "{out}");
        assert!(out.contains("let b: Int64 = -2;"), "{out}");
        assert!(out.contains("let c: Uint64 = 4294967296ul;"), "{out}");
        assert!(out.contains("let d: Bool = false;"), "{out}");
        assert!(out.contains("let e: array<Int32>;"), "{out}");
        assert!(out.contains("let f: E = E.A;"), "{out}");
        assert!(out.contains("let g: Float = -1.0;"), "{out}");
        assert_eq!(
            codes(&diags),
            [
                "int-literal-range",
                "field-initializer",
                "field-initializer"
            ]
        );
        assert_eq!(diags[2].severity, crate::Severity::Warning);
    }

    #[test]
    fn added_field_initializers() {
        // Retrothrusters_QoL: `@addField(AirThrustersDecisions) let m_lastThrusterRefireTime:
        // Float = 0.0;` made REDmod's whole-mod compile fail with no error message
        let (out, diags) = to_redmod(
            r#"@addField(A)
let a: Float = 0.0;

@addField(A)
let b: Uint32 = 0u;

@addField(A)
let c: Bool = false;

@addField(A)
let d: array<Int32> = [];

@addField(A)
let e: String = "";

@addField(A)
let f: Float = -0.0;

@addField(A)
let g: Uint32 = 1u;

@addField(A)
let h: Bool = true;

@addField(A)
let i: CName = n"";

class C {
  let j: Float = 0.0;
}
"#,
            run,
        );
        let out = squash(&out);
        for expected in [
            "let a: Float;",
            "let b: Uint32;",
            "let c: Bool;",
            "let d: array<Int32>;",
            "let e: String;",
            "let f: Float;",
            // not the default value: kept, with a warning
            "let g: Uint32 = 1;",
            "let h: Bool = true;",
            r#"let i: CName = n"";"#,
            // the classes of the mod accept literal initializers
            "let j: Float = 0.0;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["added-field-initializer"; 9]);
        let warnings = diags
            .iter()
            .filter(|d| d.severity == crate::Severity::Warning)
            .count();
        assert_eq!(warnings, 3, "{diags:?}");
    }

    #[test]
    fn field_initializers_with_minimum_values_are_reported() {
        let (out, diags) = to_redmod(
            "class C {
  let a: Int32 = -2147483648;
  let b: Int64 = -9223372036854775808l;
  let c: Int32 = -2147483647;
}
",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("let c: Int32 = -2147483647;"), "{out}");
        assert_eq!(codes(&diags), ["int-literal-range", "int-literal-range"]);
        assert!(
            diags
                .iter()
                .all(|d| d.message.contains("minimum integer value"))
        );
        assert!(diags.iter().all(|d| d.severity == crate::Severity::Warning));
    }
}

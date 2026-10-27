//! Suffixed integer literals: `1u` -> `Cast<Uint32>(1)`, `1ul` -> `Cast<Uint64>(1)`,
//! `1l` -> `Cast<Int64>(1)`.
//!
//! REDmod field initializers only accept literals, so there the suffix is dropped instead
//! (`0u` -> `0`, a plain literal is accepted for any integer field), empty array initializers
//! are dropped (arrays start empty) and other initializers are reported.

use redscript_ast::{Constant, Expr, Item, SourceModule, UnOp};

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
        if let Item::Let(field) = &mut decl.item
            && let Some(default) = &mut field.default
            && !field_default(cx, field.name.0, default)
        {
            field.default = None;
        }
    });
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
            "class C {\n  let a: Uint32 = 0u;\n  let b: Int64 = -2l;\n  let c: Uint64 = 4294967296ul;\n  let d: Bool = false;\n  let e: array<Int32> = [];\n  let f: E = E.A;\n  let g: Float = -1.0;\n}\n\n@addField(D)\nlet h: Uint32 = 1u;\n",
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
        assert!(out.contains("let h: Uint32 = 1;"), "{out}");
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

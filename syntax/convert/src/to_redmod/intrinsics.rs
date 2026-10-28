//! Community intrinsics with an operator form in REDmod:
//! - `Equals(a, b)` -> `a == b`, `NotEquals(a, b)` -> `a != b`
//! - `EnumInt(e)` -> `e as Int32`
//! - `IntEnum<T>(i)` -> `i as T`
//! - `ToVariant(x)` -> `x as Variant`
//! - `FromVariant<T>(v)` -> `v as T`
//!
//! `IntEnum`/`FromVariant` without a type argument take the target type from a typed `let`, a
//! `return` or an assignment to a typed local, otherwise from the type information expected for
//! the call. Otherwise a diagnostic is emitted.

use redscript_ast::{BinOp, Expr, SourceModule, Span, Stmt};

use crate::index::LocalTypes;
use crate::walk::{
    MutVisitor, SExpr, SStmt, SType, for_each_function, free_call_name, plain_type, synth,
    walk_body, walk_expr, walk_stmt,
};
use crate::{Cx, RefKind, TypeHint};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        let locals = LocalTypes::of(func);
        let return_type = func.return_type.as_deref().cloned();
        if let Some(body) = &mut func.body {
            let mut visitor = Intrinsics {
                cx: &mut *cx,
                locals: &locals,
                return_type,
            };
            walk_body(&mut visitor, body);
        }
    });
}

struct Intrinsics<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    locals: &'a LocalTypes<'src>,
    return_type: Option<SType<'src>>,
}

impl<'src> Intrinsics<'_, '_, 'src> {
    /// The target type of `IntEnum(..)`/`FromVariant(..)` from the type expected for the call.
    fn expected_target(&self, name: &str, span: Span) -> Option<SType<'src>> {
        let hint = self.cx.opts.types?.expected_type_of(span)?;
        let valid = match (&hint, name) {
            (TypeHint::Enum(_), _) => true,
            (_, "IntEnum") => false,
            (TypeHint::Primitive(name), _) => name != "Void",
            (TypeHint::Struct(_) | TypeHint::Class(_) | TypeHint::Variant, _) => true,
            (TypeHint::Ref(RefKind::Strong, inner), _) => matches!(**inner, TypeHint::Class(_)),
            _ => false,
        };
        let syn = synth(span);
        valid.then(|| Some((hint.to_ast_type(self.cx.arena, syn)?, syn)))?
    }
}

/// Fills in the type argument of `IntEnum(..)`/`FromVariant(..)` from the expected type.
fn infer_type_arg<'src>(expr: &mut SExpr<'src>, expected: Option<&SType<'src>>) {
    let Some(expected) = expected else {
        return;
    };
    if let Expr::Call {
        expr: callee,
        type_args,
        ..
    } = &mut expr.0
        && matches!(callee.0, Expr::Ident("IntEnum" | "FromVariant"))
        && type_args.is_empty()
    {
        *type_args = [expected.clone()].into();
    }
}

impl<'src> MutVisitor<'src> for Intrinsics<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &mut stmt.0 {
            Stmt::Let {
                typ: Some(typ),
                value: Some(value),
                ..
            } => infer_type_arg(value, Some(typ)),
            Stmt::Return(Some(value)) => infer_type_arg(value, self.return_type.as_ref()),
            Stmt::Expr(expr) => {
                if let Expr::Assign { lhs, rhs } = &mut expr.0
                    && let Expr::Ident(name) = lhs.0
                {
                    infer_type_arg(rhs, self.locals.get(name));
                }
            }
            _ => {}
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let Some(name) = free_call_name(&expr.0) else {
            return;
        };
        let span = expr.1;
        let Expr::Call {
            type_args, args, ..
        } = &mut expr.0
        else {
            return;
        };
        let syn = synth(span);
        let replacement = match (name, &mut args[..]) {
            ("Equals" | "NotEquals", [lhs, rhs]) => Expr::BinOp {
                lhs: Box::new(std::mem::replace(lhs, (Expr::Error, syn))),
                op: if name == "Equals" {
                    BinOp::Eq
                } else {
                    BinOp::Ne
                },
                rhs: Box::new(std::mem::replace(rhs, (Expr::Error, syn))),
            },
            ("EnumInt" | "ToVariant", [arg]) => Expr::DynCast {
                expr: Box::new(std::mem::replace(arg, (Expr::Error, syn))),
                typ: Box::new(plain_type(
                    if name == "EnumInt" {
                        "Int32"
                    } else {
                        "Variant"
                    },
                    syn,
                )),
            },
            ("IntEnum" | "FromVariant", [arg]) => {
                let Some(typ) = type_args
                    .first()
                    .cloned()
                    .or_else(|| self.expected_target(name, span))
                else {
                    self.cx.warn(
                        span,
                        "cast-target",
                        format!(
                            "the target type of {name}(..) could not be determined, convert it \
                             to `x as Type` manually"
                        ),
                    );
                    return;
                };
                Expr::DynCast {
                    expr: Box::new(std::mem::replace(arg, (Expr::Error, syn))),
                    typ: Box::new(typ),
                }
            }
            _ => return,
        };
        expr.0 = replacement;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn equality_intrinsics_become_operators() {
        let (out, _) = to_redmod(
            "func F(a: Int32, b: Int32) -> Bool {\n  return Equals(a, b) && NotEquals(a + 1, b);\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("return a == b && a + 1 != b;"), "{out}");
    }

    #[test]
    fn enum_and_variant_intrinsics_become_casts() {
        let (out, diags) = to_redmod(
            "func F(e: EState, v: Variant) -> EState {\n  let i: Int32 = EnumInt(e);\n  let x: Variant = ToVariant(i + 1);\n  let s: EState = IntEnum<EState>(i);\n  let t: EState = IntEnum(i);\n  let f: Float = FromVariant(v);\n  let g = FromVariant<Int32>(v);\n  s = IntEnum(1);\n  Log(IntEnum(2));\n  return IntEnum(0);\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("let i: Int32 = e as Int32;"), "{out}");
        assert!(
            out.contains("let x: Variant = (i + 1) as Variant;"),
            "{out}"
        );
        assert!(out.contains("let s: EState = i as EState;"), "{out}");
        assert!(out.contains("let t: EState = i as EState;"), "{out}");
        assert!(out.contains("let f: Float = v as Float;"), "{out}");
        assert!(out.contains("let g = v as Int32;"), "{out}");
        assert!(out.contains("s = 1 as EState;"), "{out}");
        assert!(out.contains("Log(IntEnum(2));"), "{out}");
        assert!(out.contains("return 0 as EState;"), "{out}");
        assert_eq!(codes(&diags), ["cast-target"]);
    }

    #[test]
    fn cast_targets_from_type_information() {
        let src = "func F(i: Int32, v: Variant) {
  this.state = IntEnum(i);
  Take(FromVariant(v));
  Other(FromVariant(v));
  Log(IntEnum(2));
  Again(IntEnum(3));
  Void(FromVariant(v));
}
";
        let enum_ = TypeHint::Enum("Mod.EState".to_owned());
        let types = MapTypeQuery::new()
            .with_expected(span_of(src, "IntEnum(i)", 0), enum_.clone())
            .with_expected(
                span_of(src, "FromVariant(v)", 0),
                TypeHint::Ref(RefKind::Strong, TypeHint::Class("A".to_owned()).into()),
            )
            .with_expected(
                span_of(src, "FromVariant(v)", 1),
                TypeHint::Array(TypeHint::Primitive("Int32".to_owned()).into()),
            )
            // not an enum
            .with_expected(
                span_of(src, "IntEnum(3)", 0),
                TypeHint::Primitive("Int32".to_owned()),
            )
            .with_expected(
                span_of(src, "FromVariant(v)", 2),
                TypeHint::Primitive("Void".to_owned()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "this.state = i as EState;",
            "Take(v as A);",
            "Other(FromVariant(v));",
            "Log(IntEnum(2));",
            "Again(IntEnum(3));",
            "Void(FromVariant(v));",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["cast-target"; 4]);
    }
}

//! Intrinsics whose REDmod form depends on the type of their argument, rewritten only when type
//! information answers for the argument:
//! - `IsDefined(x)` used directly as an `if`/`else if`/`while` condition, with `x` an object or a
//!   `wref`: `if (x)`
//! - `IsDefined(x)` anywhere, with `x` an `EntityID`, a `GlobalNodeRef` or a `TweakDBID`:
//!   `EntityID.IsDefined(x)`, `GlobalNodeRef.IsDefined(x)`, `TDBID.IsValid(x)`
//! - `ToString(x)`, by the type of `x` (see [`super::to_string`]): `IntToString(x)`,
//!   `FloatToString(x)`, `BoolToString(x)`, `NameToString(x)`, `x`, `IntToString(Cast<Int32>(x))`,
//!   `EnumValueToString("E", Cast<Int64>((x as Int32)))` or `S.ToString(x)`
//! - `Deref(x)` with `x` a `script_ref`: `x`
//!
//! This pass runs before the diagnostics, which keep reporting the calls left untouched.
//! Rewritten calls keep their span, so the type recorded for the call (rather than the one of its
//! argument) stays attached to the node replacing it.

use redscript_ast::{Expr, LetCondition, SourceModule, Stmt};

use super::to_string::{self, Plan};
use crate::walk::{
    MutVisitor, SExpr, SStmt, for_each_function, free_call_name, ident, synth, walk_body,
    walk_expr, walk_stmt,
};
use crate::{Cx, RefKind, TypeHint, TypeQuery};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let Some(types) = cx.opts.types else {
        return;
    };
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            walk_body(
                &mut TypedIntrinsics {
                    types,
                    cx: &mut *cx,
                },
                body,
            );
        }
    });
}

struct TypedIntrinsics<'a, 'b, 'src> {
    types: &'a dyn TypeQuery,
    cx: &'a mut Cx<'b, 'src>,
}

/// The only argument of a call to the intrinsic `name`.
fn sole_arg<'a, 'src>(expr: &'a SExpr<'src>, name: &str) -> Option<&'a SExpr<'src>> {
    if free_call_name(&expr.0) != Some(name) {
        return None;
    }
    match &expr.0 {
        Expr::Call {
            type_args, args, ..
        } if type_args.is_empty() => match &args[..] {
            [arg] => Some(arg),
            _ => None,
        },
        _ => None,
    }
}

/// Replaces a call with its only argument, keeping the span of the call.
fn unwrap_call(expr: &mut SExpr<'_>) {
    let Expr::Call { args, .. } = &mut expr.0 else {
        return;
    };
    let mut args = std::mem::take(args).into_vec();
    expr.0 = args.remove(0).0;
}

/// Replaces a call with a call to `Type.method` with the same arguments.
fn to_static_call<'src>(expr: &mut SExpr<'src>, typ: &'src str, method: &'src str) {
    let span = expr.1;
    let Expr::Call { expr: callee, .. } = &mut expr.0 else {
        return;
    };
    let syn = synth(span);
    callee.0 = Expr::Member {
        expr: Box::new(ident(typ, syn)),
        member: method,
    };
    callee.1 = syn;
}

impl<'src> TypedIntrinsics<'_, '_, 'src> {
    fn is_object(&self, arg: &SExpr<'_>) -> bool {
        matches!(
            self.types.type_of(arg.1),
            Some(TypeHint::Class(_) | TypeHint::Ref(RefKind::Strong | RefKind::Weak, _))
        )
    }

    fn rewrite_condition(&self, cond: &mut LetCondition<'_, redscript_ast::WithSpan>) {
        if let LetCondition::Expr(expr) = cond
            && sole_arg(expr, "IsDefined").is_some_and(|arg| self.is_object(arg))
        {
            unwrap_call(expr);
        }
    }

    /// Rewrites `ToString(x)` when `typ`, the type of `x`, has a REDmod equivalent.
    fn rewrite_to_string(&mut self, expr: &mut SExpr<'src>, typ: &TypeHint) {
        let plan = to_string::plan(self.types, typ);
        if matches!(plan, Plan::Warn(_)) {
            return;
        }
        let span = expr.1;
        let Expr::Call { args, .. } = &mut expr.0 else {
            return;
        };
        let arg = std::mem::take(args).into_vec().remove(0);
        let note = match &plan {
            Plan::Enum(name, true) => Some(to_string::enum_note(name)),
            _ => None,
        };
        let Ok(rewritten) = to_string::build(plan, arg, span, self.cx.arena) else {
            return;
        };
        expr.0 = rewritten.0;
        if let Some(note) = note {
            self.cx.info(span, "enum-to-string", note);
        }
    }
}

impl<'src> MutVisitor<'src> for TypedIntrinsics<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &mut stmt.0 {
            Stmt::If { blocks, .. } => {
                for block in blocks.iter_mut() {
                    self.rewrite_condition(&mut block.condition);
                }
            }
            Stmt::While(block) => self.rewrite_condition(&mut block.condition),
            _ => {}
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let Some(name) = free_call_name(&expr.0) else {
            return;
        };
        let Some(arg) = sole_arg(expr, name) else {
            return;
        };
        let Some(typ) = self.types.type_of(arg.1) else {
            return;
        };
        let value_type = match &typ {
            TypeHint::Primitive(_) | TypeHint::Struct(_) => typ.simple_name(),
            _ => None,
        };
        match (name, value_type) {
            ("IsDefined", Some("EntityID")) => to_static_call(expr, "EntityID", "IsDefined"),
            ("IsDefined", Some("GlobalNodeRef")) => {
                to_static_call(expr, "GlobalNodeRef", "IsDefined");
            }
            ("IsDefined", Some("TweakDBID")) => to_static_call(expr, "TDBID", "IsValid"),
            ("ToString", _) => self.rewrite_to_string(expr, &typ),
            ("Deref", _) if matches!(typ, TypeHint::Ref(RefKind::Script, _)) => unwrap_call(expr),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics;
    use crate::test_util::{
        MapTypeQuery, codes, span_in, span_of, squash, to_redmod, to_redmod_with_types,
    };

    fn class(name: &str) -> TypeHint {
        TypeHint::Class(name.to_owned())
    }

    fn prim(name: &str) -> TypeHint {
        TypeHint::Primitive(name.to_owned())
    }

    /// Runs the pass followed by the diagnostics, as the conversion does.
    fn run_with_diagnostics<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
        run(module, cx);
        diagnostics::community(module, cx);
    }

    #[test]
    fn is_defined_on_objects_in_conditions() {
        let src =
            "func F(a: ref<A>, b: wref<A>, c: ref<A>, d: ref<A>, e: ref<A>, s: script_ref<A>) {
  if IsDefined(a) {
  } else if IsDefined(b) {
  }
  while IsDefined(c) {
  }
  if !IsDefined(d) {
  }
  let x = IsDefined(e);
  if IsDefined(s) {
  }
}
";
        let types = MapTypeQuery::new()
            .with_type(span_in(src, "(a)", 0, "a"), class("A"))
            .with_type(
                span_in(src, "(b)", 0, "b"),
                TypeHint::Ref(RefKind::Weak, class("A").into()),
            )
            .with_type(
                span_in(src, "(c)", 0, "c"),
                TypeHint::Ref(RefKind::Strong, class("A").into()),
            )
            .with_type(span_in(src, "(d)", 0, "d"), class("A"))
            .with_type(span_in(src, "(e)", 0, "e"), class("A"))
            .with_type(
                span_in(src, "(s)", 0, "s"),
                TypeHint::Ref(RefKind::Script, class("A").into()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        for expected in [
            "if (a) { } else if (b) { }",
            "while (c) { }",
            // the truthiness of `!obj` is not documented for REDmod
            "if (!IsDefined(d))",
            // only conditions
            "let x = IsDefined(e);",
            "if (IsDefined(s))",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["is-defined"; 3]);
    }

    #[test]
    fn is_defined_on_ids() {
        let src = "func F(e: EntityID, n: GlobalNodeRef, t: TweakDBID, v: Vector4) -> Bool {
  let a = IsDefined(e);
  if IsDefined(n) {
  }
  return IsDefined(t) && IsDefined(v);
}
";
        let strukt = |name: &str| TypeHint::Struct(name.to_owned());
        let types = MapTypeQuery::new()
            .with_type(span_in(src, "(e)", 0, "e"), strukt("EntityID"))
            .with_type(span_in(src, "(n)", 0, "n"), strukt("GlobalNodeRef"))
            .with_type(span_in(src, "(t)", 0, "t"), prim("TweakDBID"))
            .with_type(span_in(src, "(v)", 0, "v"), strukt("Vector4"));
        let (out, diags) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        for expected in [
            "let a = EntityID.IsDefined(e);",
            "if (GlobalNodeRef.IsDefined(n))",
            "return TDBID.IsValid(t) && IsDefined(v);",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["is-defined"]);
    }

    #[test]
    fn to_string_by_type() {
        let src = "func F(i: Int32, f: Float, b: Bool, n: CName, s: String, l: Int64, o: ref<A>) {
  Log(ToString(i));
  Log(ToString(f));
  Log(ToString(b));
  Log(ToString(n));
  Log(ToString(s));
  Log(ToString(l));
  Log(ToString(o));
}
";
        let mut types = MapTypeQuery::new();
        for (name, typ) in [
            ("i", prim("Int32")),
            ("f", prim("Float")),
            ("b", prim("Bool")),
            ("n", prim("CName")),
            ("s", prim("String")),
            ("l", prim("Int64")),
            ("o", class("A")),
        ] {
            let arg = span_in(src, &format!("({name})"), 0, name);
            types = types.with_type(arg, typ);
        }
        let (out, diags) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        for expected in [
            "Log(IntToString(i));",
            "Log(FloatToString(f));",
            "Log(BoolToString(b));",
            "Log(NameToString(n));",
            "Log(s);",
            "Log(ToString(l));",
            "Log(ToString(o));",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["to-string"; 2]);
    }

    /// Converts `Log(ToString(a));` for an argument `a` of each given type, with the types
    /// answered by `types` for `(a)`.
    fn to_string_of(
        ty: &str,
        hint: Option<TypeHint>,
        statics: &[&str],
    ) -> (String, Vec<crate::ConvertDiagnostic>) {
        let src = format!("func F(a: {ty}) {{\n  Log(ToString(a));\n}}\n");
        let mut types = MapTypeQuery::new();
        if let Some(hint) = hint {
            types = types.with_type(span_in(&src, "(a)", 0, "a"), hint);
        }
        for name in statics {
            types = types.with_static_method(name, "ToString");
        }
        let (out, diags) = to_redmod_with_types(&src, &types, run_with_diagnostics);
        // the output is valid REDmod
        let id = crate::test_util::test_file_id();
        let (doc, errors) =
            redscript_formatter::parse_document(&out, id, redscript_ast::Dialect::Redmod);
        assert!(doc.is_some() && errors.is_empty(), "{errors:?}\n{out}");
        (squash(&out), diags)
    }

    #[test]
    fn to_string_of_enums() {
        for name in ["E", "My.Mod.E"] {
            let (out, diags) = to_string_of("E", Some(TypeHint::Enum(name.to_owned())), &[]);
            assert!(
                out.contains("Log(EnumValueToString(\"E\", Cast<Int64>((a as Int32))));"),
                "{out}"
            );
            assert_eq!(codes(&diags), ["enum-to-string"]);
            assert_eq!(diags[0].severity, crate::Severity::Info);
            assert!(diags[0].message.contains("`E` being registered at runtime"));
        }
    }

    #[test]
    fn to_string_of_small_ints() {
        for ty in ["Int8", "Int16", "Uint8", "Uint16"] {
            let (out, diags) = to_string_of(ty, Some(prim(ty)), &[]);
            assert!(out.contains("Log(IntToString(Cast<Int32>(a)));"), "{out}");
            assert!(diags.is_empty(), "{diags:?}");
        }
    }

    #[test]
    fn to_string_of_wide_ints_is_reported() {
        for ty in ["Uint32", "Int64", "Uint64"] {
            let (out, diags) = to_string_of(ty, Some(prim(ty)), &[]);
            assert!(out.contains("Log(ToString(a));"), "{out}");
            assert_eq!(codes(&diags), ["to-string"]);
            let message = &diags[0].message;
            assert!(message.contains(&format!("`{ty}`")), "{message}");
            assert!(
                message.contains("no lossless REDmod equivalent"),
                "{message}"
            );
            assert!(message.contains("above 2^31"), "{message}");
        }
    }

    #[test]
    fn to_string_of_structs() {
        let strukt = || Some(TypeHint::Struct("My.GameTime".to_owned()));
        let (out, diags) = to_string_of("GameTime", strukt(), &["My.GameTime"]);
        assert!(out.contains("Log(GameTime.ToString(a));"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");

        // the static method is declared by another type
        let (out, diags) = to_string_of("GameTime", strukt(), &["Other"]);
        assert!(out.contains("Log(ToString(a));"), "{out}");
        assert_eq!(codes(&diags), ["to-string"]);
        assert!(diags[0].message.contains("`GameTime` struct"), "{diags:?}");
    }

    #[test]
    fn to_string_of_objects_is_reported() {
        let hints = [
            class("Widget"),
            TypeHint::Ref(RefKind::Strong, class("Widget").into()),
            TypeHint::Ref(RefKind::Weak, class("Widget").into()),
        ];
        for hint in hints {
            let (out, diags) = to_string_of("ref<Widget>", Some(hint), &[]);
            assert!(out.contains("Log(ToString(a));"), "{out}");
            assert_eq!(codes(&diags), ["to-string"]);
            let message = &diags[0].message;
            assert!(message.contains("`Widget`"), "{message}");
            assert!(
                message.contains("NameToString(x.GetClassName())"),
                "{message}"
            );
        }
    }

    #[test]
    fn to_string_of_other_types_names_the_type() {
        let hint = TypeHint::Array(prim("Int32").into());
        let (out, diags) = to_string_of("array<Int32>", Some(hint), &[]);
        assert!(out.contains("Log(ToString(a));"), "{out}");
        assert!(diags[0].message.contains("`array<Int32>`"), "{diags:?}");
    }

    #[test]
    fn to_string_of_untyped_values_says_so() {
        let (out, diags) = to_string_of("Int32", None, &[]);
        assert!(out.contains("Log(ToString(a));"), "{out}");
        assert_eq!(codes(&diags), ["to-string"]);
        assert!(
            diags[0].message.contains("could not be determined"),
            "{diags:?}"
        );
    }

    #[test]
    fn small_int_operands_are_parenthesized() {
        let src = "func F(a: Uint8, b: Uint8, c: Bool) {
  Log(ToString(a + b));
  Log(ToString(c ? a : b));
  Log(ToString(-a));
}
";
        let types = MapTypeQuery::new()
            .with_type(span_of(src, "a + b", 0), prim("Uint8"))
            .with_type(span_of(src, "c ? a : b", 0), prim("Uint8"))
            .with_type(span_of(src, "-a", 0), prim("Uint8"));
        let (out, _) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        for expected in [
            "Log(IntToString(Cast<Int32>((a + b))));",
            "Log(IntToString(Cast<Int32>((c ? a : b))));",
            "Log(IntToString(Cast<Int32>((-a))));",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
    }

    #[test]
    fn user_casts_and_typed_calls_parenthesize_operators_per_argument() {
        let src = "func F(a: Int32, b: Int32, x: Int32) {
  let c = Cast<Int32>(a * b);
  let d = Cast<Int32>(a);
  Foo<Int32>(a + b, x);
  Bar(a + b, x);
}
";
        let (out, diags) = to_redmod(src, run_with_diagnostics);
        let out = squash(&out);
        for expected in [
            "Cast<Int32>((a * b))",
            "Cast<Int32>(a)",
            "Foo<Int32>((a + b), x);",
            "Bar(a + b, x);",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn game_enums_get_no_note() {
        let src = "func F(a: E, b: M) {\n  Log(ToString(a));\n  Log(ToString(b));\n}\n";
        let types = MapTypeQuery::new()
            .with_type(
                span_in(src, "(a)", 0, "a"),
                TypeHint::Enum("gamePSM".into()),
            )
            .with_type(span_in(src, "(b)", 0, "b"), TypeHint::Enum("Mod.M".into()))
            .with_game_type("gamePSM");
        let (out, diags) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        assert!(out.contains("EnumValueToString(\"gamePSM\""), "{out}");
        assert!(out.contains("EnumValueToString(\"M\""), "{out}");
        assert_eq!(codes(&diags), ["enum-to-string"]);
        assert!(diags[0].message.contains("`<Module>.M`"), "{diags:?}");
    }
    #[test]
    fn deref_of_script_refs() {
        let src = "func F(r: script_ref<Int32>, s: Int32) {
  let a: Int32 = Deref(r);
  let b: Int32 = Deref(s);
}
";
        let types = MapTypeQuery::new()
            .with_type(
                span_in(src, "(r)", 0, "r"),
                TypeHint::Ref(RefKind::Script, prim("Int32").into()),
            )
            .with_type(span_in(src, "(s)", 0, "s"), prim("Int32"));
        let (out, diags) = to_redmod_with_types(src, &types, run_with_diagnostics);
        let out = squash(&out);
        assert!(out.contains("let a: Int32 = r;"), "{out}");
        assert!(out.contains("let b: Int32 = Deref(s);"), "{out}");
        assert_eq!(codes(&diags), ["deref"]);
    }

    #[test]
    fn nothing_changes_without_type_information() {
        let src = "func F(a: ref<A>, i: Int32, r: script_ref<Int32>) {
  if IsDefined(a) {
  }
  Log(ToString(i));
  let x: Int32 = Deref(r);
}
";
        let (out, diags) = to_redmod(src, run_with_diagnostics);
        let out = squash(&out);
        assert!(out.contains("if (IsDefined(a))"), "{out}");
        assert!(out.contains("Log(ToString(i));"), "{out}");
        assert!(out.contains("let x: Int32 = Deref(r);"), "{out}");
        assert_eq!(codes(&diags), ["is-defined", "to-string", "deref"]);
    }
}

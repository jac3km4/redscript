//! `s"a \(x) b"` -> `"a " + XToString(x) + " b"`.
//!
//! Parts whose type is known are converted with the matching REDmod function (`IntToString`,
//! `FloatToString`, `BoolToString`, `NameToString`, strings are used as is; with compiler types the
//! rewrites of `to_string` also apply: enums, small ints, structs with a static `ToString`).
//! Other parts are wrapped in a `ToString(..)` placeholder and reported, since REDmod has no
//! generic `ToString`.

use std::borrow::Cow;

use redscript_ast::{BinOp, Constant, Expr, SourceModule, StrPart};

use super::to_string::{self, Plan};
use crate::Cx;
use crate::index::{LocalTypes, ModuleIndex, TypeEnv};
use crate::walk::{
    MutVisitor, SExpr, call, for_each_function, ident, synth, type_name, walk_body, walk_expr,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |info, func| {
        let locals = LocalTypes::of(func);
        let env = TypeEnv::new(&index, &locals, info.class, cx);
        if let Some(body) = &mut func.body {
            walk_body(
                &mut Interpolation {
                    cx: &mut *cx,
                    env: &env,
                },
                body,
            );
        }
    });
}

struct Interpolation<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    env: &'a TypeEnv<'a, 'src>,
}

impl<'src> Interpolation<'_, '_, 'src> {
    fn stringify(&mut self, expr: SExpr<'src>) -> SExpr<'src> {
        let typ = self.env.type_of(&expr);
        let func = match typ.as_ref().and_then(type_name) {
            Some("String") => return expr,
            Some("Int32") => "IntToString",
            Some("Float") => "FloatToString",
            Some("Bool") => "BoolToString",
            Some("CName") => "NameToString",
            _ => return self.stringify_other(expr),
        };
        let span = expr.1;
        call(ident(func, synth(span)), vec![], vec![expr], synth(span))
    }

    /// Converts a part with the help of the compiler types when they say how, otherwise wraps it
    /// in a `ToString(..)` placeholder and reports it.
    fn stringify_other(&mut self, expr: SExpr<'src>) -> SExpr<'src> {
        let span = expr.1;
        let types = self.cx.opts.types;
        let plan = types
            .and_then(|types| Some((types, types.type_of(span)?)))
            .map(|(types, hint)| to_string::plan(types, &hint));
        let note = match &plan {
            Some(Plan::Enum(name, true)) => Some(to_string::enum_note(name)),
            _ => None,
        };
        let expr = match plan {
            Some(plan) => match to_string::build(plan, expr, synth(span), self.cx.arena) {
                Ok(rewritten) => {
                    if let Some(note) = note {
                        self.cx.info(span, "enum-to-string", note);
                    }
                    return rewritten;
                }
                Err(expr) => expr,
            },
            None => expr,
        };
        self.cx.warn(
            span,
            "interpolation-to-string",
            to_string::interpolation_warning(types, span),
        );
        call(
            ident("ToString", synth(span)),
            vec![],
            vec![expr],
            synth(span),
        )
    }
}

impl<'src> MutVisitor<'src> for Interpolation<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let span = expr.1;
        let Expr::InterpolatedString(parts) = &mut expr.0 else {
            return;
        };
        let mut items: Vec<SExpr<'src>> = vec![];
        for part in std::mem::take(parts).into_vec() {
            match part {
                StrPart::Str(s) if s.is_empty() => {}
                StrPart::Str(s) => items.push((Expr::Constant(Constant::String(s)), synth(span))),
                StrPart::Expr(e) => items.push(self.stringify(e)),
            }
        }
        let mut it = items.into_iter();
        let mut result = it
            .next()
            .unwrap_or((Expr::Constant(Constant::String(Cow::Borrowed(""))), span));
        for item in it {
            result = (
                Expr::BinOp {
                    lhs: Box::new(result),
                    op: BinOp::Add,
                    rhs: Box::new(item),
                },
                synth(span),
            );
        }
        if result.1 == synth(span) {
            result.1 = span;
        }
        *expr = result;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TypeHint;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn interpolation_becomes_concatenation() {
        let (out, diags) = to_redmod(
            "class A {\n  let count: Int32;\n  func F(s: String, f: Float, b: Bool, n: CName, o: ref<A>) -> String {\n    let x = s\"count: \\(this.count), \\(s)/\\(f)/\\(b)/\\(n) \\(o)!\";\n    let y = s\"\\(this.count+1)\";\n    return s\"\\(s)\";\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "let x = \"count: \" + IntToString(this.count) + \", \" + s + \"/\" + FloatToString(f) + \"/\" + BoolToString(b) + \"/\" + NameToString(n) + \" \" + ToString(o) + \"!\";"
            ),
            "{out}"
        );
        assert!(out.contains("return s;"), "{out}");
        assert!(
            out.contains("let y = IntToString(this.count + 1);"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["interpolation-to-string"]);
    }

    #[test]
    fn interpolated_types_from_type_information() {
        let src = r#"func F(o: ref<A>) -> String {
  return s"\(o.Count()) \(o.Ratio()) \(o.Flag()) \(o.Id()) \(o.Name()) \(o.Big()) \(o.Unknown())";
}
"#;
        let prim = |name: &str| TypeHint::Primitive(name.to_owned());
        let types = MapTypeQuery::new()
            .with_type(span_of(src, "o.Count()", 0), prim("Int32"))
            .with_type(span_of(src, "o.Ratio()", 0), prim("Float"))
            .with_type(span_of(src, "o.Flag()", 0), prim("Bool"))
            .with_type(span_of(src, "o.Id()", 0), prim("CName"))
            .with_type(span_of(src, "o.Name()", 0), prim("String"))
            .with_type(span_of(src, "o.Big()", 0), prim("Int64"));
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains(
                "return IntToString(o.Count()) + \" \" + FloatToString(o.Ratio()) + \" \" + BoolToString(o.Flag()) + \" \" + NameToString(o.Id()) + \" \" + o.Name() + \" \" + ToString(o.Big()) + \" \" + ToString(o.Unknown());"
            ),
            "{out}"
        );
        assert_eq!(codes(&diags), ["interpolation-to-string"; 2]);
    }

    #[test]
    fn interpolated_enums_ints_structs_and_objects() {
        let src = r#"func F(o: ref<A>) -> String {
  return s"\(o.E()) \(o.Small()) \(o.Wide()) \(o.Time()) \(o.Size()) \(o.Obj()) \(o.Nope())";
}
"#;
        let types = MapTypeQuery::new()
            .with_type(span_of(src, "o.E()", 0), TypeHint::Enum("My.E".to_owned()))
            .with_type(
                span_of(src, "o.Small()", 0),
                TypeHint::Primitive("Uint8".to_owned()),
            )
            .with_type(
                span_of(src, "o.Wide()", 0),
                TypeHint::Primitive("Uint32".to_owned()),
            )
            .with_type(
                span_of(src, "o.Time()", 0),
                TypeHint::Struct("GameTime".into()),
            )
            .with_type(span_of(src, "o.Size()", 0), TypeHint::Struct("Size".into()))
            .with_type(span_of(src, "o.Obj()", 0), TypeHint::Class("A".into()))
            .with_static_method("GameTime", "ToString");
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains(
                "return EnumValueToString(\"E\", Cast<Int64>((o.E() as Int32))) + \" \" + IntToString(Cast<Int32>(o.Small())) + \" \" + ToString(o.Wide()) + \" \" + GameTime.ToString(o.Time()) + \" \" + ToString(o.Size()) + \" \" + ToString(o.Obj()) + \" \" + ToString(o.Nope());"
            ),
            "{out}"
        );
        assert_eq!(
            codes(&diags),
            [
                "enum-to-string",
                "interpolation-to-string",
                "interpolation-to-string",
                "interpolation-to-string",
                "interpolation-to-string"
            ]
        );
        let messages = diags.iter().map(|d| d.message.as_str()).collect::<Vec<_>>();
        assert!(messages[1].contains("above 2^31"), "{messages:?}");
        assert!(messages[2].contains("`Size` struct"), "{messages:?}");
        assert!(messages[3].contains("GetClassName()"), "{messages:?}");
        assert!(
            messages[4].contains("could not be determined"),
            "{messages:?}"
        );
    }

    #[test]
    fn interpolated_small_int_operands_are_parenthesized() {
        let src = r#"func F(o: ref<A>) -> String {
  return s"\(o.A()+o.B())";
}
"#;
        let types = MapTypeQuery::new().with_type(
            span_of(src, "o.A()+o.B()", 0),
            TypeHint::Primitive("Uint8".to_owned()),
        );
        let (out, _) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("return IntToString(Cast<Int32>((o.A() + o.B())));"),
            "{out}"
        );
    }
}

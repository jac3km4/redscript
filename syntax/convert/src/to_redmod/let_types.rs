//! REDmod often fails to infer the type of a local: `let x = v;` -> `let x: T = v;` when the
//! type of `v` is known from declarations (literals, casts, `new`, typed locals, fields and
//! methods declared in the file). Other untyped locals are reported.

use redscript_ast::{SourceFunctionBody, SourceModule, Stmt};

use crate::Cx;
use crate::index::{LocalTypes, ModuleIndex, TypeEnv};
use crate::walk::{MutVisitor, SParam, SStmt, for_each_function, synth, walk_body, walk_stmt};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |info, func| {
        let locals = LocalTypes::of(func);
        if let Some(body) = &mut func.body {
            walk_body(
                &mut LetTypes {
                    cx: &mut *cx,
                    index: &index,
                    locals,
                    class: info.class,
                },
                body,
            );
        }
    });
}

struct LetTypes<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    index: &'a ModuleIndex<'src>,
    locals: LocalTypes<'src>,
    class: Option<&'src str>,
}

impl<'src> MutVisitor<'src> for LetTypes<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        walk_stmt(self, stmt);
        let (
            Stmt::Let {
                name,
                typ: typ @ None,
                value,
            },
            span,
        ) = stmt
        else {
            return;
        };
        let env = TypeEnv::new(self.index, &self.locals, self.class, self.cx);
        let inferred = value
            .as_deref()
            .and_then(|value| env.type_of(value))
            .filter(|t| !matches!(t, redscript_ast::Type::Named { name: "Void", .. }));
        match inferred {
            Some(inferred) => {
                let inferred = (inferred, synth(name.1));
                self.locals.infer(name.0, inferred.clone());
                *typ = Some(Box::new(inferred));
            }
            None => self.cx.warn(
                *span,
                "untyped-let",
                format!(
                    "the type of `{}` could not be determined and REDmod often fails to infer \
                     it, add a type",
                    name.0
                ),
            ),
        }
    }

    // lambdas are reported separately
    fn visit_lambda(&mut self, _: &mut Box<[SParam<'src>]>, _: &mut SourceFunctionBody<'src>) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};
    use crate::{RefKind, TypeHint};

    #[test]
    fn let_types_are_inferred_when_known() {
        let (out, diags) = to_redmod(
            "class A {\n  let items: [String];\n  func Get() -> ref<A> = this;\n  func F(x: Float) {\n    let a = 1;\n    let b = a;\n    let c = \"s\" + a;\n    let d = this.items;\n    let e = ArraySize(this.items);\n    let f = this.Get();\n    let g = x as Int32;\n    let h = new A();\n    let i = this.items[0];\n    let j = Unknown();\n    let k = 1;\n    let k = 2;\n    let l = k;\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        for expected in [
            "let a: Int32 = 1;",
            "let b: Int32 = a;",
            "let c: String = \"s\" + a;",
            "let d: array<String> = this.items;",
            "let e: Int32 = ArraySize(this.items);",
            "let f: A = this.Get();",
            "let g: Int32 = x as Int32;",
            "let h: A = new A;",
            "let i: String = this.items[0];",
            "let j = Unknown();",
            "let k: Int32 = 1;",
            "let k: Int32 = 2;",
            "let l = k;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["untyped-let"; 2]);
    }

    #[test]
    fn type_information_takes_precedence_over_declarations() {
        let src = "func F() {
  let a = Make();
  let b = Other();
  let c = Items();
  let d = 1;
  let e = Script();
  let f = Unknown();
  let g = 2;
}
";
        let class = |name: &str| TypeHint::Class(name.to_owned());
        let types = MapTypeQuery::new()
            .with_type(
                span_of(src, "Make()", 0),
                TypeHint::Ref(RefKind::Strong, class("Mod.Foo").into()),
            )
            .with_type(
                span_of(src, "Other()", 0),
                TypeHint::Ref(RefKind::Weak, class("Bar").into()),
            )
            .with_type(
                span_of(src, "Items()", 0),
                TypeHint::Array(TypeHint::Primitive("Int32".to_owned()).into()),
            )
            .with_type(
                span_of(src, "1", 0),
                TypeHint::Primitive("Float".to_owned()),
            )
            .with_type(
                span_of(src, "Script()", 0),
                TypeHint::Ref(RefKind::Script, TypeHint::Struct("Baz".to_owned()).into()),
            )
            .with_type(
                span_of(src, "Unknown()", 0),
                TypeHint::Other("Function0".to_owned()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let a: Foo = Make();",
            "let b: wref<Bar> = Other();",
            "let c: array<Int32> = Items();",
            "let d: Float = 1;",
            "let e: script_ref<Baz> = Script();",
            "let f = Unknown();",
            // unanswered queries fall back to the declarations
            "let g: Int32 = 2;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["untyped-let"]);
    }

    #[test]
    fn container_types_are_printed_for_redmod() {
        let src = "func F() {
  let a = Fixed();
  let b = Weak();
  let c = Strong();
  let d = Nested();
}
";
        let class = || Box::new(TypeHint::Class("Mod.A".to_owned()));
        let types = MapTypeQuery::new()
            .with_type(
                span_of(src, "Fixed()", 0),
                TypeHint::StaticArray(TypeHint::Primitive("Int32".to_owned()).into(), 3),
            )
            .with_type(
                span_of(src, "Weak()", 0),
                TypeHint::Array(TypeHint::Ref(RefKind::Weak, class()).into()),
            )
            .with_type(
                span_of(src, "Strong()", 0),
                TypeHint::Array(TypeHint::Ref(RefKind::Strong, class()).into()),
            )
            .with_type(
                span_of(src, "Nested()", 0),
                TypeHint::Array(TypeHint::Array(TypeHint::Enum("E".to_owned()).into()).into()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let a: Int32[3] = Fixed();",
            "let b: array<wref<A>> = Weak();",
            "let c: array<A> = Strong();",
            "let d: array<array<E>> = Nested();",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }
}

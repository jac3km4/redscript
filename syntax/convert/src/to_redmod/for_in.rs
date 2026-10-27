//! `for x in arr { body }` ->
//!
//! ```text
//! let i: Int32 = 0;
//! while i < arr.Size() {
//!     let x: T = arr[i];
//!     i += 1;
//!     body
//! }
//! ```
//!
//! The index is incremented right after reading the element, before the body runs, so a
//! `continue` in the body needs no special handling. An iterated expression that is not a plain
//! variable is evaluated once into a local first (REDmod only calls array methods on variables).
//! When the element type cannot be determined from declarations, the element local is left
//! untyped and a diagnostic is emitted.

use redscript_ast::{BinOp, ConditionalBlock, Expr, LetCondition, SourceBlock, SourceModule, Stmt};

use crate::Cx;
use crate::index::{LocalTypes, ModuleIndex, Names, TypeEnv};
use crate::walk::{
    MutVisitor, SStmt, elem_type, expand_stmts, for_each_function, ident, int, let_stmt,
    method_call, plain_type, synth, walk_body, walk_stmts,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |info, func| {
        let locals = LocalTypes::of(func);
        let names = Names::of(func, cx.arena);
        let env = TypeEnv::new(&index, &locals, info.class, cx);
        if let Some(body) = &mut func.body {
            walk_body(
                &mut ForIn {
                    cx: &mut *cx,
                    env: &env,
                    names,
                },
                body,
            );
        }
    });
}

struct ForIn<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    env: &'a TypeEnv<'a, 'src>,
    names: Names<'src>,
}

impl<'src> ForIn<'_, '_, 'src> {
    fn expand(&mut self, stmt: SStmt<'src>, out: &mut Vec<SStmt<'src>>) {
        let (Stmt::ForIn { name, iter, body }, span) = stmt else {
            out.push(stmt);
            return;
        };
        let first = out.len();
        let syn = synth(span);
        let iter_type = self.env.type_of(&iter);

        // community iterates over a snapshot of anything but a local variable (the iterated
        // expression is evaluated once into a hidden local), so only plain locals are used as is
        let array = if matches!(iter.0, Expr::Ident(_)) {
            *iter
        } else {
            let local = self.names.fresh("arr");
            let typ = iter_type.clone().map(|t| (t, syn));
            if typ.is_none() {
                self.cx.warn(
                    iter.1,
                    "for-in-array-type",
                    format!(
                        "the type of the iterated array could not be determined, add a type to \
                         `{local}`"
                    ),
                );
            }
            out.push(let_stmt(local, typ, Some(*iter), syn));
            ident(local, syn)
        };

        let index = self.names.fresh("i");
        out.push(let_stmt(
            index,
            Some(plain_type("Int32", syn)),
            Some(int(0, syn)),
            syn,
        ));

        let elem_type = iter_type.as_ref().and_then(elem_type).cloned();
        if elem_type.is_none() {
            self.cx.warn(
                name.1,
                "for-in-element-type",
                format!(
                    "the element type of the loop could not be determined, add a type to `{}`",
                    name.0
                ),
            );
        }
        let element = (
            Expr::Index {
                expr: Box::new(array.clone()),
                index: Box::new(ident(index, syn)),
            },
            syn,
        );
        let increment = (
            Stmt::Expr(Box::new((
                Expr::BinOp {
                    lhs: Box::new(ident(index, syn)),
                    op: BinOp::AssignAdd,
                    rhs: Box::new(int(1, syn)),
                },
                syn,
            ))),
            syn,
        );
        let mut stmts = vec![
            (
                Stmt::Let {
                    name,
                    typ: elem_type.map(Box::new),
                    value: Some(Box::new(element)),
                },
                syn,
            ),
            increment,
        ];
        stmts.extend(body.stmts.into_vec());

        let condition = (
            Expr::BinOp {
                lhs: Box::new(ident(index, syn)),
                op: BinOp::Lt,
                rhs: Box::new(method_call(array, "Size", vec![], syn)),
            },
            syn,
        );
        let block = ConditionalBlock::new(LetCondition::Expr(condition), SourceBlock::new(stmts));
        // the first generated statement takes the span of the loop, so that comments above the
        // loop stay above it
        out[first].1 = span;
        out.push((Stmt::While(Box::new(block)), span));
    }
}

impl<'src> MutVisitor<'src> for ForIn<'_, '_, 'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        // expand first so that fresh names are allocated in source order, the bodies (now
        // nested in the generated loops) are walked afterwards
        expand_stmts(stmts, |stmt, out| self.expand(stmt, out));
        walk_stmts(self, stmts);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};
    use crate::{RefKind, TypeHint};

    #[test]
    fn for_in_becomes_while() {
        let (out, diags) = to_redmod(
            "func F(items: [Int32]) -> Int32 {\n  let i = 5;\n  for x in items {\n    if x == 0 {\n      continue;\n    }\n    i += x;\n  }\n  return i;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "let i_1: Int32 = 0; while (i_1 < items.Size()) { let x: Int32 = items[i_1]; i_1 += 1; if (x == 0) { continue; } i += x; }"
            ),
            "{out}"
        );
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn for_in_over_fields_and_calls() {
        let (out, diags) = to_redmod(
            "class A {\n  let names: [CName];\n  func Get() -> [String] = [];\n  func F(other: ref<B>) {\n    for n in this.names {\n      Log(n);\n    }\n    for s in this.Get() {\n      Log(s);\n    }\n    for o in other.Get() {\n      for p in o {\n        Log(p);\n      }\n    }\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "let arr: array<CName> = this.names; let i: Int32 = 0; while (i < arr.Size()) { let n: CName = arr[i];"
            ),
            "{out}"
        );
        assert!(
            out.contains(
                "let arr_1: array<String> = this.Get(); let i_1: Int32 = 0; while (i_1 < arr_1.Size()) { let s: String = arr_1[i_1];"
            ),
            "{out}"
        );
        assert!(
            out.contains("let arr_2 = other.Get(); let i_2: Int32 = 0;"),
            "{out}"
        );
        assert!(out.contains("let o = arr_2[i_2];"), "{out}");
        assert!(out.contains("let p = o[i_3];"), "{out}");
        assert_eq!(
            codes(&diags),
            [
                "for-in-array-type",
                "for-in-element-type",
                "for-in-element-type"
            ]
        );
    }

    #[test]
    fn fields_are_iterated_over_a_snapshot() {
        let (out, _) = to_redmod(
            "class A {\n  let items: array<Int32>;\n  func F() {\n    for x in this.items {\n      ArrayRemove(this.items, x);\n    }\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "let arr: array<Int32> = this.items; let i: Int32 = 0; while (i < arr.Size()) { let x: Int32 = arr[i]; i += 1; ArrayRemove(this.items, x); }"
            ),
            "{out}"
        );
    }

    #[test]
    fn for_in_types_from_type_information() {
        let src = "func F(o: ref<B>, ids: array<TweakDBID>) {
  for x in o.Get() {
    Log(x);
  }
  for w in o.Weak() {
    Log(w);
  }
  for f in o.Fixed() {
    Log(f);
  }
}
";
        let class = || Box::new(TypeHint::Class("A".to_owned()));
        let types = MapTypeQuery::new()
            .with_type(span_of(src, "o.Get()", 0), TypeHint::Array(class()))
            .with_type(
                span_of(src, "o.Weak()", 0),
                TypeHint::Array(TypeHint::Ref(RefKind::Weak, class()).into()),
            )
            .with_type(
                span_of(src, "o.Fixed()", 0),
                TypeHint::StaticArray(TypeHint::Primitive("Float".to_owned()).into(), 4),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let arr: array<A> = o.Get(); let i: Int32 = 0; while (i < arr.Size()) { let x: A = arr[i];",
            "let arr_1: array<wref<A>> = o.Weak();",
            "let w: wref<A> = arr_1[i_1];",
            "let arr_2: Float[4] = o.Fixed();",
            "let f: Float = arr_2[i_2];",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }
}

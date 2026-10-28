//! `ArraySize(a)` -> `a.Size()`, `ArrayPush(a, x)` -> `a.PushBack(x)`, etc.
//!
//! REDmod only calls array methods on arrays held in variables. When the array is not a plain
//! variable path, the call is rewritten through a local declared right before the statement, but
//! only if
//! - the method does not mutate the array (mutating a copy would lose the change),
//! - the type of the array is known from declarations,
//! - hoisting does not change when the array expression is evaluated (it is not in a loop
//!   condition, an `else if` condition, the right-hand side of `&&`/`||` or a ternary branch).
//!
//! Otherwise the call is left as is and a diagnostic is emitted.

use redscript_ast::{Expr, SourceModule};

use super::hoist::{Hoist, Hoisting, Rewrite};
use crate::Cx;
use crate::index::{LocalTypes, ModuleIndex, Names, TypeEnv};
use crate::walk::{
    SExpr, elem_type, for_each_function, free_call_name, is_variable_path, method_call, walk_body,
};

/// Community intrinsic, REDmod method, whether the method mutates the array.
const METHODS: [(&str, &str, bool); 12] = [
    ("ArraySize", "Size", false),
    ("ArrayPush", "PushBack", true),
    ("ArrayPop", "PopBack", true),
    ("ArrayClear", "Clear", true),
    ("ArrayResize", "Resize", true),
    ("ArrayContains", "Contains", false),
    ("ArrayFindFirst", "FindFirst", false),
    ("ArrayFindLast", "FindLast", false),
    ("ArrayInsert", "Insert", true),
    ("ArrayRemove", "Remove", true),
    ("ArrayErase", "Erase", true),
    ("ArrayLast", "Last", false),
];

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |info, func| {
        let locals = LocalTypes::of(func);
        let names = Names::of(func, cx.arena);
        let env = TypeEnv::new(&index, &locals, info.class, cx);
        if let Some(body) = &mut func.body {
            let rewrite = ArrayMethods {
                cx: &mut *cx,
                env: &env,
            };
            walk_body(&mut Hoisting::new(rewrite, names), body);
        }
    });
}

struct ArrayMethods<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    env: &'a TypeEnv<'a, 'src>,
}

impl<'src> Rewrite<'src> for ArrayMethods<'_, '_, 'src> {
    fn rewrite(&mut self, expr: &mut SExpr<'src>, hoist: &mut Hoist<'src>) {
        let Some(name) = free_call_name(&expr.0) else {
            return;
        };
        let Some(&(_, method, mutating)) = METHODS.iter().find(|(n, _, _)| *n == name) else {
            return;
        };
        let span = expr.1;
        let Expr::Call {
            args, type_args, ..
        } = &mut expr.0
        else {
            return;
        };
        if args.is_empty() || !type_args.is_empty() {
            return;
        }
        let mut args = std::mem::take(args).into_vec();
        let receiver = args.remove(0);

        if is_variable_path(&receiver.0) {
            *expr = method_call(receiver, method, args, span);
            return;
        }

        let typ = self
            .env
            .type_of(&receiver)
            .filter(|t| elem_type(t).is_some());
        let reason = match (typ, mutating, hoist.in_place(), hoist.effects_before()) {
            (Some(typ), false, true, false) => {
                let local = hoist.hoist("arr", typ, receiver);
                *expr = method_call(local, method, args, span);
                return;
            }
            (_, true, _, _) => "it is called on a temporary array that it modifies",
            (None, _, _, _) => "the type of the array could not be determined",
            (Some(_), _, false, _) => "the array expression cannot be moved out of its context",
            (Some(_), _, _, true) => {
                "moving the array expression before the statement would change what is \
                 evaluated before it"
            }
        };
        self.cx.warn(
            span,
            "array-receiver",
            format!(
                "REDmod only calls array methods on variables, {name}(..) was not converted to \
                 `.{method}(..)` because {reason}: store the array in a local first"
            ),
        );
        let mut all = vec![receiver];
        all.extend(args);
        let Expr::Call { args, .. } = &mut expr.0 else {
            unreachable!()
        };
        *args = all.into();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TypeHint;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn array_intrinsics_become_methods() {
        let (out, diags) = to_redmod(
            "class A {\n  let items: [Int32];\n  func F(arr: [Int32], x: Int32) {\n    let n = ArraySize(arr);\n    ArrayPush(arr, x);\n    ArrayPop(arr);\n    ArrayClear(this.items);\n    ArrayResize(arr, 3);\n    let c = ArrayContains(arr, x);\n    let f = ArrayFindFirst(arr, x) + ArrayFindLast(arr, x);\n    ArrayInsert(arr, 0, x);\n    ArrayRemove(arr, x);\n    ArrayErase(arr, 1);\n    let l = ArrayLast(this.items);\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        for expected in [
            "let n = arr.Size();",
            "arr.PushBack(x);",
            "arr.PopBack();",
            "this.items.Clear();",
            "arr.Resize(3);",
            "let c = arr.Contains(x);",
            "let f = arr.FindFirst(x) + arr.FindLast(x);",
            "arr.Insert(0, x);",
            "arr.Remove(x);",
            "arr.Erase(1);",
            "let l = this.items.Last();",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn temporaries_are_hoisted_when_safe() {
        let (out, diags) = to_redmod(
            "class A {\n  func Get() -> [Int32] = [];\n  func F(o: ref<B>) {\n    let arr = 1;\n    let n = ArraySize(this.Get()) + 1;\n    if ArrayContains(this.Get(), 2) {\n      Log(n);\n    } else if ArrayContains(this.Get(), 3) {\n      Log(n);\n    }\n    while ArraySize(this.Get()) > 0 {\n      ArrayPush(this.Get(), 1);\n    }\n    let b = n > 0 && ArraySize(this.Get()) > 0;\n    let c = ArraySize(o.Unknown());\n    let d = this.Next() + ArraySize(this.Get());\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains("let arr_1: array<Int32> = this.Get(); let n = arr_1.Size() + 1;"),
            "{out}"
        );
        assert!(
            out.contains("let arr_2: array<Int32> = this.Get(); if (arr_2.Contains(2))"),
            "{out}"
        );
        assert!(
            out.contains("else if (ArrayContains(this.Get(), 3))"),
            "{out}"
        );
        assert!(out.contains("while (ArraySize(this.Get()) > 0)"), "{out}");
        assert!(out.contains("ArrayPush(this.Get(), 1);"), "{out}");
        assert!(
            out.contains("let b = n > 0 && ArraySize(this.Get()) > 0;"),
            "{out}"
        );
        assert!(out.contains("let c = ArraySize(o.Unknown());"), "{out}");
        assert!(
            out.contains("let d = this.Next() + ArraySize(this.Get());"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["array-receiver"; 6]);
    }

    #[test]
    fn temporaries_typed_by_type_information() {
        let src = "func F(o: ref<B>) {
  let n = ArraySize(o.Unknown());
  let m = ArrayContains(o.Other(), 1);
}
";
        let types = MapTypeQuery::new().with_type(
            span_of(src, "o.Unknown()", 0),
            TypeHint::Array(TypeHint::Struct("Vector4".to_owned()).into()),
        );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("let arr: array<Vector4> = o.Unknown(); let n = arr.Size();"),
            "{out}"
        );
        assert!(
            out.contains("let m = ArrayContains(o.Other(), 1);"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["array-receiver"]);
    }

    #[test]
    fn reads_before_the_array_block_hoisting() {
        // `Arr()` changes `f.count`
        let (out, diags) = to_redmod(
            "class Foo {
  let count: Int32;
  func Arr() -> [Int32] = [];
}

func F(f: ref<Foo>) {
  let n = f.count + ArraySize(f.Arr());
  let m = ArraySize(f.Arr()) + f.count;
  let k = 1 + ArraySize(f.Arr());
}
",
            run,
        );
        let out = squash(&out);
        for expected in [
            "let n = f.count + ArraySize(f.Arr());",
            "let arr: array<Int32> = f.Arr(); let m = arr.Size() + f.count;",
            "let arr_1: array<Int32> = f.Arr(); let k = 1 + arr_1.Size();",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["array-receiver"]);
        assert!(
            diags[0]
                .message
                .contains("would change what is evaluated before it"),
            "{}",
            diags[0].message
        );
    }
}

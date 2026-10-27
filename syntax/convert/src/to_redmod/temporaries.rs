//! REDmod reads struct fields and calls array methods only on values held in variables:
//! `widget.GetSize().Y` fails with `Parent scope does not come from variable` and
//! `sys.GetNames().Contains(x)` with `only arrays coming from variables call array functions`.
//!
//! When type information says that the result of a call is a struct whose field is read, or an
//! array whose method is called, the call is moved into a typed local declared before the
//! statement (`let tmp: Vector2 = widget.GetSize(); tmp.Y`), following the rules of
//! [`hoist`](super::hoist). When the call cannot be moved, a diagnostic is emitted. Classes are
//! fine on temporaries, and nothing changes without type information.

use redscript_ast::{Expr, SourceModule, Span};

use super::hoist::{Hoist, Hoisting, Rewrite};
use crate::index::Names;
use crate::walk::{SExpr, for_each_function, walk_body};
use crate::{Cx, TypeHint, TypeQuery};

const ARRAY_METHODS: [&str; 12] = [
    "Size",
    "PushBack",
    "PopBack",
    "Clear",
    "Resize",
    "Contains",
    "FindFirst",
    "FindLast",
    "Insert",
    "Remove",
    "Erase",
    "Last",
];

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let Some(types) = cx.opts.types else {
        return;
    };
    for_each_function(module, |_, func| {
        let names = Names::of(func, cx.arena);
        if let Some(body) = &mut func.body {
            let rewrite = Temporaries {
                cx: &mut *cx,
                types,
            };
            walk_body(&mut Hoisting::new(rewrite, names), body);
        }
    });
}

struct Temporaries<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    types: &'a dyn TypeQuery,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Struct,
    Array,
}

impl<'src> Temporaries<'_, '_, 'src> {
    /// Moves `receiver`, the result of a call of kind `kind`, into a local. `span` and `member`
    /// describe the access, for the diagnostic.
    fn hoist_receiver(
        &mut self,
        receiver: &mut SExpr<'src>,
        kind: Kind,
        member: &str,
        span: Span,
        hoist: &mut Hoist<'src>,
    ) {
        if !matches!(receiver.0, Expr::Call { .. }) {
            return;
        }
        let Some(hint) = self.types.type_of(receiver.1) else {
            return;
        };
        let matches = match kind {
            Kind::Struct => matches!(hint, TypeHint::Struct(_)),
            Kind::Array => matches!(hint, TypeHint::Array(_) | TypeHint::StaticArray(..)),
        };
        if !matches {
            return;
        }
        let Some(typ) = hint.to_ast_type(self.cx.arena, crate::walk::synth(receiver.1)) else {
            return;
        };
        let reason = match (hoist.in_place(), hoist.effects_before()) {
            (true, false) => {
                let base = if kind == Kind::Array { "arr" } else { "tmp" };
                let value = std::mem::replace(receiver, (Expr::Error, receiver.1));
                *receiver = hoist.hoist(base, typ, value);
                return;
            }
            (false, _) => "the call cannot be moved out of its context",
            (true, true) => {
                "moving the call before the statement would change what is evaluated before it"
            }
        };
        let message = match kind {
            Kind::Struct => format!(
                "REDmod only reads struct fields of variables, `.{member}` is read from the \
                 result of a call that was not moved into a local because {reason}: store it in \
                 a local first"
            ),
            Kind::Array => format!(
                "REDmod only calls array methods on variables, `.{member}(..)` is called on the \
                 result of a call that was not moved into a local because {reason}: store it in \
                 a local first"
            ),
        };
        self.cx.warn(span, "temporary-receiver", message);
    }
}

impl<'src> Rewrite<'src> for Temporaries<'_, '_, 'src> {
    fn rewrite(&mut self, expr: &mut SExpr<'src>, hoist: &mut Hoist<'src>) {
        let span = expr.1;
        match &mut expr.0 {
            // a member read: structs have no instance methods, so this is a field when the
            // receiver is a struct, even in callee position
            Expr::Member {
                expr: receiver,
                member,
            } => {
                let member = *member;
                self.hoist_receiver(receiver, Kind::Struct, member, span, hoist);
            }
            Expr::Call { expr: callee, .. } => {
                if let Expr::Member {
                    expr: receiver,
                    member,
                } = &mut callee.0
                    && ARRAY_METHODS.contains(member)
                {
                    let member = *member;
                    self.hoist_receiver(receiver, Kind::Array, member, span, hoist);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};

    const SRC: &str = "func F(w: ref<inkWidget>, sys: ref<Sys>, x: CName) -> Float {
  let y = w.GetSize().Y;
  if sys.GetNames().Contains(x) {
    Log(w.GetName());
  }
  while sys.GetNames().Size() > 0 {
  }
  let z = sys.Next() + w.GetSize().X;
  let ok = x == n\"a\" && w.GetSize().X > 0.0;
  return w.GetMargin().left;
}
";

    fn types() -> MapTypeQuery {
        let size = TypeHint::Struct("Vector2".to_owned());
        let names = TypeHint::Array(TypeHint::Primitive("CName".to_owned()).into());
        MapTypeQuery::new()
            .with_type(span_of(SRC, "w.GetSize()", 0), size.clone())
            .with_type(span_of(SRC, "sys.GetNames()", 0), names.clone())
            .with_type(span_of(SRC, "sys.GetNames()", 1), names)
            .with_type(span_of(SRC, "w.GetSize()", 1), size.clone())
            .with_type(span_of(SRC, "w.GetSize()", 2), size)
            .with_type(
                span_of(SRC, "w.GetName()", 0),
                TypeHint::Primitive("CName".to_owned()),
            )
            // classes are fine on temporaries
            .with_type(
                span_of(SRC, "w.GetMargin()", 0),
                TypeHint::Class("inkMargin".to_owned()),
            )
    }

    #[test]
    fn temporaries_are_moved_into_locals() {
        let (out, diags) = to_redmod_with_types(SRC, &types(), run);
        let out = squash(&out);
        for expected in [
            "let tmp: Vector2 = w.GetSize(); let y = tmp.Y;",
            "let arr: array<CName> = sys.GetNames(); if (arr.Contains(x))",
            "Log(w.GetName());",
            "while (sys.GetNames().Size() > 0)",
            "let z = sys.Next() + w.GetSize().X;",
            "let ok = x == n\"a\" && w.GetSize().X > 0.0;",
            "return w.GetMargin().left;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["temporary-receiver"; 3]);
    }

    #[test]
    fn nothing_changes_without_type_information() {
        let (out, diags) = to_redmod(SRC, run);
        let out = squash(&out);
        assert!(out.contains("let y = w.GetSize().Y;"), "{out}");
        assert!(out.contains("if (sys.GetNames().Contains(x))"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn lambda_bodies_do_not_forget_earlier_reads() {
        // `Next()` changes `f.n`, which is read before the lambda
        let src = "func F(f: ref<Foo>) {
  Set(f.n, (x: Int32) -> { return 1; }, f.Next().X);
}
";
        let size = TypeHint::Struct("Vector4".to_owned());
        let types = MapTypeQuery::new().with_type(span_of(src, "f.Next()", 0), size);
        let (out, _) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(!out.contains("let tmp"), "{out}");
        assert!(out.contains("f.Next().X"), "{out}");
    }

    #[test]
    fn reads_before_the_call_block_hoisting() {
        // `Next()` changes `f.n`
        let src = "func F(f: ref<Foo>, x: Float, i: Int32) {
  let a = f.n + f.Next().X;
  let b = f.Next().X + f.n;
  let c = 1.0 + f.Next().X;
  x = f.Next().X;
  Set(i, f.Next().X);
}
";
        let size = TypeHint::Struct("Vector4".to_owned());
        let mut types = MapTypeQuery::new();
        for n in 0..5 {
            types = types.with_type(span_of(src, "f.Next()", n), size.clone());
        }
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let a = f.n + f.Next().X;",
            "let tmp: Vector4 = f.Next(); let b = tmp.X + f.n;",
            "let tmp_1: Vector4 = f.Next(); let c = 1.0 + tmp_1.X;",
            "let tmp_2: Vector4 = f.Next(); x = tmp_2.X;",
            // `i` could be an out argument
            "Set(i, f.Next().X);",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["temporary-receiver"; 2]);
    }
}

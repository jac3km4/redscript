//! REDmod has no chained assignments. In the 0.5.x compatibility mode `a = b = c` is parsed as
//! `a = (b = c)`, and the same goes for the compound assignments (`a = b += 1`). The inner
//! assignment is moved into a statement of its own before the statement and replaced with its
//! left-hand side: `b = c; a = b;`. Longer chains are split from the inside out.
//!
//! The split follows the rules of [`hoist`](super::hoist): the chain must be evaluated where its
//! statement starts, with nothing but constants evaluated before it. On top of that, the
//! left-hand side of the inner assignment is evaluated twice after the split, so it must be a
//! place without side effects (a variable, a field or an array element with a plain index), and
//! the place of the outer assignment, evaluated before the inner assignment originally, must not
//! depend on what the inner assignment changes. Chains that cannot be split get a warning.

use hashbrown::HashSet;
use redscript_ast::{BinOp, Expr, SourceExpr, SourceModule};

use super::hoist::{Hoist, Hoisting, Rewrite};
use crate::Cx;
use crate::index::Names;
use crate::walk::{MutVisitor, SExpr, for_each_function, synth, walk_body, walk_expr};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        let names = Names::of(func, cx.arena);
        if let Some(body) = &mut func.body {
            let rewrite = Chains { cx: &mut *cx };
            walk_body(&mut Hoisting::new(rewrite, names), body);
        }
    });
}

struct Chains<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
}

impl<'src> Rewrite<'src> for Chains<'_, '_, 'src> {
    fn rewrite(&mut self, expr: &mut SExpr<'src>, hoist: &mut Hoist<'src>) {
        let span = expr.1;
        let Some((outer, inner)) = assignment(&mut expr.0) else {
            return;
        };
        let Some((inner_lhs, _)) = assignment(&mut inner.0) else {
            return;
        };
        let reason = if !hoist.in_place() {
            Some("the assignment cannot be moved out of its context")
        } else if hoist.effects_before() {
            Some("moving the assignment would change what is evaluated before it")
        } else if !is_pure_place(&inner_lhs.0) {
            Some("its left-hand side has side effects")
        } else if !is_pure_place(&outer.0) || reads_written(&outer.0, &written_names(&inner_lhs.0))
        {
            Some("the place assigned first depends on the assignment")
        } else {
            None
        };
        if let Some(reason) = reason {
            self.cx.warn(
                span,
                "chained-assignment",
                format!(
                    "REDmod has no chained assignments and this one was not split because \
                     {reason}: split it into one assignment per statement"
                ),
            );
            return;
        }
        let mut read = (**inner_lhs).clone();
        Synthesize.visit_expr(&mut read);
        let assignment = std::mem::replace(&mut **inner, read);
        hoist.hoist_stmt(assignment);
    }
}

/// The left-hand side and the right-hand side of an assignment, simple or compound.
fn assignment<'a, 'src>(
    expr: &'a mut SourceExpr<'src>,
) -> Option<(&'a mut Box<SExpr<'src>>, &'a mut Box<SExpr<'src>>)> {
    match expr {
        Expr::Assign { lhs, rhs }
        | Expr::BinOp {
            lhs,
            op:
                BinOp::AssignAdd
                | BinOp::AssignSub
                | BinOp::AssignMul
                | BinOp::AssignDiv
                | BinOp::AssignBitOr
                | BinOp::AssignBitAnd,
            rhs,
        } => Some((lhs, rhs)),
        _ => None,
    }
}

/// Whether evaluating `expr` has no side effects: variables, constants, fields and array
/// elements of such values.
fn is_pure_value(expr: &SourceExpr<'_>) -> bool {
    match expr {
        Expr::Ident(_) | Expr::Constant(_) | Expr::This | Expr::Super | Expr::Null => true,
        Expr::Member { expr, .. } => is_pure_value(&expr.0),
        Expr::Index { expr, index } => is_pure_value(&expr.0) && is_pure_value(&index.0),
        _ => false,
    }
}

/// Whether `expr` is a place whose evaluation has no side effects.
fn is_pure_place(expr: &SourceExpr<'_>) -> bool {
    matches!(
        expr,
        Expr::Ident(_) | Expr::Member { .. } | Expr::Index { .. }
    ) && is_pure_value(expr)
}

/// The names of the variables and fields that assigning to the place `expr` changes.
fn written_names<'src>(expr: &SourceExpr<'src>) -> HashSet<&'src str> {
    let mut names = HashSet::new();
    let mut current = expr;
    loop {
        match current {
            Expr::Ident(name) => {
                names.insert(*name);
                break;
            }
            Expr::Member { member, .. } => {
                names.insert(*member);
                break;
            }
            // an element of an array changes the array
            Expr::Index { expr, .. } => current = &expr.0,
            _ => break,
        }
    }
    names
}

/// Whether evaluating the place `expr` (not its value) reads one of `names`.
fn reads_written(expr: &SourceExpr<'_>, names: &HashSet<&str>) -> bool {
    match expr {
        Expr::Ident(_) => false,
        Expr::Member { expr, .. } => mentions(&expr.0, names),
        Expr::Index { expr, index } => reads_written(&expr.0, names) || mentions(&index.0, names),
        _ => true,
    }
}

/// Whether the value `expr` mentions one of `names`, as a variable or a field.
fn mentions(expr: &SourceExpr<'_>, names: &HashSet<&str>) -> bool {
    match expr {
        Expr::Ident(name) => names.contains(name),
        Expr::Member { expr, member } => names.contains(member) || mentions(&expr.0, names),
        Expr::Index { expr, index } => mentions(&expr.0, names) || mentions(&index.0, names),
        _ => false,
    }
}

/// Gives every node a zero-length span, for the copies of original nodes.
struct Synthesize;

impl<'src> MutVisitor<'src> for Synthesize {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        expr.1 = synth(expr.1);
        walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn statements_are_split() {
        let src = "func F(arr: array<Int32>, i: Int32) {
  let a: Int32;
  let b: Int32;
  let c: Int32;
  a = b = 1;
  a = b = c = 2;
  a = b += 1;
  a += b -= 1;
  this.x = this.y = 3;
  arr[i] = b = 4;
  a = arr[i] = 5;
  a = b + 1;
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        for expected in [
            "b = 1; a = b;",
            "c = 2; b = c; a = b;",
            "b += 1; a = b;",
            "b -= 1; a += b;",
            "this.y = 3; this.x = this.y;",
            "b = 4; arr[i] = b;",
            "arr[i] = 5; a = arr[i];",
            "a = b + 1;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn chains_inside_expressions_are_hoisted() {
        let src = "func F() -> Int32 {
  let a: Int32;
  let b: Int32;
  Log(a = b = 1);
  let c = a = b = 2;
  if a = b = 3 {
  }
  return a = b = 4;
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        for expected in [
            "b = 1; Log(a = b);",
            "b = 2; let c = a = b;",
            "b = 3; if (a = b)",
            "b = 4; return a = b;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn chains_that_cannot_move_keep_a_warning() {
        let src = "func F(arr: array<Int32>, i: Int32) {
  let a: Int32;
  let b: Int32;
  arr[I()] = a = b;
  arr[i] = i = 1;
  a = arr[I()] = 2;
  while a = b = 3 {
  }
  Log(b, a = b = 4);
  let ok = a > 0 && Check(a = b = 5);
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        for expected in [
            "arr[I()] = a = b;",
            "arr[i] = i = 1;",
            "a = arr[I()] = 2;",
            "while (a = b = 3)",
            "Log(b, a = b = 4);",
            "a > 0 && Check(a = b = 5);",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["chained-assignment"; 6]);
    }

    #[test]
    fn copies_do_not_reuse_spans() {
        let src = "func F() {
  let a: Int32;
  let b: Int32;
  // comment
  a = b = 1;
}
";
        let (out, _) = to_redmod(src, run);
        assert_eq!(out.matches("// comment").count(), 1, "{out}");
        let out = squash(&out);
        assert!(out.contains("// comment b = 1; a = b;"), "{out}");
    }
}

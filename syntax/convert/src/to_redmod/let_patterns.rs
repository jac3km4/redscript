//! REDmod has no pattern matching. The simple forms of `if let` are desugared:
//!
//! - `if let x as T = e { .. }` -> `let x: T = e as T; if (x) { .. }`
//! - `if let x? = e { .. }` -> `let x = e; if (x) { .. }`
//! - `if let x = e { .. }` -> `let x = e; if (true) { .. }`
//!
//! The bound variable gets a fresh name if the function already uses its name, so that moving
//! the declaration out of the `if` cannot capture other references. Only the first condition of
//! an `if` can be desugared this way (an `else if` condition is evaluated only when the previous
//! ones fail). Every other pattern (`else if let`, `while let`, `case let`, aggregate and array
//! patterns) is reported.

use redscript_ast::{
    Condition, Constant, Expr, LetCondition, Pattern, SourceFunctionBody, SourceModule, Stmt,
};

use crate::Cx;
use crate::index::{NameCounts, Names};
use crate::walk::{
    MutVisitor, RenameIdent, SParam, SStmt, expand_stmts, for_each_function, ident, let_stmt,
    synth, walk_body, walk_stmt, walk_stmts,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        let names = Names::of(func, cx.arena);
        let counts = NameCounts::of_function(func);
        if let Some(body) = &mut func.body {
            walk_body(
                &mut LetPatterns {
                    cx: &mut *cx,
                    names,
                    counts,
                },
                body,
            );
        }
    });
}

struct LetPatterns<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    names: Names<'src>,
    counts: NameCounts<'src>,
}

/// The variable bound by a simple pattern, its type and whether the pattern is a test.
fn simple_binding<'src>(
    pattern: &redscript_ast::SourcePattern<'src>,
) -> Option<(&'src str, Option<crate::walk::SType<'src>>, bool)> {
    match pattern {
        Pattern::Name((name, _)) => Some((*name, None, false)),
        Pattern::As(inner, typ) => match &inner.0 {
            Pattern::Name((name, _)) => Some((*name, Some(typ.clone()), true)),
            _ => None,
        },
        Pattern::Nullable(inner) => match &inner.0 {
            Pattern::Name((name, _)) => Some((*name, None, true)),
            _ => None,
        },
        _ => None,
    }
}

impl<'src> LetPatterns<'_, '_, 'src> {
    fn expand(&mut self, mut stmt: SStmt<'src>, out: &mut Vec<SStmt<'src>>) {
        let span = stmt.1;
        let Stmt::If { blocks, .. } = &mut stmt.0 else {
            out.push(stmt);
            return;
        };
        let Some(first) = blocks.first_mut() else {
            out.push(stmt);
            return;
        };
        let LetCondition::LetPattern((pattern, _), value) = &first.condition else {
            out.push(stmt);
            return;
        };
        let Some((name, typ, test)) = simple_binding(pattern) else {
            out.push(stmt);
            return;
        };
        let value = value.clone();
        let syn = synth(span);
        // the name is kept if the binding is its only declaration and all of its references are
        // in the guarded block, otherwise moving the declaration out could capture references
        let body_counts = NameCounts::of_block(&first.body);
        let local =
            if self.counts.decls(name) == 1 && self.counts.refs(name) == body_counts.refs(name) {
                name
            } else {
                self.names.fresh(name)
            };
        let value = match &typ {
            Some(typ) => (
                Expr::DynCast {
                    expr: Box::new(value.clone()),
                    typ: Box::new(typ.clone()),
                },
                synth(value.1),
            ),
            None => value,
        };
        let cond = if test {
            ident(local, syn)
        } else {
            (Expr::Constant(Constant::Bool(true)), syn)
        };
        first.condition = LetCondition::Expr(cond);
        if local != name {
            RenameIdent {
                old: name,
                new: local,
            }
            .visit_stmts(&mut first.body.stmts);
        }
        // the declaration takes the span of the `if` so that comments above the `if` stay
        // above it
        let mut decl = let_stmt(local, typ, Some(value), syn);
        decl.1 = span;
        out.push(decl);
        out.push(stmt);
    }

    fn report(&mut self, span: redscript_ast::Span) {
        self.cx.warn(
            span,
            "let-pattern",
            "REDmod has no pattern matching and this pattern could not be desugared, rewrite it \
             with a local and an `if`",
        );
    }
}

impl<'src> MutVisitor<'src> for LetPatterns<'_, '_, 'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        expand_stmts(stmts, |stmt, out| self.expand(stmt, out));
        walk_stmts(self, stmts);
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &stmt.0 {
            Stmt::If { blocks, .. } => {
                for block in &blocks[..] {
                    if let LetCondition::LetPattern((_, span), _) = &block.condition {
                        self.report(*span);
                    }
                }
            }
            Stmt::While(block) => {
                if let LetCondition::LetPattern((_, span), _) = &block.condition {
                    self.report(*span);
                }
            }
            Stmt::Switch { cases, .. } => {
                for case in &cases[..] {
                    if let Condition::Pattern((_, span)) = &case.condition {
                        self.report(*span);
                    }
                }
            }
            _ => {}
        }
        walk_stmt(self, stmt);
    }

    // lambdas are reported separately
    fn visit_lambda(&mut self, _: &mut Box<[SParam<'src>]>, _: &mut SourceFunctionBody<'src>) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn simple_patterns_are_desugared() {
        let (out, diags) = to_redmod(
            "func F(o: ref<A>, x: Int32) {\n  if let b as B = o {\n    b.Foo();\n  } else if let c as C = o {\n    c.Bar();\n  }\n  if let x? = o {\n    x.Foo();\n    let x = 1;\n    Log(x);\n  }\n  if let y = o as B {\n    y.Foo();\n  }\n  while let z as B = o {\n    z.Foo();\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains("let b: B = o as B; if (b) { b.Foo(); } else if (let c as C = o)"),
            "{out}"
        );
        assert!(
            out.contains("let x_1 = o; if (x_1) { x_1.Foo(); let x = 1; Log(x); }"),
            "{out}"
        );
        assert!(
            out.contains("let y = o as B; if (true) { y.Foo(); }"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["let-pattern", "let-pattern"]);
    }

    fn conv(src: &str) -> String {
        squash(&to_redmod(src, run).0)
    }

    #[test]
    fn nested_same_name_pattern_uses_inner_binding() {
        let out = conv(
            "func F(o1: ref<A>, o2: ref<A>) {\n  if let b as B = o1 {\n    if let b as B = o2 {\n      b.Foo();\n    }\n    b.Bar();\n  }\n}\n",
        );
        assert!(out.contains("if (b_2) { b_2.Foo(); } b_1.Bar();"), "{out}");
    }

    #[test]
    fn else_if_and_while_patterns_shadow_the_outer_binding() {
        let out = conv(
            "func F(o1: ref<A>, o2: ref<A>) {\n  if let b as B = o1 {\n    if o2 == null {\n      b.Foo();\n    } else if let b as B = o2 {\n      b.Bar();\n    }\n    while let b as B = o2 {\n      b.Baz();\n    }\n  }\n}\n",
        );
        assert!(out.contains("b_1.Foo();"), "{out}");
        assert!(
            out.contains("else if (let b as B = o2) { b.Bar(); }"),
            "{out}"
        );
        assert!(
            out.contains("while (let b as B = o2) { b.Baz(); }"),
            "{out}"
        );
    }

    #[test]
    fn pattern_value_is_still_renamed() {
        let out = conv(
            "func F(o1: ref<A>) {\n  if let b as B = o1 {\n    if let b as C = b {\n      b.Foo();\n    }\n  }\n}\n",
        );
        assert!(out.contains("b_1 as C"), "{out}");
    }

    #[test]
    fn lambda_param_and_inner_pattern_shadow() {
        let out = conv(
            "func F(o1: ref<A>, o2: ref<A>) {\n  if let b as B = o1 {\n    let f = (x: Int32) -> { if let b as B = o2 { b.Foo(); } };\n    b.Foo();\n  }\n}\n",
        );
        assert!(out.contains("if (let b as B = o2) { b.Foo(); }"), "{out}");
        assert!(out.contains("b_1.Foo(); }"), "{out}");
    }

    #[test]
    fn switch_case_patterns_shadow_the_outer_binding() {
        let out = conv(
            "func F(o1: ref<A>, o2: ref<A>) {\n  if let b as B = o1 {\n    switch o2 {\n      case let b as B:\n        b.Foo();\n      default:\n        b.Bar();\n    }\n  }\n}\n",
        );
        assert!(out.contains("case let b as B: b.Foo();"), "{out}");
        assert!(out.contains("default: b_1.Bar();"), "{out}");
    }
}

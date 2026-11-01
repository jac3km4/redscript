//! Inside `@wrapMethod`, `wrappedMethod(..)` -> a call to the method by its own name.

use redscript_ast::{Expr, SourceModule};

use crate::Cx;
use crate::walk::{MutVisitor, SExpr, for_each_function, walk_body, walk_expr};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |info, func| {
        if !info.has_annotation("wrapMethod") {
            return;
        }
        let mut visitor = Rename {
            cx: &mut *cx,
            name: func.name.0,
        };
        if let Some(body) = &mut func.body {
            walk_body(&mut visitor, body);
        }
    });
}

struct Rename<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    name: &'src str,
}

impl<'src> MutVisitor<'src> for Rename<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let span = expr.1;
        if let Expr::Call { expr: callee, .. } = &mut expr.0
            && let Expr::Ident(name) = &mut callee.0
        {
            if *name == "wrappedMethod" {
                *name = self.name;
            } else if *name == self.name {
                self.cx.warn(
                    span,
                    "wrap-own-name-call",
                    format!(
                        "in REDmod, calling `{}` by its own name inside the wrapper calls the wrapped method instead of this one, use `this.{}(..)` to keep the current behaviour",
                        self.name, self.name
                    ),
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn wrapped_method_calls_use_own_name() {
        let (out, diags) = to_redmod(
            "@wrapMethod(A)\nfunc Get(x: Int32) -> Int32 {\n  if x > 0 {\n    return Get(x - 1);\n  }\n  return wrappedMethod(x) + 1;\n}\n\nfunc Other() {\n  wrappedMethod();\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("return Get(x) + 1;"), "{out}");
        // outside of a wrapper, nothing changes
        assert!(out.contains("wrappedMethod();"), "{out}");
        assert_eq!(codes(&diags), ["wrap-own-name-call"]);
    }
}

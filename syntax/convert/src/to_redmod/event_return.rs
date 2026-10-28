//! REDmod events have no return type: `return true;`/`return false;` -> `return;`.

use redscript_ast::{Constant, Expr, ItemQualifiers, SourceFunctionBody, SourceModule, Stmt};

use crate::Cx;
use crate::walk::{MutVisitor, SParam, SStmt, for_each_function, walk_body, walk_stmt};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |info, func| {
        if !info.qualifiers.contains(ItemQualifiers::CALLBACK) {
            return;
        }
        if let Some(body) = &mut func.body {
            walk_body(&mut StripReturns { cx: &mut *cx }, body);
        }
    });
}

struct StripReturns<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
}

impl<'src> MutVisitor<'src> for StripReturns<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        walk_stmt(self, stmt);
        let (Stmt::Return(value @ Some(_)), span) = stmt else {
            return;
        };
        match value.as_deref() {
            Some((Expr::Constant(Constant::Bool(_)), _)) => *value = None,
            _ => self.cx.warn(
                *span,
                "event-return-value",
                "REDmod events cannot return a value, this return needs to be rewritten manually",
            ),
        }
    }

    // returns inside lambdas belong to the lambda
    fn visit_lambda(
        &mut self,
        _params: &mut Box<[SParam<'src>]>,
        _body: &mut SourceFunctionBody<'src>,
    ) {
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn event_returns_are_stripped() {
        let (out, diags) = to_redmod(
            "class A {\n  protected cb func OnX(e: ref<E>) -> Bool {\n    if this.y {\n      return false;\n    }\n    return this.Z();\n  }\n  func F() -> Bool {\n    return true;\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains("event OnX(e: E) { if (this.y) { return; }"),
            "{out}"
        );
        assert!(out.contains("return this.Z();"), "{out}");
        assert!(
            out.contains("function F() -> Bool { return true; }"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["event-return-value"]);
    }
}

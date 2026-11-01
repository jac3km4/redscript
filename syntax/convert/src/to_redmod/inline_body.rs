//! `func F() -> T = expr;` -> `function F() -> T { return expr; }`

use redscript_ast::{Block, FunctionBody, SourceModule, Stmt};

use crate::Cx;
use crate::walk::{for_each_function, type_name};

pub fn run<'src>(module: &mut SourceModule<'src>, _cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        if !matches!(func.body, Some(FunctionBody::Inline(_))) {
            return;
        }
        let Some(FunctionBody::Inline(expr)) = func.body.take() else {
            unreachable!()
        };
        let span = expr.1;
        let returns_void = func
            .return_type
            .as_deref()
            .is_none_or(|(typ, _)| type_name(typ) == Some("Void"));
        let stmt = if returns_void {
            Stmt::Expr(expr)
        } else {
            Stmt::Return(Some(expr))
        };
        func.body = Some(FunctionBody::Block(Block::single((stmt, span))));
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{squash, to_redmod};

    #[test]
    fn inline_bodies_become_blocks() {
        let (out, diags) = to_redmod(
            "class A {\n  func F() -> Int32 = 2;\n  func G() = this.F();\n  func H() -> Void = this.F();\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("function F() -> Int32 { return 2; }"), "{out}");
        assert!(out.contains("function G() { this.F(); }"), "{out}");
        assert!(out.contains("function H() -> Void { this.F(); }"), "{out}");
        assert!(diags.is_empty());
    }
}

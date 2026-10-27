//! `return [];` -> `return empty;` with `let empty: array<T>;` declared at the top of the
//! function, `array<T>` being the declared return type.

use redscript_ast::{Expr, FunctionBody, SourceFunctionBody, SourceModule, Stmt};

use crate::Cx;
use crate::index::Names;
use crate::walk::{
    MutVisitor, SParam, SStmt, SType, elem_type, for_each_function, ident, let_stmt, synth,
    walk_stmt,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        let mut names = Names::of(func, cx.arena);
        let return_type = func
            .return_type
            .as_deref()
            .filter(|(t, _)| elem_type(t).is_some())
            .cloned();
        let Some(FunctionBody::Block(block)) = &mut func.body else {
            return;
        };
        let mut visitor = EmptyReturns {
            cx: &mut *cx,
            return_type: return_type.as_ref(),
            names: &mut names,
            local: None,
        };
        visitor.visit_stmts(&mut block.stmts);
        if let (Some((local, span)), Some(typ)) = (visitor.local, return_type) {
            let decl = let_stmt(local, Some((typ.0, span)), None, span);
            let mut stmts = vec![decl];
            stmts.extend(std::mem::take(&mut block.stmts).into_vec());
            block.stmts = stmts.into();
        }
    });
}

struct EmptyReturns<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    return_type: Option<&'a SType<'src>>,
    names: &'a mut Names<'src>,
    local: Option<(&'src str, redscript_ast::Span)>,
}

impl<'src> MutVisitor<'src> for EmptyReturns<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        walk_stmt(self, stmt);
        let (Stmt::Return(Some(value)), _) = stmt else {
            return;
        };
        let (Expr::ArrayLit(elems), span) = &mut **value else {
            return;
        };
        if !elems.is_empty() {
            return;
        }
        if self.return_type.is_none() {
            self.cx.warn(
                *span,
                "empty-array-return",
                "REDmod does not accept `return [];` and the array type is unknown, return a \
                 typed empty local instead",
            );
            return;
        }
        let names = &mut *self.names;
        let (local, _) = *self
            .local
            .get_or_insert_with(|| (names.fresh("empty"), synth(*span)));
        **value = ident(local, *span);
    }

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
    fn empty_array_returns_use_a_typed_local() {
        let (out, diags) = to_redmod(
            "func F(x: Int32) -> [String] {\n  let empty = 1;\n  if x > 0 {\n    return [];\n  }\n  return [];\n}\n\nfunc G() -> Int32 {\n  return 1;\n}\n\nfunc H() -> Variant {\n  return [];\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "function F(x: Int32) -> array<String> { let empty_1: array<String>; let empty = 1; if (x > 0) { return empty_1; } return empty_1; }"
            ),
            "{out}"
        );
        assert!(
            out.contains("function H() -> Variant { return []; }"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["empty-array-return"]);
    }
}

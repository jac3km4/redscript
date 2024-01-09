use redscript::ast::Expr;

use super::{Diagnostic, ExprDiagnosticPass, FunctionMetadata};
use crate::typechecker::TypedAst;
use crate::visit_expr;

#[derive(Debug)]
pub struct StatementFallthroughCheck;

impl ExprDiagnosticPass for StatementFallthroughCheck {
    fn diagnose(&self, body: &Expr<TypedAst>, _meta: &FunctionMetadata, results: &mut Vec<Diagnostic>) {
        StatementFallthroughVisitor { results }.on_expr(body);
    }
}

struct StatementFallthroughVisitor<'a> {
    results: &'a mut Vec<Diagnostic>,
}

impl StatementFallthroughVisitor<'_> {
    fn on_expr(&mut self, expr: &Expr<TypedAst>) {
        if let Expr::Switch(_, cases, _, _) = expr {
            for case in cases.iter().take(cases.len() - 1) {
                if !matches!(case.body.exprs.last(), None | Some(Expr::Break(_) | Expr::Return(_, _))) {
                    self.results.push(Diagnostic::StatementFallthrough(case.matcher.span()));
                }
            }
        }
        visit_expr!(self, on_expr, expr);
    }
}

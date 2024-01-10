use redscript::ast::Expr;
use redscript::bytecode::IntrinsicOp;

use super::{Diagnostic, ExprDiagnosticPass, FunctionMetadata};
use crate::typechecker::{Callable, Member, TypedExpr, TypedExprExt};
use crate::visit_expr;

#[derive(Debug)]
pub struct InvalidUseOfTemporaryCheck;

impl ExprDiagnosticPass for InvalidUseOfTemporaryCheck {
    fn diagnose(&self, body: &TypedExpr, _meta: &FunctionMetadata, results: &mut Vec<Diagnostic>) {
        InvalidUseOfTemporaryVisitor { results }.on_expr(body);
    }
}

struct InvalidUseOfTemporaryVisitor<'a> {
    results: &'a mut Vec<Diagnostic>,
}

impl InvalidUseOfTemporaryVisitor<'_> {
    fn on_expr(&mut self, expr: &TypedExpr) {
        match expr {
            Expr::Member(inner, Member::StructField(_), _) if inner.is_rvalue() => {
                self.results.push(Diagnostic::InvalidUseOfTemporary(inner.span()));
            }
            Expr::ArrayElem(inner, _, _) if inner.is_rvalue() => {
                self.results.push(Diagnostic::InvalidUseOfTemporary(inner.span()));
            }
            Expr::Call(callable, _, args, _) => match (callable, &args[..]) {
                (
                    Callable::Intrinsic(
                        IntrinsicOp::ArrayContains
                        | IntrinsicOp::ArrayCount
                        | IntrinsicOp::ArrayFindFirst
                        | IntrinsicOp::ArrayFindLast
                        | IntrinsicOp::ArrayLast
                        | IntrinsicOp::ArrayPop
                        | IntrinsicOp::ArraySize,
                        _,
                    ),
                    [inner, ..],
                ) if inner.is_rvalue() => self.results.push(Diagnostic::InvalidUseOfTemporary(inner.span())),
                _ => {}
            },
            _ => {}
        }
        visit_expr!(self, on_expr, expr);
    }
}

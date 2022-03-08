use std::collections::HashSet;

use redscript::ast::{Expr, Seq, Span};
use redscript::bundle::PoolIndex;
use redscript::definition::{Local, Parameter};

use super::{DiagnosticPass, FunctionMetadata};
use crate::scope::{Reference, Value};
use crate::typechecker::TypedAst;
use crate::unit::Diagnostic;
use crate::visit_expr;

pub struct UnusedCheck;

impl DiagnosticPass for UnusedCheck {
    fn diagnose(&self, body: &Seq<TypedAst>, _meta: &FunctionMetadata) -> Vec<Diagnostic> {
        let names = UsedNames::from_seq(body);
        let mut diagnostics = vec![];

        for (local, span) in names.declared {
            if !names.used_locals.contains(&local) {
                diagnostics.push(Diagnostic::UnusedLocal(span))
            }
        }

        diagnostics
    }
}

#[derive(Default)]
struct UsedNames {
    declared: Vec<(PoolIndex<Local>, Span)>,
    used_locals: HashSet<PoolIndex<Local>>,
    used_params: HashSet<PoolIndex<Parameter>>,
}

impl UsedNames {
    fn from_seq(seq: &Seq<TypedAst>) -> Self {
        let mut names = Self::default();
        for expr in &seq.exprs {
            names.on_expr(expr);
        }
        names
    }

    fn on_expr(&mut self, expr: &Expr<TypedAst>) {
        match expr {
            Expr::Declare(local, _, _, span) => {
                self.declared.push((*local, *span));
            }
            Expr::ForIn(local, _, _, span) => {
                self.declared.push((*local, *span));
            }
            Expr::Ident(Reference::Value(Value::Local(local)), _) => {
                self.used_locals.insert(*local);
            }
            Expr::Ident(Reference::Value(Value::Parameter(local)), _) => {
                self.used_params.insert(*local);
            }
            _ => {}
        };
        visit_expr!(self, on_expr, expr)
    }
}

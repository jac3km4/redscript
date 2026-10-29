use hashbrown::{HashMap, HashSet};
use redscript_ast::Span;

use super::DiagnosticPass;
use crate::visitor::Visitor;
use crate::{CompileErrorReporter, Diagnostic, LoweredFunction, ir};

#[derive(Debug, Default)]
pub struct UnusedLocals;

impl DiagnosticPass for UnusedLocals {
    fn run<'ctx>(&self, func: &LoweredFunction<'ctx>, reporter: &mut CompileErrorReporter<'ctx>) {
        let mut visitor = UnusedLocalVisitor::new(func);
        visitor.visit_block(&func.block);

        for (_, span) in visitor.unused_locals() {
            reporter.report(Diagnostic::UnusedLocal(span));
        }
    }
}

#[derive(Debug, Default)]
struct UnusedLocalVisitor {
    spans: HashMap<ir::Local, Span>,
    used: HashSet<ir::Local>,
}

impl UnusedLocalVisitor {
    fn new(func: &LoweredFunction<'_>) -> Self {
        let mut this = Self::default();
        for local in &func.locals {
            this.register_local(local);
        }
        this
    }

    fn register_local(&mut self, local: &ir::LocalInfo<'_>) {
        if let Some(span) = local.span {
            self.spans.insert(local.id, span);
        }
    }

    fn unused_locals(&self) -> impl Iterator<Item = (ir::Local, Span)> {
        self.spans
            .iter()
            .filter(|&(local, _)| !self.used.contains(local))
            .map(|(&local, &span)| (local, span))
    }
}

impl<'ctx> Visitor<'ctx> for UnusedLocalVisitor {
    fn visit_init_default(&mut self, _local: ir::Local, _typ: &ir::Type<'ctx>, _span: Span) {}

    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, _span: Span) {
        match place {
            ir::Expr::Local(_, _) => {}
            other => self.visit_expr(other),
        }
        self.visit_expr(value);
    }

    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, _span: Span) {
        for local in &closure.locals {
            self.register_local(local);
        }
        self.used.extend(closure.captures.iter().copied());
        self.visit_block(&closure.block);
    }

    fn visit_local(&mut self, local: ir::Local, _span: Span) {
        self.used.insert(local);
    }
}

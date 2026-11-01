use std::collections::HashSet;

use redscript_community_ast::Span;

use crate::diagnostic::DiagnosticLevel;
use crate::ir::Visitor;
use crate::{
    CompileErrorReporter, Diagnostic, LoweredCompilationUnit, MethodId, Param, SemanticPolicy,
    Symbols, Type, ir, predef,
};

#[derive(Debug, thiserror::Error)]
#[error("cannot write through a const parameter")]
struct ConstParameterWrite;

pub(super) fn validate<'policy, 'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    policy: impl Fn(Span) -> &'policy SemanticPolicy,
    reporter: &mut CompileErrorReporter<'ctx>,
) {
    let mut check = |function: &crate::LoweredFunction<'ctx>, params: &[Param<'ctx>]| {
        ConstWrites {
            symbols,
            readonly: params
                .iter()
                .enumerate()
                .filter_map(|(i, p)| p.flags().is_const().then_some(ir::Local::Param(i as u16)))
                .collect(),
            captures: HashSet::new(),
            policy: policy(function.span),
            reporter,
        }
        .visit_block(&function.block);
    };
    for (id, function) in &unit.functions {
        check(function, symbols[*id].type_().params());
    }
    for (id, class) in &unit.classes {
        for (method, function) in &class.methods {
            check(
                function,
                symbols[MethodId::new(*id, *method)].type_().params(),
            );
        }
    }
    for (id, function) in unit
        .added_methods
        .iter()
        .filter_map(|(id, f)| f.as_ref().map(|f| (id, f)))
        .chain(unit.method_replacements.iter())
        .chain(
            unit.method_wrappers
                .iter()
                .flat_map(|(id, fs)| fs.iter().map(move |f| (id, f))),
        )
    {
        check(function, symbols[*id].type_().params());
    }
}

struct ConstWrites<'a, 'ctx> {
    symbols: &'a Symbols<'ctx>,
    readonly: HashSet<ir::Local>,
    captures: HashSet<ir::Local>,
    policy: &'a SemanticPolicy,
    reporter: &'a mut CompileErrorReporter<'ctx>,
}

impl<'ctx> ConstWrites<'_, 'ctx> {
    fn readonly_place(&self, expr: &ir::Expr<'ctx>) -> bool {
        match expr {
            ir::Expr::Local(local, _) => self.readonly.contains(local),
            ir::Expr::Capture(local, _) => self.captures.contains(local),
            ir::Expr::Field { receiver, .. } => self.readonly_place(receiver),
            ir::Expr::Index { array, .. } => {
                !self.policy.const_param_element_writes && self.readonly_place(array)
            }
            ir::Expr::Conditional { then, else_, .. } => {
                self.readonly_place(then) || self.readonly_place(else_)
            }
            ir::Expr::DynCast { expr, .. } => self.readonly_place(expr),
            ir::Expr::Call { call, .. } => matches!(&**call,
                ir::Call::FreeFunction { function, args, .. }
                if matches!(self.symbols[*function].intrinsic(), Some(ir::Intrinsic::Deref | ir::Intrinsic::AsRef | ir::Intrinsic::WeakRefToRef | ir::Intrinsic::RefToWeakRef))
                    && args.first().is_some_and(|arg| self.readonly_place(arg))),
            _ => false,
        }
    }

    fn reject(&mut self, expr: &ir::Expr<'ctx>) {
        if self.readonly_place(expr) {
            self.reporter.report(Diagnostic::Other(
                Box::new(ConstParameterWrite),
                DiagnosticLevel::Error,
                expr.span(),
            ));
        }
    }

    fn mutable_type(&self, typ: &Type<'ctx>) -> bool {
        typ.ref_type().is_some()
            || typ.upper_bound().is_some_and(|typ| {
                typ.id() == predef::ARRAY
                    || self
                        .symbols
                        .get_type(typ.id())
                        .and_then(|def| def.schema().as_aggregate())
                        .is_some_and(|agg| !agg.flags().is_struct() && !agg.flags().is_never_ref())
            })
    }
}

impl<'ctx> Visitor<'ctx> for ConstWrites<'_, 'ctx> {
    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, _span: Span) {
        self.reject(place);
        self.visit_expr(place);
        self.visit_expr(value);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, _span: Span) {
        let intrinsic = match call {
            ir::Call::FreeFunction { function, .. } => self.symbols[*function].intrinsic(),
            _ => None,
        };
        let params = match call {
            ir::Call::FreeFunction { function, .. } => self.symbols[*function].type_().params(),
            ir::Call::Instance { method, .. } | ir::Call::Static { method, .. } => {
                self.symbols[*method].type_().params()
            }
            ir::Call::Closure { .. } => &[],
        };
        if matches!(
            intrinsic,
            Some(
                ir::Intrinsic::ArrayClear
                    | ir::Intrinsic::ArrayResize
                    | ir::Intrinsic::ArrayPush
                    | ir::Intrinsic::ArrayPop
                    | ir::Intrinsic::ArrayInsert
                    | ir::Intrinsic::ArrayRemove
                    | ir::Intrinsic::ArrayGrow
                    | ir::Intrinsic::ArrayErase
                    | ir::Intrinsic::ArraySort
                    | ir::Intrinsic::ArraySortByPredicate
            )
        ) {
            if let Some(receiver) = call.args().first() {
                self.reject(receiver);
            }
        } else if intrinsic.is_none() {
            for (arg, param) in call.args().iter().zip(params) {
                if param.flags().is_out()
                    || (!self.policy.const_param_non_out_forwarding
                        && !param.flags().is_const()
                        && self.mutable_type(param.type_()))
                {
                    self.reject(arg);
                }
            }
        }
        // Function values do not carry parameter flags, but their resolved
        // argument types still identify reference/container forwarding.
        if !self.policy.const_param_non_out_forwarding
            && let ir::Call::Closure {
                closure_type, args, ..
            } = call
            && let Ok(typ) = closure_type.coalesced(self.symbols)
        {
            for (arg, typ) in args.iter().zip(typ.args()) {
                if self.mutable_type(typ) {
                    self.reject(arg);
                }
            }
        }
        if let Some(receiver) = call.receiver() {
            self.visit_expr(receiver);
        }
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }

    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, _span: Span) {
        // Capture IDs refer to the defining scope, not the closure's parameters.
        // Do not accidentally treat its Param(0) as the enclosing const Param(0).
        let captures = closure
            .captures
            .iter()
            .copied()
            .filter(|local| self.readonly.contains(local) || self.captures.contains(local))
            .collect();
        let readonly = std::mem::take(&mut self.readonly);
        let old_captures = std::mem::replace(&mut self.captures, captures);
        self.visit_block(&closure.block);
        self.readonly = readonly;
        self.captures = old_captures;
    }
}

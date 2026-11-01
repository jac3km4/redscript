use std::collections::HashMap;

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

#[derive(Debug, thiserror::Error)]
#[error("cannot write through a const local")]
struct ConstLocalWrite;

#[derive(Debug, Clone, Copy)]
enum ConstRoot {
    Parameter,
    Local { rebindable: bool },
    ParameterAndLocal,
}

impl ConstRoot {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Parameter, Self::Parameter) => Self::Parameter,
            (Self::Local { rebindable: a }, Self::Local { rebindable: b }) => {
                Self::Local { rebindable: a && b }
            }
            _ => Self::ParameterAndLocal,
        }
    }
}

pub(super) fn validate<'policy, 'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    policy: impl Fn(Span) -> &'policy SemanticPolicy,
    reporter: &mut CompileErrorReporter<'ctx>,
) {
    let mut check = |function: &crate::LoweredFunction<'ctx>, params: &[Param<'ctx>]| {
        let mut readonly: HashMap<_, _> = params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                p.flags()
                    .is_const()
                    .then_some((ir::Local::Param(i as u16), ConstRoot::Parameter))
            })
            .collect();
        add_const_locals(
            &mut readonly,
            &function.locals,
            symbols,
            policy(function.span),
        );
        ConstWrites {
            symbols,
            readonly,
            captures: HashMap::new(),
            initializers: declaration_initializers(&function.locals),
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

fn add_const_locals<'ctx>(
    roots: &mut HashMap<ir::Local, ConstRoot>,
    locals: &[ir::LocalInfo<'ctx>],
    symbols: &Symbols<'ctx>,
    policy: &SemanticPolicy,
) {
    for local in locals.iter().filter(|local| local.is_const) {
        let rebindable = policy.const_local_handle_rebinding
            && local.typ.coalesce(symbols).is_ok_and(|typ| {
                let typ = typ.upper_bound();
                let typ = typ.and_then(|typ| match typ.strip_ref() {
                    Some((crate::RefType::Strong | crate::RefType::Weak, inner)) => {
                        inner.upper_bound()
                    }
                    Some(_) => None,
                    None => Some(typ),
                });
                typ.is_some_and(|typ| {
                    symbols
                        .get_type(typ.id())
                        .and_then(|def| def.schema().as_aggregate())
                        .is_some_and(|agg| !agg.flags().is_struct() && !agg.flags().is_never_ref())
                })
            });
        roots.insert(local.id, ConstRoot::Local { rebindable });
    }
}

fn declaration_initializers(locals: &[ir::LocalInfo<'_>]) -> HashMap<ir::Local, Span> {
    locals
        .iter()
        .filter_map(|local| local.initializer.map(|span| (local.id, span)))
        .collect()
}

struct ConstWrites<'a, 'ctx> {
    symbols: &'a Symbols<'ctx>,
    readonly: HashMap<ir::Local, ConstRoot>,
    captures: HashMap<ir::Local, ConstRoot>,
    initializers: HashMap<ir::Local, Span>,
    policy: &'a SemanticPolicy,
    reporter: &'a mut CompileErrorReporter<'ctx>,
}

impl<'ctx> ConstWrites<'_, 'ctx> {
    fn readonly_place(&self, expr: &ir::Expr<'ctx>) -> Option<ConstRoot> {
        match expr {
            ir::Expr::Local(local, _) => self.readonly.get(local).copied(),
            ir::Expr::Capture(local, _) => self.captures.get(local).copied(),
            ir::Expr::Field { receiver, .. } => self.readonly_place(receiver),
            ir::Expr::Index { array, .. } => self.readonly_place(array).and_then(|root| {
                match (root, self.policy.const_param_element_writes) {
                    (ConstRoot::Parameter, true) => None,
                    (ConstRoot::ParameterAndLocal, true) => {
                        Some(ConstRoot::Local { rebindable: false })
                    }
                    _ => Some(root),
                }
            }),
            ir::Expr::Conditional { then, else_, .. } => {
                match (self.readonly_place(then), self.readonly_place(else_)) {
                    (Some(a), Some(b)) => Some(a.merge(b)),
                    (a, b) => a.or(b),
                }
            }
            ir::Expr::DynCast { expr, .. } => self.readonly_place(expr),
            ir::Expr::Call { call, .. } => match &**call {
                ir::Call::FreeFunction { function, args, .. }
                    if matches!(
                        self.symbols[*function].intrinsic(),
                        Some(
                            ir::Intrinsic::Deref
                                | ir::Intrinsic::AsRef
                                | ir::Intrinsic::WeakRefToRef
                                | ir::Intrinsic::RefToWeakRef
                        )
                    ) =>
                {
                    args.first().and_then(|arg| self.readonly_place(arg))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn reject(&mut self, expr: &ir::Expr<'ctx>) {
        if let Some(root) = self.readonly_place(expr) {
            let error: Box<dyn std::error::Error> = match root {
                ConstRoot::Parameter | ConstRoot::ParameterAndLocal => {
                    Box::new(ConstParameterWrite)
                }
                ConstRoot::Local { .. } => Box::new(ConstLocalWrite),
            };
            self.reporter.report(Diagnostic::Other(
                error,
                DiagnosticLevel::Error,
                expr.span(),
            ));
        }
    }

    fn rebindable_handle(&self, expr: &ir::Expr<'ctx>) -> bool {
        match expr {
            ir::Expr::Local(..) | ir::Expr::Capture(..) => matches!(
                self.readonly_place(expr),
                Some(ConstRoot::Local { rebindable: true })
            ),
            ir::Expr::DynCast { expr, .. } => self.rebindable_handle(expr),
            ir::Expr::Conditional { then, else_, .. } => {
                (self.readonly_place(then).is_none() || self.rebindable_handle(then))
                    && (self.readonly_place(else_).is_none() || self.rebindable_handle(else_))
            }
            ir::Expr::Call { call, .. } => matches!(&**call,
                ir::Call::FreeFunction { function, args, .. }
                if matches!(self.symbols[*function].intrinsic(), Some(ir::Intrinsic::WeakRefToRef | ir::Intrinsic::RefToWeakRef))
                    && args.first().is_some_and(|arg| self.rebindable_handle(arg))),
            _ => false,
        }
    }

    fn reject_forwarding(&mut self, expr: &ir::Expr<'ctx>) {
        if !self.rebindable_handle(expr) {
            self.reject(expr);
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
    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, span: Span) {
        // Only the declaration's store is initialization. In particular, writes
        // nested in its value and writes to captured bindings are still checked.
        let initializer = if let ir::Expr::Local(local, _) = place
            && self.initializers.get(local) == Some(&span)
        {
            self.initializers.remove(local);
            !self.policy.const_local_handle_rebinding
        } else {
            false
        };
        if !initializer
            && (!matches!(place, ir::Expr::Local(..) | ir::Expr::Capture(..))
                || !matches!(
                    self.readonly_place(place),
                    Some(ConstRoot::Local { rebindable: true })
                ))
        {
            self.reject(place);
        }
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
                    || ((!self.policy.const_param_non_out_forwarding
                        || matches!(
                            self.readonly_place(arg),
                            Some(ConstRoot::Local { .. } | ConstRoot::ParameterAndLocal)
                        ))
                        && !param.flags().is_const()
                        && self.mutable_type(param.type_()))
                {
                    self.reject_forwarding(arg);
                }
            }
        }
        // Function values do not carry parameter flags, but their resolved
        // argument types still identify reference/container forwarding.
        if let ir::Call::Closure {
            closure_type, args, ..
        } = call
            && let Ok(typ) = closure_type.coalesced(self.symbols)
        {
            for (arg, typ) in args.iter().zip(typ.args()) {
                if self.mutable_type(typ)
                    && (!self.policy.const_param_non_out_forwarding
                        || matches!(
                            self.readonly_place(arg),
                            Some(ConstRoot::Local { .. } | ConstRoot::ParameterAndLocal)
                        ))
                {
                    self.reject_forwarding(arg);
                }
            }
        }
        if let ir::Call::Instance {
            method, receiver, ..
        } = call
            && !self.symbols[*method].flags().is_const()
            && matches!(
                self.readonly_place(receiver),
                Some(ConstRoot::Local { .. } | ConstRoot::ParameterAndLocal)
            )
        {
            self.reject(receiver);
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
            .filter_map(|local| {
                self.readonly
                    .get(local)
                    .or_else(|| self.captures.get(local))
                    .map(|root| (*local, *root))
            })
            .collect();
        let readonly = std::mem::take(&mut self.readonly);
        add_const_locals(
            &mut self.readonly,
            &closure.locals,
            self.symbols,
            self.policy,
        );
        let old_captures = std::mem::replace(&mut self.captures, captures);
        let initializers = std::mem::replace(
            &mut self.initializers,
            declaration_initializers(&closure.locals),
        );
        self.visit_block(&closure.block);
        self.initializers = initializers;
        self.readonly = readonly;
        self.captures = old_captures;
    }
}

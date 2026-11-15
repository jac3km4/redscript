use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use redscript_community_ast::Span;

use crate::ir::Visitor;
use crate::{
    CompileErrorReporter, Diagnostic, LoweredCompilationUnit, MethodId, Param, RefType,
    SemanticPolicy, Symbols, Type, TypeApp, TypeId, ir, predef,
};

#[derive(Debug, Clone, Copy)]
enum ConstViolation {
    Write,
    Forwarding,
    ClosureForwarding,
    ScriptRefBinding,
}

#[derive(Debug, Clone, Copy)]
enum ConstRoot {
    Parameter,
    This,
    Local { rebindable: bool },
    ParameterAndLocal,
    Return,
}

impl ConstRoot {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Parameter, Self::Parameter) => Self::Parameter,
            (Self::This, Self::This) => Self::This,
            (Self::Local { rebindable: a }, Self::Local { rebindable: b }) => {
                Self::Local { rebindable: a && b }
            }
            (Self::Return, _) | (_, Self::Return) => Self::Return,
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
    let mut check = |function: &crate::LoweredFunction<'ctx>,
                     params: &[Param<'ctx>],
                     is_const_method: bool| {
        let func_policy = policy(function.span);
        let mut readonly: HashMap<_, _> = params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                p.flags()
                    .is_const()
                    .then_some((ir::Local::Param(i as u16), ConstRoot::Parameter))
            })
            .collect();
        if is_const_method && !func_policy.const_param_non_out_forwarding {
            readonly.insert(ir::Local::This, ConstRoot::This);
        }
        add_const_locals(&mut readonly, &function.locals, symbols, func_policy);
        let mut script_ref_locals: HashSet<_> = params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                (p.type_().ref_type() == Some(RefType::Script))
                    .then_some(ir::Local::Param(i as u16))
            })
            .collect();
        add_script_ref_locals(&mut script_ref_locals, &function.locals, symbols);
        let readonly_array_temps =
            readonly_array_temps(&function.locals, &readonly, &HashMap::new());
        ConstWrites {
            symbols,
            readonly,
            captures: HashMap::new(),
            readonly_array_temps,
            script_ref_locals,
            script_ref_captures: HashSet::new(),
            initializers: declaration_initializers(&function.locals),
            policy: func_policy,
            reporter,
        }
        .visit_block(&function.block);
    };
    for (id, function) in &unit.functions {
        check(function, symbols[*id].type_().params(), false);
    }
    for (id, class) in &unit.classes {
        for (method, function) in &class.methods {
            let sym = &symbols[MethodId::new(*id, *method)];
            check(
                function,
                sym.type_().params(),
                sym.flags().is_const() && !sym.flags().is_static(),
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
        let sym = &symbols[*id];
        check(
            function,
            sym.type_().params(),
            sym.flags().is_const() && !sym.flags().is_static(),
        );
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

fn add_script_ref_locals<'ctx>(
    locals_set: &mut HashSet<ir::Local>,
    locals: &[ir::LocalInfo<'ctx>],
    symbols: &Symbols<'ctx>,
) {
    for local in locals {
        if local.typ.ref_type(symbols) == Some(RefType::Script) {
            locals_set.insert(local.id);
        }
    }
}

fn readonly_array_temps(
    locals: &[ir::LocalInfo<'_>],
    readonly: &HashMap<ir::Local, ConstRoot>,
    captures: &HashMap<ir::Local, ConstRoot>,
) -> HashSet<ir::Local> {
    locals
        .iter()
        .filter_map(|local| match local.temporary_owner {
            Some(ir::TemporaryOwner::Destination(dest))
                if readonly.contains_key(&dest) || captures.contains_key(&dest) =>
            {
                Some(local.id)
            }
            _ => None,
        })
        .collect()
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
    readonly_array_temps: HashSet<ir::Local>,
    script_ref_locals: HashSet<ir::Local>,
    script_ref_captures: HashSet<ir::Local>,
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
                ir::Call::FreeFunction { function, .. } => {
                    let typ = self.symbols[*function].type_();
                    (typ.is_const_return() && typ.return_type().ref_type() == Some(RefType::Script))
                        .then_some(ConstRoot::Return)
                }
                ir::Call::Instance { method, .. } | ir::Call::Static { method, .. } => {
                    let typ = self.symbols[*method].type_();
                    (typ.is_const_return() && typ.return_type().ref_type() == Some(RefType::Script))
                        .then_some(ConstRoot::Return)
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn reject(&mut self, expr: &ir::Expr<'ctx>, violation: ConstViolation) {
        if let Some(root) = self.readonly_place(expr) {
            let diagnostic = match violation {
                ConstViolation::Write => match root {
                    ConstRoot::Parameter | ConstRoot::ParameterAndLocal => {
                        Diagnostic::ConstParameterWrite(expr.span())
                    }
                    ConstRoot::This => Diagnostic::ConstReceiverWrite(expr.span()),
                    ConstRoot::Local { .. } => Diagnostic::ConstLocalWrite(expr.span()),
                    ConstRoot::Return => Diagnostic::ConstReturnWrite(expr.span()),
                },
                ConstViolation::Forwarding
                | ConstViolation::ClosureForwarding
                | ConstViolation::ScriptRefBinding => {
                    let root = match root {
                        ConstRoot::Parameter => "parameter",
                        ConstRoot::This => "receiver",
                        ConstRoot::Local { .. } => "local",
                        ConstRoot::ParameterAndLocal => "parameter or local",
                        ConstRoot::Return => "return",
                    };
                    let target = match violation {
                        ConstViolation::ClosureForwarding => "function value",
                        ConstViolation::ScriptRefBinding => "non-const script_ref",
                        _ => "non-const parameter",
                    };
                    Diagnostic::ConstForwarding {
                        root,
                        target,
                        span: expr.span(),
                    }
                }
            };
            self.reporter.report(diagnostic);
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

    fn reject_forwarding(&mut self, expr: &ir::Expr<'ctx>, violation: ConstViolation) {
        if !self.rebindable_handle(expr) {
            self.reject(expr, violation);
        }
    }

    fn has_script_ref_value(&self, expr: &ir::Expr<'ctx>) -> bool {
        match expr {
            ir::Expr::Local(local, _) => self.script_ref_locals.contains(local),
            ir::Expr::Capture(local, _) => self.script_ref_captures.contains(local),
            ir::Expr::Field { field, .. } => {
                self.symbols[*field].type_().ref_type() == Some(RefType::Script)
            }
            ir::Expr::Index { array_type, .. } => {
                array_type.arg_ref_type(self.symbols, 0) == Some(RefType::Script)
            }
            ir::Expr::Call { call, .. } => match &**call {
                ir::Call::FreeFunction { function, .. } => {
                    self.symbols[*function].intrinsic() == Some(ir::Intrinsic::AsRef)
                        || self.symbols[*function].type_().return_type().ref_type()
                            == Some(RefType::Script)
                }
                ir::Call::Instance { method, .. } | ir::Call::Static { method, .. } => {
                    self.symbols[*method].type_().return_type().ref_type() == Some(RefType::Script)
                }
                ir::Call::Closure { .. } => false,
            },
            ir::Expr::Conditional { then, else_, .. } => {
                self.has_script_ref_value(then) || self.has_script_ref_value(else_)
            }
            _ => false,
        }
    }

    fn check_script_ref_store(&mut self, value: &ir::Expr<'ctx>) {
        if self.has_script_ref_value(value)
            && let Some(root) = self.readonly_place(value)
            && (!self.policy.const_param_non_out_forwarding
                || matches!(
                    root,
                    ConstRoot::Local { .. } | ConstRoot::ParameterAndLocal | ConstRoot::Return
                ))
        {
            self.reject_forwarding(value, ConstViolation::ScriptRefBinding);
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

    fn call_type_env(&self, call: &ir::Call<'ctx>) -> HashMap<&'ctx str, Type<'ctx>> {
        let mut env = HashMap::new();
        let bind_args = |env: &mut HashMap<&'ctx str, Type<'ctx>>,
                         params: &[Rc<crate::CtxVar<'ctx, crate::Immutable>>],
                         args: &[ir::Type<'ctx>]| {
            for (var, arg) in params.iter().zip(args) {
                if let Ok(typ) = arg.coalesced(self.symbols) {
                    env.insert(var.name(), typ);
                }
            }
        };
        match call {
            ir::Call::FreeFunction {
                function,
                type_args,
                ..
            } => {
                bind_args(
                    &mut env,
                    self.symbols[*function].type_().type_params(),
                    type_args,
                );
            }
            ir::Call::Static {
                parent_id,
                parent_type_args,
                method,
                type_args,
                ..
            } => {
                if let Some(app) = instantiate_type_app_as(
                    TypeApp::new(
                        *parent_id,
                        parent_type_args
                            .iter()
                            .filter_map(|t| t.coalesced(self.symbols).ok())
                            .collect::<Rc<_>>(),
                    ),
                    method.parent(),
                    self.symbols,
                ) {
                    for (var, arg) in self.symbols[method.parent()]
                        .params()
                        .iter()
                        .zip(app.args())
                    {
                        env.insert(var.name(), arg.clone());
                    }
                }
                bind_args(
                    &mut env,
                    self.symbols[*method].type_().type_params(),
                    type_args,
                );
            }
            ir::Call::Instance {
                receiver_type,
                method,
                type_args,
                ..
            } => {
                if let Ok(app) = receiver_type.coalesced(self.symbols)
                    && let Some(app) =
                        instantiate_type_app_as(app, method.parent(), self.symbols)
                {
                    for (var, arg) in self.symbols[method.parent()]
                        .params()
                        .iter()
                        .zip(app.args())
                    {
                        env.insert(var.name(), arg.clone());
                    }
                }
                bind_args(
                    &mut env,
                    self.symbols[*method].type_().type_params(),
                    type_args,
                );
            }
            ir::Call::Closure { .. } => {}
        }
        env
    }
}

fn substitute_type<'ctx>(typ: &Type<'ctx>, env: &HashMap<&'ctx str, Type<'ctx>>) -> Type<'ctx> {
    if env.is_empty() {
        return typ.clone();
    }
    match typ {
        Type::Ctx(var) => env.get(var.name()).cloned().unwrap_or_else(|| typ.clone()),
        Type::Data(app) if !app.args().is_empty() => {
            let args: Rc<[Type<'ctx>]> = app
                .args()
                .iter()
                .map(|arg| substitute_type(arg, env))
                .collect();
            Type::app(app.id(), args)
        }
        _ => typ.clone(),
    }
}

fn instantiate_type_app_as<'ctx>(
    mut cur: TypeApp<'ctx>,
    target: TypeId<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Option<TypeApp<'ctx>> {
    while cur.id() != target {
        let class = &symbols[cur.id()];
        let base = class.schema().base_type()?;
        let env: HashMap<&'ctx str, Type<'ctx>> =
            class.vars().zip(cur.args().iter().cloned()).collect();
        let args: Rc<[Type<'ctx>]> = base
            .args()
            .iter()
            .map(|arg| substitute_type(arg, &env))
            .collect();
        cur = TypeApp::new(base.id(), args);
    }
    Some(cur)
}

impl<'ctx> Visitor<'ctx> for ConstWrites<'_, 'ctx> {
    fn visit_init_array(
        &mut self,
        local: ir::Local,
        elements: &[ir::Expr<'ctx>],
        _element_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        if !self.readonly.contains_key(&local) && !self.readonly_array_temps.contains(&local) {
            for elem in elements {
                self.check_script_ref_store(elem);
            }
        }
        self.visit_local(local, span);
        for elem in elements {
            self.visit_expr(elem);
        }
    }

    fn visit_new_struct(
        &mut self,
        _struct_: &ir::TypeApp<'ctx>,
        args: &[ir::Expr<'ctx>],
        _span: Span,
    ) {
        for arg in args {
            self.check_script_ref_store(arg);
            self.visit_expr(arg);
        }
    }

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
            self.reject(place, ConstViolation::Write);
        }
        if self.readonly_place(place).is_none() {
            self.check_script_ref_store(value);
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
                self.reject(receiver, ConstViolation::Write);
                if self.readonly_place(receiver).is_none() {
                    match (intrinsic, call.args()) {
                        (Some(ir::Intrinsic::ArrayPush), [_, elem])
                        | (Some(ir::Intrinsic::ArrayInsert), [_, _, elem]) => {
                            self.check_script_ref_store(elem);
                        }
                        _ => {}
                    }
                }
            }
        } else if intrinsic == Some(ir::Intrinsic::ToVariant) {
            if let ir::Call::FreeFunction {
                type_args, args, ..
            } = call
                && let (Some(arg), Some(type_arg)) = (args.first(), type_args.first())
                && let Ok(typ) = type_arg.coalesced(self.symbols)
                && self.mutable_type(&typ)
                && (!self.policy.const_param_non_out_forwarding
                    || matches!(
                        self.readonly_place(arg),
                        Some(
                            ConstRoot::Local { .. }
                                | ConstRoot::ParameterAndLocal
                                | ConstRoot::Return
                        )
                    ))
            {
                self.reject_forwarding(arg, ConstViolation::Forwarding);
            }
        } else if intrinsic.is_none() {
            let type_env = self.call_type_env(call);
            for (arg, param) in call.args().iter().zip(params) {
                let param_type = substitute_type(param.type_(), &type_env);
                if param.flags().is_out() {
                    self.reject_forwarding(arg, ConstViolation::Write);
                } else if (!self.policy.const_param_non_out_forwarding
                    || matches!(
                        self.readonly_place(arg),
                        Some(
                            ConstRoot::Local { .. }
                                | ConstRoot::ParameterAndLocal
                                | ConstRoot::Return
                        )
                    ))
                    && !param.flags().is_const()
                    && self.mutable_type(&param_type)
                {
                    self.reject_forwarding(arg, ConstViolation::Forwarding);
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
                            Some(
                                ConstRoot::Local { .. }
                                    | ConstRoot::ParameterAndLocal
                                    | ConstRoot::Return
                            )
                        ))
                {
                    self.reject_forwarding(arg, ConstViolation::ClosureForwarding);
                }
            }
        }
        if let ir::Call::Instance {
            method, receiver, ..
        } = call
            && !self.symbols[*method].flags().is_const()
            && matches!(
                self.readonly_place(receiver),
                Some(root)
                    if !self.policy.const_param_non_out_forwarding
                        || matches!(
                            root,
                            ConstRoot::Local { .. }
                                | ConstRoot::ParameterAndLocal
                                | ConstRoot::Return
                        )
            )
        {
            self.reject(receiver, ConstViolation::Write);
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
        let script_ref_captures = closure
            .captures
            .iter()
            .filter(|local| {
                self.script_ref_locals.contains(local) || self.script_ref_captures.contains(local)
            })
            .copied()
            .collect();
        let readonly = std::mem::take(&mut self.readonly);
        add_const_locals(
            &mut self.readonly,
            &closure.locals,
            self.symbols,
            self.policy,
        );
        let script_ref_locals = std::mem::take(&mut self.script_ref_locals);
        add_script_ref_locals(&mut self.script_ref_locals, &closure.locals, self.symbols);
        let old_readonly_array_temps = std::mem::replace(
            &mut self.readonly_array_temps,
            readonly_array_temps(&closure.locals, &self.readonly, &captures),
        );
        let old_captures = std::mem::replace(&mut self.captures, captures);
        let old_script_ref_captures =
            std::mem::replace(&mut self.script_ref_captures, script_ref_captures);
        let initializers = std::mem::replace(
            &mut self.initializers,
            declaration_initializers(&closure.locals),
        );
        self.visit_block(&closure.block);
        self.initializers = initializers;
        self.readonly = readonly;
        self.readonly_array_temps = old_readonly_array_temps;
        self.script_ref_locals = script_ref_locals;
        self.captures = old_captures;
        self.script_ref_captures = old_script_ref_captures;
    }
}

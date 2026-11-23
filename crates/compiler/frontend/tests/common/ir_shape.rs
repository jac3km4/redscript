//! Canonical, type-free structural description of a lowered function.
//!
//! Raw `Debug` of IR embeds lazily-coalesced type caches (`Uninit`/`Init`) and
//! inference-variable identities, which legitimately differ between two
//! compilations even when the shape is identical. This visitor records only
//! spans, node kinds, local/capture identities and constants, so two independent
//! lowerings of the same function can be compared token-for-token.
//!
//! Included verbatim by the `redscript-compiler-frontend` and
//! `redscript-redmod-adapter` query-parity suites through `#[path]`, which keeps
//! the two copies of the expectation in lockstep without a cross-crate test
//! dependency cycle.

use redscript_community_ast::Span;
use redscript_compiler_frontend::{FieldId, LoweredFunction, RefType, ir};

#[derive(Default)]
pub struct FunctionShape {
    tokens: String,
}

impl FunctionShape {
    fn push(&mut self, token: impl std::fmt::Display) {
        self.tokens.push_str(&token.to_string());
        self.tokens.push(';');
    }
}

impl<'ctx> ir::Visitor<'ctx> for FunctionShape {
    fn visit_block(&mut self, block: &ir::Block<'ctx>) {
        self.push(format_args!("block[{}]", block.stmts.len()));
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&mut self, while_loop: &ir::ConditionalBlock<'ctx>, span: Span) {
        self.push(format_args!("while@{}", span.start));
        self.visit_expr(&while_loop.condition);
        self.visit_block(&while_loop.block);
    }

    fn visit_if(
        &mut self,
        branches: &[ir::ConditionalBlock<'ctx>],
        default: Option<&ir::Block<'ctx>>,
        span: Span,
    ) {
        self.push(format_args!(
            "branches@{}:{}:{}",
            span.start,
            branches.len(),
            default.is_some()
        ));
        for branch in branches {
            self.visit_expr(&branch.condition);
            self.visit_block(&branch.block);
        }
        if let Some(block) = default {
            self.visit_block(block);
        }
    }

    fn visit_switch(
        &mut self,
        scrutinee: &ir::Expr<'ctx>,
        _scrutinee_type: &ir::Type<'ctx>,
        branches: &[ir::Case<'ctx>],
        default_index: usize,
        default: Option<&ir::Block<'ctx>>,
        span: Span,
    ) {
        self.push(format_args!(
            "switch@{}:{}:{}:{}",
            span.start,
            branches.len(),
            default_index,
            default.is_some()
        ));
        self.visit_expr(scrutinee);
        for index in 0..=branches.len() {
            if index == default_index
                && let Some(block) = default
            {
                self.visit_block(block);
            }
            if let Some(branch) = branches.get(index) {
                self.visit_expr(&branch.matcher);
                self.visit_block(&branch.block);
            }
        }
    }

    fn visit_init_array(
        &mut self,
        local: ir::Local,
        elements: &[ir::Expr<'ctx>],
        _element_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("init_array:{local:?}:{}", elements.len()));
        self.visit_local(local, span);
        for element in elements {
            self.visit_expr(element);
        }
    }

    fn visit_init_default(&mut self, local: ir::Local, _typ: &ir::Type<'ctx>, span: Span) {
        self.push(format_args!("init_default:{local:?}"));
        self.visit_local(local, span);
    }

    fn visit_break(&mut self, span: Span) {
        self.push(format_args!("break@{}", span.start));
    }

    fn visit_continue(&mut self, span: Span) {
        self.push(format_args!("continue@{}", span.start));
    }

    fn visit_return(&mut self, expr: Option<&ir::Expr<'ctx>>, span: Span) {
        self.push(format_args!("return@{}:{}", span.start, expr.is_some()));
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_new_instance(&mut self, _class: &ir::TypeApp<'ctx>, span: Span) {
        self.push(format_args!("new@{}", span.start));
    }

    fn visit_construct(
        &mut self,
        _struct_: &ir::TypeApp<'ctx>,
        args: &[ir::Expr<'ctx>],
        span: Span,
    ) {
        self.push(format_args!("struct@{}", span.start));
        for arg in args {
            self.visit_expr(arg);
        }
    }

    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, span: Span) {
        self.push(format_args!("closure@{}", span.start));
        for local in &closure.locals {
            self.push(format_args!(
                "closure_local:{:?}:{:?}",
                local.id, local.name
            ));
        }
        self.push(format_args!("captures:{:?}", closure.captures));
        self.visit_block(&closure.block);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, span: Span) {
        let kind = match call {
            ir::Call::FreeFunction { .. } => "free",
            ir::Call::Static { .. } => "static",
            ir::Call::Instance { .. } => "instance",
            ir::Call::Closure { .. } => "closure",
        };
        self.push(format_args!("call:{kind}@{}", span.start));
        if let Some(receiver) = call.receiver() {
            self.visit_expr(receiver);
        }
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }

    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, span: Span) {
        self.push(format_args!("assign@{}", span.start));
        self.visit_expr(place);
        self.visit_expr(value);
    }

    fn visit_field(
        &mut self,
        receiver: &ir::Expr<'ctx>,
        _receiver_type: &ir::TypeApp<'ctx>,
        _receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("field:{field:?}@{}", span.start));
        self.visit_expr(receiver);
    }

    fn visit_index(
        &mut self,
        array: &ir::Expr<'ctx>,
        _array_type: &ir::Type<'ctx>,
        index: &ir::Expr<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("index@{}", span.start));
        self.visit_expr(array);
        self.visit_expr(index);
    }

    fn visit_conditional(
        &mut self,
        condition: &ir::Expr<'ctx>,
        then_branch: &ir::Expr<'ctx>,
        else_branch: &ir::Expr<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("conditional@{}", span.start));
        self.visit_expr(condition);
        self.visit_expr(then_branch);
        self.visit_expr(else_branch);
    }

    fn visit_dyn_cast(
        &mut self,
        expr: &ir::Expr<'ctx>,
        _expr_type: &ir::Type<'ctx>,
        _target_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("cast@{}", span.start));
        self.visit_expr(expr);
    }

    fn visit_local(&mut self, local: ir::Local, span: Span) {
        self.push(format_args!("local:{local:?}@{}", span.start));
    }

    fn visit_capture(&mut self, capture: ir::Local, span: Span) {
        self.push(format_args!("capture:{capture:?}@{}", span.start));
    }

    fn visit_const(&mut self, constant: &ir::Const<'ctx>) {
        self.push(format_args!("const:{constant:?}"));
    }

    fn visit_null(&mut self, is_weak: bool, span: Span) {
        self.push(format_args!("null:{is_weak}@{}", span.start));
    }
}

/// Records the local table and every reachable node's structural token.
pub fn function_shape(function: &LoweredFunction<'_>) -> String {
    let mut shape = FunctionShape::default();
    for local in &function.locals {
        shape.push(format_args!(
            "local {:?} const={} init={:?} decl={:?} name={:?} purpose={:?} owner={:?} span={:?}",
            local.id,
            local.is_const,
            local.initializer_span,
            local.declaration,
            local.name,
            local.temporary_purpose,
            local.temporary_owner,
            local.name_span,
        ));
    }
    ir::Visitor::visit_block(&mut shape, &function.block);
    shape.tokens
}

/// The expression of a trailing `return`, if the block ends in one.
pub fn maybe_return_expr<'a, 'ctx>(
    function: &'a LoweredFunction<'ctx>,
) -> Option<&'a ir::Expr<'ctx>> {
    match function.block.stmts.back() {
        Some(ir::Stmt::Return(Some(expr), _)) => Some(expr),
        _ => None,
    }
}

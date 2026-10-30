use redscript_community_ast::Span;

use crate::{FieldId, RefType, ir};

pub trait Visitor<'ctx> {
    fn visit_block(&mut self, block: &ir::Block<'ctx>) {
        block.stmts.iter().for_each(|stmt| self.visit_stmt(stmt));
    }

    fn visit_while(&mut self, while_loop: &ir::ConditionalBlock<'ctx>, _span: Span) {
        self.visit_expr(&while_loop.condition);
        self.visit_block(&while_loop.block);
    }

    fn visit_branches(
        &mut self,
        branches: &[ir::ConditionalBlock<'ctx>],
        default: Option<&ir::Block<'ctx>>,
        _span: Span,
    ) {
        branches.iter().for_each(|branch| {
            self.visit_expr(&branch.condition);
            self.visit_block(&branch.block);
        });
        default.inspect(|block| self.visit_block(block));
    }

    fn visit_switch(
        &mut self,
        scrutinee: &ir::Expr<'ctx>,
        _scrutinee_type: &ir::Type<'ctx>,
        branches: &[ir::Case<'ctx>],
        default: Option<&ir::Block<'ctx>>,
        _span: Span,
    ) {
        self.visit_expr(scrutinee);
        branches.iter().for_each(|branch| {
            self.visit_expr(&branch.matcher);
            self.visit_block(&branch.block);
        });
        default.inspect(|block| self.visit_block(block));
    }

    fn visit_init_array(
        &mut self,
        local: ir::Local,
        elements: &[ir::Expr<'ctx>],
        _element_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        self.visit_local(local, span);
        elements.iter().for_each(|element| self.visit_expr(element));
    }

    fn visit_init_default(&mut self, local: ir::Local, _typ: &ir::Type<'ctx>, span: Span) {
        self.visit_local(local, span);
    }

    fn visit_break(&mut self, _span: Span) {}

    fn visit_continue(&mut self, _span: Span) {}

    fn visit_return(&mut self, expr: Option<&ir::Expr<'ctx>>, _span: Span) {
        expr.inspect(|expr| self.visit_expr(expr));
    }

    fn visit_stmt(&mut self, stmt: &ir::Stmt<'ctx>) {
        match stmt {
            ir::Stmt::Expr(expr) => self.visit_expr(expr),
            ir::Stmt::Block(block, _) => self.visit_block(block),
            ir::Stmt::While(while_loop, span) => self.visit_while(while_loop, *span),
            ir::Stmt::Branches {
                branches,
                default,
                span,
            } => self.visit_branches(branches, default.as_ref(), *span),
            ir::Stmt::Switch {
                scrutinee,
                scrutinee_type,
                branches,
                default,
                span,
            } => self.visit_switch(scrutinee, scrutinee_type, branches, default.as_ref(), *span),
            ir::Stmt::InitArray {
                local,
                elements,
                element_type,
                span,
            } => self.visit_init_array(*local, elements, element_type, *span),
            ir::Stmt::InitDefault { local, typ, span } => {
                self.visit_init_default(*local, typ, *span);
            }
            ir::Stmt::Break(span) => self.visit_break(*span),
            ir::Stmt::Continue(span) => self.visit_continue(*span),
            ir::Stmt::Return(expr, span) => self.visit_return(expr.as_deref(), *span),
        }
    }

    fn visit_new_class(&mut self, _class: &ir::TypeApp<'ctx>, _span: Span) {}

    fn visit_new_struct(
        &mut self,
        _struct_: &ir::TypeApp<'ctx>,
        args: &[ir::Expr<'ctx>],
        _span: Span,
    ) {
        args.iter().for_each(|arg| self.visit_expr(arg));
    }

    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, _span: Span) {
        self.visit_block(&closure.block);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, _span: Span) {
        call.receiver()
            .inspect(|receiver| self.visit_expr(receiver));
        call.args().iter().for_each(|arg| self.visit_expr(arg));
    }

    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, _span: Span) {
        self.visit_expr(place);
        self.visit_expr(value);
    }

    fn visit_field(
        &mut self,
        receiver: &ir::Expr<'ctx>,
        _receiver_type: &ir::TypeApp<'ctx>,
        _receiver_ref: Option<RefType>,
        _field: FieldId<'ctx>,
        _span: Span,
    ) {
        self.visit_expr(receiver);
    }

    fn visit_index(
        &mut self,
        array: &ir::Expr<'ctx>,
        _array_type: &ir::Type<'ctx>,
        index: &ir::Expr<'ctx>,
        _span: Span,
    ) {
        self.visit_expr(array);
        self.visit_expr(index);
    }

    fn visit_conditional(
        &mut self,
        condition: &ir::Expr<'ctx>,
        then_branch: &ir::Expr<'ctx>,
        else_branch: &ir::Expr<'ctx>,
        _span: Span,
    ) {
        self.visit_expr(condition);
        self.visit_expr(then_branch);
        self.visit_expr(else_branch);
    }

    fn visit_dyn_cast(
        &mut self,
        expr: &ir::Expr<'ctx>,
        _expr_type: &ir::Type<'ctx>,
        _target_type: &ir::Type<'ctx>,
        _span: Span,
    ) {
        self.visit_expr(expr);
    }

    fn visit_local(&mut self, _local: ir::Local, _span: Span) {}

    fn visit_capture(&mut self, _capture: ir::Local, _span: Span) {}

    fn visit_const(&mut self, _constant: &ir::Const<'ctx>) {}

    fn visit_null(&mut self, _is_weak: bool, _span: Span) {}

    fn visit_expr(&mut self, expr: &ir::Expr<'ctx>) {
        match expr {
            ir::Expr::NewInstance { class_type, span } => self.visit_new_class(class_type, *span),
            ir::Expr::Construct {
                struct_type,
                args,
                span,
            } => self.visit_new_struct(struct_type, args, *span),
            ir::Expr::NewClosure { closure, span } => self.visit_new_closure(closure, *span),
            ir::Expr::Call { call, span } => self.visit_call(call, *span),
            ir::Expr::Assign { place, expr, span } => self.visit_assign(place, expr, *span),
            ir::Expr::Field {
                receiver,
                receiver_type,
                receiver_ref,
                field,
                span,
            } => self.visit_field(receiver, receiver_type, *receiver_ref, *field, *span),
            ir::Expr::Index {
                array,
                array_type,
                index,
                span,
            } => self.visit_index(array, array_type, index, *span),
            ir::Expr::Conditional {
                condition,
                then,
                else_,
                span,
            } => self.visit_conditional(condition, then, else_, *span),
            ir::Expr::DynCast {
                expr,
                expr_type,
                target_type,
                span,
            } => self.visit_dyn_cast(expr, expr_type, target_type, *span),
            ir::Expr::Local(local, span) => self.visit_local(*local, *span),
            ir::Expr::Capture(local, span) => self.visit_capture(*local, *span),
            ir::Expr::Const(const_, _) => self.visit_const(const_),
            ir::Expr::Null { is_weak, span } => self.visit_null(*is_weak, *span),
        }
    }
}

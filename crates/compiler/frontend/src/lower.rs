use std::cell::Cell;
use std::fmt::Debug;
use std::rc::Rc;
use std::{iter, mem, slice};

use ast::AstVisitor;
use bon::bon;
use env::{Capture, Locals};
pub use env::{Env, SemanticPolicy, TypeEnv, TypeRef};
pub use error::{CoalesceError, Error, LowerResult, TypeError};
use redscript_community_ast as ast;
use redscript_community_ast::{Span, Spanned};
use types::Coercion;
pub use types::{InferredTypeApp, Poly, PolyType};

use crate::diagnostic::{ErrorWithSpan, FunctionSignature};
use crate::lower::error::InaccessibleMember;
use crate::symbols::{
    AnyBaseType, FieldId, FreeFunctionIndex, FunctionEntry, FunctionKey, FunctionKind,
    FunctionType, Symbols, TypeSchema, Visibility,
};
use crate::types::{RefType, Type, TypeApp, TypeId, predef};
use crate::utils::ScopedMap;
use crate::{IndexSet, LowerReporter, MethodId, Param, Variance, ir};

mod calls;
mod env;
mod error;
mod pattern;
mod simplify;
mod types;

#[derive(Debug)]
pub struct Lower<'scope, 'ctx> {
    locals: Locals<'scope, 'ctx>,
    return_type: PolyType<'ctx>,
    captures: IndexSet<Capture>,
    stmt_prefix: Vec<Vec<ir::Stmt<'ctx>>>,
    symbols: &'scope Symbols<'ctx>,
    reporter: &'scope mut LowerReporter<'ctx>,
    context: Option<TypeId<'ctx>>,
    does_return_value: bool,
    hoisted_declarations: crate::IndexMap<Span, Option<ir::LocalInfo<'ctx>>>,
}

/// Collect explicitly typed locals in the current function, not nested closures.
#[derive(Default)]
struct LocalDeclarations<'ctx>(Vec<(Spanned<&'ctx str>, Spanned<ast::SourceType<'ctx>>)>);

impl<'ctx> ast::AstVisitor<'ctx, ast::WithSpan> for LocalDeclarations<'ctx> {
    type Error = std::convert::Infallible;

    fn visit_let(
        &mut self,
        name: &Spanned<&'ctx str>,
        typ: &Option<Box<Spanned<ast::SourceType<'ctx>>>>,
        _value: &Option<Box<Spanned<ast::SourceExpr<'ctx>>>>,
    ) -> Result<(), Self::Error> {
        if let Some(typ) = typ {
            self.0.push((*name, typ.as_ref().clone()));
        }
        Ok(())
    }

    fn visit_lambda(
        &mut self,
        _params: &[Spanned<ast::SourceParam<'ctx>>],
        _body: &ast::SourceFunctionBody<'ctx>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[bon]
impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    fn new(
        locals: Locals<'scope, 'ctx>,
        return_type: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &'scope Symbols<'ctx>,
        reporter: &'scope mut LowerReporter<'ctx>,
    ) -> Self {
        Self {
            locals,
            return_type,
            captures: IndexSet::default(),
            stmt_prefix: Vec::new(),
            symbols,
            reporter,
            context,
            does_return_value: false,
            hoisted_declarations: crate::IndexMap::default(),
        }
    }

    #[inline]
    fn into_output(self) -> LowerOutput<'ctx> {
        let mut captures = self.captures;
        // Nested lambdas may capture locals owned by this function, not its caller.
        captures.retain(|cap| cap.scope_depth < self.locals.depth());
        LowerOutput::new(self.locals.into_vec(), captures)
    }

    pub fn function(
        body: &ast::SourceFunctionBody<'ctx>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        env: Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &Symbols<'ctx>,
    ) -> (ir::Block<'ctx>, LowerOutput<'ctx>, Vec<Error<'ctx>>) {
        let mut reporter = LowerReporter::default();
        let (block, output) = Lower::function_builder()
            .body(body)
            .local_counter(&Cell::new(0))
            .params(params)
            .env(env)
            .return_type(return_t)
            .maybe_context(context)
            .symbols(symbols)
            .reporter(&mut reporter)
            .build();
        (block, output, reporter.into_reported())
    }

    #[builder(finish_fn = build)]
    fn function_builder(
        body: &ast::SourceFunctionBody<'ctx>,
        local_counter: &Cell<u16>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        mut env: Env<'_, 'ctx>,
        return_type: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
        fn inner<'ctx>(
            symbols: &Symbols<'ctx>,
            reporter: &mut LowerReporter<'ctx>,
            locals: Locals<'_, 'ctx>,
            ret_t: PolyType<'ctx>,
            context: Option<TypeId<'ctx>>,
            body: &ast::SourceFunctionBody<'ctx>,
            env: &mut Env<'_, 'ctx>,
        ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
            match body {
                ast::FunctionBody::Block(block) => {
                    let mut lower = Lower::new(locals, ret_t.clone(), context, symbols, reporter);
                    let mut prologue = Vec::new();
                    if let Some((_, span)) = block.stmts.first()
                        && env.types().policy(*span).hoist_locals
                    {
                        let mut declarations = LocalDeclarations::default();
                        let Ok(()) = declarations.visit_block(block);
                        let mut names: IndexSet<_> = env.locals().top().keys().copied().collect();
                        for ((name, name_span), (typ, type_span)) in declarations.0 {
                            lower.hoisted_declarations.insert(name_span, None);
                            if !names.insert(name) {
                                lower
                                    .reporter
                                    .report(Error::LocalRedefinition(name, name_span));
                                continue;
                            }
                            let resolved = lower.resolve_type(&typ, env, type_span);
                            if let Some(typ) = lower.reporter.unwrap_err(resolved) {
                                let local = lower
                                    .locals
                                    .add_var(Some(name), typ.clone(), name_span)
                                    .clone();
                                env.define_local(name, local.clone());
                                lower
                                    .hoisted_declarations
                                    .insert(name_span, Some(local.clone()));
                                prologue.push(ir::Stmt::InitDefault {
                                    local: local.id,
                                    typ: typ.into(),
                                    span: name_span,
                                });
                            }
                        }
                    }
                    let mut block = lower.lower_block(&block.stmts, env);
                    block.push_prologue(prologue);
                    if !lower.does_return_value && ret_t.is_fresh() {
                        ret_t
                            .constrain_base(&PolyType::nullary(predef::VOID), symbols)
                            .ok();
                    }
                    (block, lower.into_output())
                }
                ast::FunctionBody::Inline(expr) => {
                    let result = Lower::expr(expr, locals, env, ret_t, context, symbols, reporter);
                    let (body, output) = reporter.unwrap_err(result).unzip();
                    (body.unwrap_or_default(), output.unwrap_or_default())
                }
            }
        }

        let mut locals = Locals::new(local_counter, env.locals().scope_iter().count());
        for (name, typ) in params {
            env.define_local(name, locals.add_param(typ, None).clone());
        }
        inner(
            symbols,
            reporter,
            locals,
            return_type,
            context,
            body,
            &mut env,
        )
    }

    pub fn constant(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
        expected: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> (Option<ir::Const<'ctx>>, Vec<Error<'ctx>>) {
        let mut reporter = LowerReporter::default();
        let counter = Cell::new(0);
        let locals = Locals::new(&counter, 0);
        let mut lower = Lower::new(locals, expected.clone(), None, symbols, &mut reporter);
        let res = (|| {
            let (mut expr, typ) = lower.lower_expr_with(expr, Some(&expected), env)?;
            lower.coerce(&mut expr, typ, expected, env, *span)?;

            if let ir::Expr::Const(const_, _) = &expr {
                Ok(const_.clone())
            } else {
                Err(Error::UnexpectedNonConstant(*span))
            }
        })();
        let res = reporter.unwrap_err(res);
        (res, reporter.into_reported())
    }

    fn expr(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        locals: Locals<'_, 'ctx>,
        env: &Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> LowerResult<'ctx, (ir::Block<'ctx>, LowerOutput<'ctx>)> {
        let mut lower = Lower::new(locals, return_t.clone(), context, symbols, reporter);
        let (mut expr, typ) = lower.lower_expr_with(expr, Some(&return_t), env)?;
        lower.coerce(&mut expr, typ, return_t, env, *span)?;

        let mut stmts = lower.stmt_prefix.pop().unwrap_or_default();
        stmts.push(ir::Stmt::Return(Some(expr.into()), *span));
        Ok((ir::Block::new(stmts), lower.into_output()))
    }

    fn push_prefix(&mut self, stmt: impl Into<ir::Stmt<'ctx>>) {
        let mut top = self.stmt_prefix.pop().unwrap_or_default();
        top.push(stmt.into());
        self.stmt_prefix.push(top);
    }

    fn lower_block(
        &mut self,
        stmts: &[Spanned<ast::SourceStmt<'ctx>>],
        env: &mut Env<'_, 'ctx>,
    ) -> ir::Block<'ctx> {
        let mut accumulator = Vec::with_capacity(stmts.len());
        self.stmt_prefix.push(vec![]);

        for (stmt, span) in stmts {
            let res = self.lower_stmt(stmt, env, *span);
            if let Some(prefix) = self.stmt_prefix.last_mut() {
                accumulator.append(prefix);
            }
            if let Some(stmt) = self.reporter.unwrap_err(res) {
                accumulator.push(stmt);
            }
        }

        self.stmt_prefix.pop();
        ir::Block::new(accumulator)
    }

    #[inline]
    fn lower_scoped_block(
        &mut self,
        block: &[Spanned<ast::SourceStmt<'ctx>>],
        env: &Env<'_, 'ctx>,
    ) -> ir::Block<'ctx> {
        self.lower_block(block, &mut env.introduce_scope())
    }

    fn truth_value(
        &mut self,
        expr: ir::Expr<'ctx>,
        typ: PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Expr<'ctx>> {
        if let Some(intrinsic) = env
            .types()
            .policy(span)
            .condition
            .and_then(|condition| condition(&typ, self.symbols))
        {
            let (call, result_type) = self
                .free_function_call_builder()
                .name(intrinsic)
                .expected_intrinsic(intrinsic)
                .args([(expr, typ)])
                .env(env)
                .span(span)
                .build()?;
            result_type
                .constrain_base(&PolyType::nullary(predef::BOOL), self.symbols)
                .with_span(span)?;
            Ok(ir::Expr::call(call, span))
        } else {
            typ.constrain_base(&PolyType::nullary(predef::BOOL), self.symbols)
                .with_span(span)?;
            Ok(expr)
        }
    }

    fn lower_conditional_block(
        &mut self,
        block: &ast::SourceConditionalBlock<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::ConditionalBlock<'ctx>, Option<ir::Stmt<'ctx>>)> {
        self.stmt_prefix.push(vec![]);
        let result = (|| match &block.condition {
            ast::LetCondition::Expr(expr) => {
                let (_, span) = expr;
                let (condition, expr_t) = self.lower_expr(expr, env)?;
                let condition = self.truth_value(condition, expr_t, env, *span)?;
                let block = self.lower_scoped_block(&block.body.stmts, env);
                Ok((ir::ConditionalBlock::new(condition, block), None))
            }
            ast::LetCondition::LetPattern(pattern, val) => {
                let (_, span) = pattern;
                let (expr, expr_t) = self.lower_expr(val, env)?;

                let local = self
                    .locals
                    .add_temp(
                        ir::TemporaryPurpose::PatternScrutinee,
                        expr_t.clone(),
                        expr.span(),
                    )
                    .id;
                let mut scope = env.introduce_scope();
                let (prologue, condition) = self.lower_pattern_into_prologue_and_condition(
                    local, &expr_t, &mut scope, pattern,
                )?;
                let mut block = self.lower_block(&block.body.stmts, &mut scope);
                block.push_prologue(prologue);

                Ok((
                    ir::ConditionalBlock::new(condition, block),
                    Some(
                        ir::Expr::Assign {
                            place: ir::Expr::Local(local, *span).into(),
                            expr: expr.into(),
                            span: *span,
                        }
                        .into(),
                    ),
                ))
            }
        })();
        let mut prefix = self
            .stmt_prefix
            .pop()
            .expect("condition prefix scope exists");
        result.map(|(block, assignment)| {
            prefix.extend(assignment);
            let prefix = (!prefix.is_empty())
                .then(|| ir::Stmt::Block(ir::Block::new(prefix), block.condition.span()));
            (block, prefix)
        })
    }

    fn lower_case(
        &mut self,
        case: &ast::SourceCase<'ctx>,
        scrutinee_t: &PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, ir::Case<'ctx>> {
        let ast::Condition::Expr(expr) = &case.condition else {
            unimplemented!("pattern matching in regular case statements");
        };
        let (expr, expr_t) = self.lower_expr(expr, env)?;
        expr_t
            .constrain_base(scrutinee_t, self.symbols)
            .with_span(expr.span())?;
        let block = self.lower_scoped_block(&case.body, env);
        Ok(ir::Case::new(expr, block))
    }

    #[inline]
    fn lower_expr(
        &mut self,
        expr: &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        self.lower_expr_with(expr, None, env)
    }

    fn lower_expr_with(
        &mut self,
        (expr, span): &Spanned<ast::SourceExpr<'ctx>>,
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let result = match expr {
            &ast::Expr::Ident(name) => self.resolve_local(name, env, *span)?,
            ast::Expr::Constant(cn) => {
                let const_ = self.lower_constant(cn, hint, *span)?;
                let const_t = PolyType::nullary(const_.type_id());
                (ir::Expr::Const(const_, *span), const_t)
            }
            ast::Expr::ArrayLit(elems) => self.lower_array_lit(elems, hint, env, *span)?,
            ast::Expr::InterpolatedString(elems) => {
                self.lower_interpolated_string(elems, env, *span)?
            }
            ast::Expr::Assign { lhs, rhs } => {
                let lhs_span = lhs.1;
                let rhs_span = rhs.1;
                let (lhs, lhs_t) = self.lower_expr(lhs, env)?;
                let (mut rhs, rhs_t) = self.lower_expr_with(rhs, Some(&lhs_t), env)?;
                self.coerce(&mut rhs, rhs_t, lhs_t, env, rhs_span)?;

                if lhs.is_prvalue(self.symbols) {
                    self.reporter.report(Error::InvalidPlaceExpr(lhs_span));
                }

                let ir = ir::Expr::Assign {
                    place: lhs.into(),
                    expr: rhs.into(),
                    span: *span,
                };
                (ir, PolyType::nullary(predef::VOID))
            }
            ast::Expr::BinOp {
                lhs,
                op: op @ (ast::BinOp::Eq | ast::BinOp::Ne),
                rhs,
            } => {
                let (lhs, lhs_t) = self.lower_expr(lhs, env)?;
                let (rhs, rhs_t) = self.lower_expr(rhs, env)?;
                let mut args = [lhs, rhs];
                let arg_types = [lhs_t, rhs_t];
                let candidates = env.query_free_functions(op.name(), self.symbols);
                let (call, typ) = match self
                    .typed_overload_resolver()
                    .name(op.name())
                    .args(&mut args)
                    .arg_types(&arg_types)
                    .candidates(candidates)
                    .env(env)
                    .span(*span)
                    .resolve()
                {
                    Ok(res) => res.with_args(args).into_call(),
                    Err(Error::UnresolvedFunction(_, _)) => {
                        let intrinsic = match op {
                            ast::BinOp::Eq => ir::Intrinsic::Equals,
                            ast::BinOp::Ne => ir::Intrinsic::NotEquals,
                            _ => unreachable!(),
                        };
                        let candidates = env.query_free_functions(intrinsic.name(), self.symbols);
                        self.typed_overload_resolver()
                            .name(intrinsic.name())
                            .args(&mut args)
                            .arg_types(&arg_types)
                            .candidates(candidates)
                            .env(env)
                            .span(*span)
                            .resolve()?
                            .with_args(args)
                            .into_call()
                    }
                    Err(err) => return Err(err),
                };
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::BinOp { lhs, op, rhs } => {
                let mut lhs = self.lower_expr(lhs, env)?;
                let mut rhs = self.lower_expr(rhs, env)?;
                if env.types().policy(*span).condition.is_some()
                    && matches!(op, ast::BinOp::And | ast::BinOp::Or)
                {
                    lhs = (
                        self.truth_value(lhs.0, lhs.1, env, *span)?,
                        PolyType::nullary(predef::BOOL),
                    );
                    rhs = (
                        self.truth_value(rhs.0, rhs.1, env, *span)?,
                        PolyType::nullary(predef::BOOL),
                    );
                }
                let (call, typ) = self
                    .free_function_call_builder()
                    .name(op.name())
                    .args([lhs, rhs])
                    .env(env)
                    .span(*span)
                    .build()?;
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::UnOp { op, expr } => {
                if matches!(op, ast::UnOp::Neg)
                    && let &(ast::Expr::Constant(ast::Constant::I32(i)), _) = &**expr
                    && let Some(negated) = i.checked_neg()
                {
                    let const_ = self.lower_constant(&ast::Constant::I32(negated), hint, *span)?;
                    let typ = PolyType::nullary(const_.type_id());
                    return Ok((ir::Expr::Const(const_, *span), typ));
                }

                let mut arg = self.lower_expr(expr, env)?;
                if env.types().policy(*span).condition.is_some() && matches!(op, ast::UnOp::Not) {
                    arg = (
                        self.truth_value(arg.0, arg.1, env, *span)?,
                        PolyType::nullary(predef::BOOL),
                    );
                }
                let (call, typ) = self
                    .free_function_call_builder()
                    .name(op.name())
                    .args([arg])
                    .env(env)
                    .span(*span)
                    .build()?;
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::Call {
                expr,
                type_args,
                args,
            } => self.lower_call(expr, type_args, args, hint, env, *span)?,
            ast::Expr::Member { expr, member } => self.lower_member(expr, member, env, *span)?,
            ast::Expr::Index { expr, index } => self.lower_index(expr, index, env, *span)?,
            ast::Expr::DynCast { expr, typ } => self.lower_dyn_cast(expr, typ, env, *span)?,
            ast::Expr::New { typ, args } => self.lower_new(typ, args, env, *span)?,
            ast::Expr::Conditional { cond, then, else_ } => {
                let cond @ (_, cond_span) = &**cond;
                let (cond, cond_t) = self.lower_expr(cond, env)?;
                let cond = self.truth_value(cond, cond_t, env, *cond_span)?;

                let (then, then_t) = self.lower_expr_with(then, hint, env)?;
                let (else_, else_t) = self.lower_expr_with(else_, hint.or(Some(&then_t)), env)?;

                let ir = ir::Expr::Conditional {
                    condition: cond.into(),
                    then: then.into(),
                    else_: else_.into(),
                    span: *span,
                };
                let inferred = then_t.lub(&else_t, self.symbols).with_span(*span)?;
                (ir, inferred)
            }
            ast::Expr::Lambda { params, body } => {
                self.lower_closure(params, body, hint, env, *span)?
            }
            ast::Expr::This | ast::Expr::Super => self.resolve_local("this", env, *span)?,

            ast::Expr::Null => {
                let typ =
                    PolyType::with_bounds(Type::Nothing, Some(Type::nullary(predef::ISCRIPTABLE)));
                let is_weak = hint
                    .and_then(|typ| typ.ref_type(self.symbols))
                    .is_some_and(|ref_t| ref_t == RefType::Weak);
                let typ = if is_weak {
                    Type::app(predef::WREF, [typ]).into_poly()
                } else {
                    typ
                };
                let ir = ir::Expr::Null {
                    is_weak,
                    span: *span,
                };
                (ir, typ)
            }

            ast::Expr::Error => (ir::Expr::null(*span), PolyType::fresh()),
        };
        Ok(result)
    }

    fn lower_stmt(
        &mut self,
        stmt: &ast::SourceStmt<'ctx>,
        env: &mut Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Stmt<'ctx>> {
        let res = match stmt {
            ast::Stmt::Let { name, typ, value } => {
                let &(name, name_span) = name;
                if let Some(local) = self.hoisted_declarations.get(&name_span).cloned() {
                    let Some(local) = local else {
                        // The hoisting pass already reported this declaration's error.
                        return Ok(ir::Stmt::Block(ir::Block::new([]), span));
                    };
                    return if let Some(value @ (_, value_span)) = value.as_deref() {
                        let (mut value, value_t) =
                            self.lower_expr_with(value, Some(&local.typ), env)?;
                        self.coerce(&mut value, value_t, local.typ, env, *value_span)?;
                        Ok(ir::Expr::Assign {
                            place: ir::Expr::Local(local.id, name_span).into(),
                            expr: value.into(),
                            span,
                        }
                        .into())
                    } else {
                        Ok(ir::Stmt::Block(ir::Block::new([]), span))
                    };
                }
                let typ = typ
                    .as_deref()
                    .map(|(typ, span)| self.resolve_type(typ, env, *span))
                    .transpose()?;
                if let Some(value @ (_, value_span)) = value.as_deref() {
                    let (mut value, value_t) = self.lower_expr_with(value, typ.as_ref(), env)?;
                    let typ = if let Some(typ) = typ {
                        self.coerce(&mut value, value_t, typ.clone(), env, *value_span)?;
                        typ
                    } else {
                        value_t
                    };
                    let local = self.locals.add_var(Some(name), typ, name_span);
                    let local = env.define_local(name, local.clone());

                    ir::Expr::Assign {
                        place: ir::Expr::Local(local, name_span).into(),
                        expr: value.into(),
                        span,
                    }
                    .into()
                } else {
                    let typ = typ.unwrap_or_else(PolyType::fresh);
                    let local = self.locals.add_var(Some(name), typ.clone(), name_span);
                    let local = env.define_local(name, local.clone());
                    ir::Stmt::InitDefault {
                        local,
                        typ: typ.into(),
                        span,
                    }
                }
            }
            ast::Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                let (expr, expr_t) = self.lower_expr(expr, env)?;
                let default = default
                    .as_ref()
                    .map(|block| self.lower_scoped_block(block, env));

                let (scrutinee, scrutinee_type, branches) = if cases
                    .iter()
                    .all(|case| matches!(case.condition, ast::Condition::Expr(_)))
                {
                    let branches = cases
                        .iter()
                        .map(|case| self.lower_case(case, &expr_t, env))
                        .collect::<Result<_, _>>()?;
                    (expr.into(), expr_t.into(), branches)
                } else {
                    let span = expr.span();
                    let local = self.extract_local(
                        expr,
                        &expr_t,
                        span,
                        ir::TemporaryPurpose::SwitchScrutinee,
                    );
                    let branches = cases
                        .iter()
                        .enumerate()
                        .map(|(index, case)| {
                            let branch = self.lower_pattern_case(case, local, &expr_t, env)?;
                            if matches!(case.condition, ast::Condition::Expr(_))
                                && cases.get(index + 1).is_some_and(|next| {
                                    matches!(next.condition, ast::Condition::Pattern(_))
                                })
                                && !matches!(
                                    branch.block.stmts.back(),
                                    Some(ir::Stmt::Break(_) | ir::Stmt::Return(_, _))
                                )
                            {
                                self.reporter.report(Error::FallthroughIntoPatternCase(
                                    case.condition.span(),
                                ));
                            }
                            Ok(branch)
                        })
                        .collect::<Result<_, Error<'ctx>>>()?;
                    (
                        ir::Expr::Const(ir::Const::Bool(true), span).into(),
                        PolyType::nullary(predef::BOOL).into(),
                        branches,
                    )
                };

                ir::Stmt::Switch {
                    scrutinee,
                    scrutinee_type,
                    branches,
                    default,
                    span,
                }
            }
            ast::Stmt::If { blocks, else_ } => {
                let blocks = blocks
                    .iter()
                    .map(|block| self.lower_conditional_block(block, env))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut default = else_
                    .as_ref()
                    .map(|block| self.lower_scoped_block(&block.stmts, env));
                let mut branches = Vec::new();
                for (index, (block, prefix)) in blocks.into_iter().enumerate().rev() {
                    branches.insert(0, block);
                    if let Some(prefix) = prefix {
                        if index == 0 {
                            self.push_prefix(prefix);
                        } else {
                            let branch = ir::Stmt::Branches {
                                branches: mem::take(&mut branches).into(),
                                default: default.take(),
                                span,
                            };
                            default = Some(ir::Block::new([prefix, branch]));
                        }
                    }
                }
                ir::Stmt::Branches {
                    branches: branches.into(),
                    default,
                    span,
                }
            }
            ast::Stmt::While(block) => {
                let (block, prefix) = self.lower_conditional_block(block, env)?;
                let Some(prefix) = prefix else {
                    return Ok(ir::Stmt::While(block, span));
                };

                let outer = ir::ConditionalBlock::new(
                    ir::Expr::Const(ir::Const::Bool(true), span),
                    ir::Block::new([
                        prefix,
                        ir::Stmt::Branches {
                            branches: [block].into(),
                            default: Some(ir::Block::new([ir::Stmt::Break(span)])),
                            span,
                        },
                    ]),
                );
                ir::Stmt::While(outer, span)
            }
            ast::Stmt::ForIn { name, iter, body } => {
                let block = self.lower_for_in(name, iter, body, env, span)?;
                ir::Stmt::Block(block, span)
            }
            ast::Stmt::Return(Some(val)) => {
                self.does_return_value = true;
                let val @ (_, val_span) = &**val;
                let return_t = self.return_type.clone();
                let (mut val, val_t) = self.lower_expr_with(val, Some(&return_t), env)?;
                self.coerce(&mut val, val_t, return_t, env, *val_span)?;
                ir::Stmt::Return(Some(val.into()), span)
            }
            ast::Stmt::Return(None) => {
                PolyType::nullary(predef::VOID)
                    .constrain_base(&self.return_type, self.symbols)
                    .with_span(span)?;
                ir::Stmt::Return(None, span)
            }
            ast::Stmt::Break => ir::Stmt::Break(span),
            ast::Stmt::Continue => ir::Stmt::Continue(span),
            ast::Stmt::Expr(expr) => {
                let (expr, _) = self.lower_expr(expr, env)?;
                expr.into()
            }
        };
        Ok(res)
    }

    fn lower_array_lit(
        &mut self,
        elems: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let elem_hint = hint.and_then(|typ| {
            let typ = typ.upper_bound(self.symbols)?;
            (typ.id() == predef::ARRAY)
                .then_some(typ.args().first().cloned())
                .flatten()
        });

        let (elems, elem_typ) =
            elems
                .iter()
                .try_fold((vec![], PolyType::fresh()), |(mut acc, elem_typ), elem| {
                    let (mut elem, typ) = self.lower_expr_with(elem, elem_hint.as_ref(), env)?;
                    let typ = if let Some(hint) = &elem_hint {
                        let span = elem.span();
                        self.coerce(&mut elem, typ.clone(), hint.clone(), env, span)?;
                        hint.clone()
                    } else {
                        typ
                    };
                    acc.push(elem);
                    Ok((acc, elem_typ.lub(&typ, self.symbols).with_span(span)?))
                })?;

        let array_t = Type::app(predef::ARRAY, [elem_typ.clone()]).into_poly();
        let local = self
            .locals
            .add_temp(ir::TemporaryPurpose::ArrayLiteral, array_t.clone(), span)
            .id;

        self.push_prefix(ir::Stmt::InitArray {
            local,
            elements: elems.into(),
            element_type: elem_typ.into(),
            span,
        });

        Ok((ir::Expr::Local(local, span), array_t))
    }

    fn lower_interpolated_string(
        &mut self,
        elems: &[ast::SourceStrPart<'ctx>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let str = elems
            .iter()
            .try_fold(None, |acc, elem| {
                let elem = match elem {
                    ast::StrPart::Expr(expr @ (_, span)) => {
                        let args = [self.lower_expr(expr, env)?];
                        let (call, _) = self
                            .free_function_call_builder()
                            .name(ir::Intrinsic::ToString)
                            .args(args)
                            .env(env)
                            .span(*span)
                            .build()?;
                        ir::Expr::call(call, *span)
                    }
                    ast::StrPart::Str(str) => ir::Expr::Const(ir::Const::Str(str.clone()), span),
                };
                let res = match acc {
                    Some(lhs) => {
                        let (call, _) = self
                            .free_function_call_builder()
                            .name(ast::BinOp::Add)
                            .args([lhs, elem].map(|e| (e, PolyType::nullary(predef::STRING))))
                            .env(env)
                            .span(span)
                            .build()?;
                        ir::Expr::call(call, span)
                    }
                    None => elem,
                };
                Ok(Some(res))
            })?
            .unwrap_or_else(|| ir::Expr::Const(ir::Const::Str("".into()), span));
        Ok((str, PolyType::nullary(predef::STRING)))
    }

    fn lower_new(
        &mut self,
        (typ, type_span): &Spanned<ast::SourceType<'ctx>>,
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let Type::Data(typ) = env.types().resolve(typ, self.symbols, *type_span)? else {
            return Err(Error::InvalidNewType(*type_span));
        };
        let def = &self.symbols[typ.id()];

        let TypeSchema::Aggregate(agg) = def.schema() else {
            return Err(Error::InvalidNewType(*type_span));
        };

        if agg.flags().is_abstract() {
            self.reporter
                .report(Error::InstantiatingAbstract(typ.id(), span));
        }
        if agg.flags().is_struct() || agg.flags().is_never_ref() {
            self.reporter.report(Error::NewWithConstructible {
                type_id: typ.id(),
                span,
            });
        } else if !args.is_empty() {
            self.reporter
                .report(Error::ClassConstructorHasArguments(span));
        }
        if let Some(hint) = agg.constructor_hint()
            && self.context != Some(typ.id())
        {
            self.reporter
                .report(Error::PrivateConstructor(typ.id(), hint.clone(), span));
        }

        let type_args = if typ.args().is_empty() {
            def.params()
                .iter()
                .map(|_| PolyType::fresh())
                .collect::<Rc<_>>()
        } else if typ.args().len() != def.params().len() {
            let expected = def.params().len();
            return Err(Error::InvalidTypeArgCount(
                expected,
                typ.args().len(),
                *type_span,
            ));
        } else {
            typ.args().iter().map(PolyType::from_type).collect()
        };
        let inferred_args = typ.args().is_empty();
        let typ = TypeApp::new(typ.id(), type_args);
        if !inferred_args {
            self.check_type_app(&typ, *type_span)?;
        }

        let ir = ir::Expr::NewInstance {
            class_type: typ.clone().into(),
            span,
        };
        let typ = typ.into_type().into_poly();
        let typ = if agg.flags().is_mixed_ref() {
            Type::app(predef::REF, [typ]).into_poly()
        } else {
            typ
        };
        Ok((ir, typ))
    }

    fn lower_construct(
        &mut self,
        id: TypeId<'ctx>,
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let def = &self.symbols[id];
        let type_args_are_inferred = type_args.is_empty();

        let type_args = if type_args.is_empty() {
            def.params()
                .iter()
                .map(|_| PolyType::fresh())
                .collect::<Rc<_>>()
        } else if type_args.len() != def.params().len() {
            let expected = def.params().len();
            return Err(Error::InvalidTypeArgCount(expected, type_args.len(), span));
        } else {
            type_args
                .iter()
                .map(|(typ, span)| self.resolve_type(typ, env, *span))
                .collect::<Result<Rc<_>, _>>()?
        };
        let typ = TypeApp::new(id, type_args);
        if !type_args_are_inferred {
            self.check_type_app(&typ, span)?;
        }

        let TypeSchema::Aggregate(aggregate) = def.schema() else {
            return Err(Error::InvalidConstructType(span));
        };

        if !aggregate.flags().is_struct()
            && !aggregate.flags().is_mixed_ref()
            && !aggregate.flags().is_never_ref()
        {
            return Err(Error::InvalidConstructType(span));
        }

        let field_count = aggregate.fields().len();

        if aggregate.flags().is_native() && !aggregate.flags().is_fully_defined() {
            if !args.is_empty() {
                self.reporter
                    .report(Error::NonFullyDefinedNativeStructConstruction { type_id: id, span });
            }
        } else if args.len() != field_count {
            self.reporter.report(Error::InvalidArgCount(
                iter::once(field_count..=field_count).collect(),
                args.len(),
                span,
            ));
        }

        let type_env = typ.type_env(self.symbols);
        let args = args
            .iter()
            .zip(aggregate.fields().iter())
            .map(|(arg @ (_, arg_span), field)| {
                let expected = PolyType::from_type_with_env(field.field().type_(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, *arg_span))?;
                let (mut expr, typ) = self.lower_expr_with(arg, Some(&expected), env)?;
                self.coerce(&mut expr, typ, expected, env, *arg_span)?;
                Ok(expr)
            })
            .collect::<Result<_, _>>()?;
        let ir = ir::Expr::Construct {
            struct_type: typ.clone().into(),
            args,
            span,
        };
        Ok((ir, typ.into_type().into_poly()))
    }

    fn lower_index(
        &mut self,
        expr @ (_, expr_span): &Spanned<ast::SourceExpr<'ctx>>,
        index @ (_, index_span): &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let elem_t = PolyType::fresh();

        let (mut expr, expr_t) = self.lower_expr(expr, env)?;
        let type_id = if let Some(typ) = expr_t
            .upper_bound(self.symbols)
            .filter(|typ| typ.id().is_static_array())
        {
            typ.id()
        } else {
            predef::ARRAY
        };

        let array_t = Type::app(type_id, [elem_t.clone()]).into_poly();
        self.coerce(&mut expr, expr_t, array_t.clone(), env, *expr_span)?;

        let (index, index_t) = self.lower_expr(index, env)?;
        index_t
            .constrain_base(&PolyType::nullary(predef::INT32), self.symbols)
            .with_span(*index_span)?;

        if expr.is_prvalue(self.symbols) {
            self.reporter.report(Error::InvalidTemporary(*expr_span));
        }

        Ok((
            ir::Expr::Index {
                array_type: array_t.into(),
                array: expr.into(),
                index: index.into(),
                span,
            },
            elem_t,
        ))
    }

    fn lower_dyn_cast(
        &mut self,
        expr: &Spanned<ast::SourceExpr<'ctx>>,
        (typ, type_span): &Spanned<ast::SourceType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let (expr, expr_t) = self.lower_expr(expr, env)?;
        let resolved = env.types().resolve(typ, self.symbols, *type_span)?;
        let Some(agg) = resolved
            .upper_bound()
            .and_then(|bound| self.symbols[bound.id()].schema().as_aggregate())
        else {
            return Err(Error::InvalidDynCastType(expr_t, resolved, *type_span));
        };
        let generic_target = matches!(resolved, Type::Ctx(_));
        if generic_target && (agg.flags().is_struct() || agg.flags().is_never_ref()) {
            return Err(Error::InvalidDynCastType(expr_t, resolved, *type_span));
        }
        let expected = PolyType::from_type(&resolved);
        let (ref_t, inner) = expr_t.strip_ref(self.symbols).unzip();

        let constraint = inner.unwrap_or_else(|| expr_t.clone());
        let result_t = match ref_t {
            Some(RefType::Weak) => Type::app(predef::WREF, [expected.clone()]).into_poly(),
            _ if agg.flags().is_mixed_ref() => {
                Type::app(predef::REF, [expected.clone()]).into_poly()
            }
            _ => expected.clone(),
        };
        let compatible = if matches!(constraint, PolyType::Var(_)) {
            // An unresolved source (for example null) needs an inference constraint,
            // not speculative constraints in both directions.
            expected.constrain_base(&constraint, self.symbols).is_ok()
        } else {
            self.dyn_cast_compatible(&constraint, &expected)
        };
        if !compatible {
            if constraint.satisfies_bound(&expected, self.symbols) {
                Err(Error::RedundantDynCast(span))
            } else {
                Err(Error::ImpossibleDynCast(expr_t, expected, span))
            }
        } else {
            let ir = ir::Expr::DynCast {
                expr: expr.into(),
                expr_type: expr_t.into(),
                target_type: expected.into(),
                span,
            };
            Ok((ir, result_t))
        }
    }

    fn dyn_cast_compatible(&self, source: &PolyType<'ctx>, target: &PolyType<'ctx>) -> bool {
        if target.satisfies_bound(source, self.symbols) {
            return true;
        }
        if source.satisfies_bound(target, self.symbols) {
            return false;
        }
        if !matches!(source, PolyType::Mono(Type::Ctx(_)))
            && !matches!(target, PolyType::Mono(Type::Ctx(_)))
        {
            return false;
        }
        // Bounds establish possible overlap without replacing either cast operand.
        match (
            source.upper_bound(self.symbols),
            target.upper_bound(self.symbols),
        ) {
            (Some(source), Some(target)) => {
                let source = source.into_owned().into_type().into_poly();
                let target = target.into_owned().into_type().into_poly();
                source.satisfies_bound(&target, self.symbols)
                    || target.satisfies_bound(&source, self.symbols)
            }
            _ => false,
        }
    }

    fn lower_closure(
        &mut self,
        params: &[Spanned<ast::SourceParam<'ctx>>],
        body: &ast::SourceFunctionBody<'ctx>,
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let hint_bound = hint.and_then(|typ| typ.upper_bound(self.symbols));
        let arg_hints = match &hint_bound {
            Some(typ) if typ.id().is_fn() => typ.args(),
            _ => &[],
        };
        let arg_hints = arg_hints
            .iter()
            .cloned()
            .chain(iter::repeat_with(PolyType::fresh));
        let return_t = PolyType::fresh();

        let func_t_args = params
            .iter()
            .zip(arg_hints)
            .map(|((param, _), hint)| {
                param
                    .typ
                    .as_ref()
                    .map_or(Ok(hint), |(typ, span)| self.resolve_type(typ, env, *span))
            })
            .chain([Ok(return_t.clone())])
            .collect::<Result<Rc<_>, _>>()?;

        let types = params
            .iter()
            .zip(&*func_t_args)
            .map(|((param, _), typ)| (param.name, typ.clone()));

        let (block, output) = Lower::function_builder()
            .body(body)
            .local_counter(self.locals.counter())
            .params(types)
            .env(env.introduce_scope())
            .return_type(return_t)
            .maybe_context(self.context)
            .symbols(self.symbols)
            .reporter(self.reporter)
            .build();

        let (locals, captures) = output.into_inner();
        let captured = captures.iter().map(|cap| cap.captured).collect::<Box<_>>();
        self.captures.extend(captures);

        let type_id = TypeId::fn_with_arity(params.len())
            .ok_or(Error::UnsupportedArity(params.len(), span))?;
        let typ = TypeApp::new(type_id, func_t_args);
        let closure = ir::Closure::new(typ.clone(), locals, captured, block);
        let ir = ir::Expr::NewClosure {
            closure: closure.into(),
            span,
        };
        Ok((ir, typ.into_type().into_poly()))
    }

    fn lower_member(
        &mut self,
        receiver @ &(_, receiver_span): &Spanned<ast::SourceExpr<'ctx>>,
        member: &'ctx str,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        if let (ast::Expr::Ident(ident), _) = receiver
            && let Some(&TypeRef::Id(type_id)) = env.types().get(ident)
            && let TypeSchema::Enum(enum_) = self.symbols[type_id].schema()
        {
            let (field_idx, _) = enum_
                .variant_by_name(member)
                .ok_or(Error::UnresolvedMember(type_id, member, span))?;

            let ir = ir::Const::EnumVariant(FieldId::new(type_id, field_idx));
            let typ = PolyType::nullary(ir.type_id());
            return Ok((ir::Expr::Const(ir, span), typ));
        };

        let (ir, typ) = self.lower_expr(receiver, env)?;
        let (ref_type, unref_t) = typ.strip_ref(self.symbols).unzip();
        let upper_bound = unref_t
            .unwrap_or(typ)
            .force_upper_bound(self.symbols)
            .with_span(receiver_span)?
            .ok_or(Error::InsufficientTypeInformation(receiver_span))?
            .into_owned();
        let (expr, typ, _) = self
            .field_access_builder()
            .receiver(ir)
            .receiver_type(upper_bound)
            .maybe_ref_type(ref_type)
            .field(member)
            .span(span)
            .build()?;
        Ok((expr, typ))
    }

    fn lower_for_in(
        &mut self,
        &(name, name_span): &Spanned<&'ctx str>,
        iter @ &(_, iter_span): &Spanned<ast::SourceExpr<'ctx>>,
        body: &ast::SourceBlock<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Block<'ctx>> {
        let init_span = Span::new(span.start, span.start, span.file);
        let init_end_span = Span::new(iter_span.end, iter_span.end, iter_span.file);

        let (array, array_t) = self.lower_expr(iter, env)?;
        let elem_t = PolyType::fresh();
        let expected_t = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();
        array_t
            .constrain_base(&expected_t, self.symbols)
            .with_span(init_span)?;

        let mut env = env.introduce_scope();
        let counter_t = PolyType::nullary(predef::INT32);
        let counter = self
            .locals
            .add_temp(
                ir::TemporaryPurpose::IterationIndex,
                counter_t.clone(),
                init_span,
            )
            .id;
        let array_local = self.extract_local(
            array,
            &array_t,
            init_span,
            ir::TemporaryPurpose::IterationIterable,
        );
        let elem = env.define_local(
            name,
            self.locals.add_var(Some(name), elem_t, name_span).clone(),
        );

        let loop_body = {
            let (increment, _) = self
                .free_function_call_builder()
                .name(ast::BinOp::AssignAdd)
                .args([
                    (ir::Expr::Local(counter, init_end_span), counter_t.clone()),
                    (
                        ir::Expr::Const(ir::Const::I32(1), init_end_span),
                        counter_t.clone(),
                    ),
                ])
                .env(&env)
                .span(init_end_span)
                .build()?;
            let prologue = [
                ir::Expr::Assign {
                    place: ir::Expr::Local(elem, name_span).into(),
                    expr: ir::Expr::Index {
                        array_type: array_t.into(),
                        array: ir::Expr::Local(array_local, iter_span).into(),
                        index: ir::Expr::Local(counter, init_end_span).into(),
                        span: iter_span,
                    }
                    .into(),
                    span: name_span.merge(&iter_span),
                }
                .into(),
                ir::Expr::call(increment, init_end_span).into(),
            ];

            let mut body = self.lower_block(&body.stmts, &mut env);
            body.push_prologue(prologue);
            body
        };

        let (array_size, array_size_t) = self
            .free_function_call_builder()
            .name(ir::Intrinsic::ArraySize)
            .args([(ir::Expr::Local(array_local, iter_span), expected_t.clone())])
            .env(&env)
            .span(iter_span)
            .build()?;
        let (check, _) = self
            .free_function_call_builder()
            .name(ast::BinOp::Lt)
            .args([
                (ir::Expr::Local(counter, init_span), counter_t.clone()),
                (ir::Expr::call(array_size, init_span), array_size_t),
            ])
            .env(&env)
            .span(init_span)
            .build()?;

        Ok(ir::Block::new([
            ir::Expr::Assign {
                place: ir::Expr::Local(counter, init_span).into(),
                expr: ir::Expr::Const(ir::Const::I32(0), init_span).into(),
                span: init_span,
            }
            .into(),
            ir::Stmt::While(
                ir::ConditionalBlock::new(ir::Expr::call(check, init_span), loop_body),
                span,
            ),
        ]))
    }

    fn lower_constant(
        &mut self,
        cn: &ast::Constant<'ctx>,
        hint: Option<&PolyType<'ctx>>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Const<'ctx>> {
        let Some(PolyType::Mono(Type::Data(app))) = hint else {
            return Ok(ir::Const::from(cn));
        };

        let res = match (cn, app.id()) {
            (ast::Constant::F32(f), id) if id == predef::DOUBLE => Some(ir::Const::F64(*f as f64)),

            (ast::Constant::I32(i), id) if id == predef::FLOAT => Some(ir::Const::F32(*i as f32)),
            (ast::Constant::I32(i), id) if id == predef::DOUBLE => Some(ir::Const::F64(*i as f64)),

            (ast::Constant::I32(i), id) if id == predef::INT8 => {
                i8::try_from(*i).map(ir::Const::I8).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::INT16 => {
                i16::try_from(*i).map(ir::Const::I16).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::INT64 => {
                Some(ir::Const::I64(i64::from(*i)))
            }

            (ast::Constant::I32(i), id) if id == predef::UINT8 => {
                u8::try_from(*i).map(ir::Const::U8).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT16 => {
                u16::try_from(*i).map(ir::Const::U16).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT32 => {
                u32::try_from(*i).map(ir::Const::U32).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT64 => {
                u64::try_from(*i).map(ir::Const::U64).ok()
            }

            (ast::Constant::U32(i), id) if id == predef::UINT64 => {
                Some(ir::Const::U64(u64::from(*i)))
            }

            (
                ast::Constant::String(_) | ast::Constant::Resource(_) | ast::Constant::TweakDbId(_),
                id,
            ) if id == predef::CNAME => {
                return Err(Error::WrongStringLiteral(id, 'n', span));
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::TweakDbId(_),
                id,
            ) if id == predef::RES_REF => {
                return Err(Error::WrongStringLiteral(id, 'r', span));
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::Resource(_),
                id,
            ) if id == predef::TWEAK_DB_ID => {
                return Err(Error::WrongStringLiteral(id, 't', span));
            }

            _ => Some(ir::Const::from(cn)),
        };

        res.ok_or(Error::LiteralOutOfRange(app.id(), span))
    }

    fn resolve_field(
        &mut self,
        member: &'ctx str,
        receiver_t: InferredTypeApp<'ctx>,
        span: Span,
    ) -> Result<(FieldId<'ctx>, PolyType<'ctx>, InferredTypeApp<'ctx>), Error<'ctx>> {
        let (target_id, (field_idx, field)) = self
            .symbols
            .base_iter_with_self(receiver_t.id())
            .find_map(|(id, agg)| Some((id, agg.fields().by_name(member)?)))
            .ok_or_else(|| Error::UnresolvedMember(receiver_t.id(), member, span))?;
        let field_id = FieldId::new(target_id, field_idx);

        if !self.check_visibility(target_id, field.flags().visibility()) {
            self.reporter.report(Error::InaccessibleField(
                InaccessibleMember::new(field_id, member, field.flags().visibility()).into(),
                span,
            ));
        }

        let this_t = receiver_t
            .instantiate_as::<AnyBaseType>(target_id, self.symbols)
            .expect("should instantiate as base type");
        let env = this_t.type_env(self.symbols);
        let typ = PolyType::from_type_with_env(field.type_(), &env)
            .map_err(|var| Error::UnresolvedVar(var, span))?;
        Ok((field_id, typ, this_t))
    }

    fn resolve_local(
        &mut self,
        name: &'ctx str,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let Some((loc, depth)) = env.locals().get_with_depth(name) else {
            if env.types().policy(span).implicit_members
                && name != "this"
                && env.locals().get("this").is_some()
                && self.context.is_some_and(|id| {
                    self.symbols
                        .base_iter_with_self(id)
                        .any(|(_, agg)| agg.fields().by_name(name).is_some())
                })
            {
                return self.lower_member(&(ast::Expr::This, span), name, env, span);
            }
            return Err(Error::UnresolvedVar(name, span));
        };

        let scope_depth = env.locals().scope_iter().count() - usize::from(depth);
        let ir = if scope_depth < self.locals.depth() {
            self.captures.insert(Capture::new(loc.id, scope_depth));
            ir::Expr::Capture(loc.id, span)
        } else {
            ir::Expr::Local(loc.id, span)
        };
        Ok((ir, loc.typ.clone()))
    }

    fn resolve_type(
        &self,
        typ: &ast::SourceType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, PolyType<'ctx>> {
        let typ = PolyType::from_type(&env.types().resolve(typ, self.symbols, span)?);
        self.check_type(&typ, span)?;
        Ok(typ)
    }

    fn check_visibility(&mut self, parent: TypeId<'ctx>, visibility: Visibility) -> bool {
        match visibility {
            Visibility::Private => self.context == Some(parent),
            Visibility::Protected => self.context.is_some_and(|context| {
                self.symbols
                    .base_iter_with_self(context)
                    .any(|(id, _)| id == parent)
            }),
            _ => true,
        }
    }

    #[builder(finish_fn = build)]
    fn field_access_builder(
        &mut self,
        receiver: ir::Expr<'ctx>,
        receiver_type: InferredTypeApp<'ctx>,
        ref_type: Option<RefType>,
        field: &'ctx str,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>, FieldId<'ctx>)> {
        let (field, field_t, this_t) = self.resolve_field(field, receiver_type, span)?;

        if self.symbols[field.parent()]
            .schema()
            .as_aggregate()
            .is_some_and(|agg| agg.flags().is_struct())
            && receiver.is_prvalue(self.symbols)
        {
            self.reporter
                .report(Error::InvalidTemporary(receiver.span()));
        }

        let ir = ir::Expr::Field {
            receiver: receiver.into(),
            receiver_type: this_t.into(),
            receiver_ref: ref_type,
            field,
            span,
        };
        Ok((ir, field_t, field))
    }

    fn extract_local(
        &mut self,
        expr: ir::Expr<'ctx>,
        expr_t: &PolyType<'ctx>,
        span: Span,
        purpose: ir::TemporaryPurpose,
    ) -> ir::Local {
        if let ir::Expr::Local(local, _) = expr {
            local
        } else {
            let local = self.locals.add_temp(purpose, expr_t.clone(), span).id;
            self.push_prefix(ir::Expr::Assign {
                place: ir::Expr::Local(local, span).into(),
                expr: expr.into(),
                span,
            });
            local
        }
    }

    fn check_type(&self, typ: &PolyType<'ctx>, span: Span) -> LowerResult<'ctx, ()> {
        match typ {
            PolyType::Mono(Type::Data(app)) => self.check_type_app(app, span),
            _ => Ok(()),
        }
    }

    fn check_type_app(&self, typ: &InferredTypeApp<'ctx>, span: Span) -> LowerResult<'ctx, ()> {
        let sym = &self.symbols[typ.id()];
        if sym.params().len() != typ.args().len() {
            return Err(Error::InvalidTypeArgCount(
                sym.params().len(),
                typ.args().len(),
                span,
            ));
        }
        typ.args()
            .iter()
            .try_for_each(|arg| self.check_type(arg, span))?;

        let env = typ.type_env(self.symbols);
        for (arg, param) in typ.args().iter().zip(sym.params()) {
            if let Some(upper) = param.upper() {
                let upper = PolyType::from_type_with_env(upper, &env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
                self.check_bound(arg, &upper, span)?;
            }
            if let Some(lower) = param.lower() {
                let lower = PolyType::from_type_with_env(lower, &env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
                self.check_bound(&lower, arg, span)?;
            }
        }
        Ok(())
    }

    fn check_bound(
        &self,
        lower: &PolyType<'ctx>,
        upper: &PolyType<'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        match (lower, upper) {
            (PolyType::Mono(lhs), PolyType::Mono(rhs)) => {
                if lower.satisfies_bound(upper, self.symbols) {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(lhs.clone(), rhs.clone())).with_span(span)
                }
            }
            _ => lower.constrain_base(upper, self.symbols).with_span(span),
        }
    }
}

#[derive(Debug, Default)]
pub struct LowerOutput<'ctx> {
    locals: Vec<ir::LocalInfo<'ctx>>,
    captures: IndexSet<Capture>,
}

impl<'ctx> LowerOutput<'ctx> {
    pub fn new(locals: Vec<ir::LocalInfo<'ctx>>, captures: IndexSet<Capture>) -> Self {
        Self { locals, captures }
    }

    #[inline]
    pub fn locals(&self) -> &[ir::LocalInfo<'ctx>] {
        &self.locals
    }

    pub fn into_inner(self) -> (Vec<ir::LocalInfo<'ctx>>, IndexSet<Capture>) {
        (self.locals, self.captures)
    }
}

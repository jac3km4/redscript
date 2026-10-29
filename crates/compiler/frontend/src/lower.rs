use std::cell::Cell;
use std::fmt::Debug;
use std::ops::RangeInclusive;
use std::rc::Rc;
use std::{iter, mem, slice};

use bon::bon;
use env::{Capture, Locals};
pub use env::{Env, TypeEnv, TypeRef};
pub use error::{CoalesceError, Error, LowerResult, TypeError};
use indexmap::set::MutableValues;
use redscript_ast as ast;
use redscript_ast::{Span, Spanned};
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

mod env;
mod error;
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
        }
    }

    #[inline]
    fn into_output(self) -> LowerOutput<'ctx> {
        LowerOutput::new(self.locals.into_vec(), self.captures)
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
                    if !lower.does_return_value && ret_t.is_fresh() {
                        ret_t
                            .constrain_base(&PolyType::nullary(predef::VOID), symbols)
                            .ok();
                    }
                    (lower.lower_block(&block.stmts, env), lower.into_output())
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
                expr_t
                    .constrain_base(&PolyType::nullary(predef::BOOL), self.symbols)
                    .with_span(*span)?;
                let block = self.lower_scoped_block(&block.body.stmts, env);
                Ok((ir::ConditionalBlock::new(condition, block), None))
            }
            ast::LetCondition::LetPattern(pattern, val) => {
                let (_, span) = pattern;
                let (expr, expr_t) = self.lower_expr(val, env)?;

                let local = self.locals.add_var(None, expr_t.clone(), expr.span()).id;
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

    fn lower_projection(
        &mut self,
        projection: &Projection<'_, 'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Expr<'ctx>> {
        match projection {
            Projection::Index {
                array,
                array_type,
                index,
            } if *index >= 0 => Ok(ir::Expr::Index {
                array_type: array_type.clone().into(),
                array: self.lower_projection(array, env, span)?.into(),
                index: ir::Expr::Const(ir::Const::I32(*index), span).into(),
                span,
            }),
            Projection::Index {
                array,
                array_type,
                index,
            } => {
                let array_arg = self.lower_projection(array, env, span)?;
                let (length, length_t) = self
                    .free_function_call_builder()
                    .name(ir::Intrinsic::ArraySize)
                    .args([(array_arg, array_type.clone())])
                    .env(env)
                    .span(span)
                    .build()?;
                let (index, _) = self
                    .free_function_call_builder()
                    .name(ast::BinOp::Sub)
                    .args([
                        (ir::Expr::call(length, span), length_t),
                        (
                            ir::Expr::Const(ir::Const::I32(-index), span),
                            PolyType::nullary(predef::INT32),
                        ),
                    ])
                    .env(env)
                    .span(span)
                    .build()?;
                Ok(ir::Expr::Index {
                    array_type: array_type.clone().into(),
                    array: self.lower_projection(array, env, span)?.into(),
                    index: ir::Expr::call(index, span).into(),
                    span,
                })
            }
            Projection::Field {
                receiver,
                receiver_type,
                receiver_ref,
                field,
            } => Ok(ir::Expr::Field {
                field: *field,
                receiver_type: receiver_type.clone().into(),
                receiver_ref: *receiver_ref,
                receiver: self.lower_projection(receiver, env, span)?.into(),
                span,
            }),
            Projection::Scrutinee(local) => Ok(ir::Expr::Local(*local, span)),
        }
    }

    fn lower_pattern(
        &mut self,
        pattern: &ast::SourcePattern<'ctx>,
        projection: &Projection<'_, 'ctx>,
        projection_t: &PolyType<'ctx>,
        env: &mut Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, Pattern<'ctx>> {
        match pattern {
            ast::Pattern::Name((name, name_span)) => {
                let local = self
                    .locals
                    .add_var(Some(name), projection_t.clone(), *name_span);
                let local = env.define_local(name, local.clone());
                let projected = self.lower_projection(projection, env, *name_span)?;
                Ok(Pattern {
                    conditions: vec![],
                    prologue: vec![
                        ir::Expr::Assign {
                            place: ir::Expr::Local(local, *name_span).into(),
                            expr: projected.into(),
                            span: *name_span,
                        }
                        .into(),
                    ],
                })
            }
            ast::Pattern::As(pattern, (as_type, type_span)) => {
                let (pattern, pattern_span) = &**pattern;

                let (ref_type, unref_t) = projection_t.strip_ref(self.symbols).unzip();
                let unref_t = unref_t.unwrap_or(projection_t.clone());

                let as_type = self.resolve_type(as_type, env, *type_span)?;
                as_type
                    .constrain_base(&unref_t, self.symbols)
                    .with_span(*pattern_span)?;

                let Pattern {
                    mut conditions,
                    prologue,
                } = self.lower_pattern(pattern, projection, &as_type, env)?;

                let projected = self.lower_projection(projection, env, *type_span)?;
                let type_check = self
                    .runtime_type_check_builder()
                    .expr(projected)
                    .maybe_ref_type(ref_type)
                    .target_type(&as_type)
                    .env(env)
                    .span(*pattern_span)
                    .build()?;
                conditions.push(ir::Expr::call(type_check, *type_span));

                Ok(Pattern {
                    conditions,
                    prologue,
                })
            }
            ast::Pattern::Aggregate((name, span), fields) => {
                let Some(TypeRef::Id(type_id)) = env.types().get(name) else {
                    return Err(Error::UnresolvedType(name, *span));
                };

                let aggregate_t_app = TypeApp::from_id(*type_id, self.symbols);
                let aggregate_t = aggregate_t_app.clone().into_type().into_poly();
                let (receiver_ref, unref_t) = projection_t.strip_ref(self.symbols).unzip();
                let unref_t = unref_t.unwrap_or(projection_t.clone());

                aggregate_t
                    .constrain_base(&unref_t, self.symbols)
                    .with_span(*span)?;

                let projected = self.lower_projection(projection, env, *span)?;
                let type_check = self
                    .runtime_type_check_builder()
                    .expr(projected)
                    .maybe_ref_type(receiver_ref)
                    .target_type(&aggregate_t)
                    .env(env)
                    .span(*span)
                    .build()?;

                let nested = fields
                    .into_iter()
                    .filter_map(|((field, field_span), pattern)| {
                        let field_res =
                            self.resolve_field(field, aggregate_t_app.clone(), *field_span);
                        let (field, field_t, receiver_type) =
                            self.reporter.unwrap_err(field_res)?;

                        let project = Projection::Field {
                            receiver: projection,
                            receiver_type,
                            receiver_ref,
                            field,
                        };

                        let pattern = self.lower_pattern(pattern, &project, &field_t, env);
                        self.reporter.unwrap_err(pattern)
                    });

                let mut accumulator = Pattern::from_conditions([ir::Expr::call(type_check, *span)]);
                accumulator.extend(nested);
                Ok(accumulator)
            }
            ast::Pattern::Nullable(pattern) => {
                let (pattern, span) = &**pattern;
                let Pattern {
                    mut conditions,
                    prologue,
                } = self.lower_pattern(pattern, projection, projection_t, env)?;
                let projected = self.lower_projection(projection, env, *span)?;
                let (call, _) = self
                    .free_function_call_builder()
                    .name(ir::Intrinsic::IsDefined)
                    .args([(projected, projection_t.clone())])
                    .env(env)
                    .span(*span)
                    .build()?;
                conditions.push(ir::Expr::call(call, *span));

                Ok(Pattern {
                    conditions,
                    prologue,
                })
            }
            ast::Pattern::Array(array_spread, patterns) => {
                let (patterns, patterns_span) = patterns;

                let pattern_count = i32::try_from(patterns.len()).expect("too many patterns");
                let range = match array_spread {
                    ast::ArraySpread::Start => -(pattern_count)..0,
                    ast::ArraySpread::End | ast::ArraySpread::None => 0..pattern_count,
                };
                let operator = match array_spread {
                    ast::ArraySpread::Start | ast::ArraySpread::End => ast::BinOp::Ge,
                    ast::ArraySpread::None => ast::BinOp::Eq,
                };

                let elem_t = PolyType::fresh();
                let expected = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();
                projection_t
                    .constrain_base(&expected, self.symbols)
                    .with_span(*patterns_span)?;

                let projected = self.lower_projection(projection, env, *patterns_span)?;
                let (length, length_t) = self
                    .free_function_call_builder()
                    .name(ir::Intrinsic::ArraySize)
                    .args([(projected, projection_t.clone())])
                    .env(env)
                    .span(*patterns_span)
                    .build()?;
                let (size_check, _) = self
                    .free_function_call_builder()
                    .name(operator.name())
                    .args([
                        (ir::Expr::call(length, *patterns_span), length_t.clone()),
                        (
                            ir::Expr::Const(ir::Const::I32(pattern_count), *patterns_span),
                            PolyType::nullary(predef::INT32),
                        ),
                    ])
                    .env(env)
                    .span(*patterns_span)
                    .build()?;

                let nested = patterns.iter().zip(range).filter_map(|(pattern, index)| {
                    let projection = Projection::Index {
                        array: projection,
                        array_type: projection_t.clone(),
                        index,
                    };
                    let result = self.lower_pattern(pattern, &projection, &elem_t, env);
                    self.reporter.unwrap_err(result)
                });
                let mut pattern =
                    Pattern::from_conditions([ir::Expr::call(size_check, *patterns_span)]);
                pattern.extend(nested);
                Ok(pattern)
            }
        }
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

    fn lower_pattern_case(
        &mut self,
        case: &ast::SourceCase<'ctx>,
        scrutinee: ir::Local,
        scrutinee_t: &PolyType<'ctx>,
        env: &mut Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, ir::Case<'ctx>> {
        let mut scope = env.introduce_scope();
        let (prologue, condition) = match &case.condition {
            ast::Condition::Expr(expr) => {
                let (_, span) = expr;
                let (expr, expr_t) = self.lower_expr_with(expr, Some(scrutinee_t), &scope)?;
                scrutinee_t
                    .constrain_base(&expr_t, self.symbols)
                    .with_span(*span)?;

                let (call, _) = self
                    .free_function_call_builder()
                    .name(ir::Intrinsic::Equals)
                    .args([
                        (expr, expr_t),
                        (ir::Expr::Local(scrutinee, *span), scrutinee_t.clone()),
                    ])
                    .env(&scope)
                    .span(*span)
                    .build()?;
                (vec![], ir::Expr::call(call, *span))
            }
            ast::Condition::Pattern(pattern) => self.lower_pattern_into_prologue_and_condition(
                scrutinee,
                scrutinee_t,
                &mut scope,
                pattern,
            )?,
        };

        let mut block = self.lower_block(&case.body, &mut scope);

        if matches!(case.condition, ast::Condition::Pattern(_))
            && !matches!(
                block.stmts.back(),
                Some(ir::Stmt::Break(_) | ir::Stmt::Return(_, _))
            )
        {
            self.reporter
                .report(Error::MissingBreakInCaseLet(case.condition.span()));
        }

        block.push_prologue(prologue);
        Ok(ir::Case::new(condition, block))
    }

    fn lower_pattern_into_prologue_and_condition(
        &mut self,
        scrutinee: ir::Local,
        scrutinee_t: &PolyType<'ctx>,
        scope: &mut Env<'_, 'ctx>,
        (pattern, span): &Spanned<ast::SourcePattern<'ctx>>,
    ) -> Result<(Vec<ir::Stmt<'ctx>>, ir::Expr<'ctx>), Error<'ctx>> {
        let projection = Projection::Scrutinee(scrutinee);
        let Pattern {
            conditions,
            prologue,
        } = self.lower_pattern(pattern, &projection, scrutinee_t, scope)?;
        let expr = conditions
            .into_iter()
            .try_fold(None, |acc, cond| {
                let Some(acc) = acc else {
                    return Ok(Some(cond));
                };
                let typ = PolyType::nullary(predef::BOOL);
                let (call, _) = self
                    .free_function_call_builder()
                    .name(ast::BinOp::And)
                    .args([(acc, typ.clone()), (cond, typ)])
                    .env(scope)
                    .span(*span)
                    .build()?;
                Ok(Some(ir::Expr::call(call, *span)))
            })?
            .unwrap_or(ir::Expr::Const(ir::Const::Bool(true), *span));
        Ok((prologue, expr))
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
                let const_ = self.lower_constant(cn, hint, *span);
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
                let lhs = self.lower_expr(lhs, env)?;
                let rhs = self.lower_expr(rhs, env)?;
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
                {
                    let const_ = self.lower_constant(&ast::Constant::I32(-i), hint, *span);
                    let typ = PolyType::nullary(const_.type_id());
                    return Ok((ir::Expr::Const(const_, *span), typ));
                }

                let arg = self.lower_expr(expr, env)?;
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
                cond_t
                    .constrain_base(&PolyType::nullary(predef::BOOL), self.symbols)
                    .with_span(*cond_span)?;

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
                    let local = self.extract_local(expr, &expr_t, span);
                    let branches = cases
                        .iter()
                        .map(|case| self.lower_pattern_case(case, local, &expr_t, env))
                        .collect::<Result<_, _>>()?;
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
                self.return_type
                    .constrain_base(&PolyType::nullary(predef::VOID), self.symbols)
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
                    let (mut elem, typ) = self.lower_expr(elem, env)?;
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
        let local = self.locals.add_var(None, array_t.clone(), span).id;

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
        }
        if !args.is_empty() {
            self.reporter
                .report(Error::ClassConstructorHasArguments(span));
        }
        if let Some(hint) = agg.constructor_hint()
            && self.context != Some(typ.id())
        {
            self.reporter
                .report(Error::PrivateConstructor(hint.clone(), span));
        }

        let type_args = if typ.args().is_empty() {
            def.params()
                .iter()
                .map(|_| PolyType::fresh())
                .collect::<Rc<_>>()
        } else if typ.args().len() != def.params().len() {
            let expected = def.params().len();
            return Err(Error::InvalidTypeArgCount(expected, *type_span));
        } else {
            typ.args().iter().map(PolyType::from_type).collect()
        };
        let typ = TypeApp::new(typ.id(), type_args);

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

        let type_args = if type_args.is_empty() {
            def.params()
                .iter()
                .map(|_| PolyType::fresh())
                .collect::<Rc<_>>()
        } else if type_args.len() != def.params().len() {
            let expected = def.params().len();
            return Err(Error::InvalidTypeArgCount(expected, span));
        } else {
            type_args
                .iter()
                .map(|(typ, span)| self.resolve_type(typ, env, *span))
                .collect::<Result<Rc<_>, _>>()?
        };
        let typ = TypeApp::new(id, type_args);

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
            self.reporter
                .report(Error::InvalidArgCount(field_count..=field_count, span));
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
        let Type::Data(typ) = env.types().resolve(typ, self.symbols, *type_span)? else {
            return Err(Error::InvalidDynCastType(*type_span));
        };
        let target = InferredTypeApp::from_type(&typ);
        let Some(agg) = self.symbols[target.id()].schema().as_aggregate() else {
            return Err(Error::InvalidDynCastType(*type_span));
        };
        let expected = target.clone().into_type().into_poly();
        let (ref_t, inner) = expr_t.strip_ref(self.symbols).unzip();

        let constraint = inner.unwrap_or_else(|| expr_t.clone());
        let result_t = match ref_t {
            Some(RefType::Weak) => Type::app(predef::WREF, [expected.clone()]).into_poly(),
            _ if agg.flags().is_mixed_ref() => {
                Type::app(predef::REF, [expected.clone()]).into_poly()
            }
            _ => expected.clone(),
        };
        let inferred = expected
            .constrain_base(&constraint, self.symbols)
            .map(|_| result_t)
            .map_err(|err| (err, constraint));

        match inferred {
            Err((err, inner)) => {
                if inner.constrain_base(&expected, self.symbols).is_ok() {
                    Err(Error::RedundantDynCast(span))
                } else {
                    Err(Error::ImpossibleDynCast(err.into(), span))
                }
            }
            Ok(inferred) => {
                let ir = ir::Expr::DynCast {
                    expr: expr.into(),
                    expr_type: expr_t.into(),
                    target_type: target.into(),
                    span,
                };
                Ok((ir, inferred))
            }
        }
    }

    fn lower_call(
        &mut self,
        expr @ (_, expr_span): &Spanned<ast::SourceExpr<'ctx>>,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        call_span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        if let &(ast::Expr::Ident("Cast"), _) = expr
            && let [] | [_] = type_args
            && args.len() == 1
        {
            return self.lower_static_cast(args, type_args, hint, env, *expr_span);
        };

        // legacy syntax for NameOf
        if let &(ast::Expr::Ident("NameOf"), _) = expr
            && type_args.is_empty()
            && let [(ast::Expr::Ident(name), name_span)] = args
        {
            self.reporter.report(Error::DeprecatedNameOf(call_span));

            return self.lower_call(
                expr,
                &[(ast::Type::plain(name), *name_span)],
                &[],
                hint,
                env,
                call_span,
            );
        }

        if let &(ast::Expr::Ident(name), _) = expr
            && let Some(&TypeRef::Id(id)) = env.types().get(name)
        {
            return self.lower_construct(id, args, type_args, env, call_span);
        }

        if let &(ast::Expr::Ident(name), _) = expr
            && let mut candidates = env.query_free_functions(name, self.symbols).peekable()
            && candidates.peek().is_some()
        {
            let (call, typ) = self
                .overload_resolver()
                .name(name)
                .args(args)
                .type_args(type_args)
                .candidates(candidates)
                .maybe_type_hint(hint)
                .env(env)
                .span(*expr_span)
                .resolve()?
                .into_call();
            return Ok((ir::Expr::call(call, call_span), typ));
        };

        let (closure, typ) = if let (ast::Expr::Member { expr, member }, _) = expr {
            match self
                .member_call_builder()
                .receiver(expr)
                .member(member)
                .type_args(type_args)
                .args(args)
                .maybe_type_hint(hint)
                .env(env)
                .call_span(call_span)
                .build()?
            {
                (MemberCall::MethodCall(call), typ) => {
                    return Ok((ir::Expr::call(call, call_span), typ));
                }
                (MemberCall::FieldAccess(field), typ) => (field, typ),
            }
        } else {
            self.lower_expr(expr, env)?
        };

        let type_id =
            TypeId::fn_with_arity(args.len()).ok_or(Error::UnsupportedArity(call_span))?;
        let mut checked_args = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        for arg in args {
            let (arg, typ) = self.lower_expr(arg, env)?;
            checked_args.push(arg);
            arg_types.push(typ);
        }

        let return_t = PolyType::fresh();
        let func_t_args = arg_types
            .into_iter()
            .chain([return_t.clone()])
            .collect::<Rc<_>>();
        let func_t = TypeApp::new(type_id, func_t_args);

        typ.constrain_base(&func_t.clone().into_type().into_poly(), self.symbols)
            .with_span(*expr_span)?;

        let call = ir::Call::Closure {
            closure: closure.into(),
            args: checked_args.into(),
            closure_type: func_t.into(),
        };
        Ok((ir::Expr::call(call, call_span), return_t.clone()))
    }

    fn lower_static_cast(
        &mut self,
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let target_t = match (type_args, hint) {
            ([(target, target_span)], _) => self.resolve_type(target, env, *target_span)?,
            ([], Some(hint)) => hint.clone(),
            ([], None) => return Err(Error::UnknownStaticCastType(span)),
            _ => return Err(Error::InvalidTypeArgCount(1, span)),
        };
        let candidates = env
            .query_free_functions("Cast", self.symbols)
            .filter(|entry| {
                target_t.is_subtype_compatible(
                    entry.func().type_().return_type(),
                    Variance::Covariant,
                    self.symbols,
                )
            });
        let (call, typ) = self
            .overload_resolver()
            .name("Cast")
            .args(args)
            .candidates(candidates)
            .maybe_type_hint(hint)
            .env(env)
            .span(span)
            .resolve()?
            .into_call();
        Ok((ir::Expr::call(call, span), typ))
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

        let type_id = TypeId::fn_with_arity(params.len()).ok_or(Error::UnsupportedArity(span))?;
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
        let counter = self.locals.add_var(None, counter_t.clone(), init_span).id;
        let array_local = self.extract_local(array, &array_t, init_span);
        let elem = env.define_local(name, self.locals.add_var(None, elem_t, name_span).clone());

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
    ) -> ir::Const<'ctx> {
        let Some(PolyType::Mono(Type::Data(app))) = hint else {
            return ir::Const::from(cn);
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
                self.reporter
                    .report(Error::WrongStringLiteral(id, 'n', span));
                return ir::Const::from(cn);
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::TweakDbId(_),
                id,
            ) if id == predef::RES_REF => {
                self.reporter
                    .report(Error::WrongStringLiteral(id, 'r', span));
                return ir::Const::from(cn);
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::Resource(_),
                id,
            ) if id == predef::TWEAK_DB_ID => {
                self.reporter
                    .report(Error::WrongStringLiteral(id, 't', span));
                return ir::Const::from(cn);
            }

            _ => Some(ir::Const::from(cn)),
        };

        if let Some(const_) = res {
            const_
        } else {
            self.reporter
                .report(Error::LiteralOutOfRange(app.id(), span));
            ir::Const::from(cn)
        }
    }

    fn lower_arg(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        param: &Param<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        if param.flags().is_out() {
            rhs.constrain_base(&lhs, self.symbols).with_span(span)?;
            if expr.is_prvalue(self.symbols) {
                self.reporter.report(Error::InvalidTemporary(span));
            }
        }
        self.coerce(expr, lhs, rhs, env, span)?;
        Ok(())
    }

    #[builder(finish_fn = build)]
    fn member_call_builder(
        &mut self,
        receiver @ (_, expr_span): &Spanned<ast::SourceExpr<'ctx>>,
        member: &'ctx str,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        type_hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        call_span: Span,
    ) -> LowerResult<'ctx, (MemberCall<'ctx>, PolyType<'ctx>)> {
        if let (ast::Expr::Ident(ident), _) = receiver
            && let Some(&TypeRef::Id(id)) = env.types().get(ident)
            && let mut candidates = self
                .symbols
                .base_iter_with_self(id)
                .map(|(id, def)| {
                    def.methods()
                        .by_name(member)
                        .map(move |entry| entry.map_key(|i| MethodId::new(id, i)))
                        .peekable()
                })
                .find_map(|mut matches| matches.peek().is_some().then_some(matches))
                .into_iter()
                .flatten()
                .filter(|entry| entry.func().flags().is_static())
                .peekable()
            && candidates.peek().is_some()
        {
            let parent_args = self.symbols[id]
                .params()
                .iter()
                .map(|_| PolyType::fresh())
                .collect::<Rc<_>>();
            let typ = TypeApp::new(id, parent_args);

            let resolved = self
                .overload_resolver()
                .name(member)
                .args(args)
                .type_args(type_args)
                .candidates(candidates)
                .receiver(typ.clone())
                .maybe_type_hint(type_hint)
                .env(env)
                .span(call_span)
                .resolve()?;

            let method_id = resolved.resulution.function;
            let method = &self.symbols[method_id];
            if !self.check_visibility(method_id.parent(), method.flags().visibility()) {
                self.reporter.report(Error::InaccessibleMethod(
                    InaccessibleMember::new(method_id, member, method.flags().visibility()).into(),
                    call_span,
                ));
            }

            let parent = resolved.resulution.function.parent();
            if parent != id {
                self.reporter.report(Error::CallingBaseStaticMethod {
                    type_id: parent,
                    name: member,
                    span: call_span,
                });
            }
            let (call, call_t) = resolved.into_static_call(typ.id(), typ.args().iter().cloned());
            return Ok((MemberCall::MethodCall(call), call_t));
        }

        let (mut ir, typ) = self.lower_expr(receiver, env)?;
        let (ref_type, unref_t) = typ.strip_ref(self.symbols).unzip();
        let upper_bound = unref_t
            .unwrap_or(typ.clone())
            .force_upper_bound(self.symbols)
            .with_span(*expr_span)?
            .ok_or(Error::InsufficientTypeInformation(*expr_span))?
            .into_owned();

        let (upper_bound, mode) = if matches!(receiver, (ast::Expr::Super, _)) {
            (
                upper_bound
                    .instantiate_base::<AnyBaseType>(self.symbols)
                    .ok_or(Error::NonExistentSuperType(*expr_span))?,
                ir::CallMode::ForceStatic,
            )
        } else {
            (upper_bound, ir::CallMode::Normal)
        };

        let mut candidates = self
            .symbols
            .query_methods_by_name(upper_bound.id(), member)
            .filter(|entry| !entry.func().flags().is_static())
            .peekable();
        if candidates.peek().is_none() {
            let (field_expr, field_t, _) = self
                .field_access_builder()
                .receiver(ir)
                .receiver_type(upper_bound)
                .maybe_ref_type(ref_type)
                .field(member)
                .span(call_span)
                .build()?;
            return Ok((MemberCall::FieldAccess(field_expr), field_t));
        }

        let resolved = self
            .overload_resolver()
            .name(member)
            .args(args)
            .type_args(type_args)
            .candidates(candidates)
            .receiver(upper_bound.clone())
            .maybe_type_hint(type_hint)
            .env(env)
            .span(call_span)
            .resolve()?;

        let method_id = resolved.resulution.function;
        let method = &self.symbols[method_id];
        if !self.check_visibility(method_id.parent(), method.flags().visibility()) {
            self.reporter.report(Error::InaccessibleMethod(
                InaccessibleMember::new(method_id, member, method.flags().visibility()).into(),
                call_span,
            ));
        }

        let (call, call_t) = if let Some(aliased) = method.aliased_method()
            && method.flags().is_static_forwarder()
        {
            let expected = &self.symbols[aliased]
                .type_()
                .param_types()
                .next()
                .expect("static alias must at least accept a receiver");
            let span = ir.span();
            self.coerce(&mut ir, typ, PolyType::from_type(expected), env, span)?;

            resolved
                .with_method(aliased)
                .map_args(|args| iter::once(ir).chain(args).collect())
                .into_static_call(upper_bound.id(), upper_bound.args().iter().cloned())
        } else {
            resolved.into_instance_call(ir, upper_bound, ref_type, mode)
        };

        Ok((MemberCall::MethodCall(call), call_t))
    }

    #[builder(finish_fn = resolve)]
    fn overload_resolver<'a, Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: impl IntoIterator<
            IntoIter = impl ExactSizeIterator<Item = &'a Spanned<ast::SourceExpr<'ctx>>> + Clone,
        >,
        #[builder(default)] type_args: &[Spanned<ast::SourceType<'ctx>>],
        candidates: impl IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
        receiver: Option<InferredTypeApp<'ctx>>,
        type_hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, FunctionResultWithArgs<'ctx, Key>>
    where
        'ctx: 'a,
        Key: FunctionKey<'ctx> + Copy,
        Name: Copy,
        Func: FunctionKind<'ctx> + Copy,
    {
        let args = args.into_iter();
        let argc = args.len();

        let mut arg_bounds: Option<RangeInclusive<usize>> = None;
        let mut filtered = candidates
            .into_iter()
            .filter(|f| {
                let range = f.func().type_().min_args()..=f.func().type_().max_args();
                arg_bounds = Some(arg_bounds.as_ref().map_or(range.clone(), |other| {
                    *other.start().min(range.start())..=*other.end().max(range.end())
                }));
                range.contains(&argc)
            })
            .peekable();

        let Some(primary) = filtered.next() else {
            match arg_bounds {
                Some(bounds) => return Err(Error::InvalidArgCount(bounds, span)),
                None => return Err(Error::UnresolvedFunction(name, span)),
            }
        };

        let type_args = type_args
            .iter()
            .map(|(typ, span)| self.resolve_type(typ, env, *span))
            .collect::<Result<Vec<_>, _>>()?;

        if filtered.peek().is_none() {
            let type_env = receiver
                .and_then(|typ| {
                    typ.base_type_env::<AnyBaseType>(primary.key().parent()?, self.symbols)
                })
                .unwrap_or_default();

            let type_env = type_env.push_scope(
                self.function_env_builder()
                    .type_(primary.func().type_())
                    .type_args(&type_args)
                    .span(span)
                    .build()?
                    .pop_scope(),
            );

            let return_t =
                PolyType::from_type_with_env(primary.func().type_().return_type(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
            if let Some(hint) = type_hint {
                return_t.constrain_base(hint, self.symbols).ok();
            }

            let mut checked_args = Vec::with_capacity(args.len());
            for arg in args.zip(primary.func().type_().params()) {
                let (arg @ (_, span), param) = arg;
                let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, *span))?;
                let res = self.lower_expr_with(arg, Some(&expected), env);
                let Some((mut expr, typ)) = self.reporter.unwrap_err(res) else {
                    continue;
                };
                let res = self.lower_arg(&mut expr, typ.clone(), expected, param, env, *span);
                self.reporter.unwrap_err(res);
                checked_args.push(expr);
            }

            let type_args = type_env
                .pop_scope()
                .into_values()
                .map(ir::Type::from)
                .collect::<Box<[_]>>();
            let res = FunctionResolution::new(*primary.key(), type_args, return_t)
                .with_args(checked_args);
            Ok(res)
        } else {
            let mut checked_args = Vec::with_capacity(args.len());
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args {
                let (expr, typ) = self.lower_expr(arg, env)?;
                checked_args.push(expr);
                arg_types.push(typ);
            }

            let res = self
                .typed_overload_resolver()
                .name(name)
                .args(&mut checked_args)
                .arg_types(&arg_types)
                .type_args(&type_args)
                .candidates(iter::once(primary.clone()).chain(filtered))
                .maybe_fallback(Some(primary))
                .maybe_receiver(receiver)
                .env(env)
                .span(span)
                .resolve()?
                .with_args(checked_args);
            Ok(res)
        }
    }

    #[builder(finish_fn = resolve)]
    fn typed_overload_resolver<Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: &mut [ir::Expr<'ctx>],
        arg_types: &[PolyType<'ctx>],
        #[builder(default)] type_args: &[PolyType<'ctx>],
        candidates: impl IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
        fallback: Option<FunctionEntry<Key, Name, Func>>,
        receiver: Option<InferredTypeApp<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, FunctionResolution<'ctx, Key>>
    where
        Key: FunctionKey<'ctx> + Copy,
        Func: FunctionKind<'ctx> + Copy,
    {
        fn match_by_subtype<'a, 'ctx, It, Key, Name, Func>(
            it: It,
            arg_types: &'a [PolyType<'ctx>],
            symbols: &'a Symbols<'ctx>,
        ) -> impl Iterator<Item = FunctionEntry<Key, Name, Func>> + use<'a, 'ctx, It, Key, Name, Func>
        where
            It: IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
            Func: FunctionKind<'ctx> + Copy,
        {
            it.into_iter().filter(|entry| {
                arg_types
                    .iter()
                    .zip(entry.func().type_().params())
                    .all(|(typ, param)| match param.type_() {
                        Type::Data(id)
                            if id.id() == predef::VARIANT && entry.func().intrinsic().is_none() =>
                        {
                            true
                        }
                        _ => {
                            let variance = if param.flags().is_out() {
                                Variance::Invariant
                            } else {
                                Variance::Covariant
                            };
                            typ.unwrap_ref_or_self(symbols).is_subtype_compatible(
                                param.type_().unwrap_ref_or_self(),
                                variance,
                                symbols,
                            )
                        }
                    })
            })
        }

        let mut matches = match_by_subtype(candidates, arg_types, self.symbols).peekable();
        let selected = matches
            .next()
            .or(fallback)
            .ok_or(Error::UnresolvedFunction(name, span))?;

        let selected = if matches.peek().is_none() {
            selected
        } else {
            // retry with upper bounds lifted to resolve ambiguity
            for arg in arg_types {
                arg.force_upper_bound(self.symbols).with_span(span)?;
            }
            let candidates = iter::once(selected).chain(matches);
            let mut matches = match_by_subtype(candidates, arg_types, self.symbols).peekable();

            let primary = matches
                .next()
                .ok_or(Error::UnresolvedFunction(name, span))?;
            if matches.peek().is_some() {
                let matches = iter::once(primary)
                    .chain(matches)
                    .map(|entry| {
                        let func_t = entry.func().type_();
                        FunctionSignature::new(name, func_t.params(), func_t.return_type().clone())
                    })
                    .collect();
                return Err(Error::MultipleMatchingOverloads(matches, span));
            }
            primary
        };

        let type_env = receiver
            .and_then(|typ| {
                typ.base_type_env::<AnyBaseType>(selected.key().parent()?, self.symbols)
            })
            .unwrap_or_default();
        let type_env = type_env.push_scope(
            self.function_env_builder()
                .type_(selected.func().type_())
                .type_args(type_args)
                .span(span)
                .build()?
                .pop_scope(),
        );
        for ((arg, typ), param) in args
            .iter_mut()
            .zip(arg_types)
            .zip(selected.func().type_().params())
        {
            let span = arg.span();
            let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                .map_err(|var| Error::UnresolvedVar(var, span))?;
            self.lower_arg(&mut *arg, typ.clone(), expected, param, env, span)?;
        }

        let return_t =
            PolyType::from_type_with_env(selected.func().type_().return_type(), &type_env)
                .map_err(|var| Error::UnresolvedVar(var, span))?;
        let type_args = type_env
            .pop_scope()
            .into_values()
            .map(ir::Type::from)
            .collect::<Box<[_]>>();
        Ok(FunctionResolution::new(
            *selected.key(),
            type_args,
            return_t,
        ))
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
            return Err(Error::UnresolvedVar(name, span));
        };

        let ir = if depth > 0
            && env.locals().scope_iter().count() - usize::from(depth) < self.locals.depth()
        {
            self.captures.insert(Capture::new(loc.id, depth));
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

    fn coerce(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        let name = match lhs.coerce(&rhs, self.symbols).with_span(span)? {
            Some(Coercion::FromRef(RefType::Weak)) => ir::Intrinsic::WeakRefToRef,
            Some(Coercion::IntoRef(RefType::Weak)) => ir::Intrinsic::RefToWeakRef,
            Some(Coercion::FromRef(RefType::Script)) => ir::Intrinsic::Deref,
            Some(Coercion::IntoRef(RefType::Script)) => ir::Intrinsic::AsRef,
            Some(Coercion::IntoVariant) => ir::Intrinsic::ToVariant,
            Some(Coercion::FromRef(RefType::Strong)) => {
                if rhs
                    .upper_bound(self.symbols)
                    .and_then(|t| self.symbols[t.id()].schema().as_aggregate())
                    .is_none_or(|agg| agg.flags().is_mixed_ref())
                {
                    self.reporter.report(Error::UnexpectedRef(span));
                }
                return Ok(());
            }
            Some(Coercion::IntoRef(RefType::Strong)) => {
                self.reporter.report(Error::UnexpectedNonRef(span));
                return Ok(());
            }
            None => return Ok(()),
        };
        let arg = mem::replace(expr, ir::Expr::null(span));
        let (call, _) = self
            .free_function_call_builder()
            .name(name)
            .args([(arg, lhs)])
            .env(env)
            .span(span)
            .build()?;
        *expr = ir::Expr::call(call, span);
        Ok(())
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
    fn free_function_call_builder<'name: 'ctx>(
        &mut self,
        name: impl Into<&'name str>,
        args: impl IntoIterator<Item = (ir::Expr<'ctx>, PolyType<'ctx>)>,
        #[builder(default)] type_args: &[PolyType<'ctx>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        let name = name.into();
        let (mut args, arg_types): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let candidates = env.query_free_functions(name, self.symbols);
        let res = self
            .typed_overload_resolver()
            .name(name)
            .args(&mut args)
            .arg_types(&arg_types)
            .type_args(type_args)
            .candidates(candidates)
            .env(env)
            .span(span)
            .resolve()?
            .with_args(args)
            .into_call();
        Ok(res)
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

    #[builder(finish_fn = build)]
    fn runtime_type_check_builder(
        &mut self,
        expr: ir::Expr<'ctx>,
        ref_type: Option<RefType>,
        target_type: &PolyType<'ctx>,
        env: &mut Env<'_, 'ctx>,
        span: Span,
    ) -> Result<ir::Call<'ctx>, Error<'ctx>> {
        let upper_bound = target_type
            .force_upper_bound(self.symbols)
            .with_span(span)?
            .ok_or(Error::InsufficientTypeInformation(span))?
            .into_owned();
        target_type
            .constrain_base(&PolyType::nullary(predef::ISCRIPTABLE), self.symbols)
            .with_span(span)?;

        let (name_of, name_t) = self
            .free_function_call_builder()
            .name(ir::Intrinsic::NameOf)
            .args([])
            .type_args(slice::from_ref(target_type))
            .env(env)
            .span(span)
            .build()?;
        let candidates = self
            .symbols
            .query_methods_by_name(upper_bound.id(), "IsA")
            .filter(|entry| !entry.func().flags().is_static());
        let mut args = [ir::Expr::call(name_of, span)];
        let (call, _) = self
            .typed_overload_resolver()
            .name("IsA")
            .args(&mut args)
            .arg_types(&[name_t])
            .candidates(candidates)
            .receiver(upper_bound.clone())
            .env(env)
            .span(span)
            .resolve()?
            .with_args(args)
            .into_instance_call(expr, upper_bound, ref_type, ir::CallMode::Normal);
        Ok(call)
    }

    #[builder(finish_fn = build)]
    fn function_env_builder(
        &self,
        type_: &FunctionType<'ctx>,
        type_args: &[PolyType<'ctx>],
        span: Span,
    ) -> LowerResult<'ctx, ScopedMap<'scope, &'ctx str, PolyType<'ctx>>> {
        let map: ScopedMap<'_, _, _> = type_
            .type_params()
            .iter()
            .map(|var| (var.name(), PolyType::fresh()))
            .collect();

        for ((_, var), param) in map.top().iter().zip(type_.type_params()) {
            if let Some(upper) = param.upper() {
                let constraint = PolyType::from_type_with_env(upper, &map)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
                var.constrain_base(&constraint, self.symbols)
                    .with_span(span)?;
            }
            if let Some(lower) = param.lower() {
                PolyType::from_type_with_env(lower, &map)
                    .map_err(|var| Error::UnresolvedVar(var, span))?
                    .constrain_base(var, self.symbols)
                    .with_span(span)?;
            }
        }

        if type_args.is_empty() {
            return Ok(map);
        }

        if type_args.len() != type_.type_params().len() {
            let expected = type_.type_params().len();
            return Err(Error::InvalidTypeArgCount(expected, span));
        }

        for ((_, var), arg) in map.top().iter().zip(type_args) {
            arg.constrain_base(var, self.symbols).with_span(span)?;
        }

        Ok(map)
    }

    fn extract_local(
        &mut self,
        expr: ir::Expr<'ctx>,
        expr_t: &PolyType<'ctx>,
        span: Span,
    ) -> ir::Local {
        if let ir::Expr::Local(local, _) = expr {
            local
        } else {
            let local = self.locals.add_var(None, expr_t.clone(), span).id;
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
            return Err(Error::InvalidTypeArgCount(sym.params().len(), span));
        }
        typ.args()
            .iter()
            .try_for_each(|arg| self.check_type(arg, span))
    }
}

#[derive(Debug, Default)]
pub struct LowerOutput<'ctx> {
    locals: Vec<ir::LocalInfo<'ctx>>,
    captures: IndexSet<Capture>,
}

impl<'ctx> LowerOutput<'ctx> {
    pub fn new(locals: Vec<ir::LocalInfo<'ctx>>, mut captures: IndexSet<Capture>) -> Self {
        // we can safely discard captures that are no longer in scope
        captures.retain2(|cap| cap.pop_scope().map(|next| *cap = next).is_some());
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

#[derive(Debug)]
struct FunctionResolution<'ctx, K> {
    function: K,
    type_args: Box<[ir::Type<'ctx>]>,
    return_type: PolyType<'ctx>,
}

impl<'ctx, K> FunctionResolution<'ctx, K> {
    #[inline]
    fn new(function: K, type_args: Box<[ir::Type<'ctx>]>, return_type: PolyType<'ctx>) -> Self {
        Self {
            function,
            type_args,
            return_type,
        }
    }

    pub fn with_function(self, function: K) -> Self {
        Self { function, ..self }
    }

    fn with_args(self, args: impl Into<Box<[ir::Expr<'ctx>]>>) -> FunctionResultWithArgs<'ctx, K> {
        FunctionResultWithArgs {
            resulution: self,
            args: args.into(),
        }
    }
}

#[derive(Debug)]
struct FunctionResultWithArgs<'ctx, K> {
    resulution: FunctionResolution<'ctx, K>,
    args: Box<[ir::Expr<'ctx>]>,
}

impl<'ctx> FunctionResultWithArgs<'ctx, FreeFunctionIndex> {
    #[inline]
    fn into_call(self) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::FreeFunction {
            function: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
        };
        (call, self.resulution.return_type)
    }
}

impl<'ctx> FunctionResultWithArgs<'ctx, MethodId<'ctx>> {
    pub fn with_method(self, method: MethodId<'ctx>) -> Self {
        Self {
            resulution: self.resulution.with_function(method),
            ..self
        }
    }

    pub fn map_args<F>(self, f: F) -> Self
    where
        F: FnOnce(Box<[ir::Expr<'ctx>]>) -> Box<[ir::Expr<'ctx>]>,
    {
        Self {
            resulution: self.resulution,
            args: f(self.args),
        }
    }

    #[inline]
    fn into_instance_call(
        self,
        receiver: ir::Expr<'ctx>,
        receiver_type: impl Into<ir::TypeApp<'ctx>>,
        receiver_ref: Option<RefType>,
        mode: ir::CallMode,
    ) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::Instance {
            receiver: receiver.into(),
            receiver_type: receiver_type.into(),
            receiver_ref,
            method: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
            mode,
        };
        (call, self.resulution.return_type)
    }

    #[inline]
    fn into_static_call(
        self,
        parent_id: TypeId<'ctx>,
        parent_type_args: impl IntoIterator<Item = impl Into<ir::Type<'ctx>>>,
    ) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::Static {
            parent_id,
            parent_type_args: parent_type_args.into_iter().map(Into::into).collect(),
            method: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
        };
        (call, self.resulution.return_type)
    }
}

struct Pattern<'ctx> {
    conditions: Vec<ir::Expr<'ctx>>,
    prologue: Vec<ir::Stmt<'ctx>>,
}

impl<'ctx> Pattern<'ctx> {
    pub fn from_conditions(conditions: impl IntoIterator<Item = ir::Expr<'ctx>>) -> Self {
        Self {
            conditions: conditions.into_iter().collect(),
            prologue: Vec::new(),
        }
    }

    pub fn merge(&mut self, other: Pattern<'ctx>) {
        self.conditions.extend(other.conditions);
        self.prologue.extend(other.prologue);
    }
}

impl<'ctx> Extend<Pattern<'ctx>> for Pattern<'ctx> {
    fn extend<T: IntoIterator<Item = Pattern<'ctx>>>(&mut self, iter: T) {
        for pattern in iter {
            self.merge(pattern);
        }
    }
}

#[derive(Debug, Clone)]
enum Projection<'scope, 'ctx> {
    Field {
        receiver: &'scope Self,
        receiver_type: InferredTypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
    },
    Index {
        array: &'scope Self,
        array_type: PolyType<'ctx>,
        index: i32,
    },
    Scrutinee(ir::Local),
}

#[derive(Debug)]
enum MemberCall<'ctx> {
    MethodCall(ir::Call<'ctx>),
    FieldAccess(ir::Expr<'ctx>),
}

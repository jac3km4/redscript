use std::fmt::Debug;
use std::{iter, mem, slice};

pub use array_guard::ArrayPrefixInfo;
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
use crate::sync::SharedU16;
use crate::types::{RefType, Type, TypeApp, TypeId, predef};
use crate::utils::ScopedMap;
use crate::{IndexSet, LowerReporter, MethodId, Param, Shared, Variance, ir};

mod array_guard;
mod calls;
mod closure;
mod conversions;
mod env;
mod error;
mod expr;
mod pattern;
mod simplify;
mod stmt;
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
    in_loop: bool,
    hoisted_declarations: crate::IndexMap<Span, Option<ir::LocalInfo<'ctx>>>,
}

/// Collect explicitly typed locals in the current function, not nested closures.
#[derive(Default)]
struct LocalDeclarations<'ctx>(Vec<(Spanned<&'ctx str>, Spanned<ast::SourceType<'ctx>>, bool)>);

impl<'ctx> ast::AstVisitor<'ctx, ast::WithSpan> for LocalDeclarations<'ctx> {
    type Error = std::convert::Infallible;

    fn visit_node(
        &mut self,
        node: ast::AstNode<'_, 'ctx, ast::WithSpan>,
    ) -> Result<(), Self::Error> {
        if let ast::AstNode::Stmt((
            ast::Stmt::Let {
                name,
                typ: Some(typ),
                is_const,
                ..
            },
            _,
        )) = node
        {
            self.0.push((*name, typ.as_ref().clone(), *is_const));
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
            in_loop: false,
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
            .local_counter(&SharedU16::new(0))
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
        local_counter: &SharedU16,
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
                        for ((name, name_span), (typ, type_span), is_const) in declarations.0 {
                            lower.hoisted_declarations.insert(name_span, None);
                            if !names.insert(name) {
                                lower
                                    .reporter
                                    .report(Error::LocalRedefinition(name, name_span));
                                continue;
                            }
                            let resolved = lower.resolve_type(&typ, env, type_span);
                            if let Some(typ) = lower.reporter.unwrap_err(resolved) {
                                let mut local = lower
                                    .locals
                                    .add_const_var(Some(name), typ.clone(), name_span, is_const)
                                    .clone();
                                let declaration = ir::LocalDeclaration {
                                    scope: env.scope(),
                                    span: name_span,
                                    initialization: ir::Initialization {
                                        kind: ir::InitializationKind::Default,
                                        site: name_span,
                                    },
                                    storage: ir::LocalStorage::Hoisted {
                                        scope: env.scope(),
                                        default_site: name_span,
                                    },
                                };
                                local.declaration = Some(declaration);
                                lower.locals.set_declaration(local.id, declaration);
                                env.define_local(name, local.clone());
                                lower
                                    .hoisted_declarations
                                    .insert(name_span, Some(local.clone()));
                                prologue.push(ir::Stmt::InitDefault {
                                    local: local.id,
                                    source_binding: None,
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
        let (block, output) = inner(
            symbols,
            reporter,
            locals,
            return_type,
            context,
            body,
            &mut env,
        );
        array_guard::check_block(&block, symbols, reporter);
        (block, output)
    }

    pub fn constant(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
        expected: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> (Option<ir::Const<'ctx>>, Vec<Error<'ctx>>) {
        let mut reporter = LowerReporter::default();
        let counter = SharedU16::new(0);
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

    fn record_declaration(
        &mut self,
        local: ir::Local,
        env: &Env<'_, 'ctx>,
        span: Span,
        kind: ir::InitializationKind,
        site: Span,
    ) {
        self.locals.set_declaration(
            local,
            ir::LocalDeclaration {
                scope: env.scope(),
                span,
                initialization: ir::Initialization { kind, site },
                storage: ir::LocalStorage::Lexical,
            },
        );
    }

    fn associate_array_destination(&mut self, value: &ir::Expr<'ctx>, destination: ir::Local) {
        if let ir::Expr::Local(temp, _) = value {
            self.locals.associate_temp(
                *temp,
                ir::TemporaryPurpose::ArrayLiteral,
                ir::TemporaryOwner::Destination(destination),
            );
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

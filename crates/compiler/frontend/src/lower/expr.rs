use super::*;

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    #[inline]
    pub(super) fn lower_expr(
        &mut self,
        expr: &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        self.lower_expr_with(expr, None, env)
    }

    pub(super) fn lower_expr_with(
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
                let (mut lhs, mut lhs_t) = self.lower_expr(lhs, env)?;
                if env.types().policy(lhs_span).script_ref_write_through
                    && let Some((RefType::Script, pointee)) = lhs_t.strip_ref(self.symbols)
                {
                    self.coerce(&mut lhs, lhs_t, pointee.clone(), env, lhs_span)?;
                    lhs_t = pointee;
                }
                let (mut rhs, rhs_t) = self.lower_expr_with(rhs, Some(&lhs_t), env)?;
                if let ir::Expr::Local(destination, _) = &lhs {
                    self.associate_array_destination(&rhs, *destination);
                }
                self.coerce(&mut rhs, rhs_t, lhs_t.clone(), env, rhs_span)?;

                if lhs.is_prvalue(self.symbols) {
                    self.reporter.report(Error::InvalidPlaceExpr(lhs_span));
                }

                let ir = ir::Expr::Assign {
                    place: lhs.into(),
                    expr: rhs.into(),
                    typ: lhs_t.clone().into(),
                    span: *span,
                };
                (ir, lhs_t)
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
                    && !env
                        .query_free_functions(op.name(), self.symbols)
                        .any(|entry| {
                            calls::accepts_typed_args(
                                entry.func(),
                                &[lhs.1.clone(), rhs.1.clone()],
                                [&lhs.0, &rhs.0].into_iter(),
                                self.symbols,
                            )
                        })
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
                if env.types().policy(*span).condition.is_some()
                    && matches!(op, ast::UnOp::Not)
                    && !env
                        .query_free_functions(op.name(), self.symbols)
                        .any(|entry| {
                            calls::accepts_typed_args(
                                entry.func(),
                                std::slice::from_ref(&arg.1),
                                iter::once(&arg.0),
                                self.symbols,
                            )
                        })
                {
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

            ast::Expr::Omitted => return Err(Error::UnsupportedOmittedArgument(*span)),
            ast::Expr::Error => (ir::Expr::null(*span), PolyType::fresh()),
        };
        Ok(result)
    }

    pub(super) fn lower_array_elements(
        &mut self,
        elems: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (Vec<ir::Expr<'ctx>>, PolyType<'ctx>)> {
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
        Ok((elems, elem_typ))
    }

    fn lower_array_lit(
        &mut self,
        elems: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let (elems, elem_typ) = self.lower_array_elements(elems, hint, env, span)?;
        let array_t = Type::app(predef::ARRAY, [elem_typ.clone()]).into_poly();
        let local = self
            .locals
            .add_temp(ir::TemporaryPurpose::ArrayLiteral, array_t.clone(), span)
            .id;

        self.push_prefix(ir::Stmt::InitArray {
            local,
            source_binding: None,
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
        let resolved = env.types().resolve(typ, self.symbols, *type_span)?;
        let Type::Data(typ) = resolved else {
            let id = resolved.upper_bound().map_or(predef::NOTHING, TypeApp::id);
            return Err(Error::InvalidNewType(id, *type_span));
        };
        let def = &self.symbols[typ.id()];

        let TypeSchema::Aggregate(agg) = def.schema() else {
            return Err(Error::InvalidNewType(typ.id(), *type_span));
        };

        if agg.flags().is_abstract() {
            self.reporter
                .report(Error::InstantiatingAbstract(typ.id(), span));
        }
        if agg.flags().is_value_type() {
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
                .collect::<Shared<_>>()
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

    pub(super) fn lower_construct(
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
                .collect::<Shared<_>>()
        } else if type_args.len() != def.params().len() {
            let expected = def.params().len();
            return Err(Error::InvalidTypeArgCount(expected, type_args.len(), span));
        } else {
            type_args
                .iter()
                .map(|(typ, span)| self.resolve_type(typ, env, *span))
                .collect::<Result<Shared<_>, _>>()?
        };
        let typ = TypeApp::new(id, type_args);
        if !type_args_are_inferred {
            self.check_type_app(&typ, span)?;
        }

        let TypeSchema::Aggregate(aggregate) = def.schema() else {
            return Err(Error::InvalidConstructType(id, span));
        };

        if aggregate.flags().is_class_handle() {
            return Err(Error::InvalidConstructType(id, span));
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

        let (mut expr, mut expr_t) = self.lower_expr(expr, env)?;
        if let Some((RefType::Script, pointee)) = expr_t.strip_ref(self.symbols) {
            self.coerce(&mut expr, expr_t, pointee.clone(), env, *expr_span)?;
            expr_t = pointee;
        }
        if expr_t
            .upper_bound(self.symbols)
            .is_some_and(|typ| typ.id() != predef::ARRAY && !typ.id().is_static_array())
        {
            let index = self.lower_expr(index, env)?;
            let (call, typ) = self
                .free_function_call_builder()
                .name("OperatorArray")
                .args([(expr, expr_t), index])
                .env(env)
                .span(span)
                .build()?;
            return Ok((ir::Expr::call(call, span), typ));
        }
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
        let (mut expr, mut expr_t) = self.lower_expr(expr, env)?;
        if let Some((RefType::Script, pointee)) = expr_t.strip_ref(self.symbols) {
            let expr_span = expr.span();
            self.emit_reference_conversion(
                &mut expr,
                expr_t,
                ir::Intrinsic::Deref,
                env,
                expr_span,
            )?;
            expr_t = pointee;
        }
        let resolved = env.types().resolve(typ, self.symbols, *type_span)?;
        let Some(agg) = resolved
            .upper_bound()
            .and_then(|bound| self.symbols[bound.id()].schema().as_aggregate())
        else {
            return Err(Error::InvalidDynCastType(expr_t, resolved, *type_span));
        };
        let generic_target = matches!(resolved, Type::Ctx(_));
        if generic_target && agg.flags().is_value_type() {
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

    pub(super) fn dyn_cast_compatible(
        &self,
        source: &PolyType<'ctx>,
        target: &PolyType<'ctx>,
    ) -> bool {
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

        let (mut ir, mut typ) = self.lower_expr(receiver, env)?;
        if let Some((RefType::Script, pointee)) = typ.strip_ref(self.symbols) {
            self.coerce(&mut ir, typ, pointee.clone(), env, receiver_span)?;
            typ = pointee;
        }
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

    pub(super) fn lower_constant(
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

    pub(super) fn resolve_field(
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

    pub(super) fn resolve_type(
        &self,
        typ: &ast::SourceType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, PolyType<'ctx>> {
        let typ = PolyType::from_type(&env.types().resolve(typ, self.symbols, span)?);
        self.check_type(&typ, span)?;
        Ok(typ)
    }

    pub(super) fn check_visibility(
        &mut self,
        parent: TypeId<'ctx>,
        visibility: Visibility,
    ) -> bool {
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

    pub(super) fn extract_local(
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
                typ: expr_t.clone().into(),
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

use super::*;

#[bon]
impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    pub(super) fn lower_call(
        &mut self,
        expr @ (_, expr_span): &Spanned<ast::SourceExpr<'ctx>>,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        call_span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        if let &(ast::Expr::Ident("Cast"), _) = expr
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
            && env.types().policy(call_span).implicit_members
            && env.locals().get(name).is_none()
            && let Some(parent) = self.context
        {
            let instance = env.locals().get("this").is_some()
                && self
                    .symbols
                    .query_methods_by_name(parent, name)
                    .any(|entry| !entry.func().flags().is_static());
            let has_method = self
                .symbols
                .query_methods_by_name(parent, name)
                .any(|entry| instance || entry.func().flags().is_static());
            if has_method {
                let receiver = if instance {
                    ast::Expr::This
                } else {
                    ast::Expr::Ident(parent.as_str())
                };
                let member = (
                    ast::Expr::Member {
                        expr: Box::new((receiver, *expr_span)),
                        member: name,
                    },
                    *expr_span,
                );
                return self.lower_call(&member, type_args, args, hint, env, call_span);
            }
        }

        if let &(ast::Expr::Ident(name), _) = expr
            && (!env.types().policy(call_span).implicit_members || env.locals().get(name).is_none())
            && let Some(&TypeRef::Id(id)) = env.types().get(name)
        {
            if let Some((_, span)) = args
                .iter()
                .find(|(expr, _)| crate::arguments::is_absent(expr))
            {
                return Err(Error::UnsupportedOmittedArgument(*span));
            }
            return self.lower_construct(id, args, type_args, env, call_span);
        }

        if let &(ast::Expr::Ident(name), _) = expr
            && (!env.types().policy(call_span).implicit_members || env.locals().get(name).is_none())
            && let mut candidates = env
                .query_free_functions(name, self.symbols)
                .filter(|entry| {
                    entry.func().type_().conversion().is_none()
                        && (name != "Cast" || conversion_is_eligible(entry.func(), env))
                })
                .peekable()
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
            if let ir::Call::FreeFunction { function, .. } = &call
                && let Some(intrinsic) = self.symbols[*function].intrinsic()
                && env
                    .types()
                    .policy(call_span)
                    .allow_explicit_intrinsic
                    .is_some_and(|allow| !allow(intrinsic))
            {
                return Err(Error::UnresolvedFunction(name, *expr_span));
            }
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
            array_guard::check_runtime_callee(args, *expr_span)?;
            self.lower_expr(expr, env)?
        };

        let type_id = TypeId::fn_with_arity(args.len())
            .ok_or(Error::UnsupportedArity(args.len(), call_span))?;
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
        if let Some((_, span)) = args
            .iter()
            .find(|(expr, _)| crate::arguments::is_absent(expr))
        {
            return Err(Error::UnsupportedOmittedArgument(*span));
        }
        let target_t = match (type_args, hint) {
            ([(target, target_span)], _) => self.resolve_type(target, env, *target_span)?,
            ([], Some(hint)) => hint.clone(),
            ([], None) => return Err(Error::UnknownStaticCastType(span)),
            _ => return Err(Error::InvalidTypeArgCount(1, type_args.len(), span)),
        };
        let object_target = target_t.upper_bound(self.symbols).is_some_and(|target| {
            self.symbols[target.id()]
                .schema()
                .as_aggregate()
                .is_some_and(|agg| !agg.flags().is_struct() && !agg.flags().is_never_ref())
        });
        // Literals cannot produce Variant. Leave ordinary conversion operands in
        // source form so the selected parameter can supply their numeric context.
        let literal = is_literal_or_negated_literal(&args[0].0);
        let mut native_arg =
            if env.types().policy(span).native_object_cast && (object_target || !literal) {
                let (expr, expr_t) = self.lower_expr(&args[0], env)?;
                if expr_t
                    .upper_bound(self.symbols)
                    .is_some_and(|typ| typ.id() == predef::VARIANT)
                {
                    let (call, typ) = self
                        .free_function_call_builder()
                        .name(ir::Intrinsic::FromVariant)
                        .expected_intrinsic(ir::Intrinsic::FromVariant)
                        .type_args(&[target_t])
                        .args([(expr, expr_t)])
                        .env(env)
                        .span(span)
                        .build()?;
                    return Ok((ir::Expr::call(call, span), typ));
                }
                Some((expr, expr_t))
            } else {
                None
            };
        if native_arg.is_some() && object_target {
            let (mut expr, expr_t) = native_arg.take().unwrap();
            let constraint = expr_t.unwrap_ref_or_self(self.symbols);
            let result_t = if expr_t.ref_type(self.symbols) == Some(RefType::Weak) {
                Type::app(predef::WREF, [target_t.clone()]).into_poly()
            } else {
                target_t.clone()
            };
            // Native object casts also admit identity and upcasts. Coerce these
            // directly, without retrying lowering (which can emit statement prefixes).
            if constraint.satisfies_bound(&target_t, self.symbols) {
                self.coerce(&mut expr, expr_t, result_t.clone(), env, span)?;
                return Ok((expr, result_t));
            }
            // Keep the same downcast constraints and weak result as lower_dyn_cast.
            let compatible = if matches!(constraint, PolyType::Var(_)) {
                target_t.constrain_base(&constraint, self.symbols).is_ok()
            } else {
                self.dyn_cast_compatible(&constraint, &target_t)
            };
            if !compatible {
                return Err(Error::ImpossibleDynCast(expr_t, target_t, span));
            }
            return Ok((
                ir::Expr::DynCast {
                    expr: expr.into(),
                    expr_type: expr_t.into(),
                    target_type: target_t.into(),
                    span,
                },
                result_t,
            ));
        }
        // Ordinary Cast return-target overloads retain precedence, even if their
        // arguments later fail. Do not retry lowering the already-lowered operand.
        if let Some((_, expr_t)) = native_arg.as_ref()
            && !env.query_free_functions("Cast", self.symbols).any(|entry| {
                conversion_is_eligible(entry.func(), env)
                    && target_t.is_subtype_compatible(
                        entry.func().type_().return_type(),
                        Variance::Covariant,
                        self.symbols,
                    )
            })
            && let (Some(source), Some(target)) = (
                conversions::fixed_id(expr_t),
                conversions::fixed_id(&target_t),
            )
        {
            if let Some(function) =
                conversions::provider(source, target, false, env, self.symbols, span)?
            {
                let (expr, _) = native_arg.take().unwrap();
                return Ok((conversions::call(function, expr, span), target_t));
            }
            if let Some((intrinsic, enum_id)) =
                conversions::enum_fallback(source, target, self.symbols)
            {
                let (expr, expr_t) = native_arg.take().unwrap();
                // The probe proved exact bounds. Commit to concrete operand/type
                // arguments so intrinsic inference cannot strip or widen them.
                let source_t = PolyType::nullary(source);
                expr_t
                    .constrain_base(&source_t, self.symbols)
                    .with_span(span)?;
                let (call, typ) = self
                    .free_function_call_builder()
                    .name(intrinsic)
                    .expected_intrinsic(intrinsic)
                    .type_args(&[PolyType::nullary(enum_id)])
                    .args([(expr, source_t)])
                    .env(env)
                    .span(span)
                    .build()?;
                return Ok((ir::Expr::call(call, span), typ));
            }
        }
        let mut candidates = env
            .query_free_functions("Cast", self.symbols)
            .filter(|entry| {
                !entry.func().type_().is_valid()
                    || target_t.is_subtype_compatible(
                        entry.func().type_().return_type(),
                        Variance::Covariant,
                        self.symbols,
                    )
            })
            .peekable();
        let has_candidates = candidates.peek().is_some();
        let mut candidates = candidates
            .filter(|entry| conversion_is_eligible(entry.func(), env))
            .peekable();
        if has_candidates && candidates.peek().is_none() {
            return Err(Error::UnsupportedNativeConversion(span));
        }
        if let Some((expr, expr_t)) = native_arg {
            let mut checked_args = [expr];
            let (call, typ) = self
                .typed_overload_resolver()
                .name("Cast")
                .args(&mut checked_args)
                .arg_types(&[expr_t])
                .source_args(&[&args[0]])
                .candidates(candidates)
                .env(env)
                .span(span)
                .resolve()?
                .with_args(checked_args)
                .into_call();
            return Ok((ir::Expr::call(call, span), typ));
        }
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

    fn lower_arg(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        param: &Param<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        self.lower_arg_with_provider(expr, lhs, rhs, param, env, span, true)
    }

    // Keep recovery permission explicit rather than storing mutable lowering state.
    #[allow(clippy::too_many_arguments)]
    fn lower_arg_with_provider(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        mut lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        param: &Param<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
        allow_provider: bool,
    ) -> LowerResult<'ctx, ()> {
        if param.flags().is_out() {
            if env.types().policy(span).script_ref_write_through
                && rhs.ref_type(self.symbols) != Some(RefType::Script)
                && let Some((RefType::Script, pointee)) = lhs.strip_ref(self.symbols)
            {
                self.emit_reference_conversion(expr, lhs, ir::Intrinsic::Deref, env, span)?;
                lhs = pointee;
            }
            rhs.constrain_base(&lhs, self.symbols).with_span(span)?;
            if expr.is_prvalue(self.symbols) {
                self.reporter.report(Error::InvalidTemporary(span));
            }
        }
        self.coerce_with_provider(
            expr,
            lhs,
            rhs,
            env,
            span,
            allow_provider && !param.flags().is_out(),
        )?;
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
                .query_methods_by_name(id, member)
                .filter(|entry| entry.func().flags().is_static())
                .scan(Vec::new(), |seen, entry| {
                    let hidden =
                        seen.iter()
                            .any(|previous: &crate::symbols::MethodEntry<'_, '_>| {
                                previous.key().parent() != entry.key().parent()
                                    && same_parameter_signature(
                                        previous.func().type_(),
                                        entry.func().type_(),
                                    )
                            });
                    if !hidden {
                        seen.push(entry.clone());
                    }
                    Some((!hidden).then_some(entry))
                })
                .flatten()
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

            let method_id = resolved.resolution.function;
            let method = &self.symbols[method_id];
            if !self.check_visibility(method_id.parent(), method.flags().visibility()) {
                self.reporter.report(Error::InaccessibleMethod(
                    InaccessibleMember::new(method_id, member, method.flags().visibility()).into(),
                    call_span,
                ));
            }

            let parent = resolved.resolution.function.parent();
            if parent != id && !env.types().policy(call_span).allow_inherited_static_calls {
                self.reporter.report(Error::CallingBaseStaticMethod {
                    type_id: parent,
                    name: member,
                    span: call_span,
                });
            }
            let (call, call_t) = resolved.into_static_call(typ.id(), typ.args().iter().cloned());
            return Ok((MemberCall::MethodCall(call), call_t));
        }

        array_guard::check_runtime_callee(args, *expr_span)?;
        let (mut ir, mut typ) = self.lower_expr(receiver, env)?;
        if let Some((RefType::Script, pointee)) = typ.strip_ref(self.symbols) {
            self.emit_reference_conversion(&mut ir, typ, ir::Intrinsic::Deref, env, *expr_span)?;
            typ = pointee;
        }
        let (ref_type, unref_t) = typ.strip_ref(self.symbols).unzip();
        let upper_bound = unref_t
            .unwrap_or(typ.clone())
            .force_upper_bound(self.symbols)
            .with_span(*expr_span)?
            .ok_or(Error::InsufficientTypeInformation(*expr_span))?
            .into_owned();

        if let Some((intrinsic, requires_place)) = env
            .types()
            .policy(call_span)
            .member_intrinsic
            .and_then(|resolve| resolve(upper_bound.id(), member))
        {
            if !type_args.is_empty() {
                return Err(Error::InvalidTypeArgCount(0, type_args.len(), call_span));
            }
            // Keep receiver identity and evaluate it once, before the arguments.
            if requires_place && ir.is_prvalue(self.symbols) {
                return Err(Error::InvalidTemporary(*expr_span));
            }
            let mut values = vec![(ir, typ)];
            for arg in args {
                values.push(self.lower_expr(arg, env)?);
            }
            let (call, typ) = self
                .free_function_call_builder()
                .name(intrinsic)
                .expected_intrinsic(intrinsic)
                .args(values)
                .env(env)
                .span(call_span)
                .build()?;
            return Ok((MemberCall::MethodCall(call), typ));
        }

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

        let candidates = self
            .symbols
            .query_methods_by_name(upper_bound.id(), member)
            .filter(|entry| !entry.func().flags().is_static())
            .collect::<Vec<_>>();
        // A forwarder is an alias for a static declaration, not another instance
        // slot. A real method with that signature wins, including inherited ones.
        let mut candidates = candidates
            .iter()
            .filter(|entry| {
                !entry.func().flags().is_static_forwarder()
                    || !candidates.iter().any(|real| {
                        !real.func().flags().is_static_forwarder()
                            && same_parameter_signature(real.func().type_(), entry.func().type_())
                    })
            })
            .cloned()
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

        let method_id = resolved.resolution.function;
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
            let param = self.symbols[aliased]
                .type_()
                .params()
                .first()
                .expect("static alias must at least accept a receiver");
            let span = ir.span();
            if param.flags().is_short_circuit() {
                array_guard::check_lazy(receiver)?;
            }
            self.lower_arg(
                &mut ir,
                typ,
                PolyType::from_type(param.type_()),
                param,
                env,
                span,
            )?;

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

        let mut invalid_signature = false;
        let mut invalid_slot = None;
        let mut arg_ranges = Vec::new();
        let mut filtered = candidates
            .into_iter()
            .filter(|f| {
                if f.func().type_().conversion().is_some() {
                    return false;
                }
                if !f.func().type_().is_valid() {
                    invalid_signature = true;
                    return false;
                }
                let range = f.func().type_().min_args()..=f.func().type_().max_args();
                arg_ranges.push(range.clone());
                if !range.contains(&argc) {
                    return false;
                }
                if !crate::arguments::accepts_slots(args.clone(), f.func().type_().params()) {
                    invalid_slot = args
                        .clone()
                        .zip(f.func().type_().params())
                        .find(|((expr, _), param)| {
                            crate::arguments::is_absent(expr) && !param.flags().is_optional()
                        })
                        .map(|((_, span), _)| *span);
                    return false;
                }
                true
            })
            .peekable();

        let Some(primary) = filtered.next() else {
            return if invalid_signature {
                Err(Error::InvalidFunctionSignature(name, span))
            } else if let Some(span) = invalid_slot {
                Err(Error::RequiredArgumentOmitted(span))
            } else if arg_ranges.is_empty() {
                Err(Error::UnresolvedFunction(name, span))
            } else {
                Err(Error::InvalidArgCount(arg_ranges, argc, span))
            };
        };

        let type_args = type_args
            .iter()
            .map(|(typ, span)| self.resolve_type(typ, env, *span))
            .collect::<Result<Vec<_>, _>>()?;

        if filtered.peek().is_none() {
            // Prefix construction cannot escape a skipped argument.
            for (arg, param) in args.clone().zip(primary.func().type_().params()) {
                if param.flags().is_short_circuit() {
                    array_guard::check_lazy(arg)?;
                }
            }
            let type_env = receiver
                .and_then(|typ| {
                    typ.base_type_env::<AnyBaseType>(primary.key().parent()?, self.symbols)
                })
                .unwrap_or_default();

            let type_env = self
                .function_env_builder()
                .type_(primary.func().type_())
                .type_args(&type_args)
                .base_env(&type_env)
                .span(span)
                .build()?;

            let return_t =
                PolyType::from_type_with_env(primary.func().type_().return_type(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
            if let Some(hint) = type_hint {
                return_t.constrain_base(hint, self.symbols).ok();
            }

            let mut checked_args = Vec::with_capacity(args.len());
            for arg in args.zip(primary.func().type_().params()) {
                let (arg @ (_, span), param) = arg;
                if crate::arguments::is_absent(&arg.0) {
                    checked_args.push(ir::Expr::Nop(*span));
                    continue;
                }
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
            let source_args = args.clone().collect::<Vec<_>>();

            for arg in args.clone() {
                if crate::arguments::is_absent(&arg.0) {
                    checked_args.push(ir::Expr::Nop(arg.1));
                    arg_types.push(PolyType::fresh());
                } else {
                    let (expr, typ) = self.lower_expr(arg, env)?;
                    checked_args.push(expr);
                    arg_types.push(typ);
                }
            }

            let res = self
                .typed_overload_resolver()
                .name(name)
                .args(&mut checked_args)
                .arg_types(&arg_types)
                .source_args(&source_args)
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
    pub(super) fn typed_overload_resolver<Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: &mut [ir::Expr<'ctx>],
        arg_types: &[PolyType<'ctx>],
        #[builder(default)] source_args: &[&Spanned<ast::SourceExpr<'ctx>>],
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
            args: &'a [ir::Expr<'ctx>],
            symbols: &'a Symbols<'ctx>,
        ) -> impl Iterator<Item = FunctionEntry<Key, Name, Func>> + use<'a, 'ctx, It, Key, Name, Func>
        where
            It: IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
            Func: FunctionKind<'ctx> + Copy,
        {
            it.into_iter().filter(move |entry| {
                accepts_typed_args(entry.func(), arg_types, args.iter(), symbols)
            })
        }

        let mut arg_ranges = Vec::new();
        let mut invalid_signature = false;
        let candidates = candidates.into_iter().filter(|entry| {
            if entry.func().type_().conversion().is_some() {
                return false;
            }
            if !entry.func().type_().is_valid() {
                invalid_signature = true;
                return false;
            }
            arg_ranges.push(entry.func().type_().min_args()..=entry.func().type_().max_args());
            true
        });
        let matches =
            match_by_subtype(candidates, arg_types, args, self.symbols).collect::<Vec<_>>();
        let fallback = fallback.filter(|entry| {
            entry.func().type_().conversion().is_none()
                && entry.func().type_().is_valid()
                && (entry.func().type_().min_args()..=entry.func().type_().max_args())
                    .contains(&args.len())
        });
        let Some(selected) = matches.first().or(fallback.as_ref()) else {
            return Err(if invalid_signature {
                Error::InvalidFunctionSignature(name, span)
            } else if !arg_ranges.is_empty()
                && arg_ranges.iter().all(|range| !range.contains(&args.len()))
            {
                Error::InvalidArgCount(arg_ranges, args.len(), span)
            } else {
                Error::UnresolvedFunction(name, span)
            });
        };

        let selected = if matches.len() <= 1 {
            selected
        } else {
            // Compare declarations without constraining argument inference. Only
            // commit coercions after a unique undominated signature is known.
            let mut best = matches.iter().filter(|candidate| {
                !matches.iter().any(|other| {
                    more_specific_reference_signature(
                        other.func().type_(),
                        candidate.func().type_(),
                        args,
                        arg_types,
                        self.symbols,
                    )
                })
            });
            let primary = best.next();
            if let Some(primary) = primary
                && best.next().is_none()
            {
                primary
            } else {
                let signatures = matches
                    .iter()
                    .map(|entry| {
                        let typ = entry.func().type_();
                        FunctionSignature::new(name, typ.params(), typ.return_type().clone())
                    })
                    .collect();
                return Err(Error::MultipleMatchingOverloads(signatures, span));
            }
        };

        let type_env = receiver
            .and_then(|typ| {
                typ.base_type_env::<AnyBaseType>(selected.key().parent()?, self.symbols)
            })
            .unwrap_or_default();
        let type_env = self
            .function_env_builder()
            .type_(selected.func().type_())
            .type_args(type_args)
            .base_env(&type_env)
            .span(span)
            .build()?;
        // The source array guard already checks logical RHS operands as lazy;
        // generated logical calls also use prefix-free operands. Other typed
        // callers need source to establish that initialization cannot escape.
        for (index, param) in selected
            .func()
            .type_()
            .params()
            .iter()
            .take(args.len())
            .enumerate()
        {
            if param.flags().is_short_circuit() && !matches!(args[index], ir::Expr::Nop(_)) {
                if let Some(source) = source_args.get(index) {
                    array_guard::check_lazy(source)?;
                } else if index != 1
                    || ![ast::BinOp::And.name(), ast::BinOp::Or.name()].contains(&name)
                {
                    return Err(Error::UnsupportedSkipArgument(args[index].span()));
                }
            }
        }
        for ((arg, typ), param) in args
            .iter_mut()
            .zip(arg_types)
            .zip(selected.func().type_().params())
        {
            if matches!(arg, ir::Expr::Nop(_)) {
                continue;
            }
            let span = arg.span();
            let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                .map_err(|var| Error::UnresolvedVar(var, span))?;
            // A recovery-only signature must not become viable through a provider,
            // including one exposed by recursive script-reference adaptation.
            self.lower_arg_with_provider(
                &mut *arg,
                typ.clone(),
                expected,
                param,
                env,
                span,
                !matches.is_empty(),
            )?;
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

    pub(super) fn coerce(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        self.coerce_with_provider(expr, lhs, rhs, env, span, true)
    }

    fn coerce_with_provider(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
        allow_provider: bool,
    ) -> LowerResult<'ctx, ()> {
        if let Some(validate) = env.types().policy(span).validate_coercion {
            validate(&lhs, &rhs, self.symbols, span)?;
        }
        match (lhs.strip_ref(self.symbols), rhs.strip_ref(self.symbols)) {
            (Some((RefType::Script, pointee)), target)
                if !matches!(target, Some((RefType::Script, _))) =>
            {
                self.emit_reference_conversion(expr, lhs, ir::Intrinsic::Deref, env, span)?;
                return self.coerce_with_provider(expr, pointee, rhs, env, span, allow_provider);
            }
            (source, Some((RefType::Script, pointee)))
                if !matches!(source, Some((RefType::Script, _))) =>
            {
                self.coerce_with_provider(expr, lhs, pointee.clone(), env, span, allow_provider)?;
                return self.emit_reference_conversion(
                    expr,
                    pointee,
                    ir::Intrinsic::AsRef,
                    env,
                    span,
                );
            }
            _ => {}
        }
        if allow_provider
            && env.types().policy(span).native_object_cast
            && let Some(function) =
                conversions::implicit_provider(&lhs, &rhs, env, self.symbols, span)?
        {
            let arg = mem::replace(expr, ir::Expr::null(span));
            *expr = conversions::call(function, arg, span);
            return Ok(());
        }
        let coercion = lhs
            .coerce(&rhs, self.symbols)
            .with_span(span)
            .map_err(|error| {
                if conversions::unsupported_implicit(&lhs, &rhs, env, self.symbols) {
                    Error::UnsupportedNativeConversion(span)
                } else {
                    error
                }
            })?;
        let name = match coercion {
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
                    self.reporter.report(Error::UnexpectedRef(lhs, rhs, span));
                }
                return Ok(());
            }
            Some(Coercion::IntoRef(RefType::Strong)) => {
                self.reporter
                    .report(Error::UnexpectedNonRef(lhs, rhs, span));
                return Ok(());
            }
            None => return Ok(()),
        };
        self.emit_reference_conversion(expr, lhs, name, env, span)
    }

    fn emit_reference_conversion(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        typ: PolyType<'ctx>,
        intrinsic: ir::Intrinsic,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        // Inference for a bare generic parameter can strip reference wrappers.
        // Pin the script-reference pointee so AsRef<wref<T>> keeps the weak value.
        let type_args = match intrinsic {
            ir::Intrinsic::AsRef => vec![typ.clone()],
            ir::Intrinsic::Deref => vec![typ.unwrap_ref_or_self(self.symbols)],
            _ => vec![],
        };
        let arg = mem::replace(expr, ir::Expr::null(span));
        let (call, _) = self
            .free_function_call_builder()
            .name(intrinsic)
            .expected_intrinsic(intrinsic)
            .type_args(&type_args)
            .args([(arg, typ)])
            .env(env)
            .span(span)
            .build()?;
        *expr = ir::Expr::call(call, span);
        Ok(())
    }

    #[builder(finish_fn = build)]
    pub(super) fn free_function_call_builder<'name: 'ctx>(
        &mut self,
        name: impl Into<&'name str>,
        expected_intrinsic: Option<ir::Intrinsic>,
        args: impl IntoIterator<Item = (ir::Expr<'ctx>, PolyType<'ctx>)>,
        #[builder(default)] type_args: &[PolyType<'ctx>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        let name = name.into();
        let (mut args, arg_types): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let candidates = env
            .query_free_functions(name, self.symbols)
            .filter(|entry| {
                expected_intrinsic
                    .is_none_or(|intrinsic| entry.func().intrinsic() == Some(intrinsic))
            });
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
    pub(super) fn runtime_type_check_builder(
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
    fn function_env_builder<'env>(
        &self,
        type_: &FunctionType<'ctx>,
        type_args: &[PolyType<'ctx>],
        base_env: &'env ScopedMap<'env, &'ctx str, PolyType<'ctx>>,
        span: Span,
    ) -> LowerResult<'ctx, ScopedMap<'env, &'ctx str, PolyType<'ctx>>> {
        if !type_args.is_empty() && type_args.len() != type_.type_params().len() {
            let expected = type_.type_params().len();
            return Err(Error::InvalidTypeArgCount(expected, type_args.len(), span));
        }

        let scope = type_
            .type_params()
            .iter()
            .enumerate()
            .map(|(index, var)| {
                let typ = type_args
                    .get(index)
                    .cloned()
                    .unwrap_or_else(PolyType::fresh);
                (var.name(), typ)
            })
            .collect();
        let map = base_env.push_scope(scope);

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

        Ok(map)
    }
}

fn is_literal_or_negated_literal(expr: &ast::SourceExpr<'_>) -> bool {
    matches!(expr, ast::Expr::Constant(_))
        || matches!(expr, ast::Expr::UnOp { op: ast::UnOp::Neg, expr }
            if matches!(expr.0, ast::Expr::Constant(_)))
}

fn same_parameter_signature<'ctx>(lhs: &FunctionType<'ctx>, rhs: &FunctionType<'ctx>) -> bool {
    lhs.is_valid()
        && rhs.is_valid()
        && lhs.type_params().len() == rhs.type_params().len()
        && lhs.param_types().eq(rhs.param_types())
}

fn more_specific_reference_signature<'ctx>(
    lhs: &FunctionType<'ctx>,
    rhs: &FunctionType<'ctx>,
    args: &[ir::Expr<'ctx>],
    arg_types: &[PolyType<'ctx>],
    symbols: &Symbols<'ctx>,
) -> bool {
    if !lhs.type_params().is_empty()
        || !rhs.type_params().is_empty()
        || lhs.params().len() != rhs.params().len()
    {
        return false;
    }
    let mut strict = false;
    for (index, (left, right)) in lhs.params().iter().zip(rhs.params()).enumerate() {
        if left.flags().is_optional() != right.flags().is_optional()
            || left.flags().is_out() != right.flags().is_out()
            || left.flags().is_const() != right.flags().is_const()
            || left.flags().is_short_circuit() != right.flags().is_short_circuit()
        {
            return false;
        }
        if left.type_() == right.type_() {
            continue;
        }
        // Omitted and null arguments supply no evidence for a narrower target.
        // Out, weak/script-ref, generic and conversion preferences need their own
        // official dispatch probes; do not derive them from covariance alone.
        if left.flags().is_out()
            || args
                .get(index)
                .is_none_or(|arg| matches!(arg, ir::Expr::Nop(_) | ir::Expr::Null { .. }))
            || arg_types.get(index).is_none_or(|typ| {
                let typ = match typ {
                    PolyType::Mono(typ) => typ.clone(),
                    PolyType::Var(var) => var.lower(),
                };
                // Coalescing would select inference bounds, even for Nothing.
                // Inspect existing evidence without freezing the caller's local.
                !matches!(typ.into_poly().unwrap_ref_or_self(symbols),
                    PolyType::Mono(Type::Data(typ)) if typ.args().is_empty())
            })
        {
            return false;
        }
        let (left, right) = match (left.type_().strip_ref(), right.type_().strip_ref()) {
            (Some((RefType::Strong, left)), Some((RefType::Strong, right))) => (left, right),
            (None, None) => (left.type_(), right.type_()),
            _ => return false,
        };
        let (Type::Data(left), Type::Data(right)) = (left, right) else {
            return false;
        };
        if !left.args().is_empty()
            || !right.args().is_empty()
            || ![left.id(), right.id()].into_iter().all(|id| {
                symbols[id]
                    .schema()
                    .as_aggregate()
                    .is_some_and(|agg| !agg.flags().is_struct())
            })
            || !symbols.has_base_type(left.id(), right.id())
        {
            return false;
        }
        strict |= left.id() != right.id();
    }
    strict
}

fn conversion_is_eligible(function: &crate::FreeFunction<'_>, env: &Env<'_, '_>) -> bool {
    // Retain authored intent in every dialect without making declarations ordinary
    // Cast overloads. Imported and scripted dispatch require separate evidence.
    function.type_().conversion().is_none()
        && !function
            .span()
            .is_some_and(|span| env.types().policy(span).native_object_cast)
}

// Eligibility must remain read-only: logical truthiness fallback uses this before
// resolution, whose ambiguity handling and coercions can constrain inference vars.
pub(super) fn accepts_typed_args<'a, 'ctx: 'a>(
    function: impl FunctionKind<'ctx>,
    arg_types: &[PolyType<'ctx>],
    args: impl ExactSizeIterator<Item = &'a ir::Expr<'ctx>>,
    symbols: &Symbols<'ctx>,
) -> bool {
    let typ = function.type_();
    typ.conversion().is_none()
        && typ.is_valid()
        && (typ.min_args()..=typ.max_args()).contains(&arg_types.len())
        && arg_types.len() == args.len()
        && arg_types
            .iter()
            .zip(args)
            .zip(typ.params())
            .all(|((arg, expr), param)| {
                if matches!(expr, ir::Expr::Nop(_)) {
                    param.flags().is_optional()
                } else if matches!(param.type_(), Type::Data(id) if id.id() == predef::VARIANT)
                    && function.intrinsic().is_none()
                {
                    true
                } else {
                    let variance = if param.flags().is_out() {
                        Variance::Invariant
                    } else {
                        Variance::Covariant
                    };
                    arg.unwrap_ref_or_self(symbols).is_subtype_compatible(
                        param.type_().unwrap_ref_or_self(),
                        variance,
                        symbols,
                    )
                }
            })
}

#[derive(Debug)]
pub(super) struct FunctionResolution<'ctx, K> {
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

    pub(super) fn with_args(
        self,
        args: impl Into<Box<[ir::Expr<'ctx>]>>,
    ) -> FunctionResultWithArgs<'ctx, K> {
        FunctionResultWithArgs {
            resolution: self,
            args: args.into(),
        }
    }
}

#[derive(Debug)]
pub(super) struct FunctionResultWithArgs<'ctx, K> {
    resolution: FunctionResolution<'ctx, K>,
    args: Box<[ir::Expr<'ctx>]>,
}

impl<'ctx> FunctionResultWithArgs<'ctx, FreeFunctionIndex> {
    #[inline]
    pub(super) fn into_call(self) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::FreeFunction {
            function: self.resolution.function,
            type_args: self.resolution.type_args,
            args: self.args,
        };
        (call, self.resolution.return_type)
    }
}

impl<'ctx> FunctionResultWithArgs<'ctx, MethodId<'ctx>> {
    pub fn with_method(self, method: MethodId<'ctx>) -> Self {
        Self {
            resolution: self.resolution.with_function(method),
            ..self
        }
    }

    pub fn map_args<F>(self, f: F) -> Self
    where
        F: FnOnce(Box<[ir::Expr<'ctx>]>) -> Box<[ir::Expr<'ctx>]>,
    {
        Self {
            resolution: self.resolution,
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
            method: self.resolution.function,
            type_args: self.resolution.type_args,
            args: self.args,
            mode,
        };
        (call, self.resolution.return_type)
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
            method: self.resolution.function,
            type_args: self.resolution.type_args,
            args: self.args,
        };
        (call, self.resolution.return_type)
    }
}

#[derive(Debug)]
enum MemberCall<'ctx> {
    MethodCall(ir::Call<'ctx>),
    FieldAccess(ir::Expr<'ctx>),
}

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
        let target_t = match (type_args, hint) {
            ([(target, target_span)], _) => self.resolve_type(target, env, *target_span)?,
            ([], Some(hint)) => hint.clone(),
            ([], None) => return Err(Error::UnknownStaticCastType(span)),
            _ => return Err(Error::InvalidTypeArgCount(1, type_args.len(), span)),
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

            let method_id = resolved.resolution.function;
            let method = &self.symbols[method_id];
            if !self.check_visibility(method_id.parent(), method.flags().visibility()) {
                self.reporter.report(Error::InaccessibleMethod(
                    InaccessibleMember::new(method_id, member, method.flags().visibility()).into(),
                    call_span,
                ));
            }

            let parent = resolved.resolution.function.parent();
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

        let mut arg_ranges = Vec::new();
        let mut filtered = candidates
            .into_iter()
            .filter(|f| {
                let range = f.func().type_().min_args()..=f.func().type_().max_args();
                arg_ranges.push(range.clone());
                range.contains(&argc)
            })
            .peekable();

        let Some(primary) = filtered.next() else {
            return if arg_ranges.is_empty() {
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
    pub(super) fn typed_overload_resolver<Key, Name, Func>(
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
        let type_env = self
            .function_env_builder()
            .type_(selected.func().type_())
            .type_args(type_args)
            .base_env(&type_env)
            .span(span)
            .build()?;
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

    pub(super) fn coerce(
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

    #[builder(finish_fn = build)]
    pub(super) fn free_function_call_builder<'name: 'ctx>(
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

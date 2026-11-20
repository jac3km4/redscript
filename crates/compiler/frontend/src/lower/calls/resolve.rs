use std::cell::RefCell;
use std::cmp;
use std::ops::RangeInclusive;

use super::*;

#[derive(Default)]
struct OverloadDiagnostics {
    invalid_signature: bool,
    invalid_slot: Option<Span>,
    arg_ranges: Vec<RangeInclusive<usize>>,
}

#[bon]
impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    #[builder(finish_fn = resolve)]
    pub(in crate::lower) fn overload_resolver<'a, Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: impl IntoIterator<
            IntoIter = impl ExactSizeIterator<Item = &'a Spanned<ast::SourceExpr<'ctx>>> + Clone,
        >,
        #[builder(default)] type_args: &[Spanned<ast::SourceType<'ctx>>],
        candidates: impl IntoIterator<
            IntoIter = impl Iterator<Item = FunctionEntry<Key, Name, Func>> + Clone,
        >,
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

        let diagnostics = RefCell::new(OverloadDiagnostics::default());
        let mut filtered = candidates
            .into_iter()
            .filter(|f| {
                if f.func().type_().conversion().is_some() {
                    return false;
                }
                if !f.func().type_().is_valid() {
                    diagnostics.borrow_mut().invalid_signature = true;
                    return false;
                }
                let range = f.func().type_().min_args()..=f.func().type_().max_args();
                diagnostics.borrow_mut().arg_ranges.push(range.clone());
                if !range.contains(&argc) {
                    return false;
                }
                if !crate::arguments::accepts_slots(args.clone(), f.func().type_().params()) {
                    diagnostics.borrow_mut().invalid_slot = args
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
            let OverloadDiagnostics {
                invalid_signature,
                invalid_slot,
                arg_ranges,
            } = diagnostics.into_inner();
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
            // Diagnose escaping prefixes without discarding the legacy call.
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
    pub(in crate::lower) fn typed_overload_resolver<Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: &mut [ir::Expr<'ctx>],
        arg_types: &[PolyType<'ctx>],
        #[builder(default)] source_args: &[&Spanned<ast::SourceExpr<'ctx>>],
        #[builder(default)] type_args: &[PolyType<'ctx>],
        candidates: impl IntoIterator<
            IntoIter = impl Iterator<Item = FunctionEntry<Key, Name, Func>> + Clone,
        >,
        fallback: Option<FunctionEntry<Key, Name, Func>>,
        receiver: Option<InferredTypeApp<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, FunctionResolution<'ctx, Key>>
    where
        Key: FunctionKey<'ctx> + Copy,
        Func: FunctionKind<'ctx> + Copy,
    {
        let candidates = candidates.into_iter();
        // Scope precedence applies only to viable overloads. Within that scope,
        // a dominating signature must have a strictly higher specificity score.
        let applicable = |entry: &FunctionEntry<Key, Name, Func>| match receiver_instantiated_params(
            entry,
            receiver.as_ref(),
            self.symbols,
        ) {
            Some(params) => {
                accepts_params(entry.func(), &params, arg_types, args.iter(), self.symbols)
            }
            None => accepts_typed_args(entry.func(), arg_types, args.iter(), self.symbols),
        };
        let primary = candidates
            .clone()
            .enumerate()
            .filter(|(_, entry)| applicable(entry))
            .max_by_key(|(_, entry)| {
                (
                    cmp::Reverse(entry.func().scope_priority()),
                    reference_signature_score(entry.func().type_(), self.symbols),
                )
            });
        let has_match = primary.is_some();
        let selected = if let Some((index, primary)) = primary {
            let other_matches = candidates
                .enumerate()
                .filter(|(other_index, other)| {
                    *other_index != index
                        && other.func().scope_priority() == primary.func().scope_priority()
                        && applicable(other)
                        && !reference_signature_dominates(
                            primary.func().type_(),
                            other.func().type_(),
                            args,
                            arg_types,
                            self.symbols,
                        )
                })
                .map(|(_, entry)| entry)
                .collect::<Vec<_>>();

            if !other_matches.is_empty() {
                let signatures = iter::once(primary)
                    .chain(other_matches)
                    .map(|entry| {
                        let typ = entry.func().type_();
                        FunctionSignature::new(name, typ.params(), typ.return_type().clone())
                    })
                    .collect();
                return Err(Error::MultipleMatchingOverloads(signatures, span));
            }

            primary
        } else if let Some(fallback) = fallback.filter(|entry| {
            let typ = entry.func().type_();
            typ.conversion().is_none()
                && typ.is_valid()
                && (typ.min_args()..=typ.max_args()).contains(&args.len())
        }) {
            fallback
        } else {
            let mut arg_ranges = Vec::new();
            for entry in candidates {
                let typ = entry.func().type_();
                if typ.conversion().is_some() {
                    continue;
                }
                if !typ.is_valid() {
                    return Err(Error::InvalidFunctionSignature(name, span));
                }
                arg_ranges.push(typ.min_args()..=typ.max_args());
            }
            return Err(
                if !arg_ranges.is_empty()
                    && arg_ranges.iter().all(|range| !range.contains(&args.len()))
                {
                    Error::InvalidArgCount(arg_ranges, args.len(), span)
                } else {
                    Error::UnresolvedFunction(name, span)
                },
            );
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
        for (index, (arg, param)) in args
            .iter()
            .zip(selected.func().type_().params())
            .enumerate()
        {
            if param.flags().is_short_circuit()
                && !matches!(arg, ir::Expr::Nop(_))
                && source_args.get(index).is_none()
                && (index != 1 || ![ast::BinOp::And.name(), ast::BinOp::Or.name()].contains(&name))
            {
                return Err(Error::UnsupportedSkipArgument(arg.span()));
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
                has_match,
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
}

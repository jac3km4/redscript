use super::*;

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    pub(super) fn lower_closure(
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
}

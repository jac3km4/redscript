use super::*;

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    pub(super) fn lower_block(
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
                            typ: expr_t.into(),
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
        let (mut expr, expr_t) = self.lower_expr_with(expr, Some(scrutinee_t), env)?;
        let span = expr.span();
        // Variant comparison and script-reference scrutinee dereferencing need
        // switch-level semantics, not boxing or taking references to case values.
        if scrutinee_t
            .upper_bound(self.symbols)
            .is_some_and(|app| app.id() == predef::VARIANT)
            || matches!(
                scrutinee_t.strip_ref(self.symbols),
                Some((RefType::Script, _))
            )
        {
            expr_t
                .constrain_base(scrutinee_t, self.symbols)
                .with_span(span)?;
        } else {
            self.coerce(&mut expr, expr_t, scrutinee_t.clone(), env, span)?;
        }
        let block = self.lower_scoped_block(&case.body, env);
        Ok(ir::Case::new(expr, block))
    }

    fn lower_stmt(
        &mut self,
        stmt: &ast::SourceStmt<'ctx>,
        env: &mut Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Stmt<'ctx>> {
        let res = match stmt {
            ast::Stmt::Let {
                is_const,
                name,
                typ,
                value,
            } => {
                let is_const = *is_const;
                let &(name, name_span) = name;
                if let Some(local) = self.hoisted_declarations.get(&name_span).cloned() {
                    let Some(local) = local else {
                        // The hoisting pass already reported this declaration's error.
                        return Ok(ir::Stmt::Block(ir::Block::new([]), span));
                    };
                    let mut declaration = local.declaration.expect("hoisted declaration exists");
                    declaration.scope = env.scope();
                    declaration.span = span;
                    if value.is_some() {
                        declaration.initialization = ir::Initialization {
                            kind: ir::InitializationKind::AuthoredValue,
                            site: span,
                        };
                    }
                    self.locals.set_declaration(local.id, declaration);
                    return if let Some(value @ (_, value_span)) = value.as_deref() {
                        let (mut value, value_t) =
                            self.lower_expr_with(value, Some(&local.typ), env)?;
                        self.associate_array_destination(&value, local.id);
                        self.coerce(&mut value, value_t, local.typ.clone(), env, *value_span)?;
                        self.locals.set_initializer(local.id, span);
                        Ok(ir::Expr::Assign {
                            place: ir::Expr::Local(local.id, name_span).into(),
                            expr: value.into(),
                            typ: local.typ.into(),
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
                if let Some((ast::Expr::ArrayLit(elems), value_span)) = value.as_deref()
                    // A lexical slot reused by a loop may have escaped through
                    // script_ref on an earlier iteration. Keep temporary/copy
                    // construction there until non-escape is proven.
                    && !self.in_loop
                    && !env.types().policy(span).hoist_locals
                    && !env.types().policy(span).native_object_cast
                    && !env.types().policy(span).script_ref_write_through
                    && typ.as_ref().is_none_or(|typ| {
                        typ.upper_bound(self.symbols)
                            .is_some_and(|typ| typ.id() == predef::ARRAY)
                    })
                {
                    // Lower before introducing the binding: a shadowed name in the
                    // initializer must still resolve to the outer local.
                    let (elements, element_type) =
                        self.lower_array_elements(elems, typ.as_ref(), env, *value_span)?;
                    let array_type = Type::app(predef::ARRAY, [element_type.clone()]).into_poly();
                    let typ = typ.unwrap_or_else(|| array_type.clone());
                    let local = self
                        .locals
                        .add_const_var(Some(name), typ.clone(), name_span, is_const)
                        .clone();
                    let mut destination = ir::Expr::Local(local.id, *value_span);
                    self.coerce(&mut destination, array_type, typ.clone(), env, *value_span)?;
                    let local = env.define_local(name, local);
                    self.locals.set_initializer(local, span);
                    self.record_declaration(
                        local,
                        env,
                        span,
                        ir::InitializationKind::AuthoredValue,
                        span,
                    );
                    return Ok(if elements.is_empty() {
                        // InitArray([]) is a backend no-op; retain an explicit
                        // initialization operation for the authored empty literal.
                        ir::Stmt::InitDefault {
                            local,
                            source_binding: Some(ir::Expr::Local(local, name_span).into()),
                            typ: typ.into(),
                            span,
                        }
                    } else {
                        ir::Stmt::InitArray {
                            local,
                            source_binding: Some(ir::Expr::Local(local, name_span).into()),
                            elements: elements.into(),
                            element_type: element_type.into(),
                            span,
                        }
                    });
                }
                if let Some(value @ (_, value_span)) = value.as_deref() {
                    let (mut value, value_t) = self.lower_expr_with(value, typ.as_ref(), env)?;
                    let array_temp = match &value {
                        ir::Expr::Local(local, _) => Some(*local),
                        _ => None,
                    };
                    let typ = if let Some(typ) = typ {
                        self.coerce(&mut value, value_t, typ.clone(), env, *value_span)?;
                        typ
                    } else {
                        value_t
                    };
                    let local =
                        self.locals
                            .add_const_var(Some(name), typ.clone(), name_span, is_const);
                    let local = env.define_local(name, local.clone());
                    self.locals.set_initializer(local, span);
                    self.record_declaration(
                        local,
                        env,
                        span,
                        ir::InitializationKind::AuthoredValue,
                        span,
                    );
                    if let Some(temp) = array_temp {
                        self.locals.associate_temp(
                            temp,
                            ir::TemporaryPurpose::ArrayLiteral,
                            ir::TemporaryOwner::Destination(local),
                        );
                    }

                    ir::Expr::Assign {
                        place: ir::Expr::Local(local, name_span).into(),
                        expr: value.into(),
                        typ: typ.into(),
                        span,
                    }
                    .into()
                } else {
                    let typ = typ.unwrap_or_else(PolyType::fresh);
                    let local =
                        self.locals
                            .add_const_var(Some(name), typ.clone(), name_span, is_const);
                    let local = env.define_local(name, local.clone());
                    self.record_declaration(
                        local,
                        env,
                        span,
                        ir::InitializationKind::Default,
                        span,
                    );
                    ir::Stmt::InitDefault {
                        local,
                        source_binding: None,
                        typ: typ.into(),
                        span,
                    }
                }
            }
            ast::Stmt::Switch {
                expr,
                cases,
                default_index,
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
                    if default.as_ref().is_some_and(|block| {
                        !matches!(
                            block.stmts.back(),
                            Some(
                                ir::Stmt::Break(_) | ir::Stmt::Continue(_) | ir::Stmt::Return(_, _)
                            )
                        )
                    }) && let Some(case) = cases.get(*default_index)
                        && matches!(case.condition, ast::Condition::Pattern(_))
                    {
                        self.reporter
                            .report(Error::FallthroughIntoPatternCase(case.condition.span()));
                    }
                    let branches = cases
                        .iter()
                        .enumerate()
                        .map(|(index, case)| {
                            let branch = self.lower_pattern_case(case, local, &expr_t, env)?;
                            if matches!(case.condition, ast::Condition::Expr(_))
                                && (default.is_none() || index + 1 != *default_index)
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
                    default_index: *default_index,
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
                let was_in_loop = mem::replace(&mut self.in_loop, true);
                let result = self.lower_conditional_block(block, env);
                self.in_loop = was_in_loop;
                let (block, prefix) = result?;
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
                let was_in_loop = mem::replace(&mut self.in_loop, true);
                let result = self.lower_for_in(name, iter, body, env, span);
                self.in_loop = was_in_loop;
                let block = result?;
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
            self.locals
                .add_var(Some(name), elem_t.clone(), name_span)
                .clone(),
        );

        self.record_declaration(
            elem,
            &env,
            span,
            ir::InitializationKind::Iteration,
            name_span.merge(&iter_span),
        );

        for (temp, purpose) in [
            (counter, ir::TemporaryPurpose::IterationIndex),
            (array_local, ir::TemporaryPurpose::IterationIterable),
        ] {
            self.locals
                .associate_temp(temp, purpose, ir::TemporaryOwner::IterationBinding(elem));
        }

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
                    typ: elem_t.into(),
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
                typ: counter_t.into(),
                span: init_span,
            }
            .into(),
            ir::Stmt::While(
                ir::ConditionalBlock::new(ir::Expr::call(check, init_span), loop_body),
                span,
            ),
        ]))
    }
}

use super::*;

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
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

                let binding_t = match ref_type {
                    Some(ref_type) => {
                        let wrapper = match ref_type {
                            RefType::Strong => predef::REF,
                            RefType::Weak => predef::WREF,
                            RefType::Script => predef::SCRIPT_REF,
                        };
                        Type::app(wrapper, [as_type.clone()]).into_poly()
                    }
                    None => as_type.clone(),
                };
                let Pattern {
                    mut conditions,
                    prologue,
                } = self.lower_pattern(pattern, projection, &binding_t, env)?;

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

    pub(super) fn lower_pattern_case(
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

    pub(super) fn lower_pattern_into_prologue_and_condition(
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

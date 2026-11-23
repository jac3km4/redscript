use super::*;

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub(super) fn process_free_function<'a>(
        &mut self,
        ParsedFunction {
            index,
            file,
            path,
            meta,
            function,
            annotation,
        }: ParsedFunction<'ctx>,
        types: &TypeEnv<'a, 'ctx>,
        declarations: &mut Vec<(ast::FileId, DeclarationPath, ResolvedDeclaration<'ctx>)>,
    ) -> Option<FuncItemKind<'a, 'ctx>> {
        let (name, name_span) = function.name;
        let (func_t, type_scope) = self.create_function_env(&function, types);
        let param_names = function.param_names().collect::<Box<[_]>>();
        let span = meta.span;

        let mut intrinsic: Option<ir::Intrinsic> = None;
        let mut replaced: Option<MethodId<'ctx>> = None;
        let mut wrapped: Option<MethodId<'ctx>> = None;

        match &annotation {
            &Some(FunctionAnnotation::Intrinsic(i)) => {
                intrinsic = Some(i);
            }
            Some(ann @ FunctionAnnotation::Replace(class)) => {
                replaced = Some(self.resolve_existing_annotated_method(
                    function.name,
                    *class,
                    &meta.qualifiers,
                    &func_t,
                    ann,
                    types,
                )?);
            }
            &Some(FunctionAnnotation::Add((cls_name, cls_span))) => {
                let cls_id = self.resolve_annotated_type(cls_name, types, cls_span)?;

                if !func_t.type_params().is_empty() {
                    self.reporter
                        .report(Diagnostic::GenericMethodAnnotation(name_span));
                }

                let base = self
                    .symbols
                    .query_methods_by_name(cls_id, name)
                    .find(|entry| {
                        func_t.is_valid()
                            && entry.func().type_().is_valid()
                            && entry.func().type_().param_types().eq(func_t.param_types())
                            && entry.func().type_().return_type() == func_t.return_type()
                    });
                let base = base.as_ref().map(FunctionEntry::key).copied();

                if base.is_some_and(|base| base.parent() == cls_id) {
                    self.reporter
                        .report(Diagnostic::DuplicateMethodAnnotation(name_span));
                    return None;
                }

                let TypeSchema::Aggregate(agg) = self.symbols[cls_id].schema() else {
                    self.reporter
                        .report(Diagnostic::InvalidAnnotationType(cls_name, cls_span));
                    return None;
                };

                let parent_flags = agg.flags();
                if agg.is_user_defined() {
                    self.reporter
                        .report(Diagnostic::UserSymbolAnnotation(name_span));
                }
                let qs = meta.qualifiers;
                let visibility = meta
                    .visibility
                    .map_or(Visibility::Private, convert_visibility);
                let flags = self
                    .process_method_flags(qs, &function, parent_flags, name_span)
                    .with_visibility(visibility)
                    .with_has_implicit_visibility(meta.visibility.is_none());

                if function.body.is_none() && !flags.is_native() {
                    self.reporter
                        .report(Diagnostic::MissingFunctionBody(name_span));
                    return None;
                };

                let forwarder =
                    extract_static_forwarder(&func_t, cls_id, flags, &meta.doc, name_span);
                let member = Method::new(flags, func_t, base, meta.doc, Some(name_span));
                let methods = self.symbols[cls_id]
                    .schema_mut()
                    .as_aggregate_mut()
                    .unwrap()
                    .methods_mut();
                let func_idx = methods.add(name, member);
                let method_id = MethodId::new(cls_id, func_idx);
                if let Some(forwarder) = forwarder {
                    methods.add(name, forwarder.with_forwarded_static_method(method_id));
                }

                let item = FuncItem::new(
                    method_id,
                    span,
                    name_span,
                    param_names,
                    function.body,
                    type_scope,
                )
                .with_declaration(file, path);
                return Some(FuncItemKind::AddMethod(item));
            }
            Some(ann @ FunctionAnnotation::Wrap(class)) => {
                wrapped = Some(self.resolve_existing_annotated_method(
                    function.name,
                    *class,
                    &meta.qualifiers,
                    &func_t,
                    ann,
                    types,
                )?);
            }
            _ => {}
        }

        let mut qs = meta.qualifiers;
        let (free_func, body) = match (function.body, intrinsic, replaced, wrapped) {
            (None, Some(intrinsic), None, None) => {
                let intrinsic =
                    FreeFunction::new_intrinsic(func_t, intrinsic, meta.doc, Some(name_span));
                (intrinsic, None)
            }
            (body, None, None, None)
                if (body.is_some() && !qs.contains(ast::ItemQualifiers::NATIVE))
                    || (body.is_none() && qs.contains(ast::ItemQualifiers::NATIVE)) =>
            {
                let is_cast = name == ident::CAST && func_t.params().len() == 1;
                let is_operator = BinOp::from_name(name).is_some()
                    || UnOp::from_name(name).is_some()
                    || name == "OperatorArray";
                let flags = FreeFunctionFlags::default()
                    .with_is_exec(qs.take_flag(ast::ItemQualifiers::EXEC))
                    .with_is_native(qs.take_flag(ast::ItemQualifiers::NATIVE))
                    .with_is_cast(is_cast)
                    .with_is_operator(is_operator);
                let func = FreeFunction::new(flags, func_t, meta.doc, Some(name_span));
                (func, body)
            }
            (Some(body), None, Some(replaced), None) => {
                let func = FuncItem::new(replaced, span, name_span, param_names, body, type_scope)
                    .with_properties(func_t.properties().clone())
                    .with_declaration(file, path);
                return Some(FuncItemKind::ReplaceMethod(func));
            }
            (Some(body), None, None, Some(wrapped)) => {
                let func = FuncItem::new(wrapped, span, name_span, param_names, body, type_scope)
                    .with_properties(func_t.properties().clone())
                    .with_declaration(file, path);
                return Some(FuncItemKind::WrapMethod(func));
            }
            (Some(_), _, _, _) => {
                self.reporter
                    .report(Diagnostic::UnexpectedFunctionBody(name_span));
                return None;
            }
            (None, _, _, _) => {
                self.reporter
                    .report(Diagnostic::MissingFunctionBody(name_span));
                return None;
            }
        };

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        self.symbols.set_free_function(index, free_func);
        if let Some(body) = body {
            Some(FuncItemKind::FreeFunction(
                FuncItem::new(index, span, name_span, param_names, body, type_scope)
                    .with_declaration(file, path),
            ))
        } else {
            // Bodyless free functions (`native func`, `@intrinsic`) produce no
            // lowered item, so record their binding here directly.
            declarations.push((file, path, ResolvedDeclaration::Function(index)));
            None
        }
    }

    pub(super) fn process_free_field<'a>(
        &mut self,
        ParsedLet {
            file,
            path,
            meta,
            let_: field,
        }: ParsedLet<'ctx>,
        types: &TypeEnv<'a, 'ctx>,
        declarations: &mut Vec<(ast::FileId, DeclarationPath, ResolvedDeclaration<'ctx>)>,
    ) -> Option<FieldItem<'ctx, FieldId<'ctx>>> {
        let mut doc = meta.doc;
        let (name, name_span) = field.name;
        let mut id = None;

        let mut annotations = meta.annotations;
        let mut properties = extract_field_properties(&mut annotations);
        let mut hint = extract_field_hint(&mut annotations);

        for (ann, ann_span) in &annotations {
            match (ann.name, &ann.args[..]) {
                (annotation::ADD_FIELD, &[(ast::Expr::Ident(type_name), span)]) => {
                    let (parent_t, agg) =
                        self.resolve_annotated_aggregate(type_name, types, span)?;
                    let flags = agg.flags();

                    if agg.is_user_defined() {
                        self.reporter
                            .report(Diagnostic::UserSymbolAnnotation(name_span));
                    }

                    if flags.is_fully_defined() {
                        self.reporter
                            .report(Diagnostic::FullyDefinedNativeTypeFieldAddition(name_span));
                    } else if flags.is_struct() && !flags.is_native() {
                        self.reporter
                            .report(Diagnostic::ScriptedStructFieldAddition(name_span));
                    }

                    let (typ, span) = field.typ.as_ref();
                    let typ = self
                        .reporter
                        .unwrap_err(types.resolve(typ, &self.symbols, *span))?;
                    let visibility = meta
                        .visibility
                        .map_or(Visibility::Private, convert_visibility);
                    let flags = self
                        .process_field_flags(meta.qualifiers, flags, &typ, name_span)
                        .with_visibility(visibility);

                    let field = Field::new(
                        flags,
                        typ,
                        mem::take(&mut properties),
                        mem::take(&mut doc),
                        Some(name_span),
                    )
                    .with_hint(hint.take());

                    if let Ok(idx) = self.symbols[parent_t]
                        .schema_mut()
                        .as_aggregate_mut()
                        .unwrap()
                        .fields_mut()
                        .add(name, field)
                    {
                        id = Some(FieldId::new(parent_t, idx));
                    } else {
                        self.reporter
                            .report(Diagnostic::AddFieldConflict(name_span));
                        return None;
                    }
                }
                _ => {
                    self.reporter
                        .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                }
            }
        }

        let Some(id) = id else {
            self.reporter.report(Diagnostic::UnexpectedItem(
                "fields cannot be declared at module scope",
                name_span,
            ));
            return None;
        };
        // Record even valueless added fields; the transpiler still emits them.
        declarations.push((file, path, ResolvedDeclaration::Field(id)));

        Some(FieldItem::new(id, field.default))
    }

    pub(super) fn create_function_env<'a>(
        &mut self,
        func: &ast::SourceFunction<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> (FunctionType<'ctx>, TypeScope<'a, 'ctx>) {
        let mut invalid = false;
        for param in &func.type_params {
            if param.variance != ast::Variance::Invariant {
                invalid = true;
                let (_, span) = param.name;
                self.reporter.report(Diagnostic::NonDataVariance(span));
            }
        }

        let (types, vars) = self.create_scope_env(types, &func.type_params);

        invalid |= vars.len() != func.type_params.len();
        let params = func
            .params
            .iter()
            .map(|(param, span)| {
                let (typ, type_span) = param
                    .typ
                    .as_ref()
                    .expect("parameter type should always be present");
                let typ = self
                    .reporter
                    .unwrap_err(types.resolve(typ, &self.symbols, *type_span))
                    .unwrap_or_else(|| {
                        invalid = true;
                        Type::Nothing
                    });
                let qs = param.qualifiers;
                let flags = ParamFlags::default()
                    .with_is_short_circuit(qs.contains(ast::ParamQualifiers::SKIP))
                    .with_is_optional(qs.contains(ast::ParamQualifiers::OPTIONAL))
                    .with_is_out(qs.contains(ast::ParamQualifiers::OUT))
                    .with_is_const(qs.contains(ast::ParamQualifiers::CONST));
                Param::new(param.name, flags, typ, Some(*span))
            })
            .collect::<Vec<_>>();

        let return_t = match func.return_type.as_deref() {
            None => Type::nullary(predef::VOID),
            Some((ty, span)) => self
                .reporter
                .unwrap_err(types.resolve(ty, &self.symbols, *span))
                .unwrap_or_else(|| {
                    invalid = true;
                    Type::Nothing
                }),
        };

        let func_t = FunctionType::new(vars, params, return_t)
            .with_invalid_signature(invalid)
            .with_properties(func.properties.clone());

        let type_scope = types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();

        (func_t, type_scope)
    }

    pub(super) fn create_scope_env<'a>(
        &mut self,
        types: &'a TypeEnv<'a, 'ctx>,
        params: &'a [ast::SourceTypeParam<'ctx>],
    ) -> (TypeEnv<'a, 'ctx>, Box<[Shared<TypeParamVar<'ctx>>]>) {
        let mut types = types.introduce_scope();

        for param in params {
            let (name, name_span) = param.name;
            let typ = TypeRef::LazyVar(Shared::new(Lazy::new(Box::new(|(env, symbols)| {
                env.resolve_param(param, symbols).map(Shared::new)
            }))));
            if types.get(name).is_some() {
                self.reporter
                    .report(Diagnostic::NameRedefinition(name, name_span));
            }
            types.add(name, typ);
        }

        let vars = types
            .top()
            .iter()
            .zip(params)
            .filter_map(|((_, typ), param)| {
                let (_, span) = param.name;
                let TypeRef::LazyVar(lazy) = typ else {
                    unreachable!()
                };
                let res = lazy
                    .get((&types, &self.symbols))
                    .map_err(|_| LowerError::CyclicType(span))
                    .and_then(|res| res);
                self.reporter.unwrap_err(res)
            })
            .collect();

        (types, vars)
    }

    fn resolve_annotated_aggregate(
        &mut self,
        cls_name: &'ctx str,
        types: &TypeEnv<'_, 'ctx>,
        cls_span: Span,
    ) -> Option<(TypeId<'ctx>, &Aggregate<'ctx>)> {
        let cls_id = self.resolve_annotated_type(cls_name, types, cls_span)?;
        let TypeSchema::Aggregate(agg) = self.symbols[cls_id].schema() else {
            self.reporter
                .report(Diagnostic::InvalidAnnotationType(cls_name, cls_span));
            return None;
        };
        Some((cls_id, agg))
    }

    fn resolve_annotated_type(
        &mut self,
        cls_name: &'ctx str,
        types: &TypeEnv<'_, 'ctx>,
        cls_span: Span,
    ) -> Option<TypeId<'ctx>> {
        let target = types
            .get(cls_name)
            .ok_or(LowerError::UnresolvedType(cls_name, cls_span));
        let typ = self.reporter.unwrap_err(target)?;
        let &TypeRef::Id(id) = typ else {
            self.reporter
                .report(Diagnostic::InvalidAnnotationType(cls_name, cls_span));
            return None;
        };
        Some(id)
    }

    fn resolve_existing_annotated_method(
        &mut self,
        (func_name, func_span): ast::Spanned<&'ctx str>,
        (cls_name, cls_span): ast::Spanned<&'ctx str>,
        qualifiers: &ast::ItemQualifiers,
        func_type: &FunctionType<'ctx>,
        annotation: &FunctionAnnotation<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> Option<MethodId<'ctx>> {
        if !func_type.is_valid() {
            return None;
        }
        let (cls_id, aggregate) = self.resolve_annotated_aggregate(cls_name, types, cls_span)?;
        let res = match Self::resolve_annotated_method(func_name, cls_id, aggregate, func_type) {
            Some(res) => Some(res),
            // Retry in case the user marked a cb method that returns Bool as Void.
            // This enables backward compatibility for some mods.
            None if qualifiers.contains(ast::ItemQualifiers::CALLBACK)
                && func_type.return_type() == &Type::nullary(predef::VOID) =>
            {
                let fallback = func_type
                    .clone()
                    .with_return_type(Type::nullary(predef::BOOL));
                Self::resolve_annotated_method(func_name, cls_id, aggregate, &fallback)
            }
            _ => None,
        };

        let Some((id, method)) = res else {
            self.reporter.report(Diagnostic::AnnotatedMethodNotFound(
                annotation.clone(),
                func_span,
            ));
            return None;
        };
        if method.is_user_defined() {
            self.reporter
                .report(Diagnostic::UserSymbolAnnotation(cls_span));
            return None;
        }
        Some(id)
    }

    fn resolve_annotated_method<'a>(
        func_name: &str,
        id: TypeId<'ctx>,
        agg: &'a Aggregate<'ctx>,
        func_type: &FunctionType<'ctx>,
    ) -> Option<(MethodId<'ctx>, &'a Method<'ctx>)> {
        let entry = agg.methods().by_name(func_name).find(|entry| {
            entry.func().type_().is_valid()
                && entry
                    .func()
                    .type_()
                    .param_types()
                    .eq(func_type.param_types())
                && entry.func().type_().return_type() == func_type.return_type()
        })?;
        Some((MethodId::new(id, *entry.key()), *entry.func()))
    }
}

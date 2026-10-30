use std::borrow::Cow;
use std::collections::BTreeSet;
use std::ops::{BitAndAssign, Not};
use std::rc::Rc;
use std::{fmt, mem};

use aggregate::extract_static_forwarder;
use constants::{annotation, ident};
use hashbrown::HashMap;
use redscript_ast::{self as ast, BinOp, Span, Spanned, UnOp};

use super::TypeInference;
use super::infer::{FieldItem, FuncItem, FuncItemKind, InferStageModule};
use crate::cte::{self, Evaluator};
use crate::lower::TypeEnv;
use crate::modules::{Export, ImportError, ModuleMap};
use crate::symbols::{FreeFunctionIndexes, FunctionEntry, Visibility};
use crate::utils::{Lazy, ScopedMap};
use crate::{
    Aggregate, AggregateFlags, CompileErrorReporter, CtxVar, Diagnostic, Enum, Field, FieldId,
    FreeFunction, FreeFunctionFlags, FreeFunctionIndex, FunctionType, IndexMap, LowerError, Method,
    MethodId, Param, ParamFlags, QualifiedName, Symbols, Type, TypeDef, TypeId, TypeInterner,
    TypeRef, TypeSchema, TypeScope, ir, predef,
};

mod aggregate;
pub(super) mod constants;
mod derive_new;
mod validation;

#[derive(Debug)]
pub struct NameResolution<'scope, 'ctx> {
    modules: HashMap<Option<ast::Path<'ctx>>, Vec<ResolutionStageModule<'ctx>>>,

    symbols: Symbols<'ctx>,
    module_map: ModuleMap<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &'scope mut CompileErrorReporter<'ctx>,
}

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub fn new(
        modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
        evaluator: Evaluator<'ctx>,
        symbols: Symbols<'ctx>,
        reporter: &'scope mut CompileErrorReporter<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> Self {
        let mut this = Self {
            modules: HashMap::new(),

            symbols,
            module_map: ModuleMap::default(),
            evaluator,
            reporter,
        };
        for module in modules {
            this.add_module(module, interner);
        }
        this
    }

    fn add_module(&mut self, module: ast::SourceModule<'ctx>, interner: &'ctx TypeInterner) {
        let span = module.span();
        let path_root = module
            .path
            .as_ref()
            .map(|p| &p.segments[..])
            .unwrap_or_default();

        let mut imports = vec![];
        let mut classes = vec![];
        let mut structs = vec![];
        let mut functions = vec![];
        let mut enums = vec![];
        let mut lets = vec![];

        for (
            ast::ItemDecl {
                mut annotations,
                visibility,
                qualifiers,
                mut item,
                doc,
            },
            item_span,
        ) in module.items.into_vec()
        {
            if !process_conditionals(&mut annotations, &self.evaluator, self.reporter) {
                continue;
            }

            let mut item_meta = ParsedMeta {
                annotations,
                visibility,
                qualifiers,
                doc,
                span: item_span,
            };
            let is_public = matches!(item_meta.visibility, Some(ast::Visibility::Public));
            match item {
                ast::Item::Import(import) => {
                    imports.push(ParsedImport {
                        import,
                        span: item_span,
                    });
                }
                ast::Item::Class(ref mut aggregate) | ast::Item::Struct(ref mut aggregate) => {
                    aggregate.items.retain_mut(|(item, _)| {
                        process_conditionals(&mut item.annotations, &self.evaluator, self.reporter)
                    });

                    let (name, name_span) = aggregate.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let cls_id = interner.intern(Cow::from(&path));
                    let res = self
                        .module_map
                        .add_type(path, cls_id, is_public)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    if self.reporter.unwrap_err(res).is_none() {
                        continue;
                    }

                    // we initialize and add a stub to enable the compiler to automatically
                    // resolve the ref type whenever the type is used
                    let mut flags = AggregateFlags::new();
                    item_meta
                        .annotations
                        .retain(|(ann, _)| match (ann.name, &ann.args[..]) {
                            (annotation::NEVER_REF, &[]) => {
                                flags.set_is_never_ref(true);
                                false
                            }
                            (annotation::MIXED_REF, &[]) => {
                                flags.set_is_mixed_ref(true);
                                false
                            }
                            _ => true,
                        });
                    let mut stub = Aggregate::default();
                    stub.set_flags(flags);
                    let schema = TypeSchema::Aggregate(stub.into());
                    self.symbols.add_type(cls_id, TypeDef::new([], schema, []));

                    match item {
                        ast::Item::Class(aggregate) => {
                            classes.push(ParsedAggregate {
                                id: cls_id,
                                meta: item_meta,
                                aggregate,
                            });
                        }
                        ast::Item::Struct(aggregate) => {
                            structs.push(ParsedAggregate {
                                id: cls_id,
                                meta: item_meta,
                                aggregate,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                ast::Item::Function(function) => {
                    let (name, name_span) = function.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let index = self
                        .symbols
                        .add_free_function(path.clone(), FreeFunction::default());

                    let mut annotation: Option<FunctionAnnotation<'_>> = None;

                    for (ann, ann_span) in &item_meta.annotations {
                        match (ann.name, &ann.args[..]) {
                            (
                                annotation::INTRINSIC
                                | annotation::WRAP_METHOD
                                | annotation::REPLACE_METHOD
                                | annotation::ADD_METHOD,
                                _,
                            ) if annotation.is_some() => {
                                self.reporter
                                    .report(Diagnostic::IncompatibleAnnotations(*ann_span));
                            }
                            (annotation::INTRINSIC, &[(ast::Expr::Ident(name), span)]) => {
                                let intrinsic = ir::Intrinsic::try_from(name).map_err(|name| {
                                    Diagnostic::UnknownIntrinsic(name.into(), span)
                                });
                                annotation = self
                                    .reporter
                                    .unwrap_err(intrinsic)
                                    .map(FunctionAnnotation::Intrinsic);
                            }
                            (annotation::REPLACE_METHOD, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Replace((name, span)));
                            }
                            (annotation::ADD_METHOD, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Add((name, span)));
                            }
                            (annotation::WRAP_METHOD, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Wrap((name, span)));
                            }
                            _ => {
                                self.reporter
                                    .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                            }
                        }
                    }

                    if matches!(annotation, None | Some(FunctionAnnotation::Intrinsic(_))) {
                        let res = self
                            .module_map
                            .add_function(path, index, is_public)
                            .map_err(|_| Diagnostic::NameRedefinition(name_span));
                        self.reporter.unwrap_err(res);
                    }

                    functions.push(ParsedFunction {
                        index,
                        meta: item_meta,
                        function,
                        annotation,
                    });
                }
                ast::Item::Enum(enum_) => {
                    let (name, name_span) = enum_.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let id = interner.intern(Cow::from(&path));
                    let res = self
                        .module_map
                        .add_type(path, id, is_public)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    if self.reporter.unwrap_err(res).is_none() {
                        continue;
                    }

                    enums.push(ParsedEnum {
                        id,
                        meta: item_meta,
                        enum_,
                    });
                }
                ast::Item::Let(let_) => {
                    lets.push(ParsedLet {
                        meta: item_meta,
                        let_,
                    });
                }
            }
        }

        self.modules
            .entry(module.path)
            .or_default()
            .push(ResolutionStageModule {
                imports,
                classes,
                structs,
                functions,
                enums,
                lets,
                span,
            });
    }

    pub fn populate_globals(&mut self, scope: &mut Scope<'_, 'ctx>) {
        let funcs = scope.funcs.top_mut();
        for (name, export) in self
            .module_map
            .exports(&[])
            .expect("root should always exist")
        {
            match export {
                Export::FreeFunction { overloads, .. } => {
                    funcs
                        .entry(name)
                        .or_default()
                        .extend(overloads.iter().copied());
                }
                &Export::Type { id, .. } => {
                    scope.types.add(name, TypeRef::Id(id));
                }
            }
        }
    }

    pub fn progress<'a>(mut self, scope: &'a Scope<'_, 'ctx>) -> TypeInference<'a, 'ctx> {
        let mut results = vec![];
        for (path, modules) in mem::take(&mut self.modules) {
            let mut type_scope = scope.types.introduce_scope();
            let mut func_scope = scope.funcs.introduce_scope();

            for module in &modules {
                if path.is_some() {
                    for ((name, _), id) in module
                        .classes
                        .iter()
                        .chain(&module.structs)
                        .map(|p| (p.aggregate.name, p.id))
                        .chain(module.enums.iter().map(|p| (p.enum_.name, p.id)))
                    {
                        type_scope.add(name, TypeRef::Id(id));
                    }

                    for entry in &module.functions {
                        let (name, _) = entry.function.name;
                        func_scope
                            .top_mut()
                            .entry(name)
                            .or_default()
                            .push(entry.index);
                    }
                }
            }

            for module in modules {
                let mut type_scope = type_scope.clone();
                let mut func_scope = func_scope.clone();

                for entry in &module.imports {
                    self.module_map.visit_import(
                        &entry.import,
                        |name, typ| type_scope.add(name, TypeRef::Id(typ)),
                        |name, func| func_scope.top_mut().entry(name).or_default().push(func),
                        |error| {
                            let error = match error {
                                ImportError::NotFound(name) => {
                                    Diagnostic::ImportNotFound(name, entry.span)
                                }
                                ImportError::Private(name) => {
                                    Diagnostic::ImportIsPrivate(name, entry.span)
                                }
                            };
                            self.reporter.report(error);
                        },
                    );
                }

                let mut classes = vec![];
                for entry in module.classes {
                    classes.push(self.process_aggregate(entry, path.as_ref(), &type_scope, false));
                }
                for entry in module.structs {
                    classes.push(self.process_aggregate(entry, path.as_ref(), &type_scope, true));
                }

                let mut enums = vec![];
                for entry in module.enums {
                    enums.push(entry.id);
                    self.process_enum(entry);
                }

                let mut functions = vec![];
                for entry in module.functions {
                    if let Some(item) = self.process_free_function(entry, &type_scope) {
                        functions.push(item);
                    }
                }

                let mut fields = vec![];
                for let_ in module.lets {
                    if let Some(item) = self.process_free_field(let_, &type_scope) {
                        fields.push(item);
                    }
                }

                results.push(InferStageModule::new(
                    type_scope.pop_scope(),
                    func_scope.pop_scope(),
                    classes,
                    enums,
                    functions,
                    fields,
                    module.span,
                ));
            }
        }

        for module in &results {
            self.validate_module(module);
        }
        self.process_inheritance(results.iter().flat_map(InferStageModule::classes));

        TypeInference::new(results, self.symbols)
    }

    fn process_enum(&mut self, entry: ParsedEnum<'ctx>) {
        let (enum_name, name_span) = entry.enum_.name;

        let qs = entry.meta.qualifiers;
        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        let mut by_name = IndexMap::default();
        let mut by_val = BTreeSet::<i64>::new();

        for (variant, span) in &entry.enum_.variants {
            let val = if let Some(val) = variant.value {
                val
            } else if let Some(last) = by_val.last() {
                if let Some(next) = last.checked_add(1) {
                    next
                } else {
                    self.reporter.report(Diagnostic::ValueOverflow(*span));
                    continue;
                }
            } else {
                0
            };

            if by_name.insert(variant.name, val).is_some() {
                self.reporter.report(Diagnostic::DuplicateVariantName(
                    enum_name,
                    variant.name,
                    *span,
                ));
            }
            if !by_val.insert(val) {
                self.reporter.report(Diagnostic::DuplicateVariantValue(
                    enum_name,
                    variant.name,
                    val,
                    *span,
                ));
            }
        }

        let schema = TypeSchema::Enum(Enum::new(by_name, Some(name_span)).into());
        self.symbols
            .add_type(entry.id, TypeDef::new([], schema, entry.meta.doc));
    }

    fn process_free_function<'a>(
        &mut self,
        ParsedFunction {
            index,
            meta,
            function,
            annotation,
        }: ParsedFunction<'ctx>,
        types: &TypeEnv<'a, 'ctx>,
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
                        entry.func().type_().param_types().eq(func_t.param_types())
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
                    methods.add(name, forwarder.with_aliased(method_id));
                }

                let item = FuncItem::new(
                    method_id,
                    span,
                    name_span,
                    param_names,
                    function.body,
                    type_scope,
                );
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
                let is_operator =
                    BinOp::from_name(name).is_some() || UnOp::from_name(name).is_some();
                let flags = FreeFunctionFlags::default()
                    .with_is_exec(qs.take_flag(ast::ItemQualifiers::EXEC))
                    .with_is_native(qs.take_flag(ast::ItemQualifiers::NATIVE))
                    .with_is_cast(is_cast)
                    .with_is_operator(is_operator);
                let func = FreeFunction::new(flags, func_t, meta.doc, Some(name_span));
                (func, body)
            }
            (Some(body), None, Some(replaced), None) => {
                let func = FuncItem::new(replaced, span, name_span, param_names, body, type_scope);
                return Some(FuncItemKind::ReplaceMethod(func));
            }
            (Some(body), None, None, Some(wrapped)) => {
                let func = FuncItem::new(wrapped, span, name_span, param_names, body, type_scope);
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
        body.map(|body| {
            FuncItemKind::FreeFunction(FuncItem::new(
                index,
                span,
                name_span,
                param_names,
                body,
                type_scope,
            ))
        })
    }

    fn process_free_field<'a>(
        &mut self,
        ParsedLet { meta, let_: field }: ParsedLet<'ctx>,
        types: &TypeEnv<'a, 'ctx>,
    ) -> Option<FieldItem<'ctx, FieldId<'ctx>>> {
        let mut doc = meta.doc;
        let (name, name_span) = field.name;
        let mut id = None;

        let mut annotations = meta.annotations;
        let mut properties = extract_field_properties(&mut annotations);

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
                    );

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

        if id.is_none() {
            self.reporter.report(Diagnostic::UnexpectedItem(
                "fields cannot be declared at module scope",
                name_span,
            ));
        }

        Some(FieldItem::new(id?, field.default))
    }

    fn create_function_env<'a>(
        &mut self,
        func: &ast::SourceFunction<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> (FunctionType<'ctx>, TypeScope<'a, 'ctx>) {
        for param in &func.type_params {
            if param.variance != ast::Variance::Invariant {
                let (_, span) = param.name;
                self.reporter.report(Diagnostic::NonDataVariance(span));
            }
        }

        let (types, vars) = self.create_scope_env(types, &func.type_params);

        let params = func
            .params
            .iter()
            .filter_map(|(param, span)| {
                let (typ, type_span) = param
                    .typ
                    .as_ref()
                    .expect("parameter type should always be present");
                let typ =
                    self.reporter
                        .unwrap_err(types.resolve(typ, &self.symbols, *type_span))?;
                let qs = param.qualifiers;
                let flags = ParamFlags::default()
                    .with_is_optional(qs.contains(ast::ParamQualifiers::OPTIONAL))
                    .with_is_out(qs.contains(ast::ParamQualifiers::OUT))
                    .with_is_const(qs.contains(ast::ParamQualifiers::CONST));
                Some(Param::new(param.name, flags, typ, Some(*span)))
            })
            .collect::<Vec<_>>();

        let return_t = func
            .return_type
            .as_deref()
            .and_then(|(ty, span)| {
                self.reporter
                    .unwrap_err(types.resolve(ty, &self.symbols, *span))
            })
            .unwrap_or_else(|| Type::nullary(predef::VOID));

        let func_t = FunctionType::new(vars, params, return_t);

        let type_scope = types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();

        (func_t, type_scope)
    }

    fn create_scope_env<'a>(
        &mut self,
        types: &'a TypeEnv<'a, 'ctx>,
        params: &'a [ast::SourceTypeParam<'ctx>],
    ) -> (TypeEnv<'a, 'ctx>, Box<[Rc<CtxVar<'ctx>>]>) {
        let mut types = types.introduce_scope();

        for param in params {
            let (name, name_span) = param.name;
            let typ = TypeRef::LazyVar(Rc::new(Lazy::new(Box::new(|(env, symbols)| {
                env.resolve_param(param, symbols).map(Rc::new)
            }))));
            if types.get(name).is_some() {
                self.reporter
                    .report(Diagnostic::NameRedefinition(name_span));
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
            entry
                .func()
                .type_()
                .param_types()
                .eq(func_type.param_types())
                && entry.func().type_().return_type() == func_type.return_type()
        })?;
        Some((MethodId::new(id, *entry.key()), *entry.func()))
    }
}

fn extract_field_properties<'ctx>(
    anns: &mut Vec<ast::Spanned<ast::SourceAnnotation<'ctx>>>,
) -> Vec<(Cow<'ctx, str>, Cow<'ctx, str>)> {
    let mut results = vec![];
    anns.retain(|(ann, _)| match (ann.name, &*ann.args) {
        (
            annotation::RUNTIME_PROPERTY,
            [
                (ast::Expr::Constant(ast::Constant::String(key)), _),
                (ast::Expr::Constant(ast::Constant::String(val)), _),
            ],
        ) => {
            results.push((key.clone(), val.clone()));
            false
        }
        _ => true,
    });

    results
}

fn process_conditionals<'ctx>(
    annotations: &mut Vec<Spanned<ast::SourceAnnotation<'ctx>>>,
    evaluator: &Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> bool {
    let mut include = true;

    annotations.retain(|(ann, _)| match (ann.name, &*ann.args) {
        (annotation::IF, [arg]) => {
            let result = reporter.unwrap_err(evaluator.eval(arg));
            include = include && !matches!(result, Some(cte::Value::Bool(false)));
            false
        }
        _ => true,
    });

    include
}

fn convert_visibility(visibility: ast::Visibility) -> Visibility {
    match visibility {
        ast::Visibility::Public => Visibility::Public,
        ast::Visibility::Private => Visibility::Private,
        ast::Visibility::Protected => Visibility::Protected,
    }
}

#[derive(Debug)]
struct ResolutionStageModule<'ctx> {
    imports: Vec<ParsedImport<'ctx>>,
    classes: Vec<ParsedAggregate<'ctx>>,
    structs: Vec<ParsedAggregate<'ctx>>,
    functions: Vec<ParsedFunction<'ctx>>,
    enums: Vec<ParsedEnum<'ctx>>,
    lets: Vec<ParsedLet<'ctx>>,
    span: Option<Span>,
}

#[derive(Debug)]
struct ParsedMeta<'ctx> {
    annotations: Vec<ast::Spanned<ast::SourceAnnotation<'ctx>>>,
    visibility: Option<ast::Visibility>,
    qualifiers: ast::ItemQualifiers,
    doc: Box<[&'ctx str]>,
    span: Span,
}

#[derive(Debug)]
struct ParsedImport<'ctx> {
    import: ast::Import<'ctx>,
    span: Span,
}

#[derive(Debug)]
struct ParsedAggregate<'ctx> {
    id: TypeId<'ctx>,
    meta: ParsedMeta<'ctx>,
    aggregate: ast::SourceAggregate<'ctx>,
}

#[derive(Debug)]
struct ParsedFunction<'ctx> {
    index: FreeFunctionIndex,
    meta: ParsedMeta<'ctx>,
    function: ast::SourceFunction<'ctx>,
    annotation: Option<FunctionAnnotation<'ctx>>,
}

#[derive(Debug)]
struct ParsedLet<'ctx> {
    meta: ParsedMeta<'ctx>,
    let_: ast::SourceField<'ctx>,
}

#[derive(Debug)]
struct ParsedEnum<'ctx> {
    id: TypeId<'ctx>,
    meta: ParsedMeta<'ctx>,
    enum_: ast::SourceEnum<'ctx>,
}

#[derive(Debug, Clone)]
pub enum FunctionAnnotation<'ctx> {
    Intrinsic(ir::Intrinsic),
    Replace(Spanned<&'ctx str>),
    Add(Spanned<&'ctx str>),
    Wrap(Spanned<&'ctx str>),
}

impl fmt::Display for FunctionAnnotation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use annotation::*;
        match self {
            FunctionAnnotation::Intrinsic(i) => {
                write!(f, "@{INTRINSIC}({})", <&str>::from(*i))
            }
            FunctionAnnotation::Replace((name, _)) => {
                write!(f, "@{REPLACE_METHOD}({name})")
            }
            FunctionAnnotation::Add((name, _)) => write!(f, "@{ADD_METHOD}({name})"),
            FunctionAnnotation::Wrap((name, _)) => write!(f, "@{WRAP_METHOD}({name})"),
        }
    }
}

#[derive(Debug)]
pub struct Scope<'scope, 'ctx> {
    pub(super) types: TypeEnv<'scope, 'ctx>,
    pub(super) funcs: ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
}

impl<'scope, 'ctx> Scope<'scope, 'ctx> {
    pub fn new(symbols: &Symbols<'ctx>) -> Self {
        let mut types = TypeEnv::with_default_types();
        for (id, _) in symbols.types() {
            types.add(id.as_str(), TypeRef::Id(id));
        }

        let mut funcs = IndexMap::<&str, FreeFunctionIndexes>::default();
        for func in symbols.free_functions() {
            let key = func
                .name()
                .as_single_component()
                .expect("functions should not be scoped in base cache");
            funcs.entry(key).or_default().push(*func.key());
        }

        Self {
            types,
            funcs: funcs.into(),
        }
    }

    pub fn push(
        &self,
        types: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
        funcs: IndexMap<&'ctx str, FreeFunctionIndexes>,
    ) -> Scope<'_, 'ctx> {
        let types = self.types.push_scope(types);
        let funcs = self.funcs.push_scope(funcs);
        Scope { types, funcs }
    }
}

trait BitStructOps {
    fn take_flag(&mut self, flag: Self) -> bool;
}

impl<A> BitStructOps for A
where
    A: Copy + PartialEq + Not<Output = A> + BitAndAssign,
{
    #[inline]
    fn take_flag(&mut self, flag: A) -> bool {
        let prev = *self;
        *self &= !flag;
        prev != *self
    }
}

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::ops::{BitAndAssign, Not};
use std::rc::Rc;
use std::{fmt, mem};

use constants::{annotation, ident};
use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;
use redscript_ast::{self as ast, BinOp, Span, Spanned, UnOp};

use super::TypeInference;
use super::infer::{ClassItem, FieldItem, FuncItem, FuncItemKind, InferStageModule};
use crate::cte::{self, Evaluator};
use crate::diagnostic::FunctionSignature;
use crate::lower::{InferredTypeApp, TypeEnv};
use crate::modules::{Export, ImportError, ModuleMap};
use crate::symbols::{AnyBaseType, FreeFunctionIndexes, FunctionEntry, Visibility};
use crate::utils::{Lazy, ScopedMap};
use crate::{
    Aggregate, AggregateFlags, CompileErrorReporter, CtxVar, Diagnostic, Enum, Field, FieldFlags,
    FieldId, FieldMap, FreeFunction, FreeFunctionFlags, FreeFunctionIndex, FunctionIndex,
    FunctionType, IndexMap, IndexSet, LowerError, Method, MethodFlags, MethodId, MethodMap, Param,
    ParamFlags, PolyType, QualifiedName, RefType, Symbols, Type, TypeApp, TypeDef, TypeId,
    TypeInterner, TypeRef, TypeSchema, TypeScope, Variance, ir, predef,
};

pub(super) mod constants;
mod derive_new;

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
                    self.reporter.unwrap_err(res);

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
                    self.reporter.unwrap_err(res);

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

        results
            .iter()
            .for_each(|module| self.validate_module(module));
        self.process_inheritance(results.iter().flat_map(InferStageModule::classes));

        TypeInference::new(results, self.symbols)
    }

    fn process_aggregate<'a>(
        &mut self,
        ParsedAggregate {
            id,
            meta,
            aggregate,
        }: ParsedAggregate<'ctx>,
        path: Option<&ast::Path<'ctx>>,
        types: &TypeEnv<'a, 'ctx>,
        is_struct: bool,
    ) -> ClassItem<'a, 'ctx> {
        let (aggregate_name, name_span) = aggregate.name;
        let cls_span = meta.span;
        let mut qs = meta.qualifiers;

        let is_import_only = qs.take_flag(ast::ItemQualifiers::IMPORT_ONLY);
        let is_native = is_import_only || qs.take_flag(ast::ItemQualifiers::NATIVE);
        let cls_flags = self
            .symbols
            .get_type(id)
            .and_then(|def| def.schema().as_aggregate())
            .map(Aggregate::flags)
            .unwrap_or_default()
            .with_is_import_only(is_import_only)
            .with_is_native(is_native)
            .with_is_final(qs.take_flag(ast::ItemQualifiers::FINAL))
            .with_is_abstract(qs.take_flag(ast::ItemQualifiers::ABSTRACT))
            .with_is_struct(is_struct);

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        let mut implementations = HashMap::default();
        let mut derive_new = None;
        let mut private_constructor_hint = None;

        for (mut ann, ann_span) in meta.annotations {
            match (ann.name, &mut ann.args[..]) {
                (
                    annotation::NAME_IMPLEMENTATION,
                    [(ast::Expr::DynCast { expr, typ }, expr_span)],
                ) => {
                    let (typ, type_span) = typ.as_ref();
                    let Some(typ) =
                        self.reporter
                            .unwrap_err(types.resolve(typ, &self.symbols, *type_span))
                    else {
                        continue;
                    };
                    let Type::Data(typ) = typ else {
                        self.reporter
                            .report(Diagnostic::InvalidImplType(*type_span));
                        continue;
                    };
                    let &(ast::Expr::Ident(name), name_span) = &**expr else {
                        self.reporter
                            .report(Diagnostic::InvalidImplName(*expr_span));
                        continue;
                    };
                    let base = path.map(AsRef::as_ref).unwrap_or_default();
                    let name = QualifiedName::from_base_and_name(base, name);
                    if self.module_map.exists(&name) {
                        self.reporter
                            .report(Diagnostic::NameRedefinition(name_span));
                        continue;
                    }

                    let Ok(typ) = typ.mono(&ScopedMap::default()) else {
                        self.reporter
                            .report(Diagnostic::InvalidImplType(*type_span));
                        continue;
                    };

                    if implementations.insert(typ, name).is_some() {
                        self.reporter.report(Diagnostic::DuplicateImpl(*type_span));
                    }
                }
                (annotation::DERIVE_NEW, []) => derive_new = Some(ann_span),
                (
                    annotation::PRIVATE_CONSTRUCTOR,
                    [(ast::Expr::Constant(ast::Constant::String(msg)), _)],
                ) => {
                    private_constructor_hint = Some(mem::take(msg));
                }
                _ => {
                    self.reporter
                        .report(Diagnostic::UnknownAnnotation(ann.name, ann_span));
                }
            }
        }

        let (types, vars) = self.create_scope_env(types, &aggregate.type_params);

        if cls_flags.is_struct() && vars.iter().any(|v| v.variance() != Variance::Invariant) {
            self.reporter
                .report(Diagnostic::InvalidStructVariance(name_span));
        }

        let mut fields = FieldMap::default();
        let mut methods = MethodMap::default();

        let mut method_items = vec![];
        let mut field_items = vec![];

        let default_visibility = if cls_flags.is_struct() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        for (mut item, item_span) in aggregate.items {
            let visibility = item
                .visibility
                .map_or(default_visibility, convert_visibility);

            match item.item {
                ast::Item::Function(func) => {
                    let (name, name_span) = func.name;
                    let qs = item.qualifiers;

                    let flags = self
                        .process_method_flags(qs, &func, cls_flags, name_span)
                        .with_visibility(visibility)
                        .with_has_implicit_visibility(item.visibility.is_none());

                    let (func_t, type_scope) = self.create_function_env(&func, &types);
                    if func.body.is_none()
                        && !flags.is_native()
                        && (!cls_flags.is_abstract()
                            || !func_t.type_params().is_empty()
                            || flags.is_final()
                            || flags.is_static())
                    {
                        self.reporter
                            .report(Diagnostic::MissingFunctionBody(name_span));
                    };

                    let forwarder =
                        extract_static_forwarder(&func_t, id, flags, &meta.doc, name_span);
                    let method = Method::new(flags, func_t, None, item.doc, Some(name_span));
                    let func_idx = methods.add(name, method);
                    let params = func.param_names().collect::<Box<[_]>>();
                    let Some(body) = func.body else {
                        continue;
                    };
                    method_items.push(FuncItem::new(
                        func_idx, item_span, name_span, params, body, type_scope,
                    ));

                    if let Some(forwader) = forwarder {
                        methods.add(name, forwader.with_aliased(MethodId::new(id, func_idx)));
                    }
                }
                ast::Item::Let(let_) => {
                    let (name, name_span) = let_.name;
                    let (typ, span) = let_.typ.as_ref();

                    let Some(typ) =
                        self.reporter
                            .unwrap_err(types.resolve(typ, &self.symbols, *span))
                    else {
                        continue;
                    };
                    let flags = self
                        .process_field_flags(item.qualifiers, cls_flags, &typ, name_span)
                        .with_visibility(visibility);

                    let properties = self.extract_field_properties(&mut item.annotations);
                    for (ann, ann_span) in &item.annotations {
                        self.reporter
                            .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                    }

                    let field = Field::new(flags, typ, properties, item.doc, Some(name_span));
                    let res = fields
                        .add(name, field)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    let field_idx = self.reporter.unwrap_err(res);

                    if let Some(field_idx) = field_idx
                        && let Some(default) = let_.default
                    {
                        field_items.push(FieldItem::new(field_idx, Some(default)));
                    }
                }
                _ => self.reporter.report(Diagnostic::UnexpectedItem(item_span)),
            }
        }

        let base = aggregate
            .extends
            .as_deref()
            .and_then(|(name, span)| {
                let base_t = self
                    .reporter
                    .unwrap_err(types.resolve(name, &self.symbols, *span))?;
                Some((base_t, *span))
            })
            .and_then(|(base_t, span)| {
                if let Type::Data(type_app) = base_t {
                    Some(type_app)
                } else {
                    self.reporter.report(Diagnostic::InvalidBaseType(span));
                    None
                }
            })
            .or_else(|| {
                is_struct
                    .not()
                    .then(|| TypeApp::nullary(predef::ISCRIPTABLE))
            });

        if let Some(derive_span) = derive_new {
            let method = derive_new::derive_new_method(id, &vars, fields.iter(), derive_span);
            let func_idx = methods.add(ident::NEW_METHOD, method);
            method_items.push(derive_new::derive_new_method_item(
                func_idx,
                aggregate_name,
                fields.iter(),
                derive_span,
            ));
        }

        let mut aggregate = Aggregate::new(
            cls_flags,
            base,
            fields,
            methods,
            implementations,
            Some(name_span),
        );
        if let Some(hint) = private_constructor_hint {
            aggregate.set_constructor_hint(hint);
        }

        let def = TypeDef::new(vars, TypeSchema::Aggregate(aggregate.into()), meta.doc);
        self.symbols.add_type(id, def);

        let type_scope = types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();
        ClassItem::new(
            id,
            cls_span,
            name_span,
            type_scope,
            method_items,
            field_items,
        )
    }

    fn process_enum(&mut self, entry: ParsedEnum<'ctx>) {
        let (_, name_span) = entry.enum_.name;

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
                self.reporter
                    .report(Diagnostic::DuplicateVariantName(*span));
            }
            if !by_val.insert(val) {
                self.reporter
                    .report(Diagnostic::DuplicateVariantValue(*span));
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
                if let Some(id) = self.resolve_existing_annotated_method(
                    function.name,
                    *class,
                    &meta.qualifiers,
                    &func_t,
                    ann,
                    types,
                ) {
                    replaced = Some(id);
                }
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
                if let Some(forwader) = forwarder {
                    methods.add(name, forwader.with_aliased(method_id));
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
                if let Some(wrapped_id) = self.resolve_existing_annotated_method(
                    function.name,
                    *class,
                    &meta.qualifiers,
                    &func_t,
                    ann,
                    types,
                ) {
                    wrapped = Some(wrapped_id);
                }
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
                let is_cast = name == "Cast" && func_t.params().len() == 1;
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
        let mut properties = self.extract_field_properties(&mut annotations);

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
            self.reporter.report(Diagnostic::UnexpectedItem(name_span));
        }

        Some(FieldItem::new(id?, field.default))
    }

    fn process_method_flags(
        &mut self,
        mut qs: ast::ItemQualifiers,
        func: &ast::SourceFunction<'ctx>,
        parent_flags: AggregateFlags,
        span: Span,
    ) -> MethodFlags {
        let is_native = qs.take_flag(ast::ItemQualifiers::NATIVE);
        let is_final = qs.take_flag(ast::ItemQualifiers::FINAL);
        let is_static = qs.take_flag(ast::ItemQualifiers::STATIC);
        let flags = MethodFlags::new()
            .with_is_native(is_native)
            .with_is_final(is_final)
            .with_is_static(is_static)
            .with_is_callback(qs.take_flag(ast::ItemQualifiers::CALLBACK))
            .with_is_unimplemented(func.body.is_none() && !is_native && !is_final && !is_static);

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, span));
        }

        if parent_flags.is_struct() && !flags.is_static() {
            self.reporter
                .report(Diagnostic::NonStaticStructMethod(span));
        }

        if !parent_flags.is_native() && flags.is_native() {
            self.reporter
                .report(Diagnostic::NativeMemberOfScriptedType(span));
        }

        flags
    }

    fn process_field_flags(
        &mut self,
        mut qs: ast::ItemQualifiers,
        parent_flags: AggregateFlags,
        typ: &Type<'ctx>,
        span: Span,
    ) -> FieldFlags {
        fn can_be_persisted(app: &TypeApp<'_>) -> bool {
            match (app.id(), app.args()) {
                (_, args) if !args.is_empty() => args
                    .iter()
                    .all(|arg| matches!(arg, Type::Data(app) if can_be_persisted(app))),
                (id, _) => id != predef::STRING && id != predef::VARIANT && id != predef::RES_REF,
            }
        }

        let flags = FieldFlags::default()
            .with_is_native(qs.take_flag(ast::ItemQualifiers::NATIVE))
            .with_is_const(qs.take_flag(ast::ItemQualifiers::CONST))
            .with_is_persistent(qs.take_flag(ast::ItemQualifiers::PERSISTENT));

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, span));
        }

        if flags.is_persistent() && !matches!(typ, Type::Data(app) if can_be_persisted(app)) {
            self.reporter
                .report(Diagnostic::InvalidPersistentField(span));
        }

        if !parent_flags.is_native() && flags.is_native() {
            self.reporter
                .report(Diagnostic::NativeMemberOfScriptedType(span));
        }

        flags
    }

    fn extract_field_properties(
        &mut self,
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

    fn validate_module(&mut self, module: &InferStageModule<'_, 'ctx>) {
        for func in module.functions() {
            let (func_t, span) = match func {
                FuncItemKind::FreeFunction(func) => {
                    let func = &self.symbols[*func.id()];
                    let span = func.span().expect("user function should have a span");
                    (func.type_(), span)
                }
                _ => {
                    continue;
                }
            };
            self.reporter.unwrap_err(self.check_func_type(func_t, span));
        }

        for field in module.fields() {
            let field = &self.symbols[*field.id()];
            let span = field.span().expect("user field should have a span");
            self.reporter
                .unwrap_err(self.check_type(field.type_(), Variance::Covariant, span));
        }

        for class in module.classes() {
            if self
                .symbols
                .base_iter_with_self(class.id())
                .skip(1)
                .any(|(id, _)| id == class.id())
            {
                self.reporter
                    .report(Diagnostic::CircularInheritance(class.name_span()));
                // remove base class to prevent base type traversal from looping
                self.symbols[class.id()]
                    .schema_mut()
                    .as_aggregate_mut()
                    .expect("class should be an aggregate")
                    .set_base(None);
            }

            let agg = self.symbols[class.id()]
                .schema()
                .as_aggregate()
                .expect("class should be an aggregate");

            if let Some(base) = agg.base() {
                let span = class.name_span();
                self.reporter
                    .unwrap_err(self.check_type_app(base, Variance::Covariant, span));

                let incompatible = match self.symbols[base.id()].schema() {
                    TypeSchema::Enum(_) => Some("an enum"),
                    TypeSchema::Aggregate(base)
                        if base.flags().is_struct() && !agg.flags().is_struct() =>
                    {
                        Some("a struct")
                    }
                    TypeSchema::Aggregate(base)
                        if !base.flags().is_struct() && agg.flags().is_struct() =>
                    {
                        Some("a class")
                    }
                    TypeSchema::Aggregate(base) if base.flags().is_final() => {
                        // TODO: maybe warn
                        None
                    }
                    TypeSchema::Primitive => Some("a primitive"),
                    _ => None,
                };
                if let Some(incompatible) = incompatible {
                    self.reporter
                        .report(Diagnostic::IncompatibleBaseType(incompatible, span));
                }
            }

            for entry in agg.fields().iter() {
                let field = entry.field();
                let span = field.span().expect("user field should have a span");
                self.reporter
                    .unwrap_err(self.check_type(field.type_(), Variance::Covariant, span));
            }

            for method in agg.methods().iter() {
                let method = method.func();
                let span = method.span().expect("user method should have a span");
                self.reporter
                    .unwrap_err(self.check_func_type(method.type_(), span));
            }
        }
    }

    fn process_inheritance<'a>(
        &mut self,
        classes: impl IntoIterator<Item = &'a ClassItem<'a, 'ctx>>,
    ) where
        'ctx: 'a,
    {
        let mut classes = classes.into_iter().collect::<Vec<_>>();
        classes.sort_unstable_by_key(|class| self.symbols.base_iter_with_self(class.id()).count());

        let mut virtuals =
            HashMap::<TypeId<'ctx>, HashSet<MethodId<'ctx>>, BuildIdentityHasher<usize>>::default();

        for class in classes {
            let cls_id = class.id();
            let cls_sym = &self.symbols[cls_id];
            let aggregate = cls_sym
                .schema()
                .as_aggregate()
                .expect("class should be an aggregate");

            let Some(base) = cls_sym.schema().base_type() else {
                continue;
            };

            let this_args = cls_sym
                .params()
                .iter()
                .map(|var| PolyType::from_type(&Type::Ctx(var.clone())))
                .collect::<Rc<_>>();
            let this_t = InferredTypeApp::new(cls_id, this_args);

            let mut implemented: IndexMap<MethodId<'ctx>, FunctionIndex> = IndexMap::default();
            let unimplemented = virtuals.entry(cls_id).or_default();
            let mut duplicates = IndexSet::<&'ctx str>::default();

            for method in aggregate.methods().iter() {
                let method_t = method.func().type_();
                if method.func().flags().is_static() || !method_t.type_params().is_empty() {
                    continue;
                }

                if method.func().flags().is_unimplemented() && !aggregate.flags().is_final() {
                    unimplemented.insert(MethodId::new(cls_id, *method.key()));
                }

                let base = self
                    .symbols
                    .query_methods_by_name(cls_id, method.name())
                    .find(|entry| {
                        if entry.key() == &MethodId::new(class.id(), *method.key())
                            || !entry.func().type_().type_params().is_empty()
                            || entry.func().type_().params().len() != method_t.params().len()
                        {
                            return false;
                        }

                        let entry_this_t = this_t
                            .clone()
                            .instantiate_as::<AnyBaseType>(entry.key().parent(), &self.symbols)
                            .expect("should instantiate as parent type");
                        let env = entry_this_t.type_env(&self.symbols);

                        entry
                            .func()
                            .type_()
                            .unwrapped_param_types()
                            .map(|param| PolyType::from_type_with_env(param, &env).unwrap())
                            .eq(method_t.unwrapped_param_types().map(PolyType::from_type))
                    });

                match base {
                    Some(base) if base.key().parent() == cls_id => {
                        duplicates.insert(method.name());
                    }
                    Some(base) => {
                        if base.func().flags().is_final() {
                            self.reporter.report(Diagnostic::FinalMethodOverride(
                                method.name(),
                                class.name_span(),
                            ));
                        };
                        implemented.insert(*base.key(), *method.key());
                    }
                    _ => {}
                }
            }

            let [to_impl, unimpl] = virtuals.get_disjoint_mut([&base.id(), &cls_id]);
            let to_impl = to_impl.map(|set| set.iter()).unwrap_or_default();
            let unimpl = unimpl.expect("unimplemented set should exist");
            let mut missing = vec![];

            for &id in to_impl.filter(|&id| !implemented.contains_key(id)) {
                if aggregate.flags().is_abstract() {
                    unimpl.insert(id);
                } else {
                    let (name, base_method) = self
                        .symbols
                        .get_method(id)
                        .expect("base method should exist");

                    let parent = this_t
                        .clone()
                        .instantiate_as::<AnyBaseType>(id.parent(), &self.symbols)
                        .expect("should instantiate as parent type");
                    let env = parent.type_env(&self.symbols);
                    let base_types = base_method
                        .type_()
                        .param_types()
                        .map(|param| PolyType::from_type_with_env(param, &env).unwrap())
                        .collect::<Box<_>>();
                    let params = base_method
                        .type_()
                        .params()
                        .iter()
                        .zip(&base_types)
                        .map(|(param, typ)| {
                            Param::new(param.name(), param.flags(), typ.clone(), None)
                        })
                        .collect::<Box<_>>();
                    let return_t =
                        PolyType::from_type_with_env(base_method.type_().return_type(), &env)
                            .unwrap();
                    missing.push(FunctionSignature::new(name, params, return_t));
                }
            }

            for dup in duplicates {
                self.reporter
                    .report(Diagnostic::DuplicateMethod(dup, class.name_span()));
            }

            if !missing.is_empty() {
                self.reporter.report(Diagnostic::MissingMethodImpls(
                    missing.into(),
                    class.name_span(),
                ));
            }

            for (overriden_id, idx) in implemented {
                let overriden_flags = self.symbols[overriden_id].flags();
                let (_, method) = self
                    .symbols
                    .get_method_mut(MethodId::new(cls_id, idx))
                    .expect("method should exist");

                if method.flags().has_implicit_visibility() {
                    method
                        .flags_mut()
                        .set_visibility(overriden_flags.visibility());
                } else if method.flags().visibility() < overriden_flags.visibility()
                    && let Some(span) = method.span()
                {
                    self.reporter.report(Diagnostic::LessVisibleOverride(
                        overriden_flags.visibility(),
                        span,
                    ));
                }
                method.set_overriden(overriden_id);
            }
        }
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
        let res =
            match self.resolve_annotated_method(func_name, cls_name, func_type, types, cls_span) {
                Some(res) => Some(res),
                // Retry in case the user marked a cb method that returns Bool as Void.
                // This enables backward compatibility for some mods.
                None if qualifiers.contains(ast::ItemQualifiers::CALLBACK)
                    && func_type.return_type() == &Type::nullary(predef::VOID) =>
                {
                    let fallback = func_type
                        .clone()
                        .with_return_type(Type::nullary(predef::BOOL));
                    self.resolve_annotated_method(func_name, cls_name, &fallback, types, cls_span)
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

    fn resolve_annotated_method(
        &mut self,
        func_name: &str,
        cls_name: &'ctx str,
        func_type: &FunctionType<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
        cls_span: Span,
    ) -> Option<(MethodId<'ctx>, &Method<'ctx>)> {
        let (id, agg) = self.resolve_annotated_aggregate(cls_name, types, cls_span)?;
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

    fn check_type(
        &self,
        type_app: &Type<'ctx>,
        variance: Variance,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        match type_app {
            Type::Data(type_app) => self.check_type_app(type_app, variance, span),
            Type::Ctx(var)
                if var.variance() != Variance::Invariant && var.variance() != variance =>
            {
                Err(Diagnostic::InvalidVariance(var.name(), variance, span))
            }
            _ => Ok(()),
        }
    }

    fn check_var(
        &self,
        var: &CtxVar<'ctx>,
        typ: &Type<'ctx>,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        let Some(upper) = var.upper() else {
            return Ok(());
        };

        match (typ.upper_bound(), upper.upper_bound()) {
            (Some(x), Some(y)) if self.symbols.is_subtype(x.id(), y.id()) => Ok(()),
            (_, _) => Err(Diagnostic::UnsastisfiedBound(
                typ.clone().into(),
                upper.clone().into(),
                span,
            )),
        }
    }

    fn check_type_app(
        &self,
        type_app: &TypeApp<'ctx>,
        variance: Variance,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        let params = self.symbols[type_app.id()].params();
        if type_app.args().len() == params.len() {
            type_app
                .args()
                .iter()
                .zip(params)
                .try_for_each(|(arg, param)| {
                    self.check_var(param, arg, span)?;
                    self.check_type(arg, variance * param.variance(), span)
                })
        } else {
            Err(Diagnostic::InvalidTypeArgCount(
                type_app.id(),
                params.len(),
                span,
            ))
        }
    }

    fn check_func_type(
        &self,
        func_type: &FunctionType<'ctx>,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        for param in func_type.params() {
            let span = param
                .span()
                .expect("user function param should have a span");
            self.check_type(param.type_(), Variance::Contravariant, span)?;
        }
        self.check_type(func_type.return_type(), Variance::Covariant, span)
    }
}

fn extract_static_forwarder<'ctx>(
    func_t: &FunctionType<'ctx>,
    parent_id: TypeId<'ctx>,
    flags: MethodFlags,
    doc: &[&'ctx str],
    span: Span,
) -> Option<Method<'ctx>> {
    if flags.is_static()
        && func_t.type_params().is_empty()
        && let [fst, rem @ ..] = func_t.params()
        && (fst.type_() == &Type::nullary(parent_id)
            || fst.type_().strip_ref() == Some((RefType::Script, &Type::nullary(parent_id))))
    {
        let func_t = func_t.clone().with_params(rem);
        let flags = flags.with_is_static(false).with_is_static_forwarder(true);
        Some(Method::new(flags, func_t, None, doc, Some(span)))
    } else {
        None
    }
}

fn process_conditionals<'ctx>(
    annotations: &mut Vec<Spanned<ast::SourceAnnotation<'ctx>>>,
    evaluator: &Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> bool {
    let mut include = true;

    annotations.retain(|(ann, _)| match (ann.name, &*ann.args) {
        ("if", [arg]) => {
            let result = reporter.unwrap_err(evaluator.eval(arg));
            include = include && !matches!(result, Some(cte::Value::Bool(false)));
            false
        }
        _ => true,
    });

    include
}

fn convert_visibility(visiblity: ast::Visibility) -> Visibility {
    match visiblity {
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

use std::mem;
use std::ops::Not;

use hashbrown::HashMap;
use redscript_ast::{self as ast, Span};

use super::constants::{annotation, ident};
use super::{
    BitStructOps, NameResolution, ParsedAggregate, convert_visibility, derive_new,
    extract_field_properties,
};
use crate::lower::TypeEnv;
use crate::stages::infer::{ClassItem, FieldItem, FuncItem};
use crate::symbols::Visibility;
use crate::utils::ScopedMap;
use crate::{
    Aggregate, AggregateFlags, Diagnostic, Field, FieldFlags, FieldMap, FunctionType, Method,
    MethodFlags, MethodId, MethodMap, QualifiedName, RefType, Type, TypeApp, TypeDef, TypeId,
    TypeSchema, Variance, predef,
};

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub(super) fn process_aggregate<'a>(
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

                    if let Some(forwarder) = forwarder {
                        methods.add(name, forwarder.with_aliased(MethodId::new(id, func_idx)));
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

                    let properties = extract_field_properties(&mut item.annotations);
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
                ast::Item::Import(_) => self.reporter.report(Diagnostic::UnexpectedItem(
                    if is_struct {
                        "imports cannot be declared inside a struct"
                    } else {
                        "imports cannot be declared inside a class"
                    },
                    item_span,
                )),
                ast::Item::Class(_) | ast::Item::Struct(_) | ast::Item::Enum(_) => {
                    self.reporter.report(Diagnostic::UnexpectedItem(
                        if is_struct {
                            "nested type declarations are not allowed inside a struct"
                        } else {
                            "nested type declarations are not allowed inside a class"
                        },
                        item_span,
                    ));
                }
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

    pub(super) fn process_method_flags(
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

    pub(super) fn process_field_flags(
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
                .report(Diagnostic::InvalidPersistentField(typ.clone().into(), span));
        }

        if !parent_flags.is_native() && flags.is_native() {
            self.reporter
                .report(Diagnostic::NativeMemberOfScriptedType(span));
        }

        flags
    }
}

pub(super) fn extract_static_forwarder<'ctx>(
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

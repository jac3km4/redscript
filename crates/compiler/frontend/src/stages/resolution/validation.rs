use std::rc::Rc;

use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;
use redscript_community_ast::Span;

use super::NameResolution;
use crate::diagnostic::FunctionSignature;
use crate::lower::InferredTypeApp;
use crate::stages::infer::{ClassItem, InferStageModule};
use crate::symbols::AnyBaseType;
use crate::utils::ScopedMap;
use crate::{
    CtxVar, Diagnostic, FreeFunction, FunctionIndex, FunctionType, IndexMap, Method, MethodId,
    Param, PolyType, Type, TypeApp, TypeId, TypeSchema, Variance,
};

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub(super) fn validate_inheritance<'a>(
        &mut self,
        classes: impl IntoIterator<Item = &'a ClassItem<'a, 'ctx>>,
    ) where
        'ctx: 'a,
    {
        let mut checked = HashSet::<TypeId<'ctx>, BuildIdentityHasher<usize>>::default();
        for class in classes {
            let mut ancestors = HashSet::<TypeId<'ctx>, BuildIdentityHasher<usize>>::default();
            let cycle = self
                .symbols
                .base_iter_with_self(class.id())
                .take_while(|(id, _)| !checked.contains(id))
                .find_map(|(id, _)| (!ancestors.insert(id)).then_some(id));

            if let Some(id) = cycle {
                let aggregate = self.symbols[id]
                    .schema_mut()
                    .as_aggregate_mut()
                    .expect("cycle member should be an aggregate");
                if let Some(span) = aggregate.span() {
                    self.reporter.report(Diagnostic::CircularInheritance(span));
                }
                // Break the cycle itself, not the descendant's edge into it.
                aggregate.set_base(None);
            }
            checked.extend(ancestors);
        }
    }

    pub(super) fn validate_function_signatures(&mut self) {
        let mut candidates = self
            .symbols
            .free_functions()
            .filter_map(|entry| entry.func().span().map(|span| (entry, span)))
            .collect::<Vec<_>>();
        candidates.sort_by_key(|(_, span)| (span.file, span.start, span.end));
        let mut functions = vec![];
        for (entry, span) in candidates {
            let func = entry.func();
            if let Err(error) = self.check_func_type(func.type_(), span) {
                self.reporter.report(error);
                let typ = func.type_().clone().with_invalid_signature(true);
                let invalid = if let Some(intrinsic) = func.intrinsic() {
                    FreeFunction::new_intrinsic(typ, intrinsic, func.doc(), Some(span))
                } else if let Some(alias) = func.method_alias() {
                    FreeFunction::new_alias(typ, alias, Some(span))
                } else {
                    FreeFunction::new(func.flags(), typ, func.doc(), Some(span))
                };
                functions.push((*entry.key(), invalid));
            }
        }
        for (id, func) in functions {
            self.symbols.set_free_function(id, func);
        }

        let mut candidates = vec![];
        for (id, def) in self.symbols.types() {
            let Some(aggregate) = def.schema().as_aggregate() else {
                continue;
            };
            for entry in aggregate.methods().iter() {
                let method = *entry.func();
                let Some(span) = method.span() else {
                    continue;
                };
                candidates.push((MethodId::new(id, *entry.key()), method, span));
            }
        }
        candidates.sort_by_key(|(_, _, span)| (span.file, span.start, span.end));
        let mut methods = vec![];
        for (id, method, span) in candidates {
            let signature = method
                .aliased_method()
                .map_or(method.type_(), |id| self.symbols[id].type_());
            if let Err(error) = self.check_func_type(signature, span) {
                if method.aliased_method().is_none() {
                    self.reporter.report(error);
                }
                let mut invalid = Method::new(
                    method.flags(),
                    method.type_().clone().with_invalid_signature(true),
                    method.overriden(),
                    method.doc(),
                    Some(span),
                );
                if let Some(alias) = method.aliased_method() {
                    invalid = invalid.with_aliased(alias);
                }
                methods.push((id, invalid));
            }
        }
        for (id, method) in methods {
            *self
                .symbols
                .get_method_mut(id)
                .expect("method should exist")
                .1 = method;
        }
    }

    pub(super) fn validate_module(&mut self, module: &InferStageModule<'_, 'ctx>) {
        for field in module.fields() {
            let field = &self.symbols[*field.id()];
            let span = field.span().expect("user field should have a span");
            self.reporter
                .unwrap_err(self.check_type(field.type_(), Variance::Covariant, span));
        }

        for class in module.classes() {
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
        }
    }

    pub(super) fn process_inheritance<'a>(
        &mut self,
        classes: impl IntoIterator<Item = &'a ClassItem<'a, 'ctx>>,
        types: &crate::lower::TypeEnv<'_, 'ctx>,
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
            let mut duplicates = IndexMap::<&'ctx str, (Span, Option<Span>)>::default();

            for method in aggregate.methods().iter() {
                let method_t = method.func().type_();
                if !method_t.is_valid()
                    || method.func().flags().is_static()
                    || method.func().flags().is_static_forwarder()
                    || !method_t.type_params().is_empty()
                {
                    continue;
                }

                if method.func().flags().is_unimplemented() && !aggregate.flags().is_final() {
                    unimplemented.insert(MethodId::new(cls_id, *method.key()));
                }

                let base = self
                    .symbols
                    .query_methods_by_name(cls_id, method.name())
                    .find(|entry| {
                        if !entry.func().type_().is_valid()
                            || entry.func().flags().is_static()
                            || entry.func().flags().is_static_forwarder()
                            || (types
                                .policy(method.func().span().unwrap_or(class.name_span()))
                                .original_slot_override_returns
                                && entry.key().parent() != cls_id
                                && entry.func().flags().is_final()
                                && entry.func().flags().visibility() == crate::Visibility::Private)
                            || entry.key() == &MethodId::new(class.id(), *method.key())
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
                        duplicates.insert(
                            method.name(),
                            (
                                method.func().span().unwrap_or(class.name_span()),
                                base.func().span(),
                            ),
                        );
                    }
                    Some(base) => {
                        let span = method.func().span().unwrap_or(class.name_span());
                        if !types.policy(span).original_slot_override_returns
                            && !method.func().flags().is_callback()
                        {
                            let contract = self.symbols.override_return_type(cls_id, *base.key());
                            if !contract.as_ref().is_some_and(|contract| {
                                self.symbols
                                    .compatible_override_return(method_t.return_type(), contract)
                            }) {
                                self.reporter
                                    .report(Diagnostic::OverrideReturnTypeMismatch {
                                        expected: contract.map(Box::new),
                                        found: Box::new(method_t.return_type().clone()),
                                        span,
                                        base_span: base.func().span(),
                                    });
                            }
                        }
                        if base.func().flags().is_final() {
                            self.reporter.report(Diagnostic::FinalMethodOverride(
                                method.name(),
                                method.func().span().unwrap_or(class.name_span()),
                                base.func().span(),
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

            for (dup, (span, prev)) in duplicates {
                self.reporter
                    .report(Diagnostic::DuplicateMethod(dup, span, prev));
            }

            if !missing.is_empty() {
                missing.sort_by_cached_key(ToString::to_string);
                self.reporter.report(Diagnostic::MissingMethodImpls(
                    missing.into(),
                    class.name_span(),
                ));
            }

            for (overridden_id, idx) in implemented {
                let overridden_flags = self.symbols[overridden_id].flags();
                let overridden_span = self.symbols[overridden_id].span();
                let (_, method) = self
                    .symbols
                    .get_method_mut(MethodId::new(cls_id, idx))
                    .expect("method should exist");

                if method.flags().has_implicit_visibility() {
                    method
                        .flags_mut()
                        .set_visibility(overridden_flags.visibility());
                } else if method.flags().visibility() < overridden_flags.visibility()
                    && let Some(span) = method.span()
                    && !types.policy(span).allow_narrower_overrides
                {
                    self.reporter.report(Diagnostic::LessVisibleOverride(
                        overridden_flags.visibility(),
                        span,
                        overridden_span,
                    ));
                }
                method.set_overriden(overridden_id);
            }
        }
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
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        let Some(upper) = var.upper() else {
            return Ok(());
        };

        let bound = PolyType::from_type_with_env(upper, env);
        if bound
            .as_ref()
            .is_ok_and(|bound| PolyType::from_type(typ).satisfies_bound(bound, &self.symbols))
        {
            Ok(())
        } else {
            Err(Diagnostic::UnsatisfiedBound(
                typ.clone().into(),
                upper.clone().into(),
                var.name(),
                span,
            ))
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
            let env = InferredTypeApp::from_type(type_app).type_env(&self.symbols);
            for (arg, param) in type_app.args().iter().zip(params) {
                self.check_type(arg, variance * param.variance(), span)?;
                self.check_var(param, arg, &env, span)?;
            }
            Ok(())
        } else {
            Err(Diagnostic::InvalidTypeArgCount(
                type_app.id(),
                params.len(),
                type_app.args().len(),
                span,
            ))
        }
    }

    fn check_func_type(
        &self,
        func_type: &FunctionType<'ctx>,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        if !func_type.is_valid() {
            return Ok(());
        }
        for var in func_type.type_params() {
            if let Some(upper) = var.upper() {
                self.check_type(upper, Variance::Covariant, span)?;
            }
        }
        for param in func_type.params() {
            let span = param
                .span()
                .expect("user function param should have a span");
            self.check_type(param.type_(), Variance::Contravariant, span)?;
        }
        self.check_type(func_type.return_type(), Variance::Covariant, span)
    }
}

use std::rc::Rc;

use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;
use redscript_ast::Span;

use super::NameResolution;
use crate::diagnostic::FunctionSignature;
use crate::lower::InferredTypeApp;
use crate::stages::infer::{ClassItem, FuncItemKind, InferStageModule};
use crate::symbols::AnyBaseType;
use crate::{
    CtxVar, Diagnostic, FunctionIndex, FunctionType, IndexMap, IndexSet, MethodId, Param, PolyType,
    Type, TypeApp, TypeId, TypeSchema, Variance,
};

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub(super) fn validate_module(&mut self, module: &InferStageModule<'_, 'ctx>) {
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

    pub(super) fn process_inheritance<'a>(
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

            for (overridden_id, idx) in implemented {
                let overridden_flags = self.symbols[overridden_id].flags();
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
                {
                    self.reporter.report(Diagnostic::LessVisibleOverride(
                        overridden_flags.visibility(),
                        span,
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
            for (arg, param) in type_app.args().iter().zip(params) {
                self.check_var(param, arg, span)?;
                self.check_type(arg, variance * param.variance(), span)?;
            }
            Ok(())
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

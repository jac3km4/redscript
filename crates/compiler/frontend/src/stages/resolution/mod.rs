use std::borrow::Cow;
use std::collections::BTreeSet;
use std::ops::{BitAndAssign, Not};
use std::rc::Rc;
use std::{fmt, mem};

use aggregate::extract_static_forwarder;
use constants::{annotation, ident};
use hashbrown::HashMap;
use redscript_community_ast::{self as ast, BinOp, Span, Spanned, UnOp};

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
mod enums;
mod functions;

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
                    for (ann, ann_span) in &item_meta.annotations {
                        if ann.name == annotation::HINT {
                            self.reporter
                                .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                        }
                    }
                    if !qualifiers.is_empty() {
                        self.reporter
                            .report(Diagnostic::UnusedItemQualifiers(qualifiers, item_span));
                    }
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
                        .map_err(|_| Diagnostic::NameRedefinition(name, name_span));
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
                ast::Item::Function(mut function) => {
                    extract_function_properties(
                        &mut item_meta.annotations,
                        &mut function.properties,
                    );
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
                            .map_err(|_| Diagnostic::NameRedefinition(name, name_span));
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
                        .map_err(|_| Diagnostic::NameRedefinition(name, name_span));
                    if self.reporter.unwrap_err(res).is_none() {
                        continue;
                    }

                    enums.push(ParsedEnum {
                        id,
                        meta: item_meta,
                        enum_,
                    });
                }
                ast::Item::Default(_) => self.reporter.report(Diagnostic::UnexpectedItem(
                    "field defaults must be declared inside an aggregate",
                    item_span,
                )),
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

        self.symbols.ensure_serializable_base();
        self.validate_inheritance(results.iter().flat_map(InferStageModule::classes));
        self.validate_function_signatures();
        self.symbols.link_serializable_methods();
        for module in &results {
            self.validate_module(module);
        }
        self.process_inheritance(
            results.iter().flat_map(InferStageModule::classes),
            &scope.types,
        );

        TypeInference::new(results, self.symbols)
    }
}

fn extract_field_hint<'ctx>(
    anns: &mut Vec<ast::Spanned<ast::SourceAnnotation<'ctx>>>,
) -> Option<Cow<'ctx, str>> {
    let mut hint = None;
    anns.retain(|(ann, _)| match (ann.name, &*ann.args) {
        (annotation::HINT, [(ast::Expr::Constant(ast::Constant::String(value)), _)]) => {
            hint = Some(value.clone());
            false
        }
        _ => true,
    });
    hint
}

fn extract_function_properties<'ctx>(
    anns: &mut Vec<ast::Spanned<ast::SourceAnnotation<'ctx>>>,
    properties: &mut ast::FunctionProperties<'ctx>,
) {
    anns.retain(|(ann, _)| match (ann.name, &*ann.args) {
        (annotation::PROFILE, [(ast::Expr::Constant(ast::Constant::String(label)), _)]) => {
            properties.profile = Some(label.clone());
            false
        }
        _ => true,
    });
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
            if let Some(name) = id.as_name() {
                types.add(name, TypeRef::Id(id));
            }
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

    pub(crate) fn set_policies(&mut self, policies: &[(ast::FileId, Rc<crate::SemanticPolicy>)]) {
        self.types.set_policies(policies);
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

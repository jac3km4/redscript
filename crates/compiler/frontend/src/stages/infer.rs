use std::mem;
use std::ops::Not;

use bon::builder;
use identity_hash::BuildIdentityHasher;
use redscript_community_ast::{self as ast, FileId, Span, Spanned};
use smallvec::smallvec;

use super::provenance::{DeclarationPath, ResolvedDeclaration};
use super::resolution::Scope;
use super::resolution::constants::ident;
use crate::diagnostic::FunctionSignature;
use crate::diagnostic::pass::DiagnosticPass;
use crate::lower::{Env, Lower};
use crate::symbols::FreeFunctionIndexes;
use crate::{
    CompileErrorReporter, FieldId, FieldIndex, FreeFunction, FreeFunctionIndex, FunctionIndex,
    FunctionType, IndexMap, IndexSet, MethodId, PolyType, Shared, Symbols, Type, TypeId, TypeRef,
    TypeScope, ir,
};

#[path = "const_validation.rs"]
mod const_validation;
#[path = "script_ref_validation.rs"]
mod script_ref_validation;

#[derive(Debug)]
pub struct TypeInference<'scope, 'ctx> {
    modules: Vec<InferStageModule<'scope, 'ctx>>,
    symbols: Symbols<'ctx>,
}

impl<'scope, 'ctx> TypeInference<'scope, 'ctx> {
    pub fn new(modules: Vec<InferStageModule<'scope, 'ctx>>, symbols: Symbols<'ctx>) -> Self {
        Self { symbols, modules }
    }

    pub fn finish(
        mut self,
        scope: &'scope Scope<'_, 'ctx>,
        reporter: &mut CompileErrorReporter<'ctx>,
    ) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
        let mut modules = mem::take(&mut self.modules);
        let wraps = self.register_wrap_aliases(&mut modules);
        let tasks = modules.into_iter().zip(wraps).collect::<Vec<_>>();

        let symbols = &self.symbols;
        let lowered = crate::parallel::map_ordered(tasks, |(module, wraps)| {
            lower_module(module, &wraps, scope, symbols)
        });

        let mut compiled = LoweredCompilationUnit::default();
        for module in lowered {
            for (file, path, resolved) in module.declarations {
                compiled
                    .declarations
                    .entry(file)
                    .or_default()
                    .push((path, resolved));
            }
            for (id, class) in module.classes {
                compiled.classes.insert(id, class);
            }
            for enum_id in module.enums {
                compiled.enums.insert(enum_id);
            }
            for (id, func) in module.functions {
                compiled.functions.insert(id, func);
            }
            for (id, expr) in module.added_fields {
                compiled.added_fields.insert(id, expr);
            }
            for (id, func) in module.added_methods {
                compiled.added_methods.insert(id, func);
            }
            reporter.report_many(module.diagnostics);
            for (id, func) in module.method_replacements {
                let span = func.span;
                let previous = compiled.method_replacements.insert(id, func);
                if let Some(previous) = previous {
                    let (name, method) = self.symbols.get_method(id).unwrap();
                    let typ = method.type_();
                    reporter.report(crate::Diagnostic::ReplaceMethodConflict(
                        id.parent(),
                        Box::new(FunctionSignature::new(
                            name,
                            typ.params(),
                            typ.return_type().clone(),
                        )),
                        span,
                        previous.span,
                    ));
                }
            }
            for (id, func) in module.method_wrappers {
                compiled.method_wrappers.entry(id).or_default().push(func);
            }
            if let Some((file, scope)) = module.scope {
                compiled.scopes.insert(file, scope);
            }
        }

        const_validation::validate(
            &compiled,
            &self.symbols,
            |span| scope.types.policy(span),
            reporter,
        );
        script_ref_validation::validate(&compiled, &self.symbols, reporter);
        (compiled, self.symbols)
    }

    /// Registers `@wrapMethod` aliases before lowering so `symbols` stays
    /// immutable during the parallel phase. Returns, per module, the alias and
    /// global wrapper index of every `@wrapMethod`, in source order.
    fn register_wrap_aliases(
        &mut self,
        modules: &mut [InferStageModule<'scope, 'ctx>],
    ) -> Vec<Vec<Option<WrapInfo>>> {
        let mut counts: IndexMap<MethodId<'ctx>, usize> = IndexMap::default();
        modules
            .iter_mut()
            .map(|module| {
                let mut wraps = vec![None; module.functions.len()];
                for (index, func) in module.functions.iter_mut().enumerate() {
                    if let FuncItemKind::WrapMethod(item) = func {
                        let typ = self.symbols[item.id].type_().clone();
                        let free_func = FreeFunction::new_alias(typ, item.id, Some(item.name_span));
                        let alias = self
                            .symbols
                            .add_free_function(ident::WRAPPED_METHOD, free_func);
                        let next = counts.entry(item.id).or_insert(0);
                        wraps[index] = Some(WrapInfo {
                            alias,
                            index: *next,
                        });
                        *next += 1;
                    }
                }
                wraps
            })
            .collect()
    }
}

/// The alias and global index of a single `@wrapMethod` declaration.
#[derive(Debug, Clone, Copy)]
struct WrapInfo {
    alias: FreeFunctionIndex,
    index: usize,
}

/// Lowered output of one source module, merged into the compilation unit in
/// module order after the parallel phase completes.
#[derive(Default)]
struct LoweredModule<'ctx> {
    declarations: Vec<(FileId, DeclarationPath, ResolvedDeclaration<'ctx>)>,
    classes: Vec<(TypeId<'ctx>, LoweredClass<'ctx>)>,
    enums: Vec<TypeId<'ctx>>,
    functions: Vec<(FreeFunctionIndex, LoweredFunction<'ctx>)>,
    added_fields: Vec<(FieldId<'ctx>, Option<Box<ir::Const<'ctx>>>)>,
    added_methods: Vec<(MethodId<'ctx>, Option<LoweredFunction<'ctx>>)>,
    method_replacements: Vec<(MethodId<'ctx>, LoweredFunction<'ctx>)>,
    method_wrappers: Vec<(MethodId<'ctx>, LoweredFunction<'ctx>)>,
    scope: Option<(FileId, IndexMap<&'ctx str, TypeRef<'static, 'ctx>>)>,
    diagnostics: Vec<crate::Diagnostic<'ctx>>,
}

fn lower_module<'m, 'ctx>(
    module: InferStageModule<'m, 'ctx>,
    wraps: &[Option<WrapInfo>],
    scope: &Scope<'m, 'ctx>,
    symbols: &Symbols<'ctx>,
) -> LoweredModule<'ctx> {
    let mut out = LoweredModule::default();
    let mut reporter = CompileErrorReporter::default();
    let scope = scope.push(module.type_scope, module.func_scope);

    for (file, path, resolved) in module.declarations {
        out.declarations.push((file, path, resolved));
    }

    for class in module.classes {
        let types = scope.types.push_scope(class.scope);
        let cls_t = &symbols[class.id];

        let class_type_args = cls_t
            .params()
            .iter()
            .map(|param| Type::Ctx(param.clone()))
            .collect::<Shared<_>>();
        let this_t = PolyType::from_type(&Type::app(class.id, class_type_args));

        let mut methods = IndexMap::default();
        for item in class.methods {
            let method = &symbols[MethodId::new(class.id, item.id)];
            if !method.type_().is_valid() {
                continue;
            }
            let func = function_builder()
                .type_(method.type_())
                .param_names(&item.param_names)
                .body(&item.body)
                .env(Env::new(&types.push_scope(item.scope), &scope.funcs))
                .maybe_this_type(method.flags().is_static().not().then(|| this_t.clone()))
                .context(class.id)
                .symbols(symbols)
                .reporter(&mut reporter)
                .span(item.span)
                .build();
            methods.insert(item.id, func);
        }

        let mut defaults = Vec::new();
        let mut own_order: IndexMap<FieldIndex, u32> = IndexMap::default();
        for item in &class.fields {
            if let Some(value) = &item.default {
                own_order.insert(item.id, value.1.start);
            }
        }
        // Name resolution has completed for every module before inference:
        // even bases declared in later files now have their full fields.
        for (input, input_span) in &class.defaults {
            let Some((parent, (index, field))) =
                symbols
                    .base_iter_with_self(class.id)
                    .find_map(|(id, aggregate)| {
                        Some((id, aggregate.fields().by_name(input.target.0)?))
                    })
            else {
                reporter.report(crate::LowerError::UnresolvedMember(
                    class.id,
                    input.target.0,
                    input.target.1,
                ));
                continue;
            };
            if parent == class.id
                && own_order
                    .get(&index)
                    .is_some_and(|&start| start < input_span.start)
            {
                continue;
            }
            let receiver = crate::lower::InferredTypeApp::from_type(&crate::TypeApp::new(
                class.id,
                cls_t
                    .params()
                    .iter()
                    .cloned()
                    .map(Type::Ctx)
                    .collect::<Shared<_>>(),
            ));
            let Some(type_env) =
                receiver.base_type_env::<crate::symbols::AnyBaseType>(parent, symbols)
            else {
                continue;
            };
            let expected = match PolyType::from_type_with_env(field.type_(), &type_env) {
                Ok(expected) => expected,
                Err(name) => {
                    reporter.report(crate::LowerError::UnresolvedVar(name, input.target.1));
                    continue;
                }
            };
            let env = Env::new(&types, &scope.funcs);
            let (value, errors) = supplemental_constant(&input.value, &env, expected, symbols);
            reporter.report_many(errors);
            if let Some(value) = value {
                // Official own-field defaults are first-wins, whereas inherited
                // overrides are last-wins. Keep the resolved entries in order;
                // the backend merges inherited entries by supplying-class key.
                if parent == class.id {
                    own_order.insert(index, input_span.start);
                }
                defaults.push(LoweredFieldDefault {
                    field: FieldId::new(parent, index),
                    value,
                    literal_text: input.literal_text,
                    span: *input_span,
                });
            }
        }
        let mut fields = IndexMap::default();
        for item in class.fields {
            let Some(default) = item.default else {
                continue;
            };
            if own_order
                .get(&item.id)
                .is_some_and(|&start| start < default.1.start)
            {
                continue;
            }
            let field_id = FieldId::new(class.id, item.id);
            let env = Env::new(&types, &scope.funcs);
            if let Some(expr) = lower_constant(field_id, &default, &env, symbols, &mut reporter) {
                fields.insert(item.id, expr);
            }
        }

        out.classes.push((
            class.id,
            LoweredClass {
                methods,
                fields,
                defaults,
                span: class.span,
            },
        ));
    }

    for enum_id in module.enums {
        out.enums.push(enum_id);
    }

    for (index, func) in module.functions.into_iter().enumerate() {
        match func {
            FuncItemKind::FreeFunction(func) => {
                if !symbols[func.id].type_().is_valid() {
                    continue;
                }
                let value = function_builder()
                    .type_(symbols[func.id].type_())
                    .param_names(&func.param_names)
                    .body(&func.body)
                    .env(Env::new(&scope.types.push_scope(func.scope), &scope.funcs))
                    .symbols(symbols)
                    .reporter(&mut reporter)
                    .span(func.span)
                    .build();
                out.functions.push((func.id, value));
                if let Some((file, path)) = func.declaration {
                    out.declarations
                        .push((file, path, ResolvedDeclaration::Function(func.id)));
                }
            }
            FuncItemKind::ReplaceMethod(func) => {
                let lowered = lower_patch_method(
                    func.id,
                    func.properties.as_ref(),
                    &func.param_names,
                    &func.body,
                    Env::new(&scope.types.push_scope(func.scope), &scope.funcs),
                    symbols,
                    func.span,
                    &mut reporter,
                );
                out.method_replacements.push((func.id, lowered));
                if let Some((file, path)) = func.declaration {
                    out.declarations
                        .push((file, path, ResolvedDeclaration::Replacement(func.id)));
                }
            }
            FuncItemKind::AddMethod(func) => {
                if !symbols[func.id].type_().is_valid() {
                    continue;
                }
                let lowered = func.body.as_ref().map(|body| {
                    lower_patch_method(
                        func.id,
                        None,
                        &func.param_names,
                        body,
                        Env::new(&scope.types.push_scope(func.scope), &scope.funcs),
                        symbols,
                        func.span,
                        &mut reporter,
                    )
                });
                out.added_methods.push((func.id, lowered));
                if let Some((file, path)) = func.declaration {
                    out.declarations
                        .push((file, path, ResolvedDeclaration::AddedMethod(func.id)));
                }
            }
            FuncItemKind::WrapMethod(func) => {
                let info = wraps[index].expect("wrap alias should be registered");
                let mut funcs = scope.funcs.introduce_scope();
                funcs.insert(ident::WRAPPED_METHOD, smallvec![info.alias]);

                let lowered = lower_patch_method(
                    func.id,
                    func.properties.as_ref(),
                    &func.param_names,
                    &func.body,
                    Env::new(&scope.types.push_scope(func.scope), &funcs),
                    symbols,
                    func.span,
                    &mut reporter,
                );
                out.method_wrappers.push((func.id, lowered));
                if let Some((file, path)) = func.declaration {
                    out.declarations.push((
                        file,
                        path,
                        ResolvedDeclaration::Wrapper(func.id, info.index),
                    ));
                }
            }
        }
    }

    for item in module.fields {
        let expr = item
            .default
            .and_then(|default| {
                let env = Env::new(&scope.types, &scope.funcs);
                lower_constant(item.id, &default, &env, symbols, &mut reporter)
            })
            .map(Box::new);

        out.added_fields.push((item.id, expr));
    }

    if let Some(span) = module.span {
        let scope = scope
            .types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();
        out.scope = Some((span.file, scope));
    }

    out.diagnostics = reporter.into_reported();
    out
}

/// Lowers a `@replaceMethod`/`@addMethod`/`@wrapMethod` patch, sharing the
/// `this` typing and builder wiring across the three patch kinds. `env` already
/// carries the patch's type and free-function scopes (a wrapper adds its
/// `__wrapped` alias before calling).
#[allow(clippy::too_many_arguments)]
fn lower_patch_method<'ctx>(
    id: MethodId<'ctx>,
    properties: Option<&ast::FunctionProperties<'ctx>>,
    param_names: &[&'ctx str],
    body: &ast::SourceFunctionBody<'ctx>,
    env: Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> LoweredFunction<'ctx> {
    let sym = &symbols[id];
    let this_t = sym
        .flags()
        .is_static()
        .not()
        .then(|| PolyType::nullary(id.parent()));
    function_builder()
        .type_(sym.type_())
        .maybe_properties(properties)
        .param_names(param_names)
        .body(body)
        .env(env)
        .maybe_this_type(this_t)
        .context(id.parent())
        .symbols(symbols)
        .reporter(reporter)
        .span(span)
        .build()
}

#[builder(finish_fn = build)]
fn function_builder<'ctx>(
    type_: &FunctionType<'ctx>,
    properties: Option<&ast::FunctionProperties<'ctx>>,
    param_names: &[&'ctx str],
    body: &ast::SourceFunctionBody<'ctx>,
    mut env: Env<'_, 'ctx>,
    this_type: Option<PolyType<'ctx>>,
    context: Option<TypeId<'ctx>>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    span: Span,
) -> LoweredFunction<'ctx> {
    let this = this_type.map(|lt| ir::LocalInfo::new(ir::Local::This, None, lt, None));
    if let Some(this) = &this {
        env.define_local(ident::THIS, this.clone());
    }

    let params = type_
        .params()
        .iter()
        .zip(param_names)
        .map(|(param, &name)| (name, PolyType::from_type(param.type_())));
    let return_t = PolyType::from_type(type_.return_type());

    let (block, output, errors) = Lower::function(body, params, env, return_t, context, symbols);
    reporter.report_many(errors);

    let locals = this.iter().chain(output.locals()).cloned().collect();

    LoweredFunction {
        block,
        locals,
        span,
        properties: properties.unwrap_or_else(|| type_.properties()).clone(),
    }
}

// Normalize only the bounded literal forms used by supplemental metadata;
// do not change ordinary unary-operator resolution or community initializers.
fn supplemental_constant<'ctx>(
    value: &Spanned<ast::SourceExpr<'ctx>>,
    env: &Env<'_, 'ctx>,
    expected: PolyType<'ctx>,
    symbols: &Symbols<'ctx>,
) -> (Option<ir::Const<'ctx>>, Vec<crate::LowerError<'ctx>>) {
    if let ast::Expr::UnOp {
        op: ast::UnOp::Neg,
        expr,
    } = &value.0
    {
        let constant = match &expr.0 {
            ast::Expr::Constant(ast::Constant::F32(value)) => Some(ast::Constant::F32(-value)),
            ast::Expr::Constant(ast::Constant::F64(value)) => Some(ast::Constant::F64(-value)),
            ast::Expr::Constant(ast::Constant::I32(value)) => {
                value.checked_neg().map(ast::Constant::I32)
            }
            ast::Expr::Constant(ast::Constant::I64(value)) => {
                value.checked_neg().map(ast::Constant::I64)
            }
            _ => None,
        };
        if let Some(constant) = constant {
            return Lower::constant(
                &(ast::Expr::Constant(constant), value.1),
                env,
                expected,
                symbols,
            );
        }
    }
    Lower::constant(value, env, expected, symbols)
}

fn lower_constant<'ctx>(
    id: FieldId<'ctx>,
    expr: &Spanned<ast::SourceExpr<'ctx>>,
    env: &Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Option<ir::Const<'ctx>> {
    let field = &symbols[id];
    let (expr, errors) = Lower::constant(expr, env, PolyType::from_type(field.type_()), symbols);
    reporter.report_many(errors);
    expr
}

#[derive(Debug)]
pub struct InferStageModule<'scope, 'ctx> {
    type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
    func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,

    classes: Vec<ClassItem<'scope, 'ctx>>,
    enums: Vec<TypeId<'ctx>>,
    functions: Vec<FuncItemKind<'scope, 'ctx>>,
    fields: Vec<FieldItem<'ctx, FieldId<'ctx>>>,
    /// Resolved `(file, path)` bindings that are already final before lowering:
    /// aggregate/enum types, their members, bodyless free functions and added fields.
    declarations: Vec<(FileId, DeclarationPath, ResolvedDeclaration<'ctx>)>,
    span: Option<Span>,
}

impl<'scope, 'ctx> InferStageModule<'scope, 'ctx> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
        func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,
        classes: Vec<ClassItem<'scope, 'ctx>>,
        enums: Vec<TypeId<'ctx>>,
        functions: Vec<FuncItemKind<'scope, 'ctx>>,
        fields: Vec<FieldItem<'ctx, FieldId<'ctx>>>,
        declarations: Vec<(FileId, DeclarationPath, ResolvedDeclaration<'ctx>)>,
        span: Option<Span>,
    ) -> Self {
        Self {
            type_scope,
            func_scope,
            functions,
            classes,
            enums,
            fields,
            declarations,
            span,
        }
    }

    pub fn classes(&self) -> &[ClassItem<'scope, 'ctx>] {
        &self.classes
    }

    pub fn fields(&self) -> &[FieldItem<'ctx, FieldId<'ctx>>] {
        &self.fields
    }
}

#[derive(Debug)]
pub struct FieldItem<'ctx, K> {
    id: K,
    default: Option<Box<Spanned<ast::SourceExpr<'ctx>>>>,
}

impl<'ctx, K> FieldItem<'ctx, K> {
    pub fn new(id: K, default: Option<Box<Spanned<ast::SourceExpr<'ctx>>>>) -> Self {
        Self { id, default }
    }

    pub fn id(&self) -> &K {
        &self.id
    }
}

#[derive(Debug)]
pub struct FuncItem<'scope, 'ctx, K, B = ast::SourceFunctionBody<'ctx>> {
    id: K,
    span: Span,
    name_span: Span,
    param_names: Box<[&'ctx str]>,
    body: B,
    scope: TypeScope<'scope, 'ctx>,
    properties: Option<ast::FunctionProperties<'ctx>>,
    /// Source provenance for functions whose binding is emitted during lowering
    /// (free functions, added methods, replacements and wrappers).
    declaration: Option<(FileId, DeclarationPath)>,
}

impl<'scope, 'ctx, K, B> FuncItem<'scope, 'ctx, K, B> {
    pub fn new(
        id: K,
        span: Span,
        name_span: Span,
        param_names: impl Into<Box<[&'ctx str]>>,
        body: B,
        scope: TypeScope<'scope, 'ctx>,
    ) -> Self {
        Self {
            id,
            span,
            name_span,
            param_names: param_names.into(),
            body,
            scope,
            properties: None,
            declaration: None,
        }
    }

    pub fn with_properties(mut self, properties: ast::FunctionProperties<'ctx>) -> Self {
        self.properties = Some(properties);
        self
    }

    pub fn with_declaration(mut self, file: FileId, path: DeclarationPath) -> Self {
        self.declaration = Some((file, path));
        self
    }
}

#[derive(Debug)]
pub struct ClassItem<'scope, 'ctx> {
    id: TypeId<'ctx>,
    span: Span,
    name_span: Span,
    scope: TypeScope<'scope, 'ctx>,
    methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
    fields: Vec<FieldItem<'ctx, FieldIndex>>,
    defaults: Vec<Spanned<ast::SourceFieldDefault<'ctx>>>,
}

impl<'scope, 'ctx> ClassItem<'scope, 'ctx> {
    pub fn new(
        id: TypeId<'ctx>,
        span: Span,
        name_span: Span,
        scope: TypeScope<'scope, 'ctx>,
        methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
        fields: Vec<FieldItem<'ctx, FieldIndex>>,
        defaults: Vec<Spanned<ast::SourceFieldDefault<'ctx>>>,
    ) -> Self {
        Self {
            id,
            span,
            name_span,
            scope,
            methods,
            fields,
            defaults,
        }
    }

    pub fn id(&self) -> TypeId<'ctx> {
        self.id
    }

    pub fn name_span(&self) -> Span {
        self.name_span
    }
}

#[derive(Debug)]
pub enum FuncItemKind<'scope, 'ctx> {
    FreeFunction(FuncItem<'scope, 'ctx, FreeFunctionIndex>),
    ReplaceMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>>),
    WrapMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>>),
    AddMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>, Option<ast::SourceFunctionBody<'ctx>>>),
}

#[derive(Debug)]
pub struct LoweredFunction<'ctx> {
    pub block: ir::Block<'ctx>,
    pub locals: Box<[ir::LocalInfo<'ctx>]>,
    pub span: Span,
    pub properties: ast::FunctionProperties<'ctx>,
}

impl<'ctx> LoweredFunction<'ctx> {
    pub fn find_local(&self, loc: ir::Local) -> Option<&PolyType<'ctx>> {
        self.locals
            .binary_search_by_key(&loc, |l| l.id)
            .ok()
            .map(|idx| &self.locals[idx].typ)
    }
}

/// Identity of a lowered function owned by a [`LoweredCompilationUnit`].
/// `Method` covers both aggregate-declared methods and `@addMethod` patches.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionId<'ctx> {
    Free(FreeFunctionIndex),
    Method(MethodId<'ctx>),
    Replacement(MethodId<'ctx>),
    Wrapper(MethodId<'ctx>),
}

impl<'ctx> FunctionId<'ctx> {
    /// The patched method this identity belongs to, or `None` for a free function.
    #[inline]
    pub fn as_method(self) -> Option<MethodId<'ctx>> {
        match self {
            Self::Free(_) => None,
            Self::Method(id) | Self::Replacement(id) | Self::Wrapper(id) => Some(id),
        }
    }

    /// The resolved signature of the underlying free function or method.
    #[inline]
    pub fn function_type<'s>(self, symbols: &'s Symbols<'ctx>) -> &'s FunctionType<'ctx> {
        match self {
            Self::Free(id) => symbols[id].type_(),
            Self::Method(id) | Self::Replacement(id) | Self::Wrapper(id) => symbols[id].type_(),
        }
    }
}

#[derive(Debug, Default)]
pub struct LoweredCompilationUnit<'ctx> {
    pub classes: IndexMap<TypeId<'ctx>, LoweredClass<'ctx>, BuildIdentityHasher<usize>>,
    pub enums: IndexSet<TypeId<'ctx>, BuildIdentityHasher<usize>>,
    pub functions: IndexMap<FreeFunctionIndex, LoweredFunction<'ctx>>,

    pub added_fields: IndexMap<FieldId<'ctx>, Option<Box<ir::Const<'ctx>>>>,
    pub added_methods: IndexMap<MethodId<'ctx>, Option<LoweredFunction<'ctx>>>,
    pub method_replacements: IndexMap<MethodId<'ctx>, LoweredFunction<'ctx>>,
    pub method_wrappers: IndexMap<MethodId<'ctx>, Vec<LoweredFunction<'ctx>>>,

    pub scopes: IndexMap<FileId, IndexMap<&'ctx str, TypeRef<'static, 'ctx>>>,

    /// Per-file declaration provenance captured during name resolution and
    /// lowering. Keeps every binding, including duplicate replacements and
    /// multiple wrappers that the lowering tables collapse by [`MethodId`].
    pub declarations: IndexMap<FileId, Vec<(DeclarationPath, ResolvedDeclaration<'ctx>)>>,
}

impl<'ctx> LoweredCompilationUnit<'ctx> {
    pub fn all_functions(&self) -> impl Iterator<Item = &LoweredFunction<'ctx>> {
        self.classes
            .values()
            .flat_map(|class| class.methods.values())
            .chain(self.functions.values())
            .chain(self.added_methods.values().flatten())
            .chain(self.method_replacements.values())
            .chain(self.method_wrappers.values().flatten())
    }

    /// All lowered functions paired with their resolved identity, in a stable
    /// order: free functions, then aggregate methods, added methods, replacements
    /// and wrappers.
    pub fn functions_with_ids(
        &self,
    ) -> impl Iterator<Item = (FunctionId<'ctx>, &LoweredFunction<'ctx>)> {
        self.functions
            .iter()
            .map(|(&id, f)| (FunctionId::Free(id), f))
            .chain(self.classes.iter().flat_map(|(&id, class)| {
                class
                    .methods
                    .iter()
                    .map(move |(&idx, f)| (FunctionId::Method(MethodId::new(id, idx)), f))
            }))
            .chain(
                self.added_methods
                    .iter()
                    .filter_map(|(&id, f)| Some((FunctionId::Method(id), f.as_ref()?))),
            )
            .chain(
                self.method_replacements
                    .iter()
                    .map(|(&id, f)| (FunctionId::Replacement(id), f)),
            )
            .chain(
                self.method_wrappers
                    .iter()
                    .flat_map(|(&id, fs)| fs.iter().map(move |f| (FunctionId::Wrapper(id), f))),
            )
    }

    /// Mutable counterpart of [`Self::functions_with_ids`] with the same order.
    pub fn functions_with_ids_mut(
        &mut self,
    ) -> impl Iterator<Item = (FunctionId<'ctx>, &mut LoweredFunction<'ctx>)> {
        let Self {
            classes,
            functions,
            added_methods,
            method_replacements,
            method_wrappers,
            ..
        } = self;
        functions
            .iter_mut()
            .map(|(&id, f)| (FunctionId::Free(id), f))
            .chain(classes.iter_mut().flat_map(|(&id, class)| {
                class
                    .methods
                    .iter_mut()
                    .map(move |(&idx, f)| (FunctionId::Method(MethodId::new(id, idx)), f))
            }))
            .chain(
                added_methods
                    .iter_mut()
                    .filter_map(|(&id, f)| Some((FunctionId::Method(id), f.as_mut()?))),
            )
            .chain(
                method_replacements
                    .iter_mut()
                    .map(|(&id, f)| (FunctionId::Replacement(id), f)),
            )
            .chain(
                method_wrappers
                    .iter_mut()
                    .flat_map(|(&id, fs)| fs.iter_mut().map(move |f| (FunctionId::Wrapper(id), f))),
            )
    }

    /// All lowered functions mutably, in the same order as [`Self::all_functions`].
    pub fn all_functions_mut(&mut self) -> impl Iterator<Item = &mut LoweredFunction<'ctx>> {
        let Self {
            classes,
            functions,
            added_methods,
            method_replacements,
            method_wrappers,
            ..
        } = self;
        classes
            .values_mut()
            .flat_map(|class| class.methods.values_mut())
            .chain(functions.values_mut())
            .chain(added_methods.values_mut().flatten())
            .chain(method_replacements.values_mut())
            .chain(method_wrappers.values_mut().flatten())
    }

    pub fn run_diagnostics(
        &self,
        passes: &[&'static dyn DiagnosticPass],
        reporter: &mut CompileErrorReporter<'ctx>,
    ) {
        if passes.is_empty() {
            return;
        }

        let functions = self.all_functions().collect::<Vec<_>>();
        let diagnostics = crate::parallel::map_ordered(functions, |func| {
            let mut local = CompileErrorReporter::default();
            for pass in passes {
                pass.run(func, &mut local);
            }
            local.into_reported()
        });
        for diagnostics in diagnostics {
            reporter.report_many(diagnostics);
        }
    }
}

#[derive(Debug)]
pub struct LoweredFieldDefault<'ctx> {
    pub field: FieldId<'ctx>,
    pub value: ir::Const<'ctx>,
    pub literal_text: Option<&'ctx str>,
    pub span: Span,
}

#[derive(Debug)]
pub struct LoweredClass<'ctx> {
    pub methods: IndexMap<FunctionIndex, LoweredFunction<'ctx>>,
    pub fields: IndexMap<FieldIndex, ir::Const<'ctx>>,
    /// Defaults supplied by this aggregate; field identities may belong to bases.
    pub defaults: Vec<LoweredFieldDefault<'ctx>>,
    pub span: Span,
}

mod arguments;
mod cte;
mod diagnostic;
pub mod ir;
mod lower;
mod modules;
mod parallel;
mod query;
mod stages;
mod symbols;
mod sync;
pub mod types;
pub mod utils;
mod visitor;

pub use arguments::absent_argument;
pub use cte::Evaluator;
pub use diagnostic::{Diagnostic, DiagnosticLevel, Reporter, UnknownSource, pass};
pub use lower::{
    ArrayPrefixInfo, CoalesceError, Error as LowerError, PolyType, SemanticPolicy, TypeError,
    TypeRef,
};
pub use query::{QueryError, expr_type};
pub use redscript_community_ast as ast;
use redscript_community_parser as parser;
pub use stages::{
    DeclarationPath, FunctionId, LoweredClass, LoweredCompilationUnit, LoweredFieldDefault,
    LoweredFunction, QueryTarget, ResolvedDeclaration,
};
use stages::{NameResolution, Scope};
pub use symbols::{
    Aggregate, AggregateFlags, Enum, Field, FieldFlags, FieldId, FieldIndex, FieldMap,
    FreeFunction, FreeFunctionFlags, FreeFunctionIndex, FunctionIndex, FunctionKind, FunctionMap,
    FunctionType, Method, MethodFlags, MethodId, MethodMap, Param, ParamFlags, QualifiedName,
    ReferenceSemantics, Symbols, TypeDef, TypeSchema, Visibility,
};
pub use sync::{BoxedError, Shared};
pub use types::{
    MonoType, RefType, Type, TypeApp, TypeId, TypeIndex, TypeInterner, TypeParamVar, Variance,
    predef,
};

type IndexMap<K, V, S = hashbrown::DefaultHashBuilder> = indexmap::IndexMap<K, V, S>;
type IndexSet<K, S = hashbrown::DefaultHashBuilder> = indexmap::IndexSet<K, S>;
#[cfg(not(feature = "sync"))]
type FrozenIndexSet<K, S = hashbrown::DefaultHashBuilder> = elsa::FrozenIndexSet<K, S>;

pub type TypeScope<'scope, 'ctx> = IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>;
pub type CompileErrorReporter<'ctx> = Reporter<Diagnostic<'ctx>>;
type LowerReporter<'id> = Reporter<LowerError<'id>>;

pub fn infer_from_sources<'ctx>(
    sources: &'ctx ast::SourceMap,
    symbols: Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    let mods = parse_files(sources, reporter);
    let evaluator = Evaluator::from_modules(&mods);
    let (unit, symbols) = process_sources(mods, symbols, evaluator, reporter, interner);
    (unit, symbols)
}

pub fn parse_files<'ctx>(
    sources: &'ctx ast::SourceMap,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Vec<ast::SourceModule<'ctx>> {
    let (module, errs) = parser::parse_modules(sources.files().map(|(id, f)| (id, f.source())));
    reporter.report_many(errs);
    module
}

pub fn parse_file<'ctx>(
    id: ast::FileId,
    file: &'ctx ast::File,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Option<ast::SourceModule<'ctx>> {
    let (module, errs) = parser::parse_module(file.source(), id);
    reporter.report_many(errs);
    module
}

/// Parse every source file independently, retaining its [`ast::FileId`].
///
/// Each file is parsed exactly like [`parse_file`] (lexing then parsing), so a
/// file's diagnostics stay adjacent and files keep their source order. Unlike
/// [`parse_files`], empty files are retained because their `FileId` cannot be
/// recovered from a module without a span.
pub fn parse_files_with_ids<'ctx>(
    sources: &'ctx ast::SourceMap,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Vec<(ast::FileId, ast::SourceModule<'ctx>)> {
    let results = parallel::map_ordered(sources.files().collect(), |(id, file)| {
        (id, parser::parse_module(file.source(), id))
    });
    let mut modules = Vec::new();
    for (id, (module, errs)) in results {
        reporter.report_many(errs);
        if let Some(module) = module {
            modules.push((id, module));
        }
    }
    modules
}

pub fn process_sources<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    process_sources_with_policy(modules, symbols, evaluator, reporter, interner, &[])
}

/// Analyze source AST modules with optional per-file semantic hooks.
///
/// Files absent from `policies` use [`SemanticPolicy::default`]. If a file occurs
/// more than once, its first entry wins. Policies follow source spans rather than
/// module processing order and never modify shared symbol flags. Reuse a `Shared`
/// handle across files to share a policy, including runtime-configured policies. Analysis
/// clones handles into a shared file index; lookups borrow policies without cloning.
///
/// Callers must provide accurate file IDs on AST spans and perform any additional
/// source-language capability validation. [`process_sources`] is equivalent to
/// calling this function with an empty policy slice.
pub fn process_sources_with_policy<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
    policies: &[(ast::FileId, Shared<SemanticPolicy>)],
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    analyze(
        modules,
        symbols,
        evaluator,
        reporter,
        interner,
        policies,
        Finish::Full,
    )
}

/// Analyze source AST modules for a single IDE query, lowering only the function
/// that encloses `target`.
///
/// This performs the same name resolution as [`process_sources`] (every type,
/// field, inheritance link and signature is available in the returned
/// [`Symbols`]) but skips lowering unrelated bodies and whole-program
/// validation. Equivalent to calling
/// [`process_sources_for_query_with_policy`] with an empty policy slice.
pub fn process_sources_for_query<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
    target: QueryTarget,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    process_sources_for_query_with_policy(
        modules,
        symbols,
        evaluator,
        reporter,
        interner,
        &[],
        target,
    )
}

/// Query-mode counterpart of [`process_sources_with_policy`]. Policies follow the
/// same rules, and the returned [`Symbols`] is complete even though only the
/// function enclosing `target` is lowered.
pub fn process_sources_for_query_with_policy<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
    policies: &[(ast::FileId, Shared<SemanticPolicy>)],
    target: QueryTarget,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    analyze(
        modules,
        symbols,
        evaluator,
        reporter,
        interner,
        policies,
        Finish::Query(target),
    )
}

/// How the shared name-resolution pipeline should finish.
enum Finish {
    /// Lower every body and run whole-program validation.
    Full,
    /// Lower only the function enclosing the target, skipping whole-program
    /// validation.
    Query(QueryTarget),
}

/// Runs name resolution and inference, shared by every `process_sources*` entry
/// point so the setup stays identical across full and query mode.
fn analyze<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
    policies: &[(ast::FileId, Shared<SemanticPolicy>)],
    finish: Finish,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    let mut scope = Scope::new(&symbols);
    scope.set_policies(policies);
    let mut resolution = NameResolution::new(modules, evaluator, symbols, reporter, interner);

    resolution.populate_globals(&mut scope);
    let inference = resolution.progress(&scope);
    match finish {
        Finish::Full => inference.finish(&scope, reporter),
        Finish::Query(target) => inference.finish_for_query(&scope, target, reporter),
    }
}

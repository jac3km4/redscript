//! Strict, deliberately partial REDmod semantic adapter for parsed IDE inputs.
//! Diagnostics belong to this crate; recovered syntax must not be treated as valid.

pub mod bridge;
mod conditions;
mod control_flow;
mod defaults;
mod metadata;
mod persistent;
mod policy;
mod semantic;
mod validate;

use std::collections::HashSet;
use std::fmt;

pub use bridge::{CapabilityError, ParsedModule};
pub use policy::{ArrayOperation, array_operations};
use redscript_community_ast::{FileId, SourceModule, Span};
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, Evaluator, LowerError, LoweredCompilationUnit, QueryTarget,
    Shared, Symbols, TypeInterner,
};
pub use semantic::SemanticError;
pub use validate::validate as validate_module;

#[derive(Debug)]
pub enum Error {
    Parse(redscript_redmod_parser::Error),
    Capability { kind: CapabilityError, span: Span },
    Semantic { kind: SemanticError, span: Span },
    Condition { kind: ConditionError, span: Span },
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::Parse(error) => error.span(),
            Self::Capability { span, .. }
            | Self::Semantic { span, .. }
            | Self::Condition { span, .. } => *span,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::Parse(_) => "REDMOD_PARSE",
            Self::Capability { .. } => "REDMOD_CAPABILITY",
            Self::Semantic { .. } | Self::Condition { .. } => "REDMOD_SEMANTICS",
        }
    }

    fn semantic(kind: SemanticError, span: Span) -> Self {
        Self::Semantic { kind, span }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => error.fmt(f),
            Self::Capability { kind, .. } => kind.fmt(f),
            Self::Semantic { kind, .. } => kind.fmt(f),
            Self::Condition { kind, .. } => kind.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

/// Failures in pre-resolution conditional-version selection.
#[derive(Debug, PartialEq, Eq)]
pub enum ConditionError {
    InvalidCondition,
    Evaluation(String),
    TooManyVersions { limit: usize },
    MultiplePositiveVersions,
}

impl fmt::Display for ConditionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCondition => f.write_str(
                "unsupported condition; use ModuleExists(\"module\") or !ModuleExists(\"module\")",
            ),
            Self::Evaluation(message) => f.write_str(message),
            Self::TooManyVersions { limit } => write!(
                f,
                "conditional declaration permits at most {limit} version(s)"
            ),
            Self::MultiplePositiveVersions => {
                f.write_str("conditional versions cannot both use positive ModuleExists")
            }
        }
    }
}

fn normalize_events(
    items: &mut [(redscript_community_ast::SourceItemDecl<'_>, Span)],
    native_aggregate: bool,
) {
    use redscript_community_ast as ast;
    for (decl, _) in items {
        match &mut decl.item {
            ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) => {
                normalize_events(
                    &mut aggregate.items,
                    decl.qualifiers
                        .intersects(ast::ItemQualifiers::NATIVE | ast::ItemQualifiers::IMPORT_ONLY),
                );
            }
            ast::Item::Function(function)
                if decl.qualifiers.contains(ast::ItemQualifiers::CALLBACK) =>
            {
                // REDmod events have a Bool slot even with an empty body. Official
                // fallthrough emits no return value; do not synthesize `false`.
                function.return_type = Some(Box::new((ast::Type::plain("Bool"), function.name.1)));
                // A semicolon event in a script class is an empty implementation,
                // unlike a callback prototype declared by an imported class.
                // Importonly types cannot acquire scripted implementations either.
                if !native_aggregate && function.body.is_none() {
                    function.body = Some(ast::FunctionBody::Block(ast::Block::new([])));
                }
            }
            _ => {}
        }
    }
}

// Official event bodies terminate with Nop, not a synthesized Bool return.
// Keep this dialect fixup out of the backend so community callbacks are unchanged.
fn finish_events<'ctx>(
    unit: &mut LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    files: &HashSet<FileId>,
) {
    use redscript_compiler_frontend::ir;
    let finish = |function: &mut redscript_compiler_frontend::LoweredFunction<'ctx>| {
        if files.contains(&function.span.file) {
            function
                .block
                .stmts
                .push_back(ir::Stmt::Expr(Box::new(ir::Expr::Nop(function.span))));
        }
    };
    for (id, function) in unit.functions_with_ids_mut() {
        let is_callback = id
            .as_method()
            .is_some_and(|method| symbols[method].flags().is_callback());
        if is_callback {
            finish(function);
        }
    }
}

/// Convenience syntax/capability entry point. Use `parse_file_with_metadata` and
/// `process_sources` for analysis so explicit override intent is not lost.
/// Loops requiring a typed fixup fail closed here: a bare shared AST cannot
/// preserve their continue/header semantics without the REDmod plan.
pub fn parse_file(source: &str, file: FileId) -> (Option<SourceModule<'_>>, Vec<Error>) {
    let (parsed, mut errors) = parse_file_with_metadata(source, file);
    let module = parsed.and_then(|parsed| {
        if let Some(plan) = parsed.control_flow.first() {
            let kind = match plan.kind {
                control_flow::LoopKind::For => CapabilityError::ForLoop,
                control_flow::LoopKind::DoWhile => CapabilityError::DoWhileLoop,
            };
            errors.push(Error::Capability {
                kind,
                span: plan.span,
            });
            None
        } else {
            Some(parsed.module)
        }
    });
    (module, errors)
}

pub fn parse_file_with_metadata(
    source: &str,
    file: FileId,
) -> (Option<ParsedModule<'_>>, Vec<Error>) {
    let (native, errors) = redscript_redmod_parser::parse_module(source, file);
    let mut errors: Vec<_> = errors.into_iter().map(Error::Parse).collect();
    // Never expose a recovered module to the shared compiler.
    if !errors.is_empty() {
        return (None, errors);
    }
    let parsed = native.and_then(|module| match bridge::lower(module) {
        Ok(mut parsed) => {
            defaults::retain_literal_text(&mut parsed.module.items, source);
            Some(parsed)
        }
        Err((kind, span)) => {
            errors.push(Error::Capability { kind, span });
            None
        }
    });
    if let Some(parsed) = &parsed {
        errors.extend(validate::validate(&parsed.module));
    }
    (parsed, errors)
}

/// Promotes resolved-IR array-prefix warnings for authored files into the
/// crate's distinct `REDMOD_CAPABILITY` contract.
///
/// Array-prefix safety is diagnosed once on resolved IR. Only the diagnostics
/// emitted by the current analysis are considered, so reusing a
/// [`CompileErrorReporter`] across calls cannot re-promote earlier failures.
fn collect_unsafe_array_capabilities(
    diagnostics: &[Diagnostic<'_>],
    files: &[FileId],
    errors: &mut Vec<Error>,
) {
    errors.extend(
        diagnostics
            .iter()
            .filter_map(|diagnostic| match diagnostic {
                Diagnostic::LoweringError(LowerError::UnsafeArrayEvaluation(span))
                    if files.contains(&span.file) =>
                {
                    Some(Error::Capability {
                        kind: CapabilityError::UnsafeArrayEvaluation,
                        span: *span,
                    })
                }
                _ => None,
            }),
    );
}

/// Inputs shared by [`process_sources`] and [`process_sources_for_query`] after
/// module validation, conditional selection and event normalization.
struct PreparedSources<'ctx> {
    /// Dependency modules followed by the surviving authored modules.
    sources: Vec<SourceModule<'ctx>>,
    /// Authored file IDs, in module order.
    files: Vec<FileId>,
    /// REDmod loop-header fixups from every authored module.
    control_flow: Vec<control_flow::LoopPlan>,
    /// Explicit `@override` spans from every authored module.
    overrides: HashSet<Span>,
}

/// Validates, conditionally selects and normalizes authored modules, then
/// assembles the shared source list and per-module metadata. Diagnostics are
/// appended to `errors`.
fn prepare_sources<'ctx>(
    modules: impl IntoIterator<Item = ParsedModule<'ctx>>,
    dependencies: impl IntoIterator<Item = SourceModule<'ctx>>,
    errors: &mut Vec<Error>,
) -> PreparedSources<'ctx> {
    let mut modules: Vec<_> = modules
        .into_iter()
        .filter(|parsed| {
            let diagnostics = validate::validate(&parsed.module);
            let valid = diagnostics.is_empty();
            errors.extend(diagnostics);
            valid
        })
        .collect();
    let dependencies: Vec<_> = dependencies.into_iter().collect();
    conditions::select(&mut modules, &dependencies, errors);
    for parsed in &mut modules {
        normalize_events(&mut parsed.module.items, false);
    }
    let overrides = modules
        .iter()
        .flat_map(|parsed| parsed.overrides.iter().copied())
        .collect();
    let files = modules
        .iter()
        .filter_map(|parsed| parsed.module.span().map(|span| span.file))
        .collect();
    let control_flow = modules
        .iter()
        .flat_map(|parsed| parsed.control_flow.iter().cloned())
        .collect();
    let sources = dependencies
        .into_iter()
        .chain(modules.into_iter().map(|parsed| parsed.module))
        .collect();
    PreparedSources {
        sources,
        files,
        control_flow,
        overrides,
    }
}

/// Analyze REDmod modules alongside already parsed community dependencies.
/// Capability failures exclude only the invalid module, rather than blocking
/// unrelated files or enabling community extensions. Shared diagnostics use `reporter`.
/// Callers must also retain/check diagnostics returned during parsing.
pub fn process_sources<'ctx>(
    modules: impl IntoIterator<Item = ParsedModule<'ctx>>,
    dependencies: impl IntoIterator<Item = SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>, Vec<Error>) {
    let mut errors = Vec::new();
    let PreparedSources {
        sources,
        files,
        control_flow,
        overrides,
    } = prepare_sources(modules, dependencies, &mut errors);
    let evaluator = Evaluator::from_modules(&sources);
    let policy = Shared::new(policy::POLICY);
    let policies: Vec<_> = files.iter().map(|&file| (file, policy.clone())).collect();
    let reported_start = reporter.reported().len();
    let (mut unit, symbols) = redscript_compiler_frontend::process_sources_with_policy(
        sources, symbols, evaluator, reporter, interner, &policies,
    );
    // Array-prefix safety is diagnosed once on resolved IR. Preserve the
    // crate's distinct REDMOD_CAPABILITY contract by promoting the frontend
    // warning for authored files to a capability failure.
    collect_unsafe_array_capabilities(&reporter.reported()[reported_start..], &files, &mut errors);
    control_flow::apply(&mut unit, &control_flow, &mut errors);
    let files: HashSet<_> = files.into_iter().collect();
    finish_events(&mut unit, &symbols, &files);
    validate::overrides(&unit, &symbols, &files, &overrides, &mut errors);
    persistent::validate(&unit, &symbols, &files, &mut errors);
    (unit, symbols, errors)
}

/// Query-mode counterpart of [`process_sources`] for IDE requests that inspect a
/// single function.
///
/// Conditional selection, capability validation, event normalization and the
/// REDmod loop-header fixups are retained, but only the function enclosing
/// `target` is lowered. Bytecode-only passes (`finish_events`), override
/// ownership and persistent validation are skipped.
pub fn process_sources_for_query<'ctx>(
    modules: impl IntoIterator<Item = ParsedModule<'ctx>>,
    dependencies: impl IntoIterator<Item = SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
    target: QueryTarget,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>, Vec<Error>) {
    let mut errors = Vec::new();
    let PreparedSources {
        sources,
        files,
        control_flow,
        ..
    } = prepare_sources(modules, dependencies, &mut errors);
    // `control_flow::apply` only traverses the single lowered function, so the
    // plan set stays tiny; keep only the target file's headers.
    let control_flow: Vec<_> = control_flow
        .into_iter()
        .filter(|plan| plan.span.file == target.file)
        .collect();
    let evaluator = Evaluator::from_modules(&sources);
    let policy = Shared::new(policy::POLICY);
    let policies: Vec<_> = files.iter().map(|&file| (file, policy.clone())).collect();
    let reported_start = reporter.reported().len();
    let (mut unit, symbols) = redscript_compiler_frontend::process_sources_for_query_with_policy(
        sources, symbols, evaluator, reporter, interner, &policies, target,
    );
    // Capability validation is retained in query mode: promote the frontend
    // warning for authored files exactly like `process_sources`.
    collect_unsafe_array_capabilities(&reporter.reported()[reported_start..], &files, &mut errors);
    control_flow::apply(&mut unit, &control_flow, &mut errors);
    (unit, symbols, errors)
}

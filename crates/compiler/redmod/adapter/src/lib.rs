//! Strict, deliberately partial REDmod semantic adapter for parsed IDE inputs.
//! Diagnostics belong to this crate; recovered syntax must not be treated as valid.

mod array_guard;
pub mod bridge;
mod conditions;
mod control_flow;
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
    CompileErrorReporter, Evaluator, LoweredCompilationUnit, Symbols, TypeInterner,
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
                "condition must be ModuleExists(\"module\") or !ModuleExists(\"module\")",
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

fn normalize_events(items: &mut [(redscript_community_ast::SourceItemDecl<'_>, Span)]) {
    use redscript_community_ast as ast;
    for (decl, _) in items {
        match &mut decl.item {
            ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) => {
                normalize_events(&mut aggregate.items);
            }
            ast::Item::Function(function)
                if decl.qualifiers.contains(ast::ItemQualifiers::CALLBACK) =>
            {
                // The shared frontend infers omitted returns. A source event is always
                // Void, including bodies that erroneously return a value.
                function.return_type = Some(Box::new((ast::Type::plain("Void"), function.name.1)));
            }
            _ => {}
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
        Ok(parsed) => Some(parsed),
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
    conditions::select(&mut modules, &dependencies, &mut errors);
    for parsed in &mut modules {
        normalize_events(&mut parsed.module.items);
    }
    let overrides: HashSet<_> = modules
        .iter()
        .flat_map(|parsed| parsed.overrides.iter().copied())
        .collect();
    let files: Vec<_> = modules
        .iter()
        .filter_map(|parsed| parsed.module.span().map(|span| span.file))
        .collect();
    let control_flow: Vec<_> = modules
        .iter()
        .flat_map(|parsed| parsed.control_flow.iter().cloned())
        .collect();
    let sources: Vec<_> = dependencies
        .into_iter()
        .chain(modules.into_iter().map(|parsed| parsed.module))
        .collect();
    let evaluator = Evaluator::from_modules(&sources);
    let policies: Vec<_> = files.iter().map(|&file| (file, policy::POLICY)).collect();
    let (mut unit, symbols) = redscript_compiler_frontend::process_sources_with_policy(
        sources, symbols, evaluator, reporter, interner, &policies,
    );
    control_flow::apply(&mut unit, &control_flow, &mut errors);
    let files = files.into_iter().collect();
    validate::overrides(&unit, &symbols, &files, &overrides, &mut errors);
    persistent::validate(&unit, &symbols, &files, &mut errors);
    (unit, symbols, errors)
}

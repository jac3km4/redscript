use std::borrow::Cow;
use std::path::Path;
use std::{fmt, io};

use bon::bon;
pub use redscript_community_ast as ast;
use redscript_community_ast::SourceMap;
use redscript_compiler_backend::{AssembleError, PoolError, PoolMappings};
pub use redscript_compiler_backend::{CompilationInputs, TypeFlagRegistry, TypeFlags};
use redscript_compiler_frontend::pass::{DiagnosticPass, UnusedLocals};
pub use redscript_compiler_frontend::{
    Aggregate, CompileErrorReporter, Diagnostic, DiagnosticLevel, Enum, Evaluator, Field, FieldId,
    FunctionType, LowerError, LoweredCompilationUnit, LoweredFunction, MethodId, PolyType,
    QueryError, Symbols, TypeError, TypeId, TypeIndex, TypeInterner, TypeSchema, TypeScope,
    UnknownSource, expr_type, infer_from_sources, ir, parse_file, parse_files, pass,
    process_sources, types,
};
use redscript_io::byte;
pub use redscript_io::{SaveError, ScriptBundle, WriteableBundle};
use thiserror::Error;

const DEFAULT_DIAGNOSTICS: &[&'static dyn DiagnosticPass] = &[&UnusedLocals];

/// Semantic checking results, without backend mappings or generated output.
///
/// This can be built from default symbols for standalone sources, or from symbols
/// loaded with `CompilationInputs::load_without_mapping` for an existing bundle.
pub struct CheckedSources<'ctx> {
    unit: LoweredCompilationUnit<'ctx>,
    symbols: Symbols<'ctx>,
    diagnostics: Diagnostics<'ctx>,
    fatal_diagnostic_level: DiagnosticLevel,
}

#[bon]
impl<'ctx> CheckedSources<'ctx> {
    #[builder(finish_fn = check)]
    pub fn new(
        sources: &'ctx SourceMap,
        symbols: Symbols<'ctx>,
        type_interner: &'ctx TypeInterner,
        #[builder(default = DEFAULT_DIAGNOSTICS)] diagnostics: &[&'static dyn DiagnosticPass],
        #[builder(default = DiagnosticLevel::ErrorAllowedAtRuntime)]
        fatal_diagnostic_level: DiagnosticLevel,
    ) -> Self {
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(sources, &mut reporter);
        Self::from_modules()
            .modules(modules)
            .parse_diagnostics(reporter.into_reported())
            .symbols(symbols)
            .type_interner(type_interner)
            .diagnostics(diagnostics)
            .fatal_diagnostic_level(fatal_diagnostic_level)
            .check()
    }

    /// Checks parsed modules, retaining parse diagnostics even for files without an AST.
    /// Conditional compilation uses exactly the supplied modules.
    #[builder(finish_fn = check)]
    pub fn from_modules(
        modules: Vec<ast::SourceModule<'ctx>>,
        #[builder(default)] parse_diagnostics: Vec<Diagnostic<'ctx>>,
        symbols: Symbols<'ctx>,
        type_interner: &'ctx TypeInterner,
        #[builder(default = DEFAULT_DIAGNOSTICS)] diagnostics: &[&'static dyn DiagnosticPass],
        #[builder(default = DiagnosticLevel::ErrorAllowedAtRuntime)]
        fatal_diagnostic_level: DiagnosticLevel,
    ) -> Self {
        let mut reporter = CompileErrorReporter::default();
        reporter.report_many(parse_diagnostics);
        let evaluator = Evaluator::from_modules(&modules);
        let (unit, symbols) =
            process_sources(modules, symbols, evaluator, &mut reporter, type_interner);
        unit.run_diagnostics(diagnostics, &mut reporter);
        let mut diagnostics = reporter.into_reported();
        diagnostics.sort_by_key(Diagnostic::is_error);
        Self {
            unit,
            symbols,
            diagnostics: Diagnostics(diagnostics),
            fatal_diagnostic_level,
        }
    }

    pub fn unit(&self) -> &LoweredCompilationUnit<'ctx> {
        &self.unit
    }

    pub fn symbols(&self) -> &Symbols<'ctx> {
        &self.symbols
    }

    pub fn diagnostics(&self) -> &Diagnostics<'ctx> {
        &self.diagnostics
    }

    /// Whether source diagnostics block emission under the configured policy.
    /// Code generation may still fail independently.
    pub fn has_fatal_errors(&self) -> bool {
        self.diagnostics
            .has_fatal_errors(self.fatal_diagnostic_level)
    }

    pub fn fatal_diagnostic_level(&self) -> DiagnosticLevel {
        self.fatal_diagnostic_level
    }

    pub fn into_parts(
        self,
    ) -> (
        LoweredCompilationUnit<'ctx>,
        Symbols<'ctx>,
        Diagnostics<'ctx>,
    ) {
        (self.unit, self.symbols, self.diagnostics)
    }
}

pub struct Compilation<'ctx> {
    sources: &'ctx SourceMap,
    symbols: Symbols<'ctx>,
    mappings: PoolMappings<'ctx>,
    unit: LoweredCompilationUnit<'ctx>,
    bundle: ScriptBundle<'ctx>,
    diagnostics: Diagnostics<'ctx>,
    fatal_diagnostic_level: DiagnosticLevel,
}

#[bon]
impl<'ctx> Compilation<'ctx> {
    #[builder(finish_fn = compile)]
    pub fn new(
        bundle: &'ctx [u8],
        sources: &'ctx SourceMap,
        type_interner: &'ctx TypeInterner,
        #[builder(default = Cow::Owned(Default::default()))] type_flags: Cow<'_, TypeFlagRegistry>,
        #[builder(default = DEFAULT_DIAGNOSTICS)] diagnostics: &[&'static dyn DiagnosticPass],
        #[builder(default = DiagnosticLevel::ErrorAllowedAtRuntime)]
        fatal_diagnostic_level: DiagnosticLevel,
    ) -> Result<Self, Error> {
        let bundle = ScriptBundle::from_bytes(bundle)?;
        let (symbols, mappings) =
            CompilationInputs::load(&bundle, type_interner, &type_flags)?.into_inner();
        let checked = CheckedSources::builder()
            .sources(sources)
            .symbols(symbols)
            .type_interner(type_interner)
            .diagnostics(diagnostics)
            .fatal_diagnostic_level(fatal_diagnostic_level)
            .check();

        Ok(Self {
            sources,
            symbols: checked.symbols,
            mappings,
            unit: checked.unit,
            bundle,
            diagnostics: checked.diagnostics,
            fatal_diagnostic_level,
        })
    }

    /// Generates the output bundle without performing filesystem I/O.
    pub fn emit(mut self) -> Result<CompilationArtifact<'ctx>, EmitError<'ctx>> {
        if self.has_fatal_errors() {
            return Err(EmitError::FatalErrors(self.diagnostics, self.symbols));
        }

        let mut monomorph = self.mappings.into_monomorphizer(self.sources);
        if let Err(err) = monomorph.monomorphize(&self.unit, &self.symbols, &mut self.bundle) {
            if let Some(span) = err.span() {
                self.diagnostics.push(Diagnostic::Other(
                    Box::new(err),
                    DiagnosticLevel::Error,
                    span,
                ));
                return Err(EmitError::FatalErrors(self.diagnostics, self.symbols));
            }
            return Err(EmitError::Assemble(err));
        }

        Ok(CompilationArtifact {
            bundle: self.bundle.into_writeable(),
            symbols: self.symbols,
            diagnostics: self.diagnostics,
        })
    }

    /// Generates and saves the output bundle.
    pub fn flush(
        self,
        path: impl AsRef<Path>,
    ) -> Result<(Symbols<'ctx>, Diagnostics<'ctx>), FlushError<'ctx>> {
        let artifact = self.emit()?;
        artifact.bundle.save(path)?;
        Ok((artifact.symbols, artifact.diagnostics))
    }

    pub fn symbols(&self) -> &Symbols<'ctx> {
        &self.symbols
    }

    pub fn unit(&self) -> &LoweredCompilationUnit<'ctx> {
        &self.unit
    }

    pub fn bundle(&self) -> &ScriptBundle<'ctx> {
        &self.bundle
    }

    pub fn diagnostics(&self) -> &Diagnostics<'ctx> {
        &self.diagnostics
    }

    /// Whether source diagnostics block emission under the configured policy.
    /// Code generation may still fail independently.
    pub fn has_fatal_errors(&self) -> bool {
        self.diagnostics
            .has_fatal_errors(self.fatal_diagnostic_level)
    }

    pub fn fatal_diagnostic_level(&self) -> DiagnosticLevel {
        self.fatal_diagnostic_level
    }
}

/// Generated output and the semantic results that produced it.
pub struct CompilationArtifact<'ctx> {
    pub bundle: WriteableBundle<'ctx>,
    pub symbols: Symbols<'ctx>,
    pub diagnostics: Diagnostics<'ctx>,
}

#[derive(Debug, Error)]
pub enum EmitError<'ctx> {
    #[error("fatal errors found")]
    FatalErrors(Diagnostics<'ctx>, Symbols<'ctx>),
    #[error("code generation error: {0}")]
    Assemble(AssembleError<'ctx>),
}

impl<'ctx> From<EmitError<'ctx>> for FlushError<'ctx> {
    fn from(error: EmitError<'ctx>) -> Self {
        match error {
            EmitError::FatalErrors(diagnostics, symbols) => Self::FatalErrors(diagnostics, symbols),
            EmitError::Assemble(error) => Self::Assemble(error),
        }
    }
}

#[derive(Debug)]
pub struct Diagnostics<'ctx>(Vec<Diagnostic<'ctx>>);

impl<'ctx> Diagnostics<'ctx> {
    pub fn has_fatal_errors(&self, fatal_diagnostic_level: DiagnosticLevel) -> bool {
        self.0.iter().any(|d| d.level() >= fatal_diagnostic_level)
    }

    /// Selects diagnostics and orders them by path, span, code, and message.
    /// Storage order is unchanged. The predicate is called in storage order,
    /// before calculating sort keys, and only selected diagnostics are formatted.
    pub fn source_ordered(
        &self,
        sources: &SourceMap,
        filter: impl Into<DiagnosticFilter>,
    ) -> Vec<&Diagnostic<'ctx>> {
        let filter = filter.into();
        let mut diagnostics = self
            .into_iter()
            .filter(|diagnostic| filter.check(diagnostic))
            .collect::<Vec<_>>();
        diagnostics.sort_by_cached_key(|diagnostic| {
            let span = diagnostic.span();
            (
                sources.get(span.file).map(ast::File::path),
                span.start,
                span.end,
                diagnostic.code(),
                diagnostic.to_string(),
            )
        });

        diagnostics
    }

    /// Renders source-ordered diagnostics and their summary without global logging.
    /// Counts include only diagnostics selected by the filter.
    pub fn write_to(
        &self,
        mut writer: impl io::Write,
        sources: &SourceMap,
        filter: impl Into<DiagnosticFilter>,
    ) -> Result<DiagnosticSummary, DiagnosticRenderError> {
        let mut summary = DiagnosticSummary::default();
        for diagnostic in self.source_ordered(sources, filter) {
            writeln!(writer, "{}", diagnostic.display(sources)?)?;
            summary.record(diagnostic);
        }
        writeln!(writer, "{summary}")?;
        Ok(summary)
    }

    pub fn dump(
        &self,
        sources: &SourceMap,
        filter: impl Into<DiagnosticFilter>,
    ) -> Result<(), UnknownSource> {
        let mut summary = DiagnosticSummary::default();
        for diagnostic in self.source_ordered(sources, filter) {
            if diagnostic.is_error() {
                log::error!(target: "redscript::diagnostic", "{}", diagnostic.display(sources)?);
                summary.errors += 1;
            } else {
                log::warn!(target: "redscript::diagnostic", "{}", diagnostic.display(sources)?);
                summary.warnings += 1;
            }
        }
        log::info!("{summary}");
        Ok(())
    }

    fn push(&mut self, diagnostic: Diagnostic<'ctx>) {
        self.0.push(diagnostic);
    }
}

impl<'ctx> IntoIterator for Diagnostics<'ctx> {
    type IntoIter = std::vec::IntoIter<Diagnostic<'ctx>>;
    type Item = Diagnostic<'ctx>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, 'ctx> IntoIterator for &'a Diagnostics<'ctx> {
    type IntoIter = std::slice::Iter<'a, Diagnostic<'ctx>>;
    type Item = &'a Diagnostic<'ctx>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'ctx> From<Vec<Diagnostic<'ctx>>> for Diagnostics<'ctx> {
    fn from(diagnostics: Vec<Diagnostic<'ctx>>) -> Self {
        Self(diagnostics)
    }
}

impl fmt::Display for Diagnostics<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.into_iter().try_for_each(|d| writeln!(f, "{d}"))
    }
}

impl<'ctx> AsRef<[Diagnostic<'ctx>]> for Diagnostics<'ctx> {
    fn as_ref(&self) -> &[Diagnostic<'ctx>] {
        &self.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticSummary {
    pub warnings: usize,
    pub errors: usize,
}

impl DiagnosticSummary {
    fn record(&mut self, diagnostic: &Diagnostic<'_>) {
        if diagnostic.is_error() {
            self.errors += 1;
        } else {
            self.warnings += 1;
        }
    }
}

impl fmt::Display for DiagnosticSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Completed with {} warnings and {} errors",
            self.warnings, self.errors
        )
    }
}

#[derive(Debug, Error)]
pub enum DiagnosticRenderError {
    #[error("{0}")]
    UnknownSource(#[from] UnknownSource),
    #[error("diagnostic output error: {0}")]
    Write(#[from] io::Error),
}

pub enum DiagnosticFilter {
    Minimum(DiagnosticLevel),
    Predicate(Box<dyn for<'a, 'b> Fn(&'a Diagnostic<'b>) -> bool>),
}

impl DiagnosticFilter {
    pub fn check(&self, diagnostic: &Diagnostic<'_>) -> bool {
        match self {
            DiagnosticFilter::Minimum(minimum) => diagnostic.level() >= *minimum,
            DiagnosticFilter::Predicate(predicate) => predicate(diagnostic),
        }
    }
}

impl From<DiagnosticLevel> for DiagnosticFilter {
    fn from(level: DiagnosticLevel) -> Self {
        Self::Minimum(level)
    }
}

impl From<Box<dyn for<'a, 'b> Fn(&'a Diagnostic<'b>) -> bool>> for DiagnosticFilter {
    fn from(predicate: Box<dyn for<'a> Fn(&Diagnostic<'a>) -> bool>) -> Self {
        Self::Predicate(predicate)
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("pool error: {0}")]
    Pool(#[from] PoolError),
    #[error("serialization error: {0}")]
    Decoding(#[from] byte::Error),
}

#[derive(Debug, Error)]
pub enum FlushError<'ctx> {
    #[error("fatal errors found")]
    FatalErrors(Diagnostics<'ctx>, Symbols<'ctx>),
    #[error("code generation error: {0}")]
    Assemble(AssembleError<'ctx>),
    #[error("write error: {0}")]
    Write(#[from] SaveError),
}

impl<'ctx> From<AssembleError<'ctx>> for FlushError<'ctx> {
    fn from(err: AssembleError<'ctx>) -> Self {
        Self::Assemble(err)
    }
}

pub trait SourceMapExt {
    fn populate_boot_lib(&self);
}

impl SourceMapExt for SourceMap {
    fn populate_boot_lib(&self) {
        self.push_front(
            "boot.reds",
            include_str!("../../../../assets/reds/boot.reds"),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_order_sorts_by_path_span_code_and_message_without_reordering_storage() {
        let sources = SourceMap::new();
        let z = sources.push_back("z.reds", "test");
        let a = sources.push_back("a.reds", "test");
        let diagnostics = Diagnostics::from(vec![
            Diagnostic::UnusedLocal(ast::Span::new(0, 1, z)),
            Diagnostic::UnusedLocal(ast::Span::new(2, 3, a)),
            Diagnostic::UnknownIntrinsic("z", ast::Span::new(0, 2, a)),
            Diagnostic::UnknownIntrinsic("a", ast::Span::new(0, 2, a)),
            Diagnostic::MissingFunctionBody(ast::Span::new(0, 2, a)),
            Diagnostic::UnusedLocal(ast::Span::new(0, 1, a)),
        ]);
        let ordered = diagnostics.source_ordered(&sources, DiagnosticLevel::Pedantic);
        assert_eq!(
            ordered.iter().map(ToString::to_string).collect::<Vec<_>>(),
            [5, 3, 2, 4, 1, 0].map(|i| diagnostics.0[i].to_string())
        );
        assert_eq!(diagnostics.0[0].span().file, z);
    }

    #[test]
    fn rendering_filters_before_formatting_and_counts_selected_diagnostics() {
        #[derive(Debug)]
        struct NeverFormat;
        impl fmt::Display for NeverFormat {
            fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
                panic!("filtered diagnostic must not be formatted");
            }
        }
        impl std::error::Error for NeverFormat {}

        let sources = SourceMap::new();
        let z = sources.push_back("z.reds", "test");
        let a = sources.push_back("a.reds", "test");
        let diagnostics = Diagnostics::from(vec![
            Diagnostic::MissingFunctionBody(ast::Span::new(0, 1, z)),
            Diagnostic::UnusedLocal(ast::Span::new(0, 1, a)),
            Diagnostic::Other(
                Box::new(NeverFormat),
                DiagnosticLevel::Pedantic,
                ast::Span::new(0, 1, a),
            ),
        ]);
        let mut output = Vec::new();
        let summary = diagnostics
            .write_to(&mut output, &sources, DiagnosticLevel::Warning)
            .unwrap();
        assert_eq!(
            summary,
            DiagnosticSummary {
                warnings: 1,
                errors: 1
            }
        );
        let output = String::from_utf8(output).unwrap();
        assert!(output.find("a.reds").unwrap() < output.find("z.reds").unwrap());
        assert!(output.contains("warning[UNUSED_LOCAL]"));
        assert!(output.ends_with("Completed with 1 warnings and 1 errors\n"));
    }

    #[test]
    fn rendering_propagates_source_and_writer_errors() {
        let sources = SourceMap::new();
        let file = sources.push_back("test.reds", "test");
        let diagnostics =
            Diagnostics::from(vec![Diagnostic::UnusedLocal(ast::Span::new(0, 1, file))]);
        let mut output = Vec::new();
        assert!(matches!(
            diagnostics.write_to(&mut output, &SourceMap::new(), DiagnosticLevel::Warning),
            Err(DiagnosticRenderError::UnknownSource(_))
        ));
        let no_space: &mut [u8] = &mut [];
        assert!(matches!(
            diagnostics.write_to(no_space, &sources, DiagnosticLevel::Warning),
            Err(DiagnosticRenderError::Write(_))
        ));
        let mut output = Vec::new();
        let summary = diagnostics
            .write_to(&mut output, &sources, DiagnosticLevel::Error)
            .unwrap();
        assert_eq!(summary, DiagnosticSummary::default());
        assert_eq!(output, b"Completed with 0 warnings and 0 errors\n");
    }

    #[test]
    fn fatal_policy_is_shared_by_checking_compilation_and_emission() {
        for (threshold, fatal) in [
            (DiagnosticLevel::ErrorAllowedAtRuntime, true),
            (DiagnosticLevel::Error, false),
        ] {
            let sources = SourceMap::new();
            let file = sources.push_back("policy.reds", "func Test() {}\n");
            let interner = TypeInterner::default();
            let diagnostic = || {
                Diagnostic::Other(
                    Box::new(std::io::Error::other("runtime-allowed error")),
                    DiagnosticLevel::ErrorAllowedAtRuntime,
                    ast::Span::new(0, 1, file),
                )
            };
            let mut checked = CheckedSources::builder()
                .sources(&sources)
                .symbols(Symbols::with_default_types())
                .type_interner(&interner)
                .fatal_diagnostic_level(threshold)
                .check();
            checked.diagnostics.push(diagnostic());
            assert_eq!(checked.has_fatal_errors(), fatal);
            assert_eq!(checked.fatal_diagnostic_level(), threshold);

            let mut compilation = compile(&sources, &interner);
            compilation.fatal_diagnostic_level = threshold;
            compilation.diagnostics.push(diagnostic());
            assert_eq!(compilation.has_fatal_errors(), fatal);
            assert_eq!(compilation.fatal_diagnostic_level(), threshold);
            assert_eq!(
                matches!(compilation.emit(), Err(EmitError::FatalErrors(..))),
                fatal
            );
        }
    }

    #[test]
    fn check_without_a_bundle_runs_diagnostics_and_retains_semantic_results() {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back("check.reds", "func Test() { let unused: Int32 = 1; }\n");
        let interner = TypeInterner::default();
        let checked = CheckedSources::builder()
            .sources(&sources)
            .symbols(Symbols::with_default_types())
            .type_interner(&interner)
            .check();
        assert!(checked.unit().all_functions().next().is_some());
        assert!(
            checked
                .symbols()
                .free_functions()
                .any(|f| f.name().to_string() == "Test")
        );
        assert!(
            checked
                .diagnostics()
                .into_iter()
                .any(|d| matches!(d, Diagnostic::UnusedLocal(_)))
        );
        assert!(
            !checked
                .diagnostics()
                .has_fatal_errors(checked.fatal_diagnostic_level())
        );
    }

    #[test]
    fn check_returns_source_errors_without_failing_setup() {
        let sources = SourceMap::new();
        sources.push_back("invalid.reds", "func Test() { missing(); }\n");
        let interner = TypeInterner::default();
        let checked = CheckedSources::builder()
            .sources(&sources)
            .symbols(Symbols::with_default_types())
            .type_interner(&interner)
            .diagnostics(&[])
            .check();
        assert!(
            checked
                .diagnostics()
                .has_fatal_errors(checked.fatal_diagnostic_level())
        );
    }

    #[test]
    fn checking_sources_and_parsed_modules_is_equivalent() {
        // Keep this fixture self-contained: boot-library closures contain inference
        // variable identities that differ between otherwise equivalent compilations.
        let sources = SourceMap::new();
        sources.push_back("valid.reds", "func Valid() { let unused: Int32 = 1; }\n");
        sources.push_back("invalid.reds", "func Invalid() { missing(); }\n");
        sources.push_back("recovered.reds", "func Recovered() { return }\n");
        let interner = TypeInterner::default();
        for passes in [DEFAULT_DIAGNOSTICS, &[]] {
            for threshold in [DiagnosticLevel::Warning, DiagnosticLevel::Error] {
                let from_sources = CheckedSources::builder()
                    .sources(&sources)
                    .symbols(Symbols::with_default_types())
                    .type_interner(&interner)
                    .diagnostics(passes)
                    .fatal_diagnostic_level(threshold)
                    .check();
                let mut reporter = CompileErrorReporter::default();
                let modules = parse_files(&sources, &mut reporter);
                assert!(!reporter.reported().is_empty());
                let from_modules = CheckedSources::from_modules()
                    .modules(modules)
                    .parse_diagnostics(reporter.into_reported())
                    .symbols(Symbols::with_default_types())
                    .type_interner(&interner)
                    .diagnostics(passes)
                    .fatal_diagnostic_level(threshold)
                    .check();
                assert_eq!(
                    format!("{:?}", from_sources.unit()),
                    format!("{:?}", from_modules.unit())
                );
                assert_eq!(function_names(&from_sources), function_names(&from_modules));
                let diagnostics = |checked: &CheckedSources<'_>| {
                    checked
                        .diagnostics()
                        .into_iter()
                        .map(|d| (d.code(), d.level(), d.span(), d.to_string()))
                        .collect::<Vec<_>>()
                };
                assert_eq!(diagnostics(&from_sources), diagnostics(&from_modules));
                assert_eq!(from_modules.fatal_diagnostic_level(), threshold);
                assert_eq!(
                    from_sources.has_fatal_errors(),
                    from_modules.has_fatal_errors()
                );
                assert_eq!(
                    from_modules
                        .diagnostics()
                        .into_iter()
                        .any(|d| matches!(d, Diagnostic::UnusedLocal(_))),
                    !passes.is_empty()
                );
                assert!(
                    function_names(&from_modules)
                        .iter()
                        .any(|name| name == "Recovered")
                );
            }
        }
    }

    #[test]
    fn checking_modules_retains_parse_errors_without_an_ast() {
        let sources = SourceMap::new();
        let file = sources.push_back("incomplete.reds", "func Test(");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        assert!(modules.is_empty());
        let expected = reporter
            .reported()
            .iter()
            .map(|d| (d.span(), d.to_string()))
            .collect::<Vec<_>>();
        assert!(!expected.is_empty());
        let checked = CheckedSources::from_modules()
            .modules(modules)
            .parse_diagnostics(reporter.into_reported())
            .symbols(Symbols::with_default_types())
            .type_interner(&interner)
            .check();
        assert!(checked.unit().all_functions().next().is_none());
        assert!(checked.has_fatal_errors());
        assert_eq!(
            checked
                .diagnostics()
                .into_iter()
                .map(|d| {
                    assert!(matches!(d, Diagnostic::SyntaxError(_)));
                    assert_eq!(d.span().file, file);
                    (d.span(), d.to_string())
                })
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn checking_empty_modules_preserves_symbols_without_diagnostics() {
        let interner = TypeInterner::default();
        let checked = CheckedSources::from_modules()
            .modules(Vec::new())
            .symbols(Symbols::with_default_types())
            .type_interner(&interner)
            .check();
        assert!(checked.unit().all_functions().next().is_none());
        assert!(checked.diagnostics().as_ref().is_empty());
        assert!(!checked.has_fatal_errors());
        assert_eq!(
            checked.fatal_diagnostic_level(),
            DiagnosticLevel::ErrorAllowedAtRuntime
        );
        assert_eq!(
            format!("{:?}", checked.symbols()),
            format!("{:?}", Symbols::with_default_types())
        );
    }

    #[test]
    fn module_conditions_use_only_the_supplied_modules_including_empty_ones() {
        let sources = SourceMap::new();
        sources.push_back("empty.reds", "module Active.Empty\n");
        sources.push_back("stale.reds", "module Stale\n");
        sources.push_back(
            "conditions.reds",
            r#"
@if(ModuleExists("Active.Empty"))
func Present() {}
@if(!ModuleExists("Active.Empty"))
func Absent() {}
@if(ModuleExists("Stale"))
func Stale() {}
@if(true)
func Always() {}
@if(false)
func Never() {}
"#,
        );
        let interner = TypeInterner::default();
        for include_empty in [true, false] {
            let mut reporter = CompileErrorReporter::default();
            let mut modules = parse_files(&sources, &mut reporter);
            assert!(reporter.reported().is_empty());
            modules.retain(|module| {
                module.path.as_ref().is_none_or(|path| {
                    include_empty && path.segments.as_ref() == ["Active", "Empty"]
                })
            });
            let checked = CheckedSources::from_modules()
                .modules(modules)
                .symbols(Symbols::with_default_types())
                .type_interner(&interner)
                .check();
            assert!(
                checked.diagnostics().as_ref().is_empty(),
                "{}",
                checked.diagnostics()
            );
            let mut names = function_names(&checked);
            names.sort();
            assert_eq!(
                names,
                if include_empty {
                    ["Always", "Present"]
                } else {
                    ["Absent", "Always"]
                }
            );
            assert_eq!(checked.unit().all_functions().count(), 2);
        }
    }

    fn function_names(checked: &CheckedSources<'_>) -> Vec<String> {
        checked
            .symbols()
            .free_functions()
            .map(|f| f.name().to_string())
            .collect()
    }

    #[test]
    fn emit_produces_roundtrippable_bytes_without_saving() {
        let sources = SourceMap::new();
        sources.push_back("valid.reds", "func Test() {}\n");
        let interner = TypeInterner::default();
        let artifact = compile(&sources, &interner).emit().unwrap();
        let bytes = artifact.bundle.to_bytes().unwrap();
        let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
        assert!(bundle.indexed_definitions().count() > 0);
        assert!(
            !artifact
                .diagnostics
                .has_fatal_errors(DiagnosticLevel::Error)
        );
    }

    #[test]
    fn flush_propagates_spanless_assembly_errors_without_writing() {
        let sources = SourceMap::default();
        sources.push_front(
            "large.reds",
            format!(
                "func Test() {{ while true {{ {} }} }}\n",
                "true;".repeat(33_000)
            ),
        );
        let interner = TypeInterner::default();
        let compilation = compile(&sources, &interner);
        let path = std::env::temp_dir().join(format!(
            "redscript-flush-{}-{}.redscripts",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::write(&path, b"unchanged").unwrap();

        let result = compilation.flush(&path);
        let contents = std::fs::read(&path).unwrap();
        std::fs::remove_file(path).unwrap();

        assert!(matches!(
            result,
            Err(FlushError::Assemble(AssembleError::LabelTooDistant))
        ));
        assert_eq!(contents, b"unchanged");
    }

    #[test]
    fn flush_reports_spanned_assembly_errors() {
        let sources = SourceMap::default();
        sources.push_front("invalid.reds", "func Test() { continue; }\n");
        let interner = TypeInterner::default();
        let compilation = compile(&sources, &interner);
        let result = compilation.flush(std::env::temp_dir());
        let Err(FlushError::FatalErrors(diagnostics, _)) = result else {
            panic!("expected fatal diagnostics");
        };
        assert!(diagnostics.has_fatal_errors(DiagnosticLevel::Error));
        let mut output = Vec::new();
        diagnostics
            .write_to(&mut output, &sources, DiagnosticLevel::Error)
            .unwrap();
        insta::assert_snapshot!(String::from_utf8(output).unwrap());
    }

    fn compile<'ctx>(sources: &'ctx SourceMap, interner: &'ctx TypeInterner) -> Compilation<'ctx> {
        let bundle = ScriptBundle::default();
        let (symbols, mappings) =
            CompilationInputs::load(&bundle, interner, &TypeFlagRegistry::default())
                .unwrap()
                .into_inner();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(sources, symbols, &mut reporter, interner);
        let diagnostics = Diagnostics::from(reporter.into_reported());
        assert!(
            !diagnostics.has_fatal_errors(DiagnosticLevel::Error),
            "{diagnostics}"
        );
        Compilation {
            sources,
            symbols,
            mappings,
            unit,
            bundle,
            diagnostics,
            fatal_diagnostic_level: DiagnosticLevel::Error,
        }
    }
}

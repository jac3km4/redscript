use std::borrow::Cow;
use std::fmt;
use std::path::Path;

use bon::bon;
pub use redscript_ast as ast;
use redscript_ast::SourceMap;
use redscript_compiler_backend::{AssembleError, PoolError, PoolMappings};
pub use redscript_compiler_backend::{CompilationInputs, TypeFlagRegistry, TypeFlags};
use redscript_compiler_frontend::UnknownSource;
use redscript_compiler_frontend::pass::{DiagnosticPass, UnusedLocals};
pub use redscript_compiler_frontend::{
    Aggregate, CompileErrorReporter, Diagnostic, DiagnosticLevel, Enum, Evaluator, Field, FieldId,
    FunctionType, LowerError, LoweredCompilationUnit, LoweredFunction, MethodId, PolyType,
    QueryError, Symbols, TypeError, TypeId, TypeIndex, TypeInterner, TypeSchema, TypeScope,
    expr_type, infer_from_sources, ir, parse_file, parse_files, pass, process_sources, types,
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
        let (unit, symbols) = infer_from_sources(sources, symbols, &mut reporter, type_interner);
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

    pub fn dump(
        &self,
        sources: &SourceMap,
        filter: impl Into<DiagnosticFilter>,
    ) -> Result<(), UnknownSource> {
        let mut warnings = 0;
        let mut errors = 0;
        let filter = filter.into();

        let mut diagnostics = self.into_iter().collect::<Vec<_>>();
        diagnostics.sort_by_cached_key(|diagnostic| {
            let span = diagnostic.span();
            (
                sources.get(span.file).map(|file| file.path()),
                span.start,
                span.end,
                diagnostic.code(),
                diagnostic.to_string(),
            )
        });

        for diagnostic in diagnostics {
            if !filter.check(diagnostic) {
                continue;
            }

            if diagnostic.is_error() {
                log::error!(target: "redscript::diagnostic", "{}", diagnostic.display(sources)?);
                errors += 1;
            } else {
                log::warn!(target: "redscript::diagnostic", "{}", diagnostic.display(sources)?);
                warnings += 1;
            }
        }
        log::info!("Completed with {warnings} warnings and {errors} errors");
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
    fn dump_sorts_by_path_span_code_and_message_without_reordering_storage() {
        use std::cell::RefCell;
        use std::rc::Rc;

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
        let observed = Rc::new(RefCell::new(Vec::new()));
        let output = observed.clone();
        let filter = DiagnosticFilter::Predicate(Box::new(move |diagnostic| {
            output.borrow_mut().push(diagnostic.to_string());
            false
        }));

        diagnostics.dump(&sources, filter).unwrap();

        assert_eq!(
            *observed.borrow(),
            [5, 3, 2, 4, 1, 0].map(|i| diagnostics.0[i].to_string())
        );
        assert_eq!(diagnostics.0[0].span().file, z);
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
        assert!(diagnostics.to_string().contains("invalid control flow"));
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

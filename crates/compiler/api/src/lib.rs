use std::fmt;
use std::path::Path;

pub use redscript_ast as ast;
use redscript_ast::SourceMap;
pub use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_backend::{AssembleError, PoolError, PoolMappings};
use redscript_compiler_frontend::UnknownSource;
use redscript_compiler_frontend::pass::{DiagnosticPass, UnusedLocals};
pub use redscript_compiler_frontend::{
    Aggregate, CompileErrorReporter, Diagnostic, Enum, Evaluator, Field, FunctionType,
    LoweredCompilationUnit, LoweredFunction, PolyType, Symbols, TypeId, TypeIndex, TypeInterner,
    TypeSchema, TypeScope, infer_from_sources, ir, parse_file, parse_files, pass, process_sources,
    types,
};
use redscript_io::byte;
pub use redscript_io::{SaveError, ScriptBundle};
use thiserror::Error;

pub struct Compilation<'ctx> {
    sources: &'ctx SourceMap,
    symbols: Symbols<'ctx>,
    mappings: PoolMappings<'ctx>,
    unit: LoweredCompilationUnit<'ctx>,
    bundle: ScriptBundle<'ctx>,
    diagnostics: Diagnostics<'ctx>,
}

impl<'ctx> Compilation<'ctx> {
    pub fn new_with(
        bundle: &'ctx [u8],
        sources: &'ctx SourceMap,
        interner: &'ctx TypeInterner,
        passes: &[Box<dyn DiagnosticPass<'ctx>>],
    ) -> Result<Self, Error> {
        let mut reporter = CompileErrorReporter::default();
        let bundle = ScriptBundle::from_bytes(bundle)?;
        let (symbols, mappings) = CompilationInputs::load(&bundle, interner)?.into_inner();
        let (unit, symbols) = infer_from_sources(sources, symbols, &mut reporter, interner);
        unit.run_diagnostics(passes, &mut reporter);

        let mut diagnostics = reporter.into_reported();
        diagnostics.sort_by_key(Diagnostic::is_fatal);

        Ok(Self {
            sources,
            symbols,
            mappings,
            unit,
            bundle,
            diagnostics: Diagnostics(diagnostics),
        })
    }

    pub fn new(
        bundle: &'ctx [u8],
        sources: &'ctx SourceMap,
        interner: &'ctx TypeInterner,
    ) -> Result<Self, Error> {
        Self::new_with(bundle, sources, interner, &default_diagnostics())
    }

    pub fn flush(
        mut self,
        path: impl AsRef<Path>,
    ) -> Result<(Symbols<'ctx>, Diagnostics<'ctx>), FlushError<'ctx>> {
        if self.diagnostics.has_fatal_errors() {
            return Err(FlushError::CompilationErrors(self.diagnostics));
        }

        let mut monomorph = self.mappings.into_monomorphizer(self.sources);
        if let Err(err) = monomorph.monomorphize(&self.unit, &self.symbols, &mut self.bundle) {
            if let Some(span) = err.span() {
                self.diagnostics
                    .push(Diagnostic::Other(Box::new(err), span));
                return Err(FlushError::CompilationErrors(self.diagnostics));
            }
        }

        self.bundle.into_writeable().save(path)?;
        Ok((self.symbols, self.diagnostics))
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
}

#[derive(Debug)]
pub struct Diagnostics<'ctx>(Vec<Diagnostic<'ctx>>);

impl<'ctx> Diagnostics<'ctx> {
    pub fn has_fatal_errors(&self) -> bool {
        self.0.iter().any(Diagnostic::is_fatal)
    }

    pub fn dump(&self, sources: &SourceMap) -> Result<(), UnknownSource> {
        let mut warnings = 0;
        let mut errors = 0;

        for diagnostic in self {
            if diagnostic.is_fatal() {
                log::error!("{}", diagnostic.display(sources)?);
                errors += 1;
            } else {
                log::warn!("{}", diagnostic.display(sources)?);
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

#[derive(Debug, Error)]
pub enum Error {
    #[error("pool error: {0}")]
    Pool(#[from] PoolError),
    #[error("serialization error: {0}")]
    Decoding(#[from] byte::Error),
}

#[derive(Debug, Error)]
pub enum FlushError<'ctx> {
    #[error("fatal diagnostis found")]
    CompilationErrors(Diagnostics<'ctx>),
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

fn default_diagnostics<'ctx>() -> Vec<Box<dyn DiagnosticPass<'ctx>>> {
    vec![Box::new(UnusedLocals)]
}

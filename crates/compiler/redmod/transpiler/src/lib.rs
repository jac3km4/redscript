//! Strict, in-memory community-to-REDmod transpilation from retained declarations and typed IR.
//!
//! This does not invoke the official compiler or certify game-runtime compatibility.
//! Use [`transpile_sources`] for community analysis and automatic unambiguous bindings,
//! or [`transpile`] with an error-free compilation and explicit structural bindings.
//! Spans are used for diagnostics, never symbol identity.

mod analyze;
mod body;

pub use analyze::{AnalyzedSources, TranspiledSource, analyze_sources, transpile_sources};
mod declarations;
pub mod package;
mod source;
mod target;
pub use target::LiteralError;
pub mod validate;

use std::borrow::Cow;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::ops::Range;

use redscript_community_ast::{SourceModule, Span};
use redscript_compiler_frontend::{
    FieldId, FreeFunctionIndex, LoweredCompilationUnit, MethodId, Symbols, TypeId,
};

/// Index path through the retained module's item arrays (including class members).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeclarationPath(pub Vec<usize>);

/// Resolved identity supplied by the analysis owner, not reconstructed from spans.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedDeclaration<'ctx> {
    Type(TypeId<'ctx>),
    Field(FieldId<'ctx>),
    Function(FreeFunctionIndex),
    Method(MethodId<'ctx>),
    AddedMethod(MethodId<'ctx>),
    Replacement(MethodId<'ctx>),
    Wrapper(MethodId<'ctx>, usize),
}

/// A retained module and its declaration associations. Imports require no binding.
/// Every other declaration must have exactly one binding, including inactive
/// conditional variants: unavailable variants must be analyzed separately or rejected.
pub struct TranspileModule<'a, 'ctx> {
    pub source: &'a SourceModule<'ctx>,
    pub bindings: BTreeMap<DeclarationPath, ResolvedDeclaration<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TranspileDiagnostic {
    pub message: Cow<'static, str>,
    /// Concrete literal failure, when emission failed on a literal value.
    pub literal_error: Option<LiteralError>,
    pub span: Option<Span>,
    pub declaration: Option<DeclarationPath>,
}

impl fmt::Display for TranspileDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}
impl std::error::Error for TranspileDiagnostic {}

impl TranspileDiagnostic {
    /// Stable diagnostic code for target capability failures.
    pub fn code(&self) -> &'static str {
        "REDMOD_TRANSPILE"
    }

    /// Render using the same source snippet and location convention as frontend diagnostics.
    /// Spanless or unavailable sources still produce an actionable error message.
    pub fn display<'a>(
        &'a self,
        sources: &'a redscript_community_ast::SourceMap,
    ) -> impl fmt::Display + 'a {
        struct Display<'a>(
            &'a TranspileDiagnostic,
            &'a redscript_community_ast::SourceMap,
        );
        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                writeln!(f, "error[{}]: {}", self.0.code(), self.0)?;
                if let Some(span) = self.0.span
                    && let Some(file) = self.1.get(span.file)
                    && let Some(snippet) = redscript_community_ast::SourceSnippet::new(file, span)
                {
                    let start = snippet.start();
                    writeln!(
                        f,
                        " --> {}:{}:{}",
                        file.path().display(),
                        start.line + 1,
                        start.col + 1
                    )?;
                    write!(f, "{snippet}")?;
                }
                Ok(())
            }
        }
        Display(self, sources)
    }
}

/// Byte range in generated UTF-8 text and its source provenance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMapping {
    pub generated: Range<usize>,
    pub source: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TranspiledModule {
    pub text: String,
    pub mappings: Vec<SourceMapping>,
}

/// Transpile only explicitly selected modules, in caller order. Analysis dependencies
/// are never emitted implicitly. Failure returns no partial artifacts and performs
/// no filesystem writes. Inputs must originate from the same successful compilation.
pub fn transpile<'ctx>(
    modules: &[TranspileModule<'_, 'ctx>],
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
) -> std::result::Result<Vec<TranspiledModule>, Vec<TranspileDiagnostic>> {
    let mut output = Vec::new();
    let mut diagnostics = Vec::new();
    let mut identities = HashSet::new();
    for module in modules {
        for (path, &identity) in &module.bindings {
            if !identities.insert(identity) {
                let mut diagnostic = error(
                    "multiple declarations bound to the same resolved identity across transpile modules",
                    declaration_span(module.source, path),
                );
                diagnostic.declaration = Some(path.clone());
                diagnostics.push(diagnostic);
            }
        }
    }
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    for module in modules {
        match declarations::emit(module, unit, symbols) {
            Ok(module) => output.push(module),
            Err(error) => diagnostics.push(error),
        }
    }
    if diagnostics.is_empty() {
        Ok(output)
    } else {
        Err(diagnostics)
    }
}

fn declaration_span(module: &SourceModule<'_>, path: &DeclarationPath) -> Option<Span> {
    let mut items = module.items.as_ref();
    for (depth, &index) in path.0.iter().enumerate() {
        let (decl, span) = items.get(index)?;
        if depth + 1 == path.0.len() {
            return Some(*span);
        }
        match &decl.item {
            redscript_community_ast::Item::Class(c) | redscript_community_ast::Item::Struct(c) => {
                items = &c.items;
            }
            _ => return None,
        }
    }
    None
}

type Result<T> = std::result::Result<T, TranspileDiagnostic>;

fn error(message: impl Into<Cow<'static, str>>, span: Option<Span>) -> TranspileDiagnostic {
    TranspileDiagnostic {
        message: message.into(),
        literal_error: None,
        span,
        declaration: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostics_borrow_static_messages_and_own_generated_messages() {
        let literal = error("unsupported feature", None);
        assert!(matches!(
            literal.message,
            Cow::Borrowed("unsupported feature")
        ));
        let generated = error(format!("unsupported feature: {}", 42), None);
        assert!(matches!(generated.message, Cow::Owned(_)));
        assert_eq!(generated.to_string(), "unsupported feature: 42");
    }
}

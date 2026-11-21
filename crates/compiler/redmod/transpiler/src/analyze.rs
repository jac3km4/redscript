//! Community analysis and conservative, span-independent declaration association.
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap, HashSet};

use redscript_community_ast::{self as ast, FileId, Item, SourceMap, SourceModule};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, LoweredCompilationUnit, Symbols, TypeInterner,
    parse_files_with_ids, process_sources,
};

use crate::{
    DeclarationPath, ResolvedDeclaration as R, Result, TranspileDiagnostic, TranspileModule,
    TranspiledModule, error,
};

/// Successful community analysis, retaining file identities independently of declaration spans.
pub struct AnalyzedSources<'ctx> {
    modules: Vec<(FileId, SourceModule<'ctx>)>,
    module_indices: HashMap<FileId, usize>,

    unit: LoweredCompilationUnit<'ctx>,
    symbols: Symbols<'ctx>,
    /// Nonfatal community diagnostics, rendered with their original diagnostic codes.
    warnings: Vec<TranspileDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TranspiledSource {
    pub file: FileId,
    pub module: TranspiledModule,
}

/// Analyze every file (including dependencies). Does not add a boot library implicitly.
///
/// All type IDs in the supplied `Symbols` must originate from the supplied
/// `TypeInterner`, except for predefined IDs.
pub fn analyze_sources<'ctx>(
    sources: &'ctx SourceMap,
    symbols: Symbols<'ctx>,
    interner: &'ctx TypeInterner,
) -> std::result::Result<AnalyzedSources<'ctx>, Vec<TranspileDiagnostic>> {
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files_with_ids(sources, &mut reporter);
    // Do not attempt semantic analysis of recovered syntax.
    if reporter
        .reported()
        .iter()
        .any(redscript_compiler_frontend::Diagnostic::is_error)
    {
        return Err(analysis_errors(reporter.reported()));
    }
    let evaluator = Evaluator::from_modules(modules.iter().map(|(_, module)| module));
    // Name resolution consumes the modules, but `AnalyzedSources` must retain the
    // ASTs for `transpile_selected`, so each module is cloned once here. This is
    // structurally required: name resolution moves each item into its own derived
    // tables, so it cannot hand the original modules back.
    let (unit, symbols) = process_sources(
        modules.iter().map(|(_, module)| module.clone()),
        symbols,
        evaluator,
        &mut reporter,
        interner,
    );
    let diagnostics = reporter.into_reported();
    let errors = analysis_errors(&diagnostics);
    if !errors.is_empty() {
        return Err(errors);
    }
    let module_indices = modules
        .iter()
        .enumerate()
        .map(|(index, (file, _))| (*file, index))
        .collect();

    Ok(AnalyzedSources {
        modules,
        module_indices,
        unit,
        symbols,
        warnings: diagnostics
            .iter()
            .filter(|d| !d.is_error())
            .map(|d| TranspileDiagnostic {
                message: Cow::Owned(d.to_string()),
                code: Some(d.code()),
                is_error: false,
                literal_error: None,
                span: Some(d.span()),
                declaration: None,
            })
            .collect(),
    })
}

/// Renders every error diagnostic from a community analysis run as a fail-closed
/// transpile diagnostic. Non-error diagnostics are dropped; warning rendering
/// with preserved codes happens separately.
fn analysis_errors(
    diagnostics: &[redscript_compiler_frontend::Diagnostic<'_>],
) -> Vec<TranspileDiagnostic> {
    diagnostics
        .iter()
        .filter(|d| d.is_error())
        .map(|d| {
            error(
                format!("community analysis [{}]: {d}", d.code()),
                Some(d.span()),
            )
        })
        .collect()
}

/// Analyze dependencies and transpile only selected authored files, in selection order.
///
/// All type IDs in the supplied `Symbols` must originate from the supplied
/// `TypeInterner`, except for predefined IDs.
pub fn transpile_sources<'ctx>(
    sources: &'ctx SourceMap,
    selected: &[FileId],
    symbols: Symbols<'ctx>,
    interner: &'ctx TypeInterner,
) -> std::result::Result<Vec<TranspiledSource>, Vec<TranspileDiagnostic>> {
    analyze_sources(sources, symbols, interner)?.transpile_selected(selected)
}

impl<'ctx> AnalyzedSources<'ctx> {
    pub fn modules(&self) -> &[(FileId, SourceModule<'ctx>)] {
        &self.modules
    }

    pub fn unit(&self) -> &LoweredCompilationUnit<'ctx> {
        &self.unit
    }

    pub fn symbols(&self) -> &Symbols<'ctx> {
        &self.symbols
    }

    pub fn warnings(&self) -> &[TranspileDiagnostic] {
        &self.warnings
    }

    pub fn transpile_selected(
        &self,
        selected: &[FileId],
    ) -> std::result::Result<Vec<TranspiledSource>, Vec<TranspileDiagnostic>> {
        self.check_replacements().map_err(|e| vec![e])?;
        // Array-prefix safety is diagnosed on resolved IR as a frontend warning.
        // Emitting a selected file that carries one would silently change
        // evaluation order, so fail closed for strictly selected output.
        let selected_files: HashSet<_> = selected.iter().copied().collect();
        let mut errors: Vec<_> = self
            .warnings
            .iter()
            .filter(|warning| warning.code == Some("UNSAFE_ARRAY_EVALUATION"))
            .filter(|warning| {
                warning
                    .span
                    .is_some_and(|span| selected_files.contains(&span.file))
            })
            .map(|warning| TranspileDiagnostic {
                is_error: true,
                ..warning.clone()
            })
            .collect();
        let mut seen = HashSet::new();
        let mut requests = Vec::new();
        for &file in selected {
            if !seen.insert(file) {
                errors.push(error("duplicate selected FileId", None));
                continue;
            }
            let Some(&index) = self.module_indices.get(&file) else {
                errors.push(error("selected FileId is not in this analysis", None));
                continue;
            };
            let source = &self.modules[index].1;
            let mut bindings = BTreeMap::new();
            match self.associate(file, &source.items, &mut bindings) {
                Ok(()) => requests.push(TranspileModule { source, bindings }),
                Err(e) => errors.push(e),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        let output = crate::transpile(&requests, &self.unit, &self.symbols)?;
        Ok(selected
            .iter()
            .copied()
            .zip(output)
            .map(|(file, module)| TranspiledSource { file, module })
            .collect())
    }

    // The lowering tables overwrite replacements and wrappers by `MethodId`, so
    // inspect the retained declaration provenance instead: it keeps every
    // resolved binding, including duplicates that lowering collapsed.
    fn check_replacements(&self) -> Result<()> {
        let mut targets = HashSet::new();
        for (file, module) in &self.modules {
            for (path, resolved) in self.unit.declarations.get(file).into_iter().flatten() {
                if let R::Replacement(id) = resolved
                    && !targets.insert(*id)
                {
                    let mut err = error(
                        "multiple @replaceMethod declarations target the same method; keep only one replacement",
                        crate::declaration_span(module, path),
                    );
                    err.declaration = Some(path.clone());
                    return Err(err);
                }
            }
        }
        Ok(())
    }

    fn associate(
        &self,
        file: FileId,
        items: &[(ast::SourceItemDecl<'ctx>, ast::Span)],
        out: &mut BTreeMap<DeclarationPath, R<'ctx>>,
    ) -> Result<()> {
        let bindings: HashMap<&DeclarationPath, R<'ctx>> = self
            .unit
            .declarations
            .get(&file)
            .into_iter()
            .flatten()
            .map(|(path, resolved)| (path, *resolved))
            .collect();
        let mut indices = Vec::new();
        self.associate_items(items, &mut indices, &bindings, out)
    }

    fn associate_items(
        &self,
        items: &[(ast::SourceItemDecl<'ctx>, ast::Span)],
        indices: &mut Vec<usize>,
        bindings: &HashMap<&DeclarationPath, R<'ctx>>,
        out: &mut BTreeMap<DeclarationPath, R<'ctx>>,
    ) -> Result<()> {
        for (i, (decl, span)) in items.iter().enumerate() {
            indices.push(i);
            let key = DeclarationPath(indices.clone());
            let result = (|| {
                if decl.annotations.iter().any(|(a, _)| a.name == "if") {
                    return Err(error(
                        "@if declarations are not supported; select the version for your dependencies and remove the other versions",
                        Some(*span),
                    ));
                }
                if matches!(decl.item, Item::Import(_)) {
                    return Ok(());
                }
                if matches!(decl.item, Item::Default(_)) {
                    return Err(error(
                        "field default directives are not supported by the transpiler",
                        Some(*span),
                    ));
                }
                let generic = match &decl.item {
                    Item::Class(c) | Item::Struct(c) => !c.type_params.is_empty(),
                    Item::Function(f) => !f.type_params.is_empty(),
                    _ => false,
                };
                if generic {
                    return Err(error(
                        "custom generic declarations are not supported; use concrete parameter and field types",
                        Some(*span),
                    ));
                }
                let Some(&resolved) = bindings.get(&key) else {
                    return Err(error(
                        "cannot identify this declaration; add explicit parameter and return types and check for duplicate wrappers",
                        Some(*span),
                    ));
                };
                if let R::Wrapper(id, _) = resolved
                    && self
                        .unit
                        .method_wrappers
                        .get(&id)
                        .is_some_and(|wrappers| wrappers.len() != 1)
                {
                    return Err(error(
                        "cannot identify this declaration; add explicit parameter and return types and check for duplicate wrappers",
                        Some(*span),
                    ));
                }
                out.insert(key.clone(), resolved);
                if let Item::Class(c) | Item::Struct(c) = &decl.item {
                    self.associate_items(&c.items, indices, bindings, out)?;
                }
                Ok(())
            })();
            result.map_err(|mut e| {
                if e.declaration.is_none() {
                    e.declaration = Some(key);
                }
                e
            })?;
            indices.pop();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_frontend::Symbols;

    use super::*;

    fn with_declarations(source: &str, check: impl FnOnce(FileId, &LoweredCompilationUnit<'_>)) {
        let sources = SourceMap::new();
        let file = sources.push_back("provenance.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, _) = redscript_compiler_frontend::infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        check(file, &unit);
    }

    #[test]
    fn overloaded_free_functions_keep_distinct_bindings() {
        with_declarations(
            "func Echo(value: Int32) -> Int32 = value; func Echo(value: Bool) -> Bool = value;",
            |file, unit| {
                let declarations = unit.declarations.get(&file).expect("bindings");
                let ids: Vec<_> = declarations
                    .iter()
                    .filter_map(|(path, resolved)| match resolved {
                        R::Function(id) => Some((path.clone(), *id)),
                        _ => None,
                    })
                    .collect();
                assert_eq!(
                    ids.iter().map(|(path, _)| path.clone()).collect::<Vec<_>>(),
                    vec![DeclarationPath(vec![0]), DeclarationPath(vec![1])]
                );
                assert_ne!(ids[0].1, ids[1].1);
            },
        );
    }

    #[test]
    fn aggregate_members_and_bodyless_methods_are_bound() {
        with_declarations(
            "native class Widget { let value: Int32; native func Probe(); native func Read() -> Int32; }",
            |file, unit| {
                let declarations = unit.declarations.get(&file).expect("bindings");
                let has = |path: &[usize], matches: fn(&R<'_>) -> bool| {
                    declarations.iter().any(|(p, r)| p.0 == path && matches(r))
                };
                assert!(has(&[0], |r| matches!(r, R::Type(_))));
                assert!(has(&[0, 0], |r| matches!(r, R::Field(_))));
                assert!(has(&[0, 1], |r| matches!(r, R::Method(_))));
                assert!(has(&[0, 2], |r| matches!(r, R::Method(_))));
            },
        );
    }

    #[test]
    fn conditional_members_preserve_original_indices() {
        with_declarations(
            "class Widget { @if(false) let dropped: Int32; let kept: Int32; }",
            |file, unit| {
                let declarations = unit.declarations.get(&file).expect("bindings");
                assert!(
                    declarations
                        .iter()
                        .any(|(p, r)| p.0 == [0, 1] && matches!(r, R::Field(_)))
                );
                assert!(!declarations.iter().any(|(p, _)| p.0 == [0, 0]));
            },
        );
    }
}

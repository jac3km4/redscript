//! Community analysis and conservative, span-independent declaration association.
use std::collections::{BTreeMap, HashMap, HashSet};

use redscript_community_ast::{self as ast, FileId, Item, SourceMap, SourceModule};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, FieldId, FreeFunctionIndex, FunctionType,
    LoweredCompilationUnit, MethodId, Symbols, Type, TypeId, TypeInterner, parse_file, predef,
    process_sources,
};

use crate::{
    DeclarationPath, ResolvedDeclaration as R, Result, TranspileDiagnostic, TranspileModule,
    TranspiledModule, error,
};

/// Successful community analysis, retaining file identities independently of declaration spans.
pub struct AnalyzedSources<'ctx> {
    modules: Vec<(FileId, SourceModule<'ctx>)>,
    module_indices: HashMap<FileId, usize>,
    interner: &'ctx TypeInterner,

    unit: LoweredCompilationUnit<'ctx>,
    symbols: Symbols<'ctx>,
    free_functions: HashMap<String, Vec<FreeFunctionIndex>>,
    /// Nonfatal community diagnostics, rendered with their original diagnostic codes.
    warnings: Vec<String>,
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
    let modules: Vec<_> = sources
        .files()
        .filter_map(|(id, file)| parse_file(id, file, &mut reporter).map(|module| (id, module)))
        .collect();
    // Do not attempt semantic analysis of recovered syntax.
    if reporter
        .reported()
        .iter()
        .any(redscript_compiler_frontend::Diagnostic::is_error)
    {
        return Err(reporter
            .into_reported()
            .into_iter()
            .filter(redscript_compiler_frontend::Diagnostic::is_error)
            .map(|d| {
                error(
                    format!("community analysis [{}]: {d}", d.code()),
                    Some(d.span()),
                )
            })
            .collect());
    }
    let evaluator = Evaluator::from_modules(modules.iter().map(|(_, module)| module));
    let (unit, symbols) = process_sources(
        modules.iter().map(|(_, module)| module.clone()),
        symbols,
        evaluator,
        &mut reporter,
        interner,
    );
    let diagnostics = reporter.into_reported();
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.is_error())
        .map(|d| {
            error(
                format!("community analysis [{}]: {d}", d.code()),
                Some(d.span()),
            )
        })
        .collect();
    if !errors.is_empty() {
        return Err(errors);
    }
    let module_indices = modules
        .iter()
        .enumerate()
        .map(|(index, (file, _))| (*file, index))
        .collect();

    let mut free_functions: HashMap<_, Vec<_>> = HashMap::new();
    for entry in symbols.free_functions() {
        free_functions
            .entry(entry.name().to_string())
            .or_default()
            .push(*entry.key());
    }
    Ok(AnalyzedSources {
        modules,
        module_indices,
        interner,
        unit,
        symbols,
        free_functions,
        warnings: diagnostics
            .iter()
            .map(|d| format!("[{}]: {d}", d.code()))
            .collect(),
    })
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

    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }

    pub fn transpile_selected(
        &self,
        selected: &[FileId],
    ) -> std::result::Result<Vec<TranspiledSource>, Vec<TranspileDiagnostic>> {
        self.check_replacements().map_err(|e| vec![e])?;
        let mut seen = HashSet::new();
        let mut requests = Vec::new();
        let mut errors = Vec::new();
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
            let prefix = source
                .path
                .as_ref()
                .map(|p| format!("{}.", p.segments.join(".")))
                .unwrap_or_default();
            match self.associate(file, &source.items, &prefix, None, &[], &mut bindings) {
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

    // Lowering overwrites replacements by MethodId. Inventory every retained
    // declaration, including dependencies and conditional variants, before using
    // that table: neither its body nor its span can establish source ownership.
    fn check_replacements(&self) -> Result<()> {
        let mut targets = Vec::new();
        for (file, module) in &self.modules {
            self.collect_replacements(*file, &module.items, &[], &mut targets)?;
        }
        Ok(())
    }

    fn collect_replacements(
        &self,
        file: FileId,
        items: &[(ast::SourceItemDecl<'ctx>, ast::Span)],
        path: &[usize],
        targets: &mut Vec<MethodId<'ctx>>,
    ) -> Result<()> {
        for (index, (decl, span)) in items.iter().enumerate() {
            let mut indices = path.to_vec();
            indices.push(index);
            if let Item::Class(c) | Item::Struct(c) = &decl.item {
                self.collect_replacements(file, &c.items, &indices, targets)?;
            }
            let Some((annotation, _)) = decl
                .annotations
                .iter()
                .find(|(a, _)| a.name == "replaceMethod")
            else {
                continue;
            };
            let result = (|| {
                let (Item::Function(f), [(ast::Expr::Ident(name), _)]) =
                    (&decl.item, &annotation.args[..])
                else {
                    return Err(error(
                        "replacement requires a function and one resolved class name",
                        Some(*span),
                    ));
                };
                let id = unique(
                    self.type_id(file, name)
                        .and_then(|id| self.symbols.get_type(id).map(|def| (id, def)))
                        .and_then(|(id, def)| def.schema().as_aggregate().map(|agg| (id, agg)))
                        .into_iter()
                        .flat_map(|(id, agg)| {
                            agg.methods()
                                .iter()
                                .filter(|e| {
                                    **e.name() == f.name.0
                                        && self.signature(file, f, e.func().type_())
                                })
                                .map(move |e| MethodId::new(id, *e.key()))
                        }),
                );
                let Some(id) = id else {
                    return Err(error(
                        "replacement has no unique resolved target/signature across analyzed sources",
                        Some(*span),
                    ));
                };
                if targets.contains(&id) {
                    return Err(error(
                        "competing replacements for the same resolved target/signature across analyzed sources; lowered body ownership is ambiguous",
                        Some(*span),
                    ));
                }
                targets.push(id);
                Ok(())
            })();
            result.map_err(|mut e| {
                e.declaration = Some(DeclarationPath(indices));
                e
            })?;
        }
        Ok(())
    }

    fn resolved_type(&self, name: &str) -> Option<TypeId<'ctx>> {
        let id = self.interner.get(name)?;
        self.symbols.get_type(id).map(|_| id)
    }

    fn type_id(&self, file: FileId, name: &str) -> Option<TypeId<'ctx>> {
        if let Some(id) = self
            .unit
            .scopes
            .get(&file)
            .and_then(|s| s.get(name))
            .and_then(redscript_compiler_frontend::TypeRef::id)
        {
            return Some(id);
        }
        let builtin = match name {
            "array" => Some(predef::ARRAY),
            "ref" => Some(predef::REF),
            "wref" => Some(predef::WREF),
            "script_ref" => Some(predef::SCRIPT_REF),
            _ => None,
        };
        builtin.or_else(|| self.resolved_type(name))
    }

    fn source_type(&self, file: FileId, typ: &ast::SourceType<'ctx>) -> Option<Type<'ctx>> {
        match typ {
            ast::Type::Named { name, args } => {
                let id = self.type_id(file, name)?;
                let args: Vec<_> = args
                    .iter()
                    .map(|(t, _)| self.source_type(file, t))
                    .collect::<Option<_>>()?;
                if id == predef::REF && args.len() == 1 {
                    let arg = args.first()?;
                    let flags = arg
                        .upper_bound()
                        .and_then(|t| self.symbols.get_type(t.id()))
                        .and_then(|d| d.schema().as_aggregate())
                        .map(redscript_compiler_frontend::Aggregate::flags)
                        .unwrap_or_default();
                    if flags.is_never_ref() || flags.is_mixed_ref() {
                        return None;
                    }
                    return Some(arg.clone());
                }
                Some(Type::app(id, args))
            }
            ast::Type::Array(t) => Some(Type::app(predef::ARRAY, [self.source_type(file, &t.0)?])),
            ast::Type::StaticArray(t, n) => Some(Type::app(
                TypeId::array_with_size(*n)?,
                [self.source_type(file, &t.0)?],
            )),
            ast::Type::Fn { .. } => None,
        }
    }

    fn signature(
        &self,
        file: FileId,
        f: &ast::SourceFunction<'ctx>,
        typ: &FunctionType<'ctx>,
    ) -> bool {
        f.type_params.is_empty()
            && typ.type_params().is_empty()
            && f.params.len() == typ.params().len()
            && f.params.iter().zip(typ.params()).all(|((p, _), resolved)| {
                let q = p.qualifiers;
                p.typ
                    .as_ref()
                    .and_then(|(t, _)| self.source_type(file, t))
                    .as_ref()
                    == Some(resolved.type_())
                    && q.contains(ast::ParamQualifiers::OUT) == resolved.flags().is_out()
                    && q.contains(ast::ParamQualifiers::OPTIONAL) == resolved.flags().is_optional()
                    && q.contains(ast::ParamQualifiers::CONST) == resolved.flags().is_const()
                    && q.contains(ast::ParamQualifiers::SKIP) == resolved.flags().is_short_circuit()
            })
            && match &f.return_type {
                Some(t) => self.source_type(file, &t.0).as_ref() == Some(typ.return_type()),
                None => typ.return_type() == &Type::nullary(predef::VOID),
            }
    }

    fn associate(
        &self,
        file: FileId,
        items: &[(ast::SourceItemDecl<'ctx>, ast::Span)],
        prefix: &str,
        parent: Option<TypeId<'ctx>>,
        path: &[usize],
        out: &mut BTreeMap<DeclarationPath, R<'ctx>>,
    ) -> Result<()> {
        for (i, (decl, span)) in items.iter().enumerate() {
            let mut indices = path.to_vec();
            indices.push(i);
            let key = DeclarationPath(indices.clone());
            let result = (|| {
                if decl.annotations.iter().any(|(a, _)| a.name == "if") {
                    return Err(error(
                        "automatic association rejects conditional declarations; inactive bodies have no resolved identity",
                        Some(*span),
                    ));
                }
                let patch = decl.annotations.iter().find(|(a, _)| {
                    matches!(
                        a.name,
                        "addField" | "addMethod" | "wrapMethod" | "replaceMethod"
                    )
                });
                let owner = if let Some((a, _)) = patch {
                    let [(ast::Expr::Ident(name), _)] = &a.args[..] else {
                        return Err(error("patch requires one resolved class name", Some(*span)));
                    };
                    Some(self.type_id(file, name).ok_or_else(|| {
                        error("ambiguous or unavailable patch class", Some(*span))
                    })?)
                } else {
                    parent
                };
                let binding = match &decl.item {
                    Item::Import(_) => return Ok(()),
                    Item::Default(_) => {
                        return Err(error(
                            "field default directives are not supported by the transpiler",
                            Some(*span),
                        ));
                    }
                    Item::Class(c) | Item::Struct(c) => {
                        if !c.type_params.is_empty() {
                            return Err(error(
                                "custom generic declarations are unsupported",
                                Some(*span),
                            ));
                        }
                        let name = format!("{prefix}{}", c.name.0);
                        let id = self
                            .resolved_type(&name)
                            .ok_or_else(|| error("unavailable resolved class", Some(*span)))?;
                        out.insert(key.clone(), R::Type(id));
                        return self.associate(file, &c.items, prefix, Some(id), &indices, out);
                    }
                    Item::Enum(e) => {
                        let name = format!("{prefix}{}", e.name.0);
                        unique(
                            self.unit
                                .enums
                                .iter()
                                .filter(|id| id.as_str() == name)
                                .map(|&id| R::Type(id)),
                        )
                    }
                    Item::Let(f) => owner.and_then(|owner| {
                        let agg = self.symbols.get_type(owner)?.schema().as_aggregate()?;
                        unique(
                            agg.fields()
                                .iter()
                                .filter(|e| e.name() == f.name.0)
                                .map(|e| R::Field(FieldId::new(owner, e.index()))),
                        )
                    }),
                    Item::Function(f) => {
                        if !f.type_params.is_empty() {
                            return Err(error(
                                "custom generic declarations are unsupported",
                                Some(*span),
                            ));
                        }
                        if let Some(owner) = owner {
                            self.symbols
                                .get_type(owner)
                                .and_then(|d| d.schema().as_aggregate())
                                .and_then(|agg| {
                                    unique(
                                        agg.methods()
                                            .iter()
                                            .filter(|e| {
                                                **e.name() == f.name.0
                                                    && self.signature(file, f, e.func().type_())
                                            })
                                            .filter_map(|e| {
                                                let id = MethodId::new(owner, *e.key());
                                                Some(match patch.map(|(a, _)| a.name) {
                                                    Some("addMethod")
                                                        if self
                                                            .unit
                                                            .added_methods
                                                            .contains_key(&id) =>
                                                    {
                                                        R::AddedMethod(id)
                                                    }
                                                    Some("replaceMethod")
                                                        if self
                                                            .unit
                                                            .method_replacements
                                                            .contains_key(&id) =>
                                                    {
                                                        R::Replacement(id)
                                                    }
                                                    Some("wrapMethod")
                                                        if self
                                                            .unit
                                                            .method_wrappers
                                                            .get(&id)
                                                            .is_some_and(|v| v.len() == 1) =>
                                                    {
                                                        R::Wrapper(id, 0)
                                                    }
                                                    None => R::Method(id),
                                                    _ => return None,
                                                })
                                            }),
                                    )
                                })
                        } else {
                            let name = format!("{prefix}{}", f.name.0);
                            unique(
                                self.free_functions
                                    .get(&name)
                                    .into_iter()
                                    .flatten()
                                    .copied()
                                    .filter(|&id| self.signature(file, f, self.symbols[id].type_()))
                                    .map(R::Function),
                            )
                        }
                    }
                };
                let Some(binding) = binding else {
                    return Err(error(
                        "declaration has no unique resolved name/signature association (inactive, ambiguous, inferred signature, or multiple wrappers)",
                        Some(*span),
                    ));
                };
                out.insert(key.clone(), binding);
                Ok(())
            })();
            result.map_err(|mut e| {
                if e.declaration.is_none() {
                    e.declaration = Some(key);
                }
                e
            })?;
        }
        Ok(())
    }
}

fn unique<T>(mut candidates: impl Iterator<Item = T>) -> Option<T> {
    let candidate = candidates.next()?;
    candidates.next().is_none().then_some(candidate)
}

#[cfg(test)]
mod tests {
    use redscript_compiler_frontend::{Aggregate, TypeDef, TypeSchema};

    use super::*;

    #[test]
    fn resolved_types_require_symbols_and_preserve_interner_identity() {
        let sources = SourceMap::new();
        let file = sources.push_back("empty.reds", "");
        let interner = TypeInterner::default();
        let present = interner.intern("Present");
        interner.intern("Absent");
        let mut symbols = Symbols::with_default_types();
        symbols.add_type(
            present,
            TypeDef::new([], TypeSchema::Aggregate(Aggregate::default().into()), []),
        );
        let analysis = analyze_sources(&sources, symbols, &interner).unwrap();
        assert_eq!(analysis.resolved_type("Present"), Some(present));
        assert_eq!(analysis.resolved_type("Absent"), None);
        assert_eq!(analysis.resolved_type("Unknown"), None);
        assert_eq!(analysis.type_id(file, "Present"), Some(present));
        assert_eq!(analysis.type_id(file, "Absent"), None);
        assert_eq!(analysis.type_id(file, "Unknown"), None);
        assert!(interner.get_index_of("Unknown").is_none());
        assert_eq!(analysis.type_id(file, "array"), Some(predef::ARRAY));
    }

    #[test]
    fn unique_candidate_rejects_missing_and_multiple_matches() {
        assert_eq!(unique([].into_iter()), None::<i32>);
        assert_eq!(unique([1].into_iter()), Some(1));
        assert_eq!(unique([1, 1].into_iter()), None);
        assert_eq!(unique([1, 2, 3].into_iter()), None);
    }
}

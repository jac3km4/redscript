//! Community analysis and conservative, span-independent declaration association.
use std::collections::{BTreeMap, HashSet};

use redscript_community_ast::{self as ast, FileId, Item, SourceMap, SourceModule};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, FieldId, FunctionType, LoweredCompilationUnit, MethodId,
    Symbols, Type, TypeId, TypeInterner, parse_file, predef, process_sources,
};

use crate::{
    DeclarationPath, ExportDiagnostic, ExportModule, ExportedModule, ResolvedDeclaration as R,
    Result, error,
};

/// Successful community analysis, retaining file identities independently of declaration spans.
pub struct AnalyzedSources<'ctx> {
    modules: Vec<(FileId, SourceModule<'ctx>)>,
    unit: LoweredCompilationUnit<'ctx>,
    symbols: Symbols<'ctx>,
    /// Nonfatal community diagnostics, rendered with their original diagnostic codes.
    warnings: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedSource {
    pub file: FileId,
    pub module: ExportedModule,
}

/// Analyze every file (including dependencies). Does not add a boot library implicitly.
pub fn analyze_sources<'ctx>(
    sources: &'ctx SourceMap,
    symbols: Symbols<'ctx>,
    interner: &'ctx TypeInterner,
) -> std::result::Result<AnalyzedSources<'ctx>, Vec<ExportDiagnostic>> {
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
    Ok(AnalyzedSources {
        modules,
        unit,
        symbols,
        warnings: diagnostics
            .iter()
            .map(|d| format!("[{}]: {d}", d.code()))
            .collect(),
    })
}

/// Analyze dependencies and export only selected authored files, in selection order.
pub fn export_sources<'ctx>(
    sources: &'ctx SourceMap,
    selected: &[FileId],
    symbols: Symbols<'ctx>,
    interner: &'ctx TypeInterner,
) -> std::result::Result<Vec<ExportedSource>, Vec<ExportDiagnostic>> {
    analyze_sources(sources, symbols, interner)?.export_selected(selected)
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

    pub fn export_selected(
        &self,
        selected: &[FileId],
    ) -> std::result::Result<Vec<ExportedSource>, Vec<ExportDiagnostic>> {
        self.check_replacements().map_err(|e| vec![e])?;
        let mut seen = HashSet::new();
        let mut requests = Vec::new();
        let mut errors = Vec::new();
        for &file in selected {
            if !seen.insert(file) {
                errors.push(error("duplicate selected FileId", None));
                continue;
            }
            let Some((_, source)) = self.modules.iter().find(|(id, _)| *id == file) else {
                errors.push(error("selected FileId is not in this analysis", None));
                continue;
            };
            let mut bindings = BTreeMap::new();
            let prefix = source
                .path
                .as_ref()
                .map(|p| format!("{}.", p.segments.join(".")))
                .unwrap_or_default();
            match self.associate(file, &source.items, &prefix, None, &[], &mut bindings) {
                Ok(()) => requests.push(ExportModule { source, bindings }),
                Err(e) => errors.push(e),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        let output = crate::export(&requests, &self.unit, &self.symbols)?;
        Ok(selected
            .iter()
            .copied()
            .zip(output)
            .map(|(file, module)| ExportedSource { file, module })
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
                let candidates: Vec<_> = self
                    .type_id(file, name)
                    .and_then(|id| self.symbols.get_type(id).map(|def| (id, def)))
                    .and_then(|(id, def)| def.schema().as_aggregate().map(|agg| (id, agg)))
                    .into_iter()
                    .flat_map(|(id, agg)| {
                        agg.methods()
                            .iter()
                            .filter(|e| {
                                **e.name() == f.name.0 && self.signature(file, f, e.func().type_())
                            })
                            .map(move |e| MethodId::new(id, *e.key()))
                    })
                    .collect();
                let [id] = candidates.as_slice() else {
                    return Err(error(
                        "replacement has no unique resolved target/signature across analyzed sources",
                        Some(*span),
                    ));
                };
                if targets.contains(id) {
                    return Err(error(
                        "competing replacements for the same resolved target/signature across analyzed sources; lowered body ownership is ambiguous",
                        Some(*span),
                    ));
                }
                targets.push(*id);
                Ok(())
            })();
            result.map_err(|mut e| {
                e.declaration = Some(DeclarationPath(indices));
                e
            })?;
        }
        Ok(())
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
        builtin.or_else(|| {
            let mut ids = self
                .symbols
                .types()
                .filter(|(id, _)| id.as_str() == name)
                .map(|(id, _)| id);
            let id = ids.next()?;
            ids.next().is_none().then_some(id)
        })
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
                let mut candidates = Vec::new();
                match &decl.item {
                    Item::Import(_) => return Ok(()),
                    Item::Class(c) | Item::Struct(c) => {
                        if !c.type_params.is_empty() {
                            return Err(error(
                                "custom generic declarations are unsupported",
                                Some(*span),
                            ));
                        }
                        let name = format!("{prefix}{}", c.name.0);
                        let id = self
                            .symbols
                            .types()
                            .find(|(id, _)| id.as_str() == name)
                            .map(|(id, _)| id)
                            .ok_or_else(|| error("unavailable resolved class", Some(*span)))?;
                        out.insert(key.clone(), R::Type(id));
                        return self.associate(file, &c.items, prefix, Some(id), &indices, out);
                    }
                    Item::Enum(e) => {
                        let name = format!("{prefix}{}", e.name.0);
                        candidates.extend(
                            self.unit
                                .enums
                                .iter()
                                .filter(|id| id.as_str() == name)
                                .map(|&id| R::Type(id)),
                        );
                    }
                    Item::Let(f) => {
                        if let Some(owner) = owner
                            && let Some(agg) = self
                                .symbols
                                .get_type(owner)
                                .and_then(|d| d.schema().as_aggregate())
                        {
                            candidates.extend(
                                agg.fields()
                                    .iter()
                                    .filter(|e| e.name() == f.name.0)
                                    .map(|e| R::Field(FieldId::new(owner, e.index()))),
                            );
                        }
                    }
                    Item::Function(f) => {
                        if !f.type_params.is_empty() {
                            return Err(error(
                                "custom generic declarations are unsupported",
                                Some(*span),
                            ));
                        }
                        if let Some(owner) = owner {
                            if let Some(agg) = self
                                .symbols
                                .get_type(owner)
                                .and_then(|d| d.schema().as_aggregate())
                            {
                                for e in agg.methods().iter().filter(|e| {
                                    **e.name() == f.name.0
                                        && self.signature(file, f, e.func().type_())
                                }) {
                                    let id = MethodId::new(owner, *e.key());
                                    let binding = match patch.map(|(a, _)| a.name) {
                                        Some("addMethod")
                                            if self.unit.added_methods.contains_key(&id) =>
                                        {
                                            R::AddedMethod(id)
                                        }
                                        Some("replaceMethod")
                                            if self.unit.method_replacements.contains_key(&id) =>
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
                                        _ => continue,
                                    };
                                    candidates.push(binding);
                                }
                            }
                        } else {
                            let name = format!("{prefix}{}", f.name.0);
                            candidates.extend(
                                self.symbols
                                    .free_functions()
                                    .filter(|e| {
                                        e.name().to_string() == name
                                            && self.signature(file, f, e.func().type_())
                                    })
                                    .map(|e| R::Function(*e.key())),
                            );
                        }
                    }
                }
                if candidates.len() != 1 {
                    return Err(error(
                        "declaration has no unique resolved name/signature association (inactive, ambiguous, inferred signature, or multiple wrappers)",
                        Some(*span),
                    ));
                }
                out.insert(key.clone(), candidates[0]);
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

use std::collections::BTreeSet;

use redscript_community_ast::{self as ast, Item, ItemQualifiers as Q, SourceItemDecl, Span};
use redscript_compiler_frontend::{
    FunctionType, LoweredCompilationUnit, Symbols, Type, TypeSchema, ir,
};

use crate::body::{Body, constant};
use crate::{
    DeclarationPath, ResolvedDeclaration as R, Result, SourceMapping, TranspileModule,
    TranspiledModule, error, target,
};

pub fn emit<'ctx>(
    module: &TranspileModule<'_, 'ctx>,
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<TranspiledModule> {
    let mut emitter = Emitter {
        module,
        unit,
        symbols,
        output: TranspiledModule {
            text: String::new(),
            mappings: Vec::new(),
        },
        used: BTreeSet::new(),
    };
    if let Some(path) = &module.source.path {
        emitter.output.text.push_str(&format!(
            "@module {};\n\n",
            target::string(target::path(&path.segments.join("."))?)?
        ));
    }
    let mut saw_declaration = false;
    for (i, (decl, span)) in module.source.items.iter().enumerate() {
        if matches!(decl.item, Item::Import(_)) {
            if saw_declaration {
                return Err(error(
                    "imports must precede source declarations",
                    Some(*span),
                ));
            }
        } else {
            saw_declaration = true;
        }
        emitter.decl(decl, *span, vec![i], false)?;
    }
    if emitter.used.len() != module.bindings.len() {
        return Err(error(
            "unused declaration binding (wrong structural path)",
            None,
        ));
    }
    Ok(emitter.output)
}

type FunctionBinding<'a, 'ctx> = (
    &'ctx str,
    &'a FunctionType<'ctx>,
    Option<&'a redscript_compiler_frontend::LoweredFunction<'ctx>>,
    bool,
    Option<redscript_compiler_frontend::MethodId<'ctx>>,
);

struct Emitter<'a, 'ctx> {
    module: &'a TranspileModule<'a, 'ctx>,
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &'a Symbols<'ctx>,
    output: TranspiledModule,
    used: BTreeSet<DeclarationPath>,
}

impl<'a, 'ctx> Emitter<'a, 'ctx> {
    fn decl(
        &mut self,
        decl: &SourceItemDecl<'ctx>,
        span: Span,
        indices: Vec<usize>,
        member: bool,
    ) -> Result<()> {
        let path = DeclarationPath(indices);
        let start = self.output.text.len();
        let result = self.decl_inner(decl, span, &path, member);
        if result.is_ok() {
            self.output.mappings.push(SourceMapping {
                generated: start..self.output.text.len(),
                source: span,
            });
        }
        result.map_err(|mut e| {
            if e.span.is_none() {
                e.span = Some(span);
            }
            if e.declaration.is_none() {
                e.declaration = Some(path);
            }
            e
        })
    }

    fn decl_inner(
        &mut self,
        decl: &SourceItemDecl<'ctx>,
        span: Span,
        path: &DeclarationPath,
        member: bool,
    ) -> Result<()> {
        if let Item::Import(import) = &decl.item {
            if member
                || !decl.annotations.is_empty()
                || !decl.qualifiers.is_empty()
                || decl.visibility.is_some()
            {
                return Err(error("unsupported import metadata", None));
            }
            let ast::Import::All(import) = import else {
                return Err(error(
                    "REDmod transpile requires wildcard imports; exact/select imports cannot be widened safely",
                    None,
                ));
            };
            let name = import.segments.join(".");
            target::path(&name)?;
            self.output
                .text
                .push_str(&format!("@using {};\n", target::string(&name)?));
            return Ok(());
        }
        let resolved = *self.module.bindings.get(path).ok_or_else(|| error("missing explicit declaration binding; inactive/ambiguous declarations cannot use span guessing", None))?;
        self.used.insert(path.clone());

        self.validate_binding(decl, path, resolved, member)?;
        if decl.qualifiers.intersects(
            Q::EDITABLE | Q::INSTANCE_EDITABLE | Q::INLINE | Q::REPLICATED | Q::TEST_ONLY,
        ) {
            return Err(error(
                "editable, instanceeditable, inlined, replicated and testonly qualifiers are not supported by REDmod transpile",
                Some(span),
            ));
        }
        self.metadata(decl)?;
        let patch = decl.annotations.iter().any(|(a, _)| {
            matches!(
                a.name,
                "addField" | "addMethod" | "wrapMethod" | "replaceMethod"
            )
        });
        let callback = decl.qualifiers.contains(Q::CALLBACK);
        // Official REDmod accepts visibility on members, not free functions.
        if !patch
            && !callback
            && (member || !matches!(decl.item, Item::Function(_)))
            && let Some(vis) = decl.visibility
        {
            self.output.text.push_str(match vis {
                ast::Visibility::Public => "public ",
                ast::Visibility::Private => "private ",
                ast::Visibility::Protected => "protected ",
            });
        }
        for (flag, text) in [
            (Q::ABSTRACT, "abstract "),
            (Q::CONST, "const "),
            (Q::EXEC, "exec "),
            (Q::FINAL, "final "),
            (Q::IMPORT_ONLY, "importonly "),
            (Q::NATIVE, "import "),
            (Q::PERSISTENT, "persistent "),
            (Q::QUEST, "quest "),
            (Q::STATIC, "static "),
        ] {
            if decl.qualifiers.contains(flag) {
                self.output.text.push_str(text);
            }
        }
        match &decl.item {
            Item::Class(source) | Item::Struct(source) => {
                let R::Type(id) = resolved else {
                    return Err(error("class binding must identify a resolved type", None));
                };
                if id.simple_name() != source.name.0 {
                    return Err(error("type binding name mismatch", None));
                }
                let def = self
                    .symbols
                    .get_type(id)
                    .ok_or_else(|| error("missing bound type", None))?;
                let agg = def
                    .schema()
                    .as_aggregate()
                    .ok_or_else(|| error("bound type is not an aggregate", None))?;
                if !source.type_params.is_empty() || !def.params().is_empty() {
                    return Err(error("custom generic declarations are unsupported", None));
                }
                target::typ(&Type::nullary(id), self.symbols)?;
                self.output
                    .text
                    .push_str(if matches!(decl.item, Item::Struct(_)) {
                        "struct "
                    } else {
                        "class "
                    });
                self.output.text.push_str(target::ident(source.name.0)?);
                if let Some(base) = agg.base() {
                    self.output.text.push_str(&format!(
                        " extends {}",
                        target::typ(&base.clone().into_type(), self.symbols)?
                    ));
                }
                self.output.text.push_str(" {\n");
                for (i, (item, span)) in source.items.iter().enumerate() {
                    let mut indices = path.0.clone();
                    indices.push(i);
                    self.decl(item, *span, indices, true)?;
                }
                self.output.text.push_str("}\n");
            }
            Item::Enum(source) => {
                let R::Type(id) = resolved else {
                    return Err(error("enum binding must identify a type", None));
                };
                let def = self
                    .symbols
                    .get_type(id)
                    .ok_or_else(|| error("missing enum type", None))?;
                let TypeSchema::Enum(enm) = def.schema() else {
                    return Err(error("enum binding kind mismatch", None));
                };
                if id.simple_name() != source.name.0 {
                    return Err(error("enum binding name mismatch", None));
                }
                self.output
                    .text
                    .push_str(&format!("enum {} {{\n", target::ident(source.name.0)?));
                for (variant, _) in &source.variants {
                    let value = enm
                        .variants()
                        .find(|(name, _)| *name == variant.name)
                        .map(|(_, value)| value)
                        .ok_or_else(|| error("missing resolved enum variant", None))?;
                    if i32::try_from(value).is_err() || value == i64::from(i32::MIN) {
                        return Err(error(
                            "enum declaration literal outside verified target range",
                            None,
                        ));
                    }
                    self.output.text.push_str(&format!(
                        "    {} = {value},\n",
                        target::ident(variant.name)?
                    ));
                }
                self.output.text.push_str("}\n");
            }
            Item::Let(source) => {
                let R::Field(id) = resolved else {
                    return Err(error("field binding must identify a field", None));
                };
                let (name, def) = self
                    .symbols
                    .get_field(id)
                    .ok_or_else(|| error("missing bound field", None))?;
                if name != source.name.0 {
                    return Err(error("field binding name mismatch", None));
                }
                if patch {
                    self.validate_patch(id.parent())?;
                }
                self.output.text.push_str(&format!(
                    "var {}: {}",
                    target::ident(name)?,
                    target::typ(def.type_(), self.symbols)?
                ));
                if source.default.is_some() {
                    let value = self
                        .unit
                        .classes
                        .get(&id.parent())
                        .and_then(|c| c.fields.get(&id.index()))
                        .or_else(|| self.unit.added_fields.get(&id).and_then(|v| v.as_deref()))
                        .ok_or_else(|| {
                            error("field initializer has no retained typed constant", None)
                        })?;
                    self.output
                        .text
                        .push_str(&format!(" = {}", constant(value, self.symbols)?));
                }
                self.output.text.push_str(";\n");
            }
            Item::Function(source) => {
                crate::source::validate(source)?;
                if !source.type_params.is_empty() {
                    return Err(error("custom generic declarations are unsupported", None));
                }
                let (name, signature, body, override_, wrapper) = self.function(resolved)?;
                if name != source.name.0 {
                    return Err(error("function binding name mismatch", None));
                }
                if override_ && !member {
                    return Err(error("override binding outside class", None));
                }
                if let R::AddedMethod(id) | R::Replacement(id) | R::Wrapper(id, _) = resolved {
                    self.validate_patch(id.parent())?;
                }
                if !signature.type_params().is_empty() {
                    return Err(error("custom generic declarations are unsupported", None));
                }
                if callback && !member && !patch {
                    return Err(error("callback requires member context", None));
                }
                let body = body
                    .map(|body| Body::new(body, self.symbols, signature, callback, wrapper))
                    .transpose()?;
                if override_ && !callback {
                    self.output.text.push_str("override ");
                }
                self.output
                    .text
                    .push_str(if callback { "event " } else { "function " });
                self.output.text.push_str(target::ident(name)?);
                self.output.text.push('(');
                for (i, param) in signature.params().iter().enumerate() {
                    if i > 0 {
                        self.output.text.push_str(", ");
                    }
                    let flags = param.flags();
                    if flags.is_optional() {
                        self.output.text.push_str("optional ");
                    }
                    if flags.is_out() {
                        self.output.text.push_str("out ");
                    }
                    if flags.is_const() {
                        self.output.text.push_str("const ");
                    }
                    let local = ir::Local::Param(
                        u16::try_from(i).map_err(|_| error("too many parameters", None))?,
                    );
                    let name = if let Some(body) = &body {
                        body.local(local)?
                    } else {
                        target::ident(param.name())?
                    };
                    self.output.text.push_str(&format!(
                        "{name}: {}",
                        target::typ(param.type_(), self.symbols)?
                    ));
                }
                self.output.text.push(')');
                if !callback {
                    self.output.text.push_str(&format!(
                        " -> {}",
                        target::typ(signature.return_type(), self.symbols)?
                    ));
                }
                if source.body.is_some() != body.is_some() {
                    return Err(error(
                        "declaration/body binding mismatch (inactive variant or wrong identity)",
                        Some(span),
                    ));
                }
                if let Some(body) = body {
                    let (text, mappings) = body.emit()?;
                    self.output.text.push(' ');
                    let offset = self.output.text.len();
                    self.output.text.push_str(&text);
                    self.output
                        .mappings
                        .extend(mappings.into_iter().map(|m| SourceMapping {
                            generated: m.generated.start + offset..m.generated.end + offset,
                            source: m.source,
                        }));
                } else {
                    self.output.text.push_str(";\n");
                }
            }
            Item::Import(_) => unreachable!(),
        }
        Ok(())
    }

    fn validate_binding(
        &self,
        decl: &SourceItemDecl<'ctx>,
        path: &DeclarationPath,
        resolved: R<'ctx>,
        member: bool,
    ) -> Result<()> {
        let mut patches = decl.annotations.iter().filter(|(a, _)| {
            matches!(
                a.name,
                "addField" | "addMethod" | "replaceMethod" | "wrapMethod"
            )
        });
        let patch = patches.next();
        if patches.next().is_some() {
            return Err(error("multiple patch annotations are unsupported", None));
        }
        let expected = match resolved {
            R::AddedMethod(id) => Some(("addMethod", id.parent())),
            R::Replacement(id) => Some(("replaceMethod", id.parent())),
            R::Wrapper(id, _) => Some(("wrapMethod", id.parent())),
            R::Field(id) if !member => Some(("addField", id.parent())),
            _ => None,
        };
        match (expected, patch) {
            (Some((kind, parent)), Some((annotation, _))) if annotation.name == kind && !member => {
                let [(ast::Expr::Ident(name), _)] = &annotation.args[..] else {
                    return Err(error("patch requires one class identifier", None));
                };
                if *name != parent.simple_name() && *name != parent.as_str() {
                    return Err(error("patch annotation and resolved parent disagree", None));
                }
            }
            (None, None) => {}
            _ => {
                return Err(error(
                    "patch annotation and resolved binding kind disagree",
                    None,
                ));
            }
        }
        if member {
            let parent_path = DeclarationPath(path.0[..path.0.len() - 1].to_vec());
            let Some(R::Type(parent)) = self.module.bindings.get(&parent_path) else {
                return Err(error("member has no containing resolved type", None));
            };
            let actual = match resolved {
                R::Field(id) => id.parent(),
                R::Method(id) => id.parent(),
                _ => return Err(error("invalid member binding kind", None)),
            };
            if *parent != actual {
                return Err(error("member binding belongs to a different class", None));
            }
        } else if matches!(resolved, R::Method(_)) {
            return Err(error("ordinary method binding outside a class", None));
        }
        Ok(())
    }

    fn validate_patch(&self, parent: redscript_compiler_frontend::TypeId<'ctx>) -> Result<()> {
        let agg = self
            .symbols
            .get_type(parent)
            .and_then(|d| d.schema().as_aggregate())
            .ok_or_else(|| error("patch target is not an aggregate", None))?;
        if agg.flags().is_import_only() {
            return Err(error("patching importonly classes is unsupported", None));
        }
        if self.unit.classes.contains_key(&parent) {
            return Err(error(
                "patching source-defined classes is unsafe in REDmod",
                None,
            ));
        }
        Ok(())
    }

    fn function(&self, resolved: R<'ctx>) -> Result<FunctionBinding<'a, 'ctx>> {
        if let R::Function(id) = resolved {
            let (name, def) = self
                .symbols
                .get_free_function(id)
                .ok_or_else(|| error("missing bound function", None))?;
            // A declaration name is unqualified; calls retain the resolved full path.
            return Ok((
                name.as_single_component()
                    .unwrap_or_else(|| name.into_iter().last().unwrap_or("")),
                def.type_(),
                self.unit.functions.get(&id),
                false,
                None,
            ));
        }
        let id = match resolved {
            R::Method(id) | R::AddedMethod(id) | R::Replacement(id) | R::Wrapper(id, _) => id,
            _ => return Err(error("function binding kind mismatch", None)),
        };
        let (name, def) = self
            .symbols
            .get_method(id)
            .ok_or_else(|| error("missing bound method", None))?;
        let body = match resolved {
            R::Method(_) => self
                .unit
                .classes
                .get(&id.parent())
                .and_then(|c| c.methods.get(&id.index())),
            R::AddedMethod(_) => self.unit.added_methods.get(&id).and_then(Option::as_ref),
            R::Replacement(_) => self.unit.method_replacements.get(&id),
            R::Wrapper(_, i) => self.unit.method_wrappers.get(&id).and_then(|v| v.get(i)),
            _ => unreachable!(),
        };
        Ok((
            name,
            def.type_(),
            body,
            matches!(resolved, R::Method(_)) && def.overriden().is_some(),
            matches!(resolved, R::Wrapper(..)).then_some(id),
        ))
    }

    fn metadata(&mut self, decl: &SourceItemDecl<'ctx>) -> Result<()> {
        for doc in &decl.doc {
            if !doc.starts_with("///") || doc.contains(['\n', '\r']) {
                return Err(error("invalid documentation comment", None));
            }
            self.output.text.push_str(doc);
            self.output.text.push('\n');
        }
        for (a, span) in &decl.annotations {
            if a.name == "mixedRef" {
                return Err(error("mixedRef types are unsupported", Some(*span)));
            }
            if !matches!(
                a.name,
                "if" | "runtimeProperty"
                    | "addField"
                    | "addMethod"
                    | "replaceMethod"
                    | "wrapMethod"
            ) {
                return Err(error(
                    format!("unsupported target annotation @{}", a.name),
                    Some(*span),
                ));
            }
            self.output.text.push_str(&format!(
                "@{}({})\n",
                a.name,
                a.args
                    .iter()
                    .map(|(e, _)| metadata_expr(e))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            ));
        }
        Ok(())
    }
}

fn metadata_expr(expr: &ast::SourceExpr<'_>) -> Result<String> {
    match expr {
        ast::Expr::Ident(name) => target::path(name).map(str::to_owned),
        ast::Expr::Constant(ast::Constant::String(s)) => Ok(target::string(s)?.to_string()),
        ast::Expr::Constant(ast::Constant::Bool(v)) => Ok(v.to_string()),
        ast::Expr::UnOp {
            op: ast::UnOp::Not,
            expr,
        } => Ok(format!("!{}", metadata_expr(&expr.0)?)),
        ast::Expr::Call {
            expr,
            type_args,
            args,
        } if type_args.is_empty() => Ok(format!(
            "{}({})",
            metadata_expr(&expr.0)?,
            args.iter()
                .map(|(e, _)| metadata_expr(e))
                .collect::<Result<Vec<_>>>()?
                .join(", ")
        )),
        _ => Err(error("unsupported annotation expression", None)),
    }
}

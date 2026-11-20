use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use redscript_community_ast as ast;
use redscript_compiler_frontend::{
    FreeFunctionIndex, FunctionType, LoweredFunction, MethodId, Symbols, Type, TypeId, TypeSchema,
    expr_type, ir, predef,
};

use crate::names::TargetNamePlan;
use crate::{Result, SourceMapping, error, target};

mod borrows;
mod constants;
mod expr;
mod intrinsics;
mod naming;

pub(crate) use constants::{constant, float_literal};
pub(crate) use naming::reserved_names;
use naming::{authored_name, contextual_temporary_name, temporary_name};

pub(crate) struct Body<'a, 'ctx> {
    pub function: &'a LoweredFunction<'ctx>,
    pub symbols: &'a Symbols<'ctx>,
    pub signature: &'a FunctionType<'ctx>,
    pub names: BTreeMap<ir::Local, String>,
    pub plan: Option<&'a TargetNamePlan>,

    pub wrapper: Option<MethodId<'ctx>>,
    pub(super) result_local: String,
    pub(super) inline_declarations: BTreeSet<ir::Local>,
    pub(super) omitted_defaults: BTreeSet<(ir::Local, ast::Span)>,
    pub(super) enum_defaults: RefCell<HashMap<TypeId<'ctx>, Option<&'ctx str>>>,
}

impl<'a, 'ctx> Body<'a, 'ctx> {
    pub fn new(
        function: &'a LoweredFunction<'ctx>,
        symbols: &'a Symbols<'ctx>,
        signature: &'a FunctionType<'ctx>,
        symbol_names: &BTreeSet<String>,
        parent_members: &BTreeSet<&str>,
        wrapper: Option<MethodId<'ctx>>,
    ) -> Result<Self> {
        if !signature.type_params().is_empty() {
            return Err(error(
                "generic functions are not supported; use concrete parameter and return types",
                Some(function.span),
            ));
        }
        let mut occupied = BTreeSet::new();
        let preferred = |local: &ir::LocalInfo<'ctx>| match local.id {
            ir::Local::Param(i) => signature
                .params()
                .get(usize::from(i))
                .map(redscript_compiler_frontend::Param::name),
            _ => local.name,
        };
        let reserved: BTreeSet<_> = function
            .locals
            .iter()
            .filter_map(&preferred)
            .filter(|name| target::ident(name).is_ok())
            .map(str::to_owned)
            .collect();
        let mut names = BTreeMap::new();
        names.insert(ir::Local::This, "this".to_owned());
        // Allocate authored bindings first so contextual temporary names can use
        // the owner's final hygienic spelling, even when it was allocated later.
        for local in function
            .locals
            .iter()
            .filter(|local| local.temporary_purpose.is_none())
            .chain(
                function
                    .locals
                    .iter()
                    .filter(|local| local.temporary_purpose.is_some()),
            )
        {
            target::typ(
                &local
                    .typ
                    .coalesce(symbols)
                    .map_err(|e| error(e.to_string(), local.span))?,
                symbols,
            )?;
            if local.id == ir::Local::This {
                continue;
            }
            if names.contains_key(&local.id) {
                return Err(error("duplicate local identity", local.span));
            }
            let source_name = preferred(local).filter(|name| target::ident(name).is_ok());
            let candidate = preferred(local).and_then(authored_name);
            let base = candidate
                .or_else(|| contextual_temporary_name(local, &names))
                .or_else(|| {
                    local
                        .temporary_purpose
                        .map(|purpose| temporary_name(purpose).to_owned())
                })
                .unwrap_or_else(|| match local.id {
                    ir::Local::Param(i) => format!("__rst_p{i}"),
                    ir::Local::Var(i) => format!("__rst_v{i}"),
                    ir::Local::This => unreachable!(),
                });
            let mut name = base.clone();
            let mut suffix = 2;
            let is_param = matches!(local.id, ir::Local::Param(_));
            let shadows_member = |n: &str| is_param && parent_members.contains(n);
            while symbol_names.contains(&name)
                || occupied.contains(&name)
                || shadows_member(&name)
                || (reserved.contains(&name) && source_name != Some(name.as_str()))
            {
                name = format!("{base}_{suffix}");
                suffix += 1;
            }
            occupied.insert(name.clone());
            names.insert(local.id, name);
        }
        let mut result_local = "__rst_result".to_owned();
        while symbol_names.contains(&result_local)
            || reserved.contains(&result_local)
            || !occupied.insert(result_local.clone())
        {
            result_local.push('_');
        }
        let mut inline_declarations = crate::locals::inline_declarations(&function.block, symbols)?;
        let omitted_defaults =
            crate::defaults::overwritten_defaults(&function.block, &inline_declarations);
        for (local, _) in &omitted_defaults {
            inline_declarations.remove(local);
        }
        Ok(Self {
            function,
            symbols,
            signature,
            names,
            plan: None,

            wrapper,
            result_local,
            inline_declarations,
            omitted_defaults,
            enum_defaults: RefCell::new(HashMap::new()),
        })
    }

    /// Attach the module's import-aware name plan. Bodies built without one keep
    /// the unchecked simple-name spelling used only by isolated unit fixtures.
    pub(crate) fn with_plan(mut self, plan: &'a TargetNamePlan) -> Self {
        self.plan = Some(plan);
        self
    }

    pub(super) fn type_text(&self, typ: &Type<'ctx>) -> Result<String> {
        target::typ_planned(typ, self.symbols, self.plan)
    }

    pub(super) fn function_name(&self, index: FreeFunctionIndex) -> Result<String> {
        if let Some(plan) = self.plan {
            plan.free_function_name(index, self.symbols)
        } else {
            let (name, _) = self
                .symbols
                .get_free_function(index)
                .ok_or_else(|| error("missing resolved function", None))?;
            target::path(&name.to_string()).map(str::to_owned)
        }
    }

    pub fn local(&self, local: ir::Local) -> Result<&str> {
        self.names
            .get(&local)
            .map(String::as_str)
            .ok_or_else(|| error("missing local identity", Some(self.function.span)))
    }

    fn local_declaration(&self, local: ir::Local) -> Result<String> {
        let typ = self
            .function
            .find_local(local)
            .ok_or_else(|| error("missing local identity", Some(self.function.span)))?
            .coalesce(self.symbols)
            .map_err(|e| error(e.to_string(), Some(self.function.span)))?;
        Ok(format!("{}: {}", self.local(local)?, self.type_text(&typ)?))
    }

    pub fn emit_at(mut self, depth: usize) -> Result<(String, Vec<SourceMapping>)> {
        let pad = "    ".repeat(depth + 1);
        let mut text = String::from("{\n");
        let mut mappings = Vec::new();
        for local in &self.function.locals {
            if matches!(local.id, ir::Local::Var(_))
                && !self.inline_declarations.contains(&local.id)
            {
                let typ = local
                    .typ
                    .coalesce(self.symbols)
                    .map_err(|e| error(e.to_string(), local.span))?;
                text.push_str(&format!(
                    "{pad}var {}: {};\n",
                    self.local(local.id)?,
                    self.type_text(&typ)?
                ));
            }
        }
        if self.is_enum(self.signature.return_type()) {
            text.push_str(&format!(
                "{pad}var {}: {};\n",
                self.result_local,
                self.type_text(self.signature.return_type())?
            ));
        }
        self.block(&self.function.block, depth + 1, &mut text, &mut mappings)?;
        text.push_str(&format!("{}}}\n", "    ".repeat(depth)));
        Ok((text, mappings))
    }

    fn block(
        &mut self,
        block: &ir::Block<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        for stmt in &block.stmts {
            self.mapped_stmt(stmt, depth, text, mappings)?;
        }
        Ok(())
    }

    fn mapped_stmt(
        &mut self,
        stmt: &ir::Stmt<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        let start = text.len();
        self.stmt(stmt, depth, text, mappings).map_err(|mut e| {
            if e.span.is_none() {
                e.span = Some(stmt.span());
            }
            e
        })?;
        mappings.push(SourceMapping {
            generated: start..text.len(),
            source: stmt.span(),
        });
        Ok(())
    }

    fn loop_block(
        &mut self,
        block: &ir::Block<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        let Some(ir::Stmt::Branches {
            branches,
            default: Some(default),
            span,
        }) = block.stmts.back()
        else {
            return self.block(block, depth, text, mappings);
        };
        let [branch] = &branches[..] else {
            return self.block(block, depth, text, mappings);
        };
        let Some(break_stmt @ ir::Stmt::Break(_)) = default.stmts.front() else {
            return self.block(block, depth, text, mappings);
        };
        if default.stmts.len() != 1 {
            return self.block(block, depth, text, mappings);
        }

        // Only the final branch directly inside the loop can become a guard:
        // its break still exits this loop, and no following statements are skipped.
        for stmt in block.stmts.iter().take(block.stmts.len() - 1) {
            self.mapped_stmt(stmt, depth, text, mappings)?;
        }
        let start = text.len();
        let pad = "    ".repeat(depth);
        text.push_str(&format!(
            "{pad}if ({}) {{\n",
            self.negated(&branch.condition)?
        ));
        mappings.push(SourceMapping {
            generated: start..text.len(),
            source: branch.condition.span(),
        });
        self.mapped_stmt(break_stmt, depth + 1, text, mappings)?;
        text.push_str(&format!("{pad}}}\n"));
        self.block(&branch.block, depth, text, mappings)?;
        mappings.push(SourceMapping {
            generated: start..text.len(),
            source: *span,
        });
        Ok(())
    }

    fn stmt(
        &mut self,
        stmt: &ir::Stmt<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        let pad = "    ".repeat(depth);
        text.push_str(&pad);
        match stmt {
            ir::Stmt::Expr(e) => {
                if let ir::Expr::Assign { place, expr, .. } = &**e
                    && let ir::Expr::Local(local, _) = &**place
                    && self.inline_declarations.remove(local)
                {
                    text.push_str(&format!(
                        "var {} = {};\n",
                        self.local_declaration(*local)?,
                        self.contextual_expr(expr, &self.type_of(place)?)?
                    ));
                } else {
                    text.push_str(&format!("{};\n", self.expr(e)?));
                }
            }
            ir::Stmt::Block(block, _) => {
                // Locals are globally hygienic. The declaration planner treats
                // these flattened blocks as part of their enclosing sequence.
                text.truncate(text.len() - pad.len());
                self.block(block, depth, text, mappings)?;
            }
            ir::Stmt::While(branch, _) => {
                text.push_str(&format!("while ({}) {{\n", self.expr(&branch.condition)?));
                if matches!(
                    &*branch.condition,
                    ir::Expr::Const(ir::Const::Bool(true), _)
                ) {
                    self.loop_block(&branch.block, depth + 1, text, mappings)?;
                } else {
                    self.block(&branch.block, depth + 1, text, mappings)?;
                }
                text.push_str(&format!("{pad}}}\n"));
            }
            ir::Stmt::Branches {
                branches, default, ..
            } => {
                for (i, branch) in branches.iter().enumerate() {
                    if i > 0 {
                        text.push_str(" else ");
                    }
                    text.push_str(&format!("if ({}) {{\n", self.expr(&branch.condition)?));
                    self.block(&branch.block, depth + 1, text, mappings)?;
                    text.push_str(&format!("{pad}}}"));
                }
                if let Some(block) = default {
                    if !branches.is_empty() {
                        text.push_str(" else ");
                    }
                    text.push_str("{\n");
                    self.block(block, depth + 1, text, mappings)?;
                    text.push_str(&format!("{pad}}}"));
                }
                text.push('\n');
            }
            ir::Stmt::Switch {
                scrutinee,
                branches,
                default_index,
                default,
                ..
            } => {
                text.push_str(&format!("switch ({}) {{\n", self.expr(scrutinee)?));
                for index in 0..=branches.len() {
                    if index == *default_index
                        && let Some(block) = default
                    {
                        text.push_str(&format!("{pad}    default:\n"));
                        self.block(block, depth + 2, text, mappings)?;
                    }
                    if let Some(case) = branches.get(index) {
                        text.push_str(&format!("{pad}    case {}:\n", self.expr(&case.matcher)?));
                        self.block(&case.block, depth + 2, text, mappings)?;
                    }
                }
                text.push_str(&format!("{pad}}}\n"));
            }
            ir::Stmt::InitArray {
                local, elements, ..
            } => {
                let name = self.local(*local)?;
                // Resize before evaluating elements, as in the shared IR. Empty
                // literal temporaries must be cleared on every execution too.
                if elements.is_empty() {
                    text.push_str(&format!("{name}.Clear();\n"));
                } else {
                    let count = i32::try_from(elements.len())
                        .map_err(|_| error("array initializer exceeds target size range", None))?;
                    text.push_str(&format!("{name}.Resize({count});\n"));
                    for (i, e) in elements.iter().enumerate() {
                        text.push_str(&format!("{pad}{name}[{i}] = {};\n", self.expr(e)?));
                    }
                }
            }
            ir::Stmt::InitDefault {
                local, typ, span, ..
            } => {
                if self.omitted_defaults.remove(&(*local, *span)) {
                    text.truncate(text.len() - pad.len());
                    return Ok(());
                }
                let inline = self.inline_declarations.remove(local);
                let name = self.local(*local)?;
                let typ = typ
                    .coalesced(self.symbols)
                    .map_err(|e| error(e.to_string(), None))?;
                if matches!(&typ, Type::Data(app) if app.id() == predef::ARRAY) {
                    text.push_str(&format!("{name}.Clear();\n"));
                } else {
                    let place = if inline {
                        format!("var {}", self.local_declaration(*local)?)
                    } else {
                        name.to_owned()
                    };
                    text.push_str(&format!("{place} = {};\n", self.default_value(&typ)?));
                }
            }
            ir::Stmt::Break(_) => text.push_str("break;\n"),
            ir::Stmt::Continue(_) => text.push_str("continue;\n"),
            ir::Stmt::Return(value, _) => {
                if let Some(value) = value {
                    if self.is_enum(self.signature.return_type()) {
                        text.push_str(&format!(
                            "{} = {};\n{pad}return {};\n",
                            self.result_local,
                            self.expr(value)?,
                            self.result_local
                        ));
                        return Ok(());
                    }
                    text.push_str(&format!(
                        "return {};\n",
                        self.contextual_expr(value, self.signature.return_type())?
                    ));
                } else {
                    text.push_str("return;\n");
                }
            }
        }
        Ok(())
    }

    fn is_enum(&self, typ: &Type<'ctx>) -> bool {
        matches!(typ, Type::Data(app) if self.symbols.get_type(app.id()).is_some_and(|d| matches!(d.schema(), TypeSchema::Enum(_))))
    }

    fn default_value(&self, typ: &Type<'ctx>) -> Result<String> {
        let Type::Data(app) = typ else {
            return Err(error(
                format!("cannot default-initialize unresolved type {typ}"),
                None,
            ));
        };
        let id = app.id();
        let type_text = self.type_text(typ)?;
        if id == predef::BOOL {
            return Ok("false".into());
        }
        if id == predef::STRING {
            return Ok("\"\"".into());
        }
        if id == predef::CNAME {
            return Ok(target::cname("")?.to_string());
        }
        if id == predef::TWEAK_DB_ID {
            return constant(&ir::Const::TweakDbId("".into()), self.symbols, self.plan);
        }
        if id == predef::FLOAT {
            return Ok("0.0".into());
        }
        if id == predef::DOUBLE {
            return Ok("Cast<Double>(0.0)".into());
        }
        if id == predef::INT32 {
            return Ok("0".into());
        }
        if [
            predef::INT8,
            predef::INT16,
            predef::INT64,
            predef::UINT8,
            predef::UINT16,
            predef::UINT32,
            predef::UINT64,
        ]
        .contains(&id)
        {
            return Ok(format!("Cast<{type_text}>(0)"));
        }
        if id == predef::WREF
            || id == predef::REF
            || self
                .symbols
                .get_type(id)
                .and_then(|d| d.schema().as_aggregate())
                .is_some_and(|a| a.flags().is_heap_reference())
        {
            return Ok("NULL".into());
        }
        if let Some(TypeSchema::Enum(enm)) = self
            .symbols
            .get_type(id)
            .map(redscript_compiler_frontend::TypeDef::schema)
        {
            // Repeated local and nested-field resets inspect this enum only once
            // per body. Zero need not be the first member or an Invalid sentinel.
            let name = *self
                .enum_defaults
                .borrow_mut()
                .entry(id)
                .or_insert_with(|| {
                    enm.variants()
                        .find(|(_, value)| *value == 0)
                        .map(|(name, _)| name)
                });
            let name = name.ok_or_else(|| {
                error(
                    format!("cannot default-initialize {typ}: no declared zero-valued member"),
                    None,
                )
            })?;
            return Ok(format!("{type_text}.{}", target::ident(name)?));
        }
        if let Some(aggregate) = self
            .symbols
            .get_type(id)
            .and_then(|def| def.schema().as_aggregate())
            && aggregate.flags().is_struct()
        {
            return Ok(format!("{type_text}()"));
        }
        Err(error(
            format!("cannot default-initialize {typ}; provide an explicit value"),
            None,
        ))
    }

    pub(super) fn type_of(&self, e: &ir::Expr<'ctx>) -> Result<Type<'ctx>> {
        expr_type(e, self.function, self.symbols)
            .map_err(|err| error(err.to_string(), Some(e.span())))
    }

    pub(super) fn simple_place(e: &ir::Expr<'ctx>) -> bool {
        matches!(e, ir::Expr::Local(..))
    }
}

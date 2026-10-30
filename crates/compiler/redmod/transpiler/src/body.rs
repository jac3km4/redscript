use std::collections::{BTreeMap, BTreeSet};

use redscript_community_ast as ast;
use redscript_compiler_frontend::{
    FunctionType, LoweredFunction, MethodId, Symbols, Type, TypeSchema, expr_type, ir, predef,
};

use crate::{Result, SourceMapping, error, target};

pub(crate) struct Body<'a, 'ctx> {
    pub function: &'a LoweredFunction<'ctx>,
    pub symbols: &'a Symbols<'ctx>,
    pub signature: &'a FunctionType<'ctx>,
    pub names: BTreeMap<ir::Local, String>,
    pub callback: bool,
    pub wrapper: Option<MethodId<'ctx>>,
    result_local: String,
}

impl<'a, 'ctx> Body<'a, 'ctx> {
    pub fn new(
        function: &'a LoweredFunction<'ctx>,
        symbols: &'a Symbols<'ctx>,
        signature: &'a FunctionType<'ctx>,
        callback: bool,
        wrapper: Option<MethodId<'ctx>>,
    ) -> Result<Self> {
        if !signature.type_params().is_empty() {
            return Err(error(
                "custom generic functions are unsupported",
                Some(function.span),
            ));
        }
        let mut occupied = BTreeSet::new();
        for entry in symbols.free_functions() {
            occupied.extend(entry.name().into_iter().map(str::to_owned));
        }
        for (id, def) in symbols.types() {
            occupied.extend(id.as_str().split('.').map(str::to_owned));
            if let Some(agg) = def.schema().as_aggregate() {
                for entry in agg.methods().iter() {
                    occupied.insert(entry.name().to_string());
                }
            }
        }
        let preferred = |local: &ir::LocalInfo<'ctx>| match local.id {
            ir::Local::Param(i) => signature
                .params()
                .get(usize::from(i))
                .map(redscript_compiler_frontend::Param::name),
            _ => local.name,
        };
        let mut reserved = occupied.clone();
        reserved.extend(
            function
                .locals
                .iter()
                .filter_map(&preferred)
                .filter(|name| target::ident(name).is_ok())
                .map(str::to_owned),
        );
        let mut names = BTreeMap::new();
        names.insert(ir::Local::This, "this".to_owned());
        for local in &function.locals {
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
            let base = source_name
                .map(str::to_owned)
                .unwrap_or_else(|| match local.id {
                    ir::Local::Param(i) => format!("__rst_p{i}"),
                    ir::Local::Var(i) => format!("__rst_v{i}"),
                    ir::Local::This => unreachable!(),
                });
            let mut name = base.clone();
            let mut suffix = 2;
            while occupied.contains(&name)
                || (reserved.contains(&name) && source_name != Some(name.as_str()))
            {
                name = format!("{base}_{suffix}");
                suffix += 1;
            }
            occupied.insert(name.clone());
            names.insert(local.id, name);
        }
        let mut result_local = "__rst_result".to_owned();
        while reserved.contains(&result_local) || !occupied.insert(result_local.clone()) {
            result_local.push('_');
        }
        Ok(Self {
            function,
            symbols,
            signature,
            names,
            callback,
            wrapper,
            result_local,
        })
    }

    pub fn local(&self, local: ir::Local) -> Result<&str> {
        self.names
            .get(&local)
            .map(String::as_str)
            .ok_or_else(|| error("missing local identity", Some(self.function.span)))
    }

    pub fn emit(&self) -> Result<(String, Vec<SourceMapping>)> {
        let mut text = String::from("{\n");
        let mut mappings = Vec::new();
        for local in &self.function.locals {
            if matches!(local.id, ir::Local::Var(_)) {
                let typ = local
                    .typ
                    .coalesce(self.symbols)
                    .map_err(|e| error(e.to_string(), local.span))?;
                text.push_str(&format!(
                    "    var {}: {};\n",
                    self.local(local.id)?,
                    target::typ(&typ, self.symbols)?
                ));
            }
        }
        if (self.callback && *self.signature.return_type() != Type::nullary(predef::VOID))
            || self.is_enum(self.signature.return_type())
        {
            text.push_str(&format!(
                "    var {}: {};\n",
                self.result_local,
                target::typ(self.signature.return_type(), self.symbols)?
            ));
        }
        self.block(&self.function.block, 1, &mut text, &mut mappings)?;
        text.push_str("}\n");
        Ok((text, mappings))
    }

    fn block(
        &self,
        block: &ir::Block<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        for stmt in &block.stmts {
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
        }
        Ok(())
    }

    fn stmt(
        &self,
        stmt: &ir::Stmt<'ctx>,
        depth: usize,
        text: &mut String,
        mappings: &mut Vec<SourceMapping>,
    ) -> Result<()> {
        let pad = "    ".repeat(depth);
        text.push_str(&pad);
        match stmt {
            ir::Stmt::Expr(e) => text.push_str(&format!("{};\n", self.expr(e)?)),
            ir::Stmt::Block(block, _) => {
                // All declarations are already hoisted and renamed, so lexical
                // grouping carries no remaining target semantics.
                text.truncate(text.len() - pad.len());
                self.block(block, depth, text, mappings)?;
            }
            ir::Stmt::While(branch, _) => {
                text.push_str(&format!("while ({}) {{\n", self.expr(&branch.condition)?));
                self.block(&branch.block, depth + 1, text, mappings)?;
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
                default,
                ..
            } => {
                text.push_str(&format!("switch ({}) {{\n", self.expr(scrutinee)?));
                for case in branches {
                    text.push_str(&format!("{pad}    case {}:\n", self.expr(&case.matcher)?));
                    self.block(&case.block, depth + 2, text, mappings)?;
                }
                if let Some(block) = default {
                    text.push_str(&format!("{pad}    default:\n"));
                    self.block(block, depth + 2, text, mappings)?;
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
            ir::Stmt::InitDefault { local, typ, .. } => {
                let name = self.local(*local)?;
                let typ = typ
                    .coalesced(self.symbols)
                    .map_err(|e| error(e.to_string(), None))?;
                if matches!(&typ, Type::Data(app) if app.id() == predef::ARRAY) {
                    text.push_str(&format!("{name}.Clear();\n"));
                } else {
                    text.push_str(&format!("{name} = {};\n", self.default_value(&typ)?));
                }
            }
            ir::Stmt::Break(_) => text.push_str("break;\n"),
            ir::Stmt::Continue(_) => text.push_str("continue;\n"),
            ir::Stmt::Return(value, _) => {
                if self.callback {
                    if let Some(e) = value {
                        // Evaluate even a lazy/side-effecting result before dropping it.
                        text.push_str(&format!(
                            "{} = {};\n{pad}",
                            self.result_local,
                            self.expr(e)?
                        ));
                    }
                    text.push_str("return;\n");
                } else if let Some(value) = value {
                    if self.is_enum(self.signature.return_type()) {
                        text.push_str(&format!(
                            "{} = {};\n{pad}return {};\n",
                            self.result_local,
                            self.expr(value)?,
                            self.result_local
                        ));
                        return Ok(());
                    }
                    text.push_str(&format!("return {};\n", self.expr(value)?));
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
            return Err(error("unresolved default initialization", None));
        };
        let id = app.id();
        target::typ(typ, self.symbols)?;
        if id == predef::BOOL {
            return Ok("false".into());
        }
        if id == predef::STRING {
            return Ok("\"\"".into());
        }
        if id == predef::CNAME {
            return Ok("n\"\"".into());
        }
        if id == predef::FLOAT {
            return Ok("0.0".into());
        }
        if id == predef::DOUBLE {
            return Ok("Cast<Double>(0.0)".into());
        }
        if [
            predef::INT8,
            predef::INT16,
            predef::INT32,
            predef::INT64,
            predef::UINT8,
            predef::UINT16,
            predef::UINT32,
            predef::UINT64,
        ]
        .contains(&id)
        {
            return Ok(format!("Cast<{}>(0)", target::typ(typ, self.symbols)?));
        }
        if id == predef::WREF
            || id == predef::REF
            || self
                .symbols
                .get_type(id)
                .and_then(|d| d.schema().as_aggregate())
                .is_some_and(|a| !a.flags().is_struct() && !a.flags().is_never_ref())
        {
            return Ok("NULL".into());
        }
        Err(error(
            "no verified target default/reset operation for this type",
            None,
        ))
    }

    fn type_of(&self, e: &ir::Expr<'ctx>) -> Result<Type<'ctx>> {
        expr_type(e, self.function, self.symbols)
            .map_err(|err| error(err.to_string(), Some(e.span())))
    }

    fn simple_place(e: &ir::Expr<'ctx>) -> bool {
        matches!(e, ir::Expr::Local(..))
    }

    pub fn expr(&self, e: &ir::Expr<'ctx>) -> Result<String> {
        if !matches!(e, ir::Expr::Null { .. }) {
            target::typ(&self.type_of(e)?, self.symbols).map_err(|mut err| {
                err.span = Some(e.span());
                err
            })?;
        }
        let result = match e {
            ir::Expr::Local(id, _) => Ok(self.local(*id)?.to_owned()),
            ir::Expr::Const(c, _) => constant(c, self.symbols),
            ir::Expr::Null { .. } => Ok("NULL".into()),
            ir::Expr::NewInstance { class_type, .. } => Ok(format!(
                "new {}",
                target::typ(
                    &class_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?
                        .into_type(),
                    self.symbols
                )?
            )),
            ir::Expr::Construct {
                struct_type, args, ..
            } => Ok(format!(
                "{}({})",
                target::typ(
                    &struct_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?
                        .into_type(),
                    self.symbols
                )?,
                self.args(args)?
            )),
            ir::Expr::Assign { place, expr, .. } => {
                if !self.place(place)? {
                    return Err(error(
                        "unsafe assignment place requires reference/write-back lowering",
                        Some(e.span()),
                    ));
                }
                Ok(format!("{} = {}", self.expr(place)?, self.expr(expr)?))
            }
            ir::Expr::Index { array, index, .. } => {
                if !Self::simple_place(array) {
                    return Err(error(
                        "array indexing requires a local receiver",
                        Some(e.span()),
                    ));
                }
                Ok(format!("{}[{}]", self.expr(array)?, self.expr(index)?))
            }
            ir::Expr::Field {
                receiver, field, ..
            } => {
                if !Self::simple_place(receiver) {
                    return Err(error(
                        "field access requires a stable local receiver",
                        Some(e.span()),
                    ));
                }
                let (name, field) = self
                    .symbols
                    .get_field(*field)
                    .ok_or_else(|| error("missing resolved field", None))?;
                if field.flags().is_persistent()
                    && !matches!(&**receiver, ir::Expr::Local(ir::Local::This, _))
                {
                    return Err(error(
                        "external persistent-field access is unsupported",
                        Some(e.span()),
                    ));
                }
                Ok(format!("{}.{}", self.expr(receiver)?, target::ident(name)?))
            }
            ir::Expr::Conditional {
                condition,
                then,
                else_,
                ..
            } => Ok(format!(
                "({} ? {} : {})",
                self.expr(condition)?,
                self.expr(then)?,
                self.expr(else_)?
            )),
            ir::Expr::DynCast {
                expr, target_type, ..
            } => Ok(format!(
                "({} as {})",
                self.expr(expr)?,
                target::typ(
                    &target_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?,
                    self.symbols
                )?
            )),
            ir::Expr::Call { call, .. } => self.call(call, e),
            ir::Expr::NewClosure { .. } | ir::Expr::Capture(..) => {
                Err(error("lambdas/captures are unsupported", Some(e.span())))
            }
        };
        result.map_err(|mut err| {
            if err.span.is_none() {
                err.span = Some(e.span());
            }
            err
        })
    }

    fn place(&self, e: &ir::Expr<'ctx>) -> Result<bool> {
        Ok(match e {
            ir::Expr::Local(..) => true,
            ir::Expr::Index { array, .. } => Self::simple_place(array),
            ir::Expr::Field { receiver, .. } => {
                let typ = self.type_of(receiver)?;
                Self::simple_place(receiver)
                    && matches!(typ, Type::Data(app) if self.symbols.get_type(app.id()).and_then(|d| d.schema().as_aggregate()).is_some_and(|a| !a.flags().is_struct() && !a.flags().is_never_ref()))
            }
            _ => false,
        })
    }

    fn args(&self, args: &[ir::Expr<'ctx>]) -> Result<String> {
        args.iter()
            .map(|e| self.expr(e))
            .collect::<Result<Vec<_>>>()
            .map(|v| v.join(", "))
    }

    fn check_out(&self, signature: &FunctionType<'ctx>, args: &[ir::Expr<'ctx>]) -> Result<()> {
        for (param, arg) in signature.params().iter().zip(args) {
            if param.flags().is_out() && !Self::simple_place(arg) {
                return Err(error(
                    "out arguments require local identity; copying a place is unsafe",
                    Some(arg.span()),
                ));
            }
        }
        Ok(())
    }

    fn call(&self, call: &ir::Call<'ctx>, whole: &ir::Expr<'ctx>) -> Result<String> {
        match call {
            ir::Call::FreeFunction {
                function,
                type_args,
                args,
            } => {
                let (name, def) = self
                    .symbols
                    .get_free_function(*function)
                    .ok_or_else(|| error("missing resolved function", None))?;
                if let Some(intrinsic) = def.intrinsic() {
                    return self.intrinsic(intrinsic, args, whole);
                }
                self.check_out(def.type_(), args)?;
                if let Some(alias) = def.method_alias() {
                    if self.wrapper != Some(alias) {
                        return Err(error(
                            "original-call alias outside its resolved wrapper",
                            None,
                        ));
                    }
                    let (name, _) = self
                        .symbols
                        .get_method(alias)
                        .ok_or_else(|| error("missing wrapper alias", None))?;
                    return Ok(format!("{}({})", target::ident(name)?, self.args(args)?));
                }
                let name = name.to_string();
                // Operators and Cast must be resolved native built-ins, not same-name user functions.
                if def.flags().is_native() {
                    if let Some(op) = ast::BinOp::from_name(&name)
                        && let [a, b] = &args[..]
                    {
                        return Ok(format!(
                            "({} {} {})",
                            self.expr(a)?,
                            binary_token(op),
                            self.expr(b)?
                        ));
                    }
                    if let Some(op) = ast::UnOp::from_name(&name)
                        && let [a] = &args[..]
                    {
                        return Ok(format!("({}{})", unary_token(op), self.expr(a)?));
                    }
                    if name == "Cast" && args.len() == 1 {
                        return Ok(format!(
                            "Cast<{}>(({}))",
                            target::typ(&self.type_of(whole)?, self.symbols)?,
                            self.expr(&args[0])?
                        ));
                    }
                }
                if !type_args.is_empty() || !def.type_().type_params().is_empty() {
                    return Err(error("custom generic calls are unsupported", None));
                }
                if matches!(
                    name.as_str(),
                    "RoundF" | "AngleNormalize" | "AngleDistance" | "AngleApproach" | "LerpAngleF"
                ) && def.flags().is_native()
                {
                    return Err(error("helper unavailable in REDmod", None));
                }
                Ok(format!("{}({})", target::path(&name)?, self.args(args)?))
            }
            ir::Call::Static {
                parent_id,
                parent_type_args,
                method,
                type_args,
                args,
            } => {
                if !parent_type_args.is_empty() || !type_args.is_empty() {
                    return Err(error("generic method calls are unsupported", None));
                }
                let (name, def) = self
                    .symbols
                    .get_method(*method)
                    .ok_or_else(|| error("missing resolved method", None))?;
                self.check_out(def.type_(), args)?;
                if !def.type_().type_params().is_empty() {
                    return Err(error("custom generic methods are unsupported", None));
                }
                if def.flags().is_callback() {
                    return Err(error("callback result/use is unsupported", None));
                }
                Ok(format!(
                    "{}.{}({})",
                    target::path(parent_id.as_str())?,
                    target::ident(name)?,
                    self.args(args)?
                ))
            }
            ir::Call::Instance {
                receiver,
                method,
                type_args,
                mode,
                ..
            } => {
                if !type_args.is_empty() {
                    return Err(error("generic method calls are unsupported", None));
                }
                let (name, def) = self
                    .symbols
                    .get_method(*method)
                    .ok_or_else(|| error("missing resolved method", None))?;
                self.check_out(def.type_(), call.args())?;
                if !def.type_().type_params().is_empty() {
                    return Err(error("custom generic methods are unsupported", None));
                }
                if def.flags().is_callback() {
                    return Err(error("callback result/use is unsupported", None));
                }
                if !Self::simple_place(receiver) {
                    return Err(error(
                        "method receiver needs verified materialization",
                        None,
                    ));
                }
                let recv = if matches!(mode, ir::CallMode::ForceStatic) {
                    if !matches!(&**receiver, ir::Expr::Local(ir::Local::This, _)) {
                        return Err(error("unsupported static dispatch receiver", None));
                    }
                    "super".into()
                } else {
                    self.expr(receiver)?
                };
                Ok(format!(
                    "{recv}.{}({})",
                    target::ident(name)?,
                    self.args(call.args())?
                ))
            }
            ir::Call::Closure { .. } => Err(error("function values are unsupported", None)),
        }
    }

    fn intrinsic(
        &self,
        intrinsic: ir::Intrinsic,
        args: &[ir::Expr<'ctx>],
        whole: &ir::Expr<'ctx>,
    ) -> Result<String> {
        use ir::Intrinsic as I;
        let unary = || {
            args.first()
                .ok_or_else(|| error("missing intrinsic operand", None))
        };
        let cast = || -> Result<String> {
            Ok(format!(
                "({} as {})",
                self.expr(unary()?)?,
                target::typ(&self.type_of(whole)?, self.symbols)?
            ))
        };
        let method = |name: &str| -> Result<String> {
            let recv = unary()?;
            if !Self::simple_place(recv) {
                return Err(error(
                    "array intrinsic requires a local place, not a copied receiver",
                    Some(recv.span()),
                ));
            }
            Ok(format!(
                "{}.{}({})",
                self.expr(recv)?,
                name,
                self.args(&args[1..])?
            ))
        };
        match intrinsic {
            I::Equals | I::NotEquals => {
                let [a, b] = args else {
                    return Err(error("invalid equality arity", None));
                };
                Ok(format!(
                    "({} {} {})",
                    self.expr(a)?,
                    if intrinsic == I::Equals { "==" } else { "!=" },
                    self.expr(b)?
                ))
            }
            I::ArrayClear => method("Clear"),
            I::ArraySize => method("Size"),
            I::ArrayResize => method("Resize"),
            I::ArrayFindFirst => method("FindFirst"),
            I::ArrayFindLast => method("FindLast"),
            I::ArrayContains => method("Contains"),
            I::ArrayPush => method("PushBack"),
            I::ArrayPop => method("PopBack"),
            I::ArrayInsert => method("Insert"),
            I::ArrayRemove => method("Remove"),
            I::ArrayErase => method("Erase"),
            I::ArrayLast => method("Last"),
            I::EnumInt | I::IntEnum | I::ToVariant | I::FromVariant => cast(),
            I::Deref => self.expr(unary()?),
            I::AsRef => {
                if !Self::simple_place(unary()?) {
                    return Err(error("AsRef requires a stable local place", None));
                }
                self.expr(unary()?)
            }
            I::RefToWeakRef | I::WeakRefToRef => {
                // REDmod assignment/call contexts perform strong/weak coercions.
                // An explicit cast to wref<T> is not a verified target operation.
                Err(error(
                    "strong/weak reference coercion requires verified contextual target lowering",
                    None,
                ))
            }
            I::IsDefined => {
                let typ = self.type_of(unary()?)?;
                let inner = typ.unwrap_ref_or_self();
                let Type::Data(app) = inner else {
                    return Err(error("unresolved IsDefined operand", None));
                };
                if self
                    .symbols
                    .get_type(app.id())
                    .and_then(|d| d.schema().as_aggregate())
                    .is_some_and(|a| !a.flags().is_struct() && !a.flags().is_never_ref())
                {
                    Ok(format!("({} != NULL)", self.expr(unary()?)?))
                } else {
                    Err(error(
                        "IsDefined only supports resolved reference classes",
                        None,
                    ))
                }
            }
            I::ToString => {
                let typ = self.type_of(unary()?)?;
                let Type::Data(app) = typ else {
                    return Err(error("unresolved ToString operand", None));
                };
                let id = app.id();
                let value = self.expr(unary()?)?;
                if id == predef::STRING {
                    return Ok(value);
                }
                let name = if id == predef::INT32 {
                    "IntToString"
                } else if id == predef::FLOAT {
                    "FloatToString"
                } else if id == predef::BOOL {
                    "BoolToString"
                } else if id == predef::CNAME {
                    "NameToString"
                } else {
                    return Err(error("no verified ToString mapping for operand type", None));
                };
                Ok(format!("{name}({value})"))
            }
            I::ArrayCount
            | I::ArrayGrow
            | I::ArraySort
            | I::ArraySortByPredicate
            | I::LongIntEnum
            | I::VariantIsDefined
            | I::VariantIsRef
            | I::VariantIsArray
            | I::VariantTypeName
            | I::NameOf => Err(error(
                format!("unsupported intrinsic {}", intrinsic.name()),
                None,
            )),
        }
    }
}

fn binary_token(op: ast::BinOp) -> &'static str {
    use ast::BinOp as B;
    match op {
        B::AssignAdd => "+=",
        B::AssignSub => "-=",
        B::AssignMul => "*=",
        B::AssignDiv => "/=",
        B::AssignBitOr => "|=",
        B::AssignBitAnd => "&=",
        B::Or => "||",
        B::And => "&&",
        B::BitOr => "|",
        B::BitXor => "^",
        B::BitAnd => "&",
        B::Eq => "==",
        B::Ne => "!=",
        B::Lt => "<",
        B::Le => "<=",
        B::Gt => ">",
        B::Ge => ">=",
        B::Add => "+",
        B::Sub => "-",
        B::Mul => "*",
        B::Div => "/",
        B::Mod => "%",
    }
}

fn unary_token(op: ast::UnOp) -> &'static str {
    match op {
        ast::UnOp::BitNot => "~",
        ast::UnOp::Not => "!",
        ast::UnOp::Neg => "-",
    }
}

// Float Display uses shortest round-tripping plain decimal notation. Keep a
// decimal point even for integral values (and -0) so the target parses a Float,
// not an Int32. Scientific notation is not accepted by the official target.
fn float_literal(value: f32) -> String {
    let mut text = value.to_string();
    if !text.contains('.') {
        text.push_str(".0");
    }
    text
}

pub(crate) fn constant<'ctx>(c: &ir::Const<'ctx>, symbols: &Symbols<'ctx>) -> Result<String> {
    use ir::Const as C;
    let cast = |value: String| -> Result<String> {
        Ok(format!(
            "Cast<{}>(({value}))",
            target::typ(&Type::nullary(c.type_id()), symbols)?
        ))
    };
    match c {
        C::Str(s) => Ok(target::string(s)?.to_string()),
        C::CName(s) => Ok(target::cname(s)?.to_string()),
        C::ResRef(s) => Ok(format!("R{}", target::resource_string(s)?)),
        C::TweakDbId(s) => Ok(format!("T{}", target::resource_string(s)?)),
        C::EnumVariant(id) => {
            let (name, _) = symbols
                .get_enum_variant(*id)
                .ok_or_else(|| error("missing resolved enum variant", None))?;
            Ok(format!(
                "{}.{}",
                target::path(id.parent().as_str())?,
                target::ident(name)?
            ))
        }
        C::Bool(v) => Ok(v.to_string()),
        C::I32(v) if *v != i32::MIN => Ok(v.to_string()),
        C::I32(_) => Ok("(-2147483647 - 1)".into()),
        C::I8(v) => cast(v.to_string()),
        C::I16(v) => cast(v.to_string()),
        C::I64(v) if i32::try_from(*v).is_ok() => cast(v.to_string()),
        C::U8(v) => cast(v.to_string()),
        C::U16(v) => cast(v.to_string()),
        C::U32(v) if i32::try_from(*v).is_ok() => cast(v.to_string()),
        C::U64(v) if i32::try_from(*v).is_ok() => cast(v.to_string()),
        C::F32(v) if v.is_finite() => Ok(float_literal(*v)),
        C::F64(v) if v.is_finite() && f64::from(*v as f32) == *v => cast(float_literal(*v as f32)),
        C::I64(_) | C::U32(_) | C::U64(_) | C::F32(_) | C::F64(_) => Err(error(
            "numeric literal needs unverified target range/precision support",
            None,
        )),
    }
}

#[cfg(test)]
mod float_tests {
    use super::*;

    #[test]
    fn decimal_float_literals_round_trip_without_exponents() {
        let symbols = Symbols::with_default_types();
        for value in [
            0.0_f32,
            -0.0,
            1e-8,
            1e20,
            f32::MIN_POSITIVE,
            f32::from_bits(1),
            f32::MAX,
            -f32::MAX,
        ] {
            let text = constant(&ir::Const::F32(value), &symbols).unwrap();
            assert!(!text.contains(['e', 'E']), "{text}");
            assert!(text.contains('.'), "{text}");
            assert_eq!(text.parse::<f32>().unwrap().to_bits(), value.to_bits());
            let double = constant(&ir::Const::F64(f64::from(value)), &symbols).unwrap();
            assert_eq!(double, format!("Cast<Double>(({text}))"));
        }
        assert_eq!(float_literal(-0.0), "-0.0");
    }
}

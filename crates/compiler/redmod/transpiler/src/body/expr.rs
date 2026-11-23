use redscript_community_ast as ast;
use redscript_compiler_frontend::{FunctionType, RefType, Type, ir, predef};

use super::{Body, constant};
use crate::{Result, error, target};

impl<'a, 'ctx> Body<'a, 'ctx> {
    // Target binding powers: assignment 1, conditional 2, binary operators 3..11,
    // `as` 12, unary 13, and primary/postfix expressions 14. Signed constants
    // count as unary so negating one cannot accidentally produce a `--` token.
    pub(super) fn precedence(&self, e: &ir::Expr<'ctx>) -> Result<u8> {
        Ok(match e {
            ir::Expr::Assign { .. } => 1,
            ir::Expr::Conditional { .. } => 2,
            ir::Expr::DynCast { .. } => 12,
            ir::Expr::Const(..) => 13,
            ir::Expr::Call { call, .. } => {
                let ir::Call::FreeFunction { function, args, .. } = &**call else {
                    return Ok(14);
                };
                let (name, def) = self
                    .symbols
                    .get_free_function(*function)
                    .ok_or_else(|| error("missing resolved function", Some(e.span())))?;
                if matches!(
                    def.intrinsic(),
                    Some(ir::Intrinsic::Deref | ir::Intrinsic::AsRef)
                ) || (def.intrinsic() == Some(ir::Intrinsic::ToString)
                    && args.first().is_some_and(|arg| {
                        self.type_of(arg)
                            .is_ok_and(|t| t == Type::nullary(predef::STRING))
                    }))
                {
                    self.precedence(
                        args.first()
                            .ok_or_else(|| error("missing intrinsic operand", None))?,
                    )?
                } else if def.intrinsic() == Some(ir::Intrinsic::ToString) {
                    // General string conversion is emitted as an `as String` cast.
                    12
                } else if matches!(
                    def.intrinsic(),
                    Some(
                        ir::Intrinsic::Equals | ir::Intrinsic::NotEquals | ir::Intrinsic::IsDefined
                    )
                ) {
                    binary_precedence(ast::BinOp::Eq)
                } else if matches!(
                    def.intrinsic(),
                    Some(
                        ir::Intrinsic::EnumInt
                            | ir::Intrinsic::IntEnum
                            | ir::Intrinsic::ToVariant
                            | ir::Intrinsic::FromVariant
                    )
                ) {
                    12
                } else if def.intrinsic().is_none()
                    && def.wrap_target().is_none()
                    && def.flags().is_native()
                {
                    let name = name.to_string();
                    if let Some(op) = ast::BinOp::from_name(&name)
                        && args.len() == 2
                    {
                        binary_precedence(op)
                    } else if ast::UnOp::from_name(&name).is_some() && args.len() == 1 {
                        13
                    } else {
                        14
                    }
                } else {
                    14
                }
            }
            _ => 14,
        })
    }

    pub(super) fn expr_at(&self, e: &ir::Expr<'ctx>, minimum: u8) -> Result<String> {
        let text = self.expr(e)?;
        if self.precedence(e)? < minimum {
            Ok(format!("({text})"))
        } else {
            Ok(text)
        }
    }

    pub(super) fn negated(&self, expr: &ir::Expr<'ctx>) -> Result<String> {
        if let ir::Expr::Const(ir::Const::Bool(value), _) = expr {
            return Ok((!value).to_string());
        }
        if let ir::Expr::Call { call, .. } = expr
            && let ir::Call::FreeFunction { function, args, .. } = &**call
        {
            let (name, def) = self
                .symbols
                .get_free_function(*function)
                .ok_or_else(|| error("missing resolved function", Some(expr.span())))?;
            let op = match def.intrinsic() {
                Some(ir::Intrinsic::Equals) => Some(ast::BinOp::Eq),
                Some(ir::Intrinsic::NotEquals) => Some(ast::BinOp::Ne),
                None if def.flags().is_native() && def.wrap_target().is_none() => {
                    let name = name.to_string();
                    if ast::UnOp::from_name(&name) == Some(ast::UnOp::Not)
                        && let [arg] = &args[..]
                        && self.type_of(arg)? == Type::nullary(predef::BOOL)
                        && self.type_of(expr)? == Type::nullary(predef::BOOL)
                    {
                        return self.expr(arg);
                    }
                    None
                }
                Some(ir::Intrinsic::IsDefined) => {
                    if let [arg] = &args[..] {
                        return self.is_defined(arg, true);
                    }
                    None
                }
                _ => None,
            };
            if let Some(op @ (ast::BinOp::Eq | ast::BinOp::Ne)) = op
                && let [a, b] = &args[..]
                && self.type_of(expr)? == Type::nullary(predef::BOOL)
            {
                let inverse = if op == ast::BinOp::Eq {
                    ast::BinOp::Ne
                } else {
                    ast::BinOp::Eq
                };
                return self.binary(inverse, a, b);
            }
        }
        // Preserve native operator selection: even equality's opposite operator
        // may have a different signature or behavior. Ordered comparisons also
        // cannot be inverted in the presence of NaNs.
        Ok(format!("!{}", self.expr_at(expr, 13)?))
    }

    pub(super) fn binary(
        &self,
        op: ast::BinOp,
        a: &ir::Expr<'ctx>,
        b: &ir::Expr<'ctx>,
    ) -> Result<String> {
        let precedence = binary_precedence(op);
        let right_associative = op.assoc() == ast::Assoc::Right;
        Ok(format!(
            "{} {} {}",
            self.expr_at(a, precedence + u8::from(right_associative))?,
            binary_token(op),
            self.expr_at(b, precedence + u8::from(!right_associative))?
        ))
    }

    pub fn expr(&self, e: &ir::Expr<'ctx>) -> Result<String> {
        if matches!(e, ir::Expr::Nop(_)) {
            return Err(error(
                "an omitted argument cannot be used as a value",
                Some(e.span()),
            ));
        }
        if !matches!(e, ir::Expr::Null { .. }) {
            self.type_text(&self.type_of(e)?).map_err(|mut err| {
                err.span = Some(e.span());
                err
            })?;
        }
        let result = match e {
            ir::Expr::Local(id, _) => Ok(self.local(*id)?.to_owned()),
            ir::Expr::Const(c, _) => constant(c, self.symbols, self.plan),
            ir::Expr::Null { .. } => Ok("NULL".into()),
            ir::Expr::NewInstance { class_type, .. } => Ok(format!(
                "new {}",
                self.type_text(
                    &class_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?
                        .into_type(),
                )?
            )),
            ir::Expr::Construct {
                struct_type, args, ..
            } => Ok(format!(
                "{}({})",
                self.type_text(
                    &struct_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?
                        .into_type(),
                )?,
                self.args(args)?
            )),
            ir::Expr::Assign { place, expr, .. } => {
                if !self.place(place)? {
                    return Err(error(
                        "assignment requires a writable variable-rooted place or a class-handle field",
                        Some(e.span()),
                    ));
                }
                Ok(format!(
                    "{} = {}",
                    self.expr_at(place, 2)?,
                    self.contextual_expr(expr, &self.type_of(place)?)?
                ))
            }
            ir::Expr::Index { array, index, .. } => {
                if !crate::field_reads::variable_rooted(array, self.symbols) {
                    return Err(error(
                        "array indexing requires a variable-rooted array",
                        Some(e.span()),
                    ));
                }
                Ok(format!(
                    "{}[{}]",
                    self.expr_at(array, 14)?,
                    self.expr(index)?
                ))
            }
            ir::Expr::Field {
                receiver, field, ..
            } => {
                if !Self::simple_place(receiver)
                    && !crate::field_reads::inline_receiver(
                        receiver,
                        &self.type_of(receiver)?,
                        self.symbols,
                    )
                {
                    return Err(error(
                        "this field read requires a local receiver",
                        Some(e.span()),
                    ));
                }
                let (name, _) = self
                    .symbols
                    .get_field(*field)
                    .ok_or_else(|| error("missing resolved field", None))?;
                Ok(format!(
                    "{}.{}",
                    self.expr_at(receiver, 14)?,
                    target::ident(name)?
                ))
            }
            ir::Expr::Conditional {
                condition,
                then,
                else_,
                ..
            } => Ok(format!(
                "{} ? {} : {}",
                self.expr_at(condition, 3)?,
                self.expr_at(then, 2)?,
                self.expr_at(else_, 2)?
            )),
            ir::Expr::DynCast {
                expr, target_type, ..
            } => Ok(format!(
                "{} as {}",
                self.expr_at(expr, 12)?,
                self.type_text(
                    &target_type
                        .coalesced(self.symbols)
                        .map_err(|e| error(e.to_string(), None))?
                )?
            )),
            ir::Expr::Call { call, .. } => self.call(call, e),
            ir::Expr::Nop(_) => unreachable!("absent slots are rejected above"),
            ir::Expr::NewClosure { .. } | ir::Expr::Capture(..) => Err(error(
                "lambdas are not supported; use a named function or method",
                Some(e.span()),
            )),
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
            ir::Expr::Index { array, .. } => {
                crate::field_reads::variable_rooted(array, self.symbols)
            }
            ir::Expr::Field { receiver, .. } => {
                let typ = self.type_of(receiver)?;
                let script_deref = matches!(&**receiver, ir::Expr::Call { call, .. }
                    if matches!(&**call, ir::Call::FreeFunction { function, .. }
                        if self.symbols[*function].intrinsic() == Some(ir::Intrinsic::Deref)));
                (!script_deref
                    && (Self::simple_place(receiver)
                        || crate::receivers::inline_class_receiver(receiver, &typ, self.symbols))
                    && crate::receivers::class_handle(&typ, self.symbols))
                    || crate::field_reads::struct_place(receiver, &typ, self.symbols)
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
            if param.flags().is_out()
                && !(Self::simple_place(arg)
                    || (matches!(arg, ir::Expr::Field { .. }) && self.place(arg)?))
            {
                return Err(error(
                    "out arguments require a local variable or a writable field",
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
                    return self.intrinsic(intrinsic, type_args, args, whole);
                }
                self.check_out(def.type_(), args)?;
                if let Some(target) = def.wrap_target() {
                    if self.wrapper != Some(target) {
                        return Err(error(
                            "wrapped method call outside its resolved wrapper",
                            None,
                        ));
                    }
                    let (name, _) = self
                        .symbols
                        .get_method(target)
                        .ok_or_else(|| error("missing wrapped method", None))?;
                    return Ok(format!("{}({})", target::ident(name)?, self.args(args)?));
                }
                let name = name.to_string();
                // Operators and Cast must be resolved native built-ins, not same-name user functions.
                if def.flags().is_native() {
                    if let Some(op) = ast::BinOp::from_name(&name)
                        && args.len() == 2
                    {
                        return self.binary_call(op, def.type_(), call);
                    }
                    if let Some(op) = ast::UnOp::from_name(&name)
                        && let [a] = &args[..]
                    {
                        let contextual = self.call_context(def.type_(), args, call, true)?;
                        let operand = self.call_arg(def.type_(), a, 0, contextual, 13)?;
                        return Ok(if op == ast::UnOp::Neg && operand.starts_with('-') {
                            format!("-({operand})")
                        } else {
                            format!("{}{operand}", unary_token(op))
                        });
                    }
                    if name == "Cast" && args.len() == 1 {
                        return Ok(format!(
                            "Cast<{}>({})",
                            self.type_text(&self.type_of(whole)?)?,
                            self.expr_at(&args[0], 14)?
                        ));
                    }
                }
                if !type_args.is_empty() || !def.type_().type_params().is_empty() {
                    return Err(error(
                        "generic function calls are not supported; call a function with concrete types",
                        None,
                    ));
                }
                if matches!(
                    name.as_str(),
                    "RoundF" | "AngleNormalize" | "AngleDistance" | "AngleApproach" | "LerpAngleF"
                ) && def.flags().is_native()
                {
                    return Err(error(format!("{name} is not supported"), None));
                }

                Ok(format!(
                    "{}({})",
                    self.function_name(*function)?,
                    self.call_args(def.type_(), args, call, def.flags().is_native())?
                ))
            }
            ir::Call::Static {
                parent_id,
                parent_type_args,
                method,
                type_args,
                args,
            } => {
                if !parent_type_args.is_empty() || !type_args.is_empty() {
                    return Err(error("generic method calls need concrete types", None));
                }
                let (name, def) = self
                    .symbols
                    .get_method(*method)
                    .ok_or_else(|| error("missing resolved method", None))?;
                self.check_out(def.type_(), args)?;
                if !def.type_().type_params().is_empty() {
                    return Err(error(
                        "generic methods need concrete parameter and return types",
                        None,
                    ));
                }
                if def.flags().is_callback() {
                    return Err(error(
                        "callbacks cannot be called directly; move shared code to a regular method",
                        None,
                    ));
                }
                Ok(format!(
                    "{}.{}({})",
                    self.type_text(&Type::nullary(*parent_id))?,
                    target::ident(name)?,
                    self.call_args(def.type_(), args, call, def.flags().is_native())?
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
                    return Err(error("generic method calls need concrete types", None));
                }
                let (name, def) = self
                    .symbols
                    .get_method(*method)
                    .ok_or_else(|| error("missing resolved method", None))?;
                self.check_out(def.type_(), call.args())?;
                if !def.type_().type_params().is_empty() {
                    return Err(error(
                        "generic methods need concrete parameter and return types",
                        None,
                    ));
                }
                if def.flags().is_callback() {
                    return Err(error(
                        "callbacks cannot be called directly; move shared code to a regular method",
                        None,
                    ));
                }
                let receiver = self.method_receiver(receiver)?;
                if !Self::simple_place(receiver)
                    && !crate::receivers::inline_class_receiver(
                        receiver,
                        &self.type_of(receiver)?,
                        self.symbols,
                    )
                {
                    return Err(error(
                        "method calls require a local variable or a class-reference receiver",
                        Some(receiver.span()),
                    ));
                }
                let recv = if matches!(mode, ir::CallMode::ForceStatic) {
                    if !matches!(receiver, ir::Expr::Local(ir::Local::This, _)) {
                        return Err(error(
                            "a super method call must use this as its receiver",
                            None,
                        ));
                    }
                    "super".into()
                } else {
                    self.expr_at(receiver, 14)?
                };
                Ok(format!(
                    "{recv}.{}({})",
                    target::ident(name)?,
                    self.call_args(def.type_(), call.args(), call, def.flags().is_native())?
                ))
            }
            ir::Call::Closure { .. } => Err(error(
                "function values are not supported; call a named function or method directly",
                None,
            )),
        }
    }

    fn method_receiver<'e>(&self, receiver: &'e ir::Expr<'ctx>) -> Result<&'e ir::Expr<'ctx>> {
        // Instance-call IR normally retains the weak receiver and its ref kind.
        // Explicit immediate promotions can also be recreated by REDmod's Context.
        if let ir::Expr::Call { call, .. } = receiver
            && let ir::Call::FreeFunction { function, args, .. } = &**call
            && self.symbols[*function].intrinsic() == Some(ir::Intrinsic::WeakRefToRef)
            && let [operand] = &args[..]
        {
            let source = self.type_of(operand)?;
            let result = self.type_of(receiver)?;
            if source.ref_type() == Some(RefType::Weak)
                && source.unwrap_ref_or_self() == result.unwrap_ref_or_self()
            {
                return Ok(operand);
            }
        }
        Ok(receiver)
    }
}

pub(super) fn binary_precedence(op: ast::BinOp) -> u8 {
    if op.precedence() == 0 {
        1
    } else {
        op.precedence() + 2
    }
}

pub(super) fn binary_token(op: ast::BinOp) -> &'static str {
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

use redscript_community_ast as ast;
use redscript_compiler_frontend::{
    FunctionType, PolyType, QueryError, RefType, Type, Variance, expr_type, ir, predef,
};

use crate::{Result, error};

use super::Body;
use super::expr::{binary_precedence, binary_token};

impl<'a, 'ctx> Body<'a, 'ctx> {
    // Only the immediate typed sink may supply a conversion. In particular, do
    // not propagate it through conditionals, casts, or another conversion: doing
    // so can remove a strong handle before its original lifetime ends.
    pub(super) fn contextual_expr(
        &self,
        e: &ir::Expr<'ctx>,
        expected: &Type<'ctx>,
    ) -> Result<String> {
        if let ir::Expr::Call { call, .. } = e
            && let ir::Call::FreeFunction { function, args, .. } = &**call
            && let Some((_, def)) = self.symbols.get_free_function(*function)
            && let Some(conversion @ (ir::Intrinsic::RefToWeakRef | ir::Intrinsic::WeakRefToRef)) =
                def.intrinsic()
        {
            let invalid = || {
                error(
                    "cannot convert this reference here; use a typed local, assignment, or return",
                    Some(e.span()),
                )
            };
            let [arg] = &args[..] else {
                return Err(invalid());
            };
            let source = self.type_of(arg)?;
            let result = self.type_of(e)?;

            let (from, to) = if conversion == ir::Intrinsic::RefToWeakRef {
                (RefType::Strong, RefType::Weak)
            } else {
                (RefType::Weak, RefType::Strong)
            };
            // Ordinary class types are the IR's canonical strong handles;
            // explicit ref<T> wrappers may also occur in resolved types.
            let reference_kind = |typ: &Type<'ctx>| {
                typ.ref_type().or_else(|| match typ {
                    Type::Data(app)
                        if self
                            .symbols
                            .get_type(app.id())
                            .and_then(|def| def.schema().as_aggregate())
                            .is_some_and(|agg| agg.flags().is_class_handle()) =>
                    {
                        Some(RefType::Strong)
                    }
                    _ => None,
                })
            };
            if reference_kind(&source) != Some(from)
                || reference_kind(&result) != Some(to)
                || reference_kind(expected) != Some(to)
                || source.unwrap_ref_or_self() != result.unwrap_ref_or_self()
                || !PolyType::from_type(result.unwrap_ref_or_self()).is_subtype_compatible(
                    expected.unwrap_ref_or_self(),
                    Variance::Covariant,
                    self.symbols,
                )
            {
                return Err(invalid());
            }
            // Validate both representations, and keep the operand at this exact
            // execution point. Ordinary expression emission still rejects any
            // inner conversion instead of cancelling a strong/weak round trip.
            self.type_text(&result)?;
            self.type_text(&source)?;
            self.type_text(expected)?;
            return self.expr_at(arg, 2);
        }
        self.expr(e)
    }

    // A temporary script reference is owned by its invocation, not by a
    // synthesized local. Keep the operand inline so the target recreates both
    // AsRef and the invocation's rvalue-reference bit at the same argument slot.
    fn borrowed_arg(
        &self,
        e: &ir::Expr<'ctx>,
        expected: &Type<'ctx>,
        slot: usize,
        minimum: u8,
    ) -> Result<String> {
        if let ir::Expr::Call { call, .. } = e
            && let ir::Call::FreeFunction { function, args, .. } = &**call
            && self.symbols[*function].intrinsic() == Some(ir::Intrinsic::AsRef)
        {
            let invalid = || {
                error(
                    "borrow requires an exact script_ref<T> parameter and an operand of type T",
                    Some(e.span()),
                )
            };
            let [arg] = &args[..] else {
                return Err(invalid());
            };
            if self.type_of(e)? != *expected
                || expected.ref_type() != Some(RefType::Script)
                || &self.type_of(arg)? != expected.unwrap_ref_or_self()
            {
                return Err(invalid());
            }
            if Self::simple_place(arg) {
                return self.expr_at(arg, minimum);
            }
            if arg.is_prvalue(self.symbols) && slot >= u16::BITS as usize {
                return Err(error(
                    "temporary borrow exceeds the invocation's 16 argument lifetime slots",
                    Some(e.span()),
                ));
            }
            return self.expr_at(arg, minimum);
        }
        let text = self.contextual_expr(e, expected)?;
        if self.precedence(e)? < minimum {
            Ok(format!("({text})"))
        } else {
            Ok(text)
        }
    }

    pub(super) fn call_args(
        &self,
        signature: &FunctionType<'ctx>,
        args: &[ir::Expr<'ctx>],
        call: &ir::Call<'ctx>,
        native: bool,
    ) -> Result<String> {
        let contextual = self.call_context(signature, args, call, native)?;
        args.iter()
            .enumerate()
            .map(|(i, arg)| self.call_arg(signature, arg, i, contextual, 2))
            .collect::<Result<Vec<_>>>()
            .map(|v| v.join(", "))
    }

    pub(super) fn call_arg(
        &self,
        signature: &FunctionType<'ctx>,
        arg: &ir::Expr<'ctx>,
        slot: usize,
        contextual: bool,
        minimum: u8,
    ) -> Result<String> {
        if contextual
            && let Some(param) = signature.params().get(slot)
            && !param.flags().is_out()
        {
            return self.borrowed_arg(arg, param.type_(), slot, minimum);
        }
        if let ir::Expr::Call { call, .. } = arg
            && let ir::Call::FreeFunction { function, args, .. } = &**call
            && self.symbols[*function].intrinsic() == Some(ir::Intrinsic::AsRef)
        {
            if self.erased_operand(arg).is_some() {
                let message = if signature
                    .params()
                    .get(slot)
                    .is_some_and(|p| p.flags().is_out())
                {
                    "nonlocal AsRef cannot supply an out parameter"
                } else {
                    "cannot recreate this borrow: generic or competing overloads may change call selection"
                };
                return Err(error(message, Some(arg.span())));
            }
            if let [inner] = &args[..]
                && Self::simple_place(inner)
            {
                let Some(param) = signature.params().get(slot) else {
                    return Err(error(
                        "borrow requires an exact script_ref<T> parameter and an operand of type T",
                        Some(arg.span()),
                    ));
                };
                if param.flags().is_out() {
                    return Err(error(
                        "AsRef cannot supply an out parameter",
                        Some(arg.span()),
                    ));
                }
                if self.type_of(arg)? != *param.type_()
                    || param.type_().ref_type() != Some(RefType::Script)
                    || &self.type_of(inner)? != param.type_().unwrap_ref_or_self()
                {
                    return Err(error(
                        "borrow requires an exact script_ref<T> parameter and an operand of type T",
                        Some(arg.span()),
                    ));
                }
            }
        }
        self.expr_at(arg, minimum)
    }

    pub(super) fn binary_call(
        &self,
        op: ast::BinOp,
        signature: &FunctionType<'ctx>,
        call: &ir::Call<'ctx>,
    ) -> Result<String> {
        let args = call.args();
        let contextual = self.call_context(signature, args, call, true)?;
        let precedence = binary_precedence(op);
        let right_associative = op.assoc() == ast::Assoc::Right;
        Ok(format!(
            "{} {} {}",
            self.call_arg(
                signature,
                &args[0],
                0,
                contextual,
                precedence + u8::from(right_associative)
            )?,
            binary_token(op),
            self.call_arg(
                signature,
                &args[1],
                1,
                contextual,
                precedence + u8::from(!right_associative)
            )?
        ))
    }

    fn erased_operand<'e>(&self, arg: &'e ir::Expr<'ctx>) -> Option<&'e ir::Expr<'ctx>> {
        let ir::Expr::Call { call, .. } = arg else {
            return None;
        };
        let ir::Call::FreeFunction { function, args, .. } = &**call else {
            return None;
        };
        let [operand] = &args[..] else { return None };
        match self.symbols[*function].intrinsic()? {
            ir::Intrinsic::RefToWeakRef | ir::Intrinsic::WeakRefToRef => Some(operand),
            ir::Intrinsic::AsRef if !Self::simple_place(operand) => Some(operand),
            _ => None,
        }
    }

    // This is a proof of noncompetition, not a replacement target overload resolver:
    // any other possibly viable declaration blocks erasure. In particular, a ref
    // versus weak overload remains a competitor even if the original IR chose one.
    pub(super) fn call_context(
        &self,
        signature: &FunctionType<'ctx>,
        args: &[ir::Expr<'ctx>],
        call: &ir::Call<'ctx>,
        native: bool,
    ) -> Result<bool> {
        let rewritten = |i: usize, arg| {
            signature
                .params()
                .get(i)
                .filter(|p| !p.flags().is_out())
                .and_then(|_| self.erased_operand(arg))
        };
        // Ordinary calls and stable local AsRef operands do not query overloads.
        if !signature.type_params().is_empty()
            || matches!(call, ir::Call::FreeFunction { type_args, .. } if !type_args.is_empty())
            || !args
                .iter()
                .enumerate()
                .any(|(i, arg)| rewritten(i, arg).is_some())
        {
            return Ok(false);
        }
        let types = args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                if matches!(arg, ir::Expr::Nop(_)) {
                    Ok(None)
                } else {
                    let arg = rewritten(i, arg).unwrap_or(arg);
                    match expr_type(arg, self.function, self.symbols) {
                        Ok(typ) => Ok(Some(typ)),
                        // Null's contextual type is not retained by the IR. Keep
                        // it unknown for viability rather than inventing a class.
                        Err(QueryError::UnknownNullType) => Ok(Some(Type::Nothing)),
                        Err(err) => Err(error(err.to_string(), Some(arg.span()))),
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;
        let viable = |candidate: &FunctionType<'ctx>| {
            if !candidate.is_valid()
                || args.len() < candidate.min_args()
                || args.len() > candidate.max_args()
            {
                return false;
            }
            // Generic substitution/target-specific ranking is not proven here.
            if !candidate.type_params().is_empty() {
                return true;
            }
            fn unresolved(typ: &Type<'_>) -> bool {
                match typ {
                    Type::Data(app) => app.args().iter().any(unresolved),
                    // Null/bottom and unresolved variables have no concrete
                    // disjoint target type, including inside a reference.
                    Type::Ctx(_) | Type::Nothing => true,
                }
            }
            fn numeric(typ: &Type<'_>) -> bool {
                typ.unwrap_ref_or_self().upper_bound().is_some_and(|app| {
                    [
                        predef::INT8,
                        predef::INT16,
                        predef::INT32,
                        predef::INT64,
                        predef::UINT8,
                        predef::UINT16,
                        predef::UINT32,
                        predef::UINT64,
                        predef::FLOAT,
                        predef::DOUBLE,
                    ]
                    .contains(&app.id())
                })
            }
            types.iter().zip(candidate.params()).all(|(typ, param)| {
                let Some(typ) = typ else {
                    return param.flags().is_optional();
                };
                // Match conservatively after reference erasure, including target
                // script-reference write-through and variant conversion. Unknown
                // receiver type substitutions must not prove noncompetition.
                // Target numeric conversions/contextual literal typing can differ
                // from shared subtype checks. Even typed numeric expressions are
                // conservatively competing; spelling is not a type proof. Out
                // flags likewise cannot prove that emitted arguments are disjoint.
                unresolved(param.type_())
                    || unresolved(typ)
                    || (numeric(typ) && numeric(param.type_()))
                    || param
                        .type_()
                        .upper_bound()
                        .is_some_and(|app| app.id() == predef::VARIANT)
                    || PolyType::from_type(typ)
                        .unwrap_ref_or_self(self.symbols)
                        .is_subtype_compatible(
                            param.type_().unwrap_ref_or_self(),
                            Variance::Covariant,
                            self.symbols,
                        )
            })
        };
        // Recreating the same borrow at every rewritten slot
        // leaves overload ranking unchanged. Other argument types are untouched;
        // unlike strong/weak conversions, borrowing adds no competing value type.
        let same_borrows = |candidate: &FunctionType<'ctx>, candidate_native: bool| {
            native
                && candidate_native
                && signature.type_params().is_empty()
                && candidate.type_params().is_empty()
                && signature.params().len() == candidate.params().len()
                && signature
                    .params()
                    .iter()
                    .zip(candidate.params())
                    .enumerate()
                    .all(|(i, (selected, other))| {
                        if selected.flags().into_bits() != other.flags().into_bits() {
                            return false;
                        }
                        let Some(arg) = args.get(i).filter(|arg| rewritten(i, arg).is_some())
                        else {
                            return true;
                        };
                        let ir::Expr::Call { call, .. } = arg else {
                            return false;
                        };
                        let ir::Call::FreeFunction { function, .. } = &**call else {
                            return false;
                        };
                        self.symbols[*function].intrinsic() == Some(ir::Intrinsic::AsRef)
                            && selected.type_() == other.type_()
                            && selected.type_().ref_type() == Some(RefType::Script)
                    })
        };
        Ok(match call {
            ir::Call::FreeFunction { function, .. } => self
                .symbols
                .free_function_overloads(*function)
                .all(|entry| {
                    entry.key() == function
                        || same_borrows(entry.func().type_(), entry.func().flags().is_native())
                        || !viable(entry.func().type_())
                }),
            ir::Call::Static {
                parent_id, method, ..
            } => {
                let (name, _) = self.symbols.get_method(*method).unwrap();
                self.symbols
                    .query_methods_by_name(*parent_id, name)
                    .all(|entry| {
                        entry.key() == method
                            || !entry.func().flags().is_static()
                            || same_borrows(entry.func().type_(), entry.func().flags().is_native())
                            || !viable(entry.func().type_())
                    })
            }
            ir::Call::Instance {
                receiver,
                method,
                mode,
                ..
            } => {
                if self.symbols[*method].flags().is_static_forwarder() {
                    return Ok(false);
                }
                let typ = self.type_of(receiver)?;
                let Some(app) = typ.unwrap_ref_or_self().upper_bound() else {
                    return Ok(false);
                };
                let parent = if matches!(mode, ir::CallMode::ForceStatic) {
                    let Some((parent, _)) = self.symbols.base_iter_with_self(app.id()).nth(1)
                    else {
                        return Ok(false);
                    };
                    parent
                } else {
                    app.id()
                };
                let (name, _) = self.symbols.get_method(*method).unwrap();
                self.symbols
                    .query_methods_by_name(parent, name)
                    .all(|entry| {
                        entry.key() == method
                            || entry.func().flags().is_static()
                            || same_borrows(entry.func().type_(), entry.func().flags().is_native())
                            || !viable(entry.func().type_())
                    })
            }
            ir::Call::Closure { .. } => false,
        })
    }
}

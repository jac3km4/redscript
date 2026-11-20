use redscript_community_ast as ast;
use redscript_compiler_frontend::utils::ScopedMap;
use redscript_compiler_frontend::{RefType, Type, ir, predef};

use crate::{Result, error, target};

use super::Body;
use super::expr::binary_precedence;

impl<'a, 'ctx> Body<'a, 'ctx> {
    pub(super) fn is_defined(&self, arg: &ir::Expr<'ctx>, negated: bool) -> Result<String> {
        let typ = self.type_of(arg)?;
        let Type::Data(app) = typ.unwrap_ref_or_self() else {
            return Err(error("unresolved IsDefined operand", None));
        };
        if !self
            .symbols
            .get_type(app.id())
            .and_then(|d| d.schema().as_aggregate())
            .is_some_and(|a| a.flags().is_heap_reference())
        {
            return Err(error("IsDefined requires a class reference", None));
        }
        // Testing a weak handle needs no strong temporary. Only remove the
        // immediate promotion: inner conversions still retain their lifetimes.
        let operand = if let ir::Expr::Call { call, .. } = arg
            && let ir::Call::FreeFunction { function, args, .. } = &**call
            && self.symbols[*function].intrinsic() == Some(ir::Intrinsic::WeakRefToRef)
            && let [operand] = &args[..]
            && self.type_of(operand)?.ref_type() == Some(RefType::Weak)
            && self.type_of(operand)?.unwrap_ref_or_self() == typ.unwrap_ref_or_self()
        {
            operand
        } else {
            arg
        };
        let op = if negated { "==" } else { "!=" };
        Ok(format!(
            "{} {op} NULL",
            self.expr_at(operand, binary_precedence(ast::BinOp::Ne))?
        ))
    }

    pub(super) fn intrinsic(
        &self,
        intrinsic: ir::Intrinsic,
        type_args: &[ir::Type<'ctx>],
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
                "{} as {}",
                self.expr_at(unary()?, 12)?,
                self.type_text(&self.type_of(whole)?)?
            ))
        };
        let method = |name: &str| -> Result<String> {
            let recv = unary()?;
            if !Self::simple_place(recv)
                && !(crate::field_reads::variable_rooted(recv, self.symbols)
                    && args[1..]
                        .iter()
                        .all(|arg| crate::effects::non_mutating(arg, self.symbols)))
            {
                return Err(error(
                    "this array operation requires a local array, or a variable-rooted array with non-mutating arguments",
                    Some(recv.span()),
                ));
            }
            let Type::Data(array) = self.type_of(recv)? else {
                return Err(error("unresolved array receiver", Some(recv.span())));
            };
            let [element] = array.args() else {
                return Err(error("missing array element type", Some(recv.span())));
            };
            let index_type = Type::nullary(predef::INT32);
            Ok(format!(
                "{}.{}({})",
                self.expr_at(recv, 14)?,
                name,
                args[1..]
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        let expected =
                            if matches!(intrinsic, I::ArrayResize | I::ArrayGrow | I::ArrayErase)
                                || (intrinsic == I::ArrayInsert && index == 0)
                            {
                                &index_type
                            } else {
                                element
                            };
                        self.contextual_expr(arg, expected)
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            ))
        };
        match intrinsic {
            I::Equals | I::NotEquals => {
                let [a, b] = args else {
                    return Err(error("invalid equality arity", None));
                };
                self.binary(
                    if intrinsic == I::Equals {
                        ast::BinOp::Eq
                    } else {
                        ast::BinOp::Ne
                    },
                    a,
                    b,
                )
            }
            I::ArrayClear => method("Clear"),
            I::ArraySize => method("Size"),
            I::ArrayResize => method("Resize"),
            I::ArrayGrow => method("Grow"),
            I::ArrayCount => method("Count"),
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
                    return Err(error(
                        "nonlocal AsRef requires an immediate exact non-out script_ref<T> call parameter",
                        None,
                    ));
                }
                self.expr(unary()?)
            }
            I::RefToWeakRef | I::WeakRefToRef => {
                // REDmod assignment/call contexts perform strong/weak coercions.
                // An explicit cast to wref<T> is not a verified target operation.
                Err(error(
                    "cannot convert this reference here; use a typed local, assignment, or return",
                    None,
                ))
            }
            I::IsDefined => self.is_defined(unary()?, false),
            I::ToString => {
                let typ = self.type_of(unary()?)?;
                let Type::Data(app) = &typ else {
                    return Err(error("unresolved ToString operand", None));
                };
                let id = app.id();
                if id == predef::STRING {
                    return self.expr(unary()?);
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
                    let operand = unary()?;
                    let value = if let ir::Expr::Call { call, .. } = operand
                        && let ir::Call::FreeFunction { function, args, .. } = &**call
                        && self.symbols[*function].intrinsic() == Some(I::WeakRefToRef)
                        && let [arg] = &args[..]
                    {
                        format!("({} as {})", self.expr_at(arg, 12)?, self.type_text(&typ)?)
                    } else {
                        self.expr_at(operand, 12)?
                    };
                    return Ok(format!("{value} as String"));
                };
                Ok(format!("{name}({})", self.expr(unary()?)?))
            }
            I::NameOf => {
                let [typ] = type_args else {
                    return Err(error("missing NameOf type argument", None));
                };
                let typ = typ
                    .coalesced(self.symbols)
                    .map_err(|_| error("unresolved NameOf type argument", None))?
                    .mono(&ScopedMap::default())
                    .map_err(|_| error("unresolved NameOf type argument", None))?;
                Ok(target::cname(&typ.to_string())?.to_string())
            }
            I::ArraySort
            | I::ArraySortByPredicate
            | I::LongIntEnum
            | I::VariantIsDefined
            | I::VariantIsRef
            | I::VariantIsArray
            | I::VariantTypeName => Err(error(
                format!("{} is not supported", intrinsic.name()),
                None,
            )),
        }
    }
}

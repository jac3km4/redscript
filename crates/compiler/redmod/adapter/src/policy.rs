use redscript_community_ast::Span;
use redscript_compiler_frontend::{
    LowerError, PolyType, RefType, SemanticPolicy, Symbols, TypeId, ir, predef,
};

/// A supported dynamic-array member and the shared intrinsic it resolves to.
#[derive(Debug, Clone, Copy)]
pub struct ArrayOperation {
    pub name: &'static str,
    pub intrinsic: ir::Intrinsic,
}

const ARRAY_OPERATIONS: &[ArrayOperation] = &[
    ArrayOperation {
        name: "Size",
        intrinsic: ir::Intrinsic::ArraySize,
    },
    ArrayOperation {
        name: "PushBack",
        intrinsic: ir::Intrinsic::ArrayPush,
    },
    ArrayOperation {
        name: "PopBack",
        intrinsic: ir::Intrinsic::ArrayPop,
    },
    ArrayOperation {
        name: "Clear",
        intrinsic: ir::Intrinsic::ArrayClear,
    },
    ArrayOperation {
        name: "Resize",
        intrinsic: ir::Intrinsic::ArrayResize,
    },
    ArrayOperation {
        name: "Contains",
        intrinsic: ir::Intrinsic::ArrayContains,
    },
    ArrayOperation {
        name: "FindFirst",
        intrinsic: ir::Intrinsic::ArrayFindFirst,
    },
    ArrayOperation {
        name: "FindLast",
        intrinsic: ir::Intrinsic::ArrayFindLast,
    },
    ArrayOperation {
        name: "Insert",
        intrinsic: ir::Intrinsic::ArrayInsert,
    },
    ArrayOperation {
        name: "Remove",
        intrinsic: ir::Intrinsic::ArrayRemove,
    },
    ArrayOperation {
        name: "Erase",
        intrinsic: ir::Intrinsic::ArrayErase,
    },
    ArrayOperation {
        name: "Last",
        intrinsic: ir::Intrinsic::ArrayLast,
    },
];

/// Dynamic-array operations understood by the semantic adapter.
/// The receiver must be an array place, and a matching intrinsic overload must
/// exist in the supplied symbols; descriptors alone do not guarantee a valid call.
pub fn array_operations() -> &'static [ArrayOperation] {
    ARRAY_OPERATIONS
}

pub const POLICY: SemanticPolicy = SemanticPolicy {
    hoist_locals: true,
    native_object_cast: true,
    condition: Some(condition),
    member_intrinsic: Some(member_intrinsic),
    validate_type: Some(validate_type),
    validate_coercion: Some(validate_coercion),
    allow_explicit_intrinsic: Some(|_| false),
};

fn validate_type<'ctx>(
    id: TypeId<'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
) -> Result<(), LowerError<'ctx>> {
    if symbols
        .get_type(id)
        .and_then(|def| def.schema().as_aggregate())
        .is_some_and(|agg| agg.flags().is_mixed_ref())
    {
        Err(LowerError::UnsupportedType(PolyType::nullary(id), span))
    } else {
        Ok(())
    }
}

fn condition<'ctx>(typ: &PolyType<'ctx>, symbols: &Symbols<'ctx>) -> Option<ir::Intrinsic> {
    let object = typ.ref_type(symbols) == Some(RefType::Weak)
        || typ.upper_bound(symbols).is_some_and(|t| {
            symbols
                .get_type(t.id())
                .and_then(|def| def.schema().as_aggregate())
                .is_some_and(|agg| {
                    !agg.flags().is_struct()
                        && !agg.flags().is_mixed_ref()
                        && !agg.flags().is_never_ref()
                })
        });
    object.then_some(ir::Intrinsic::IsDefined)
}

fn validate_coercion<'ctx>(
    lhs: &PolyType<'ctx>,
    rhs: &PolyType<'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
) -> Result<(), LowerError<'ctx>> {
    validate_value(lhs, symbols, span)?;
    validate_value(rhs, symbols, span)
}

fn validate_value<'ctx>(
    typ: &PolyType<'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
) -> Result<(), LowerError<'ctx>> {
    // Bare class values remain the shared object representation; never insert
    // the community strong wrapper, including inside arrays or script refs.
    if typ.ref_type(symbols) == Some(RefType::Strong) {
        return Err(LowerError::UnsupportedType(typ.clone(), span));
    }
    if let Some(bound) = typ.upper_bound(symbols) {
        validate_type(bound.id(), symbols, span)?;
        for arg in bound.args() {
            validate_value(arg, symbols, span)?;
        }
    }
    Ok(())
}

fn member_intrinsic(id: TypeId<'_>, member: &str) -> Option<(ir::Intrinsic, bool)> {
    if id != predef::ARRAY {
        return None;
    }
    // REDmod rejects array operations on temporary receivers, including reads.
    array_operations()
        .iter()
        .find(|operation| operation.name == member)
        .map(|operation| (operation.intrinsic, true))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn member_policy_uses_descriptors_only_for_dynamic_arrays() {
        for operation in array_operations() {
            assert_eq!(
                member_intrinsic(predef::ARRAY, operation.name),
                Some((operation.intrinsic, true)),
            );
            assert_eq!(member_intrinsic(predef::INT32, operation.name), None);
        }
        for unsupported in ["size", "ArraySize", "Count", "Grow", "Sort", "Unknown"] {
            assert_eq!(member_intrinsic(predef::ARRAY, unsupported), None);
        }
    }
}

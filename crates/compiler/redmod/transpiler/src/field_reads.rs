use redscript_compiler_frontend::{RefType, Symbols, Type, ir};

// Read eligibility is deliberately independent of writable-place eligibility.
// Native postfix syntax preserves Context/StructField nesting without copying a
// struct into a mutable temporary or inventing a write-back path.
pub(crate) fn inline_receiver<'ctx>(
    expr: &ir::Expr<'ctx>,
    typ: &Type<'ctx>,
    symbols: &Symbols<'ctx>,
) -> bool {
    if typ.ref_type() == Some(RefType::Script) {
        return false;
    }
    let Type::Data(app) = typ.unwrap_ref_or_self() else {
        return false;
    };
    app.args().is_empty()
        && symbols
            .get_type(app.id())
            .and_then(|def| def.schema().as_aggregate())
            .is_some_and(|agg| {
                !agg.flags().is_mixed_ref()
                    && if agg.flags().is_struct() {
                        variable_rooted(expr, symbols)
                    } else {
                        !agg.flags().is_never_ref()
                    }
            })
        && matches!(
            expr,
            ir::Expr::Field { .. } | ir::Expr::Call { .. } | ir::Expr::Conditional { .. }
        )
}

// v1.2.2 requires a variable parent scope for StructField. A class handle
// returned by a call can be read, but does not make its struct fields places.
pub(crate) fn variable_rooted(expr: &ir::Expr<'_>, symbols: &Symbols<'_>) -> bool {
    match expr {
        ir::Expr::Local(..) => true,
        ir::Expr::Field { receiver, .. } => variable_rooted(receiver, symbols),
        ir::Expr::Call { call, .. } => {
            matches!(&**call, ir::Call::FreeFunction { function, args, .. }
                if symbols.get_free_function(*function).is_some_and(|(_, def)| def.intrinsic() == Some(ir::Intrinsic::Deref))
                    && matches!(&args[..], [ir::Expr::Local(..)]))
        }
        _ => false,
    }
}

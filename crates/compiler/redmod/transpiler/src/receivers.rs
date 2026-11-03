use redscript_compiler_frontend::{RefType, Symbols, Type, ir};

// Only handle-valued expressions may bypass the local-place restriction. In
// particular, a struct field/element/call result must never become a copied
// mutable receiver without a separate identity/write-back plan. Keep the native
// Context expression intact: its null exit also encloses argument evaluation.
// REDmod promotes weak class handles in this receiver context.
pub(crate) fn inline_class_receiver<'ctx>(
    expr: &ir::Expr<'ctx>,
    typ: &Type<'ctx>,
    symbols: &Symbols<'ctx>,
) -> bool {
    if typ.ref_type() == Some(RefType::Script) {
        return false;
    }
    // Ordinary class types are already implicit strong handles in shared IR;
    // explicit ref wrappers survive only for particular reference categories.
    let Type::Data(class) = typ.unwrap_ref_or_self() else {
        return false;
    };
    class.args().is_empty()
        && symbols
            .get_type(class.id())
            .and_then(|def| def.schema().as_aggregate())
            .is_some_and(|agg| {
                !agg.flags().is_struct()
                    && !agg.flags().is_never_ref()
                    && !agg.flags().is_mixed_ref()
            })
        && matches!(
            expr,
            ir::Expr::Call { .. }
                | ir::Expr::Field { .. }
                | ir::Expr::Conditional { .. }
                | ir::Expr::DynCast { .. }
        )
}

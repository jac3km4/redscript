//! Bounded conversions for native consumers, not overload applicability.
use super::*;

/// Current lower bounds alone are not evidence: probing must not freeze inference.
pub(super) fn fixed_id<'ctx>(typ: &PolyType<'ctx>) -> Option<TypeId<'ctx>> {
    fn nullary<'ctx>(typ: &types::InferredType<'ctx>) -> Option<TypeId<'ctx>> {
        match typ {
            Type::Data(app) if app.args().is_empty() => Some(app.id()),
            _ => None,
        }
    }
    match typ {
        PolyType::Mono(typ) => nullary(typ),
        PolyType::Var(var) => {
            let lower = nullary(&var.lower())?;
            (nullary(&var.upper()?) == Some(lower)).then_some(lower)
        }
    }
}

pub(super) fn provider<'ctx>(
    source: TypeId<'ctx>,
    target: TypeId<'ctx>,
    implicit: bool,
    env: &Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
) -> LowerResult<'ctx, Option<FreeFunctionIndex>> {
    let mut matches = env.query_free_functions("Cast", symbols).filter(|entry| {
        let function = entry.func();
        let typ = function.type_();
        function.flags().is_native()
            && function.intrinsic().is_none()
            && function
                .span()
                .is_some_and(|span| env.types().policy(span).native_object_cast)
            && typ.is_valid()
            && typ.type_params().is_empty()
            && !typ.is_const_return()
            && typ
                .conversion()
                .is_some_and(|kind| !implicit || kind == ast::ConversionKind::Implicit)
            && matches!(typ.params(), [param] if !param.flags().is_out()
                && !param.flags().is_const() && !param.flags().is_optional()
                && !param.flags().is_short_circuit()
                && *param.type_() == Type::nullary(source))
            && *typ.return_type() == Type::nullary(target)
    });
    let Some(selected) = matches.next() else {
        return Ok(None);
    };
    // No ranking, registration-order preference, or intrinsic fallback on ambiguity.
    if matches.next().is_some() {
        return Err(Error::UnsupportedNativeConversion(span));
    }
    Ok(Some(*selected.key()))
}

pub(super) fn call<'ctx>(
    function: FreeFunctionIndex,
    arg: ir::Expr<'ctx>,
    span: Span,
) -> ir::Expr<'ctx> {
    ir::Expr::call(
        ir::Call::FreeFunction {
            function,
            type_args: Box::default(),
            args: Box::new([arg]),
        },
        span,
    )
}

/// Diagnostic attribution shares the read-only evidence rule, not provider gates.
pub(super) fn unsupported_implicit<'ctx>(
    source: &PolyType<'ctx>,
    target: &PolyType<'ctx>,
    env: &Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
) -> bool {
    let (Some(source), Some(target)) = (fixed_id(source), fixed_id(target)) else {
        return false;
    };
    env.query_free_functions("Cast", symbols).any(|entry| {
        let typ = entry.func().type_();
        typ.is_valid()
            && typ.conversion() == Some(ast::ConversionKind::Implicit)
            && typ.type_params().is_empty()
            && matches!(typ.params(), [param] if *param.type_() == Type::nullary(source))
            && *typ.return_type() == Type::nullary(target)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_evidence_requires_equal_nullary_bounds_and_does_not_freeze() {
        let symbols = Symbols::with_default_types();
        let open = PolyType::with_bounds(Type::nullary(predef::INT32), None);
        assert_eq!(fixed_id(&open), None);
        assert!(matches!(&open, PolyType::Var(var) if var.upper().is_none()));
        // A probe must leave the variable available for later constraints.
        open.constrain_base(&PolyType::nullary(predef::INT32), &symbols)
            .unwrap();
        assert_eq!(fixed_id(&open), Some(predef::INT32));
        let fixed = PolyType::with_bounds(
            Type::nullary(predef::INT32),
            Some(Type::nullary(predef::INT32)),
        );
        assert_eq!(fixed_id(&fixed), Some(predef::INT32));
        let unequal = PolyType::with_bounds(
            Type::nullary(predef::INT32),
            Some(Type::nullary(predef::VARIANT)),
        );
        assert_eq!(fixed_id(&unequal), None);
        assert_eq!(fixed_id(&PolyType::fresh()), None);
        assert_eq!(
            fixed_id(
                &Type::app(predef::SCRIPT_REF, [PolyType::nullary(predef::INT32)]).into_poly()
            ),
            None
        );
    }
}

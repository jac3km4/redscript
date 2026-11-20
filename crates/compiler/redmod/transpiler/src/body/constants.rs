use redscript_compiler_frontend::{Symbols, Type, ir};

use crate::names::TargetNamePlan;
use crate::{Result, error, target};

// Float Display uses shortest round-tripping plain decimal notation. Keep a
// decimal point even for integral values (and -0) so the target parses a Float,
// not an Int32. Scientific notation is not accepted by the official target.
pub(crate) fn float_literal(value: f32) -> String {
    let mut text = value.to_string();
    if !text.contains('.') {
        text.push_str(".0");
    }
    text
}

pub(crate) fn constant<'ctx>(
    c: &ir::Const<'ctx>,
    symbols: &Symbols<'ctx>,
    plan: Option<&TargetNamePlan>,
) -> Result<String> {
    use ir::Const as C;
    let cast = |value: String| -> Result<String> {
        Ok(format!(
            "Cast<{}>({value})",
            target::typ_planned(&Type::nullary(c.type_id()), symbols, plan)?
        ))
    };
    match c {
        C::Str(s) => Ok(target::string(s)?.to_string()),
        C::CName(s) => Ok(target::cname(s)?.to_string()),
        C::ResRef(s) => Ok(format!("R{}", target::resource_path(s)?)),
        C::TweakDbId(s) => Ok(format!("T{}", target::resource_string(s)?)),
        C::EnumVariant(id) => target::enum_variant(*id, symbols, plan),
        C::Bool(v) => Ok(v.to_string()),
        C::I32(v) if *v != i32::MIN => Ok(v.to_string()),
        C::I32(_) => Ok("(-2147483647 - 1)".into()),
        C::I8(v) => cast(v.to_string()),
        C::I16(v) => cast(v.to_string()),
        C::I64(v) if i32::try_from(*v).is_ok() => cast(v.to_string()),
        C::U8(v) => cast(v.to_string()),
        C::U16(v) => cast(v.to_string()),
        // The target has no unsigned suffix. Its Uint32 cast preserves the
        // signed operand's bits, including the full upper half of the range.
        C::U32(v) => cast((*v as i32).to_string()),
        C::U64(v) if i32::try_from(*v).is_ok() => cast(v.to_string()),
        C::F32(v) if v.is_finite() => Ok(float_literal(*v)),
        C::F64(v) if v.is_finite() && f64::from(*v as f32) == *v => cast(float_literal(*v as f32)),
        C::I64(_) | C::U64(_) | C::F32(_) | C::F64(_) => Err(error(
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
            let text = constant(&ir::Const::F32(value), &symbols, None).unwrap();
            assert!(!text.contains(['e', 'E']), "{text}");
            assert!(text.contains('.'), "{text}");
            assert_eq!(text.parse::<f32>().unwrap().to_bits(), value.to_bits());
            let double = constant(&ir::Const::F64(f64::from(value)), &symbols, None).unwrap();
            assert_eq!(double, format!("Cast<Double>({text})"));
        }
        assert_eq!(float_literal(-0.0), "-0.0");
    }
}

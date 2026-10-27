//! REDmod does not convert integer literals to `Float` when matching the parameters of game
//! functions (`SetSize(445, 125)` fails with `Cannot match function to arguments`), so integer
//! literals in a position where type information expects a `Float` or a `Double` become float
//! literals: `445` -> `445.0`, `-5` -> `-5.0`. This applies everywhere an expected type is known:
//! call arguments, operands, assignments, returned values, typed locals, array elements...
//!
//! Without type information, literals are left as they are.

use hashbrown::HashSet;
use redscript_ast::{Constant, Expr, SourceModule, Span, Stmt, UnOp};

use crate::walk::{
    MutVisitor, SExpr, SStmt, elem_type, for_each_function, type_name, walk_body, walk_expr,
    walk_stmt,
};
use crate::{Cx, TypeHint, TypeQuery};

/// Integers with a magnitude up to this value are exactly representable as `Float`.
const MAX_EXACT_F32: u32 = 1 << f32::MANTISSA_DIGITS;

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let Some(types) = cx.opts.types else {
        return;
    };
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            let mut visitor = FloatLiterals {
                cx: &mut *cx,
                types,
                reported: HashSet::new(),
            };
            walk_body(&mut visitor, body);
        }
    });
}

struct FloatLiterals<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    types: &'a dyn TypeQuery,
    /// Literals already reported as not representable.
    reported: HashSet<Span>,
}

#[derive(Clone, Copy)]
enum Float {
    Single,
    Double,
}

impl Float {
    fn of_name(name: &str) -> Option<Self> {
        match name {
            "Float" => Some(Self::Single),
            "Double" => Some(Self::Double),
            _ => None,
        }
    }

    fn of_hint(hint: &TypeHint) -> Option<Self> {
        match hint {
            TypeHint::Primitive(name) => Self::of_name(name),
            _ => None,
        }
    }
}

/// The integer constant of an integer literal, possibly negated.
fn int_literal<'a, 'src>(expr: &'a mut SExpr<'src>) -> Option<&'a mut Constant<'src>> {
    match &mut expr.0 {
        Expr::Constant(constant @ Constant::I32(_)) => Some(constant),
        Expr::UnOp {
            op: UnOp::Neg,
            expr: inner,
        } => match &mut inner.0 {
            Expr::Constant(constant @ Constant::I32(_)) => Some(constant),
            _ => None,
        },
        _ => None,
    }
}

impl<'src> FloatLiterals<'_, '_, 'src> {
    /// The float type expected for `expr`. The expected type of a negated literal can be
    /// recorded for the negation or for the literal, depending on how the call was resolved.
    fn expected_float(&self, expr: &SExpr<'src>) -> Option<Float> {
        let of = |span| Float::of_hint(&self.types.expected_type_of(span)?);
        of(expr.1).or_else(|| match &expr.0 {
            Expr::UnOp {
                op: UnOp::Neg,
                expr: inner,
            } => of(inner.1),
            _ => None,
        })
    }

    /// Turns an integer literal into a float literal of the same value, in place.
    fn convert(&mut self, expr: &mut SExpr<'src>, float: Float) {
        let span = expr.1;
        let Some(constant) = int_literal(expr) else {
            return;
        };
        let Constant::I32(value) = *constant else {
            return;
        };
        *constant = match float {
            Float::Single if value.unsigned_abs() <= MAX_EXACT_F32 =>
            {
                #[allow(clippy::cast_precision_loss)]
                Constant::F32(value as f32)
            }
            Float::Single => {
                if self.reported.insert(span) {
                    self.cx.warn(
                        span,
                        "float-literal-precision",
                        format!(
                            "{value} is used as a Float but cannot be represented exactly, \
                             REDmod needs a float literal here: write the intended value"
                        ),
                    );
                }
                return;
            }
            Float::Double => Constant::F64(value.into()),
        };
    }

    /// Converts the integer elements of an array literal whose elements are floats. The
    /// element type is taken from the type expected for the array, or for any of its elements
    /// (only the first one may be recorded).
    fn convert_array(&mut self, expr: &mut SExpr<'src>, declared: Option<Float>) {
        let span = expr.1;
        let Expr::ArrayLit(elems) = &mut expr.0 else {
            return;
        };
        let from_array = match self.types.expected_type_of(span) {
            Some(TypeHint::Array(elem) | TypeHint::StaticArray(elem, _)) => Float::of_hint(&elem),
            _ => None,
        };
        let Some(float) = declared
            .or(from_array)
            .or_else(|| elems.iter().find_map(|elem| self.expected_float(elem)))
        else {
            return;
        };
        for elem in elems.iter_mut() {
            self.convert(elem, float);
        }
    }
}

impl<'src> MutVisitor<'src> for FloatLiterals<'_, '_, 'src> {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        if let Stmt::Let {
            typ: Some(typ),
            value: Some(value),
            ..
        } = &mut stmt.0
            && let Some(float) = elem_type(&typ.0).and_then(|t| Float::of_name(type_name(&t.0)?))
        {
            self.convert_array(value, Some(float));
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        if let Some(float) = self.expected_float(expr) {
            self.convert(expr, float);
        }
        self.convert_array(expr, None);
        walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{
        MapTypeQuery, codes, span_in, span_of, squash, to_redmod, to_redmod_with_types,
    };

    fn prim(name: &str) -> TypeHint {
        TypeHint::Primitive(name.to_owned())
    }

    #[test]
    fn int_literals_expected_as_floats() {
        let src = "func F(w: ref<inkWidget>, f: Float) -> Float {
  w.SetSize(445, 125);
  let a: Float = 2;
  let d: Double = 3;
  let b = f * -5;
  Scale(-6);
  let arr: [Float] = [7, 8];
  f = 9;
  Count(10);
  Big(16777217);
  return 11;
}
";
        let float = prim("Float");
        let types = MapTypeQuery::new()
            .with_expected(span_of(src, "445", 0), float.clone())
            .with_expected(span_of(src, "125", 0), float.clone())
            .with_expected(span_in(src, "= 2;", 0, "2"), float.clone())
            .with_expected(span_in(src, "= 3;", 0, "3"), prim("Double"))
            // binary operators record the expected type of the literal itself
            .with_expected(span_in(src, "-5", 0, "5"), float.clone())
            // single candidates record it for the negation
            .with_expected(span_of(src, "-6", 0), float.clone())
            // only the first element is recorded
            .with_expected(span_of(src, "7", 0), float.clone())
            .with_expected(span_of(src, "9", 0), float.clone())
            .with_expected(span_of(src, "10", 0), prim("Int32"))
            .with_expected(span_of(src, "16777217", 0), float.clone())
            .with_expected(span_of(src, "11", 0), float);
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "w.SetSize(445.0, 125.0);",
            "let a: Float = 2.0;",
            "let d: Double = 3.0",
            "let b = f * -5.0;",
            "Scale(-6.0);",
            "let arr: array<Float> = [7.0, 8.0];",
            "f = 9.0;",
            "Count(10);",
            // not exactly representable
            "Big(16777217);",
            "return 11.0;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["float-literal-precision"]);
    }

    #[test]
    fn array_literals_use_the_declared_element_type() {
        let src = "func F() {
  let a: [Float] = [1, -2, 3.5];
  let b: [Int32] = [4, 5];
}
";
        // any type information enables the pass
        let types = MapTypeQuery::new().with_type(span_of(src, "4", 0), prim("Int32"));
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains("let a: array<Float> = [1.0, -2.0, 3.5];"),
            "{out}"
        );
        assert!(out.contains("let b: array<Int32> = [4, 5];"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn literals_are_kept_without_type_information() {
        let (out, _) = to_redmod(
            "func F(w: ref<inkWidget>) {\n  w.SetSize(445, -5);\n}\n",
            run,
        );
        assert!(squash(&out).contains("w.SetSize(445, -5);"), "{out}");
    }
}

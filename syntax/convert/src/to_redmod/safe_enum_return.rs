//! REDmod crashes the game when a function returns an enum produced by a cast directly
//! (`return IntEnum<E>(Cast<Int32>(n));`) and a caller discards the result. CDPR's "safe cast"
//! avoids it: each cast goes into a typed local of its own, and the local is returned.
//!
//! A `return` whose value is a cast to an enum, whose argument is another cast or a call, is
//! rewritten that way, following the rules of [`hoist`](super::hoist):
//!
//! ```text
//! return IntEnum<E>(Cast<Int32>(n));
//! // becomes
//! let value: Int32 = Cast<Int32>(n);
//! let enumValue: E = IntEnum<E>(value);
//! return enumValue;
//! ```
//!
//! The casts are `IntEnum(..)` (whose target is always an enum: its type argument, or else the
//! return type of the function), `x as E` and `Cast<E>(..)` when `E` is an enum declared in the
//! module or the type information says so. The argument goes into a local of its own when its
//! type is known: the type argument of a cast, `Int32` for `EnumInt(..)`, the target of `as`, or
//! the type information for a call. Otherwise only the enum cast is moved. Other returns are left
//! unchanged.

use hashbrown::HashSet;
use redscript_ast::{Expr, SourceModule, Span, Stmt};

use super::hoist::{Hoist, Hoisting, Rewrite};
use crate::index::{ModuleIndex, Names};
use crate::walk::{
    MutVisitor, SExpr, SParam, SStmt, SType, for_each_function, free_call_name, plain_type, synth,
    type_name, walk_body, walk_stmt,
};
use crate::{Cx, TypeHint, TypeQuery};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let enums = ModuleIndex::new(module).enums;
    let types = cx.opts.types;
    for_each_function(module, |_, func| {
        let Some(body) = &mut func.body else {
            return;
        };
        let mut returns = Returns(HashSet::new());
        walk_body(&mut returns, body);
        if returns.0.is_empty() {
            return;
        }
        let return_type = func.return_type.as_deref().cloned();
        let names = Names::of(func, cx.arena);
        let rewrite = SafeEnumReturn {
            cx: &mut *cx,
            types,
            enums: &enums,
            returns: &returns.0,
            return_type,
        };
        if let Some(body) = &mut func.body {
            walk_body(&mut Hoisting::new(rewrite, names), body);
        }
    });
}

/// Collects the spans of the values returned by a function, outside lambdas.
struct Returns(HashSet<Span>);

impl<'src> MutVisitor<'src> for Returns {
    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        if let Stmt::Return(Some(value)) = &stmt.0
            && value.1.start < value.1.end
        {
            self.0.insert(value.1);
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, _expr: &mut SExpr<'src>) {}

    fn visit_lambda(
        &mut self,
        _params: &mut Box<[SParam<'src>]>,
        _body: &mut redscript_ast::SourceFunctionBody<'src>,
    ) {
    }
}

struct SafeEnumReturn<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    types: Option<&'a dyn TypeQuery>,
    /// The enums declared in the module.
    enums: &'a HashSet<&'src str>,
    /// The spans of the returned values.
    returns: &'a HashSet<Span>,
    return_type: Option<SType<'src>>,
}

/// The argument of a cast, if `expr` is one.
fn cast_arg_mut<'a, 'src>(expr: &'a mut SExpr<'src>) -> Option<&'a mut SExpr<'src>> {
    let name = free_call_name(&expr.0);
    match &mut expr.0 {
        Expr::Call { args, .. }
            if matches!(name, Some("IntEnum" | "EnumInt" | "Cast" | "FromVariant")) =>
        {
            match &mut args[..] {
                [arg] => Some(arg),
                _ => None,
            }
        }
        Expr::DynCast { expr, .. } => Some(expr),
        _ => None,
    }
}

/// Whether `expr` is a call, including the calls of the cast intrinsics, or an `as` cast.
fn is_call_or_cast(expr: &SExpr<'_>) -> bool {
    matches!(expr.0, Expr::Call { .. } | Expr::DynCast { .. })
}

impl<'src> SafeEnumReturn<'_, '_, 'src> {
    fn is_enum(&self, typ: &SType<'src>, span: Span) -> bool {
        type_name(&typ.0).is_some_and(|name| self.enums.contains(name))
            || self
                .types
                .and_then(|types| types.type_of(span))
                .is_some_and(|hint| matches!(hint, TypeHint::Enum(_)))
    }

    /// The enum `expr` casts to, if it's a cast to an enum.
    fn enum_target(&self, expr: &SExpr<'src>) -> Option<SType<'src>> {
        match (&expr.0, free_call_name(&expr.0)) {
            (Expr::Call { type_args, .. }, Some("IntEnum")) => type_args
                .first()
                .cloned()
                .or_else(|| self.return_type.clone()),
            (Expr::Call { type_args, .. }, Some("Cast")) => type_args
                .first()
                .filter(|typ| self.is_enum(typ, expr.1))
                .cloned(),
            (Expr::DynCast { typ, .. }, _) => {
                Some((**typ).clone()).filter(|typ| self.is_enum(typ, expr.1))
            }
            _ => None,
        }
    }

    /// The type of the local holding `expr`, when it's known.
    fn value_type(&self, expr: &SExpr<'src>) -> Option<SType<'src>> {
        let syn = synth(expr.1);
        let declared = match (&expr.0, free_call_name(&expr.0)) {
            (Expr::Call { type_args, .. }, Some("IntEnum" | "Cast" | "FromVariant")) => {
                type_args.first().cloned()
            }
            (Expr::Call { .. }, Some("EnumInt")) => Some(plain_type("Int32", syn)),
            (Expr::DynCast { typ, .. }, _) => Some((**typ).clone()),
            _ => None,
        };
        declared.or_else(|| {
            let hint = self.types?.type_of(expr.1)?;
            Some((hint.to_ast_type(self.cx.arena, syn)?, syn))
        })
    }

    /// Moves `operand`, a call or a cast, into a local when its type is known, after the
    /// operands of its own casts.
    fn hoist_operand(&mut self, operand: &mut SExpr<'src>, hoist: &mut Hoist<'src>) {
        let Some(typ) = self.value_type(operand) else {
            return;
        };
        if let Some(inner) = cast_arg_mut(operand)
            && is_call_or_cast(inner)
        {
            self.hoist_operand(inner, hoist);
        }
        let value = std::mem::replace(operand, (Expr::Error, operand.1));
        *operand = hoist.hoist("value", typ.0, value);
    }
}

impl<'src> Rewrite<'src> for SafeEnumReturn<'_, '_, 'src> {
    fn rewrite(&mut self, expr: &mut SExpr<'src>, hoist: &mut Hoist<'src>) {
        if !self.returns.contains(&expr.1) || !hoist.in_place() || hoist.effects_before() {
            return;
        }
        let Some(target) = self.enum_target(expr) else {
            return;
        };
        let Some(arg) = cast_arg_mut(expr) else {
            return;
        };
        if !is_call_or_cast(arg) {
            return;
        }
        self.hoist_operand(arg, hoist);
        if let Expr::Call { type_args, .. } = &mut expr.0
            && type_args.is_empty()
        {
            *type_args = [(target.0.clone(), synth(expr.1))].into();
        }
        let span = expr.1;
        let name = type_name(&target.0).unwrap_or("enum");
        self.cx.info(
            span,
            "safe-enum-return",
            format!(
                "REDmod crashes when a caller discards a cast `{name}` returned directly, the \
                 casts were moved into locals and the local is returned (CDPR's safe cast)"
            ),
        );
        let value = std::mem::replace(expr, (Expr::Error, span));
        *expr = hoist.hoist("enumValue", target.0, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_of, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn casts_returned_directly_go_into_locals() {
        let src = "enum EKey { A = 0 }

func Inner(n: Uint64) -> EKey {
  return IntEnum<EKey>(Cast<Int32>(n));
}

func Inferred(n: Uint64) -> EKey {
  return IntEnum(Cast<Int32>(Cast<Uint64>(n)));
}

func FromCall(o: ref<Obj>) -> EKey {
  if o.Ready() {
    // keep me
    return IntEnum<EKey>(o.Code());
  }
  return IntEnum<EKey>(EnumInt(o.Mode()));
}
";
        let (out, diags) = to_redmod(src, run);
        let out = squash(&out);
        for expected in [
            "let value: Int32 = Cast<Int32>(n); let enumValue: EKey = IntEnum<EKey>(value); \
             return enumValue;",
            "let value: Uint64 = Cast<Uint64>(n); let value_1: Int32 = Cast<Int32>(value); \
             let enumValue: EKey = IntEnum<EKey>(value_1); return enumValue;",
            // the type of a call is unknown without type information
            "// keep me let enumValue: EKey = IntEnum<EKey>(o.Code()); return enumValue;",
            "let value: Int32 = EnumInt(o.Mode()); let enumValue_1: EKey = \
             IntEnum<EKey>(value); return enumValue_1;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["safe-enum-return"; 4]);
    }

    #[test]
    fn call_types_and_enums_from_types() {
        let src = "func F(o: ref<Obj>) -> EKey {
  return IntEnum<EKey>(o.Code());
}

func G(o: ref<Obj>) -> EKey {
  return Cast<EKey>(o.Code());
}
";
        let int = TypeHint::Primitive("Int32".to_owned());
        let types = MapTypeQuery::new()
            .with_type(span_of(src, "o.Code()", 0), int.clone())
            .with_type(span_of(src, "o.Code()", 1), int)
            .with_type(
                span_of(src, "Cast<EKey>(o.Code())", 0),
                TypeHint::Enum("EKey".to_owned()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let value: Int32 = o.Code(); let enumValue: EKey = IntEnum<EKey>(value); return \
             enumValue;",
            "let value: Int32 = o.Code(); let enumValue: EKey = Cast<EKey>(value); return \
             enumValue;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["safe-enum-return"; 2]);
    }

    #[test]
    fn other_returns_are_unchanged() {
        let src = "enum EKey { A = 0 }

func Local(i: Int32) -> EKey {
  return IntEnum<EKey>(i);
}

func NotEnum(o: ref<Obj>) -> Int32 {
  return Cast<Int32>(o.Code());
}

func UnknownAs(o: ref<Obj>) -> ref<Foo> {
  return o.Make() as Foo;
}

func Nested(o: ref<Obj>) -> Bool {
  return Use(IntEnum<EKey>(o.Code()));
}

func Lambda(o: ref<Obj>) {
  let f = (x: Int32) -> { return IntEnum<EKey>(o.Code()); };
}

func Direct(o: ref<Obj>) -> EKey {
  let k: EKey = IntEnum<EKey>(o.Code());
  return k;
}
";
        let (out, diags) = to_redmod(src, run);
        assert!(
            !out.contains("enumValue") && !out.contains("value"),
            "{out}"
        );
        assert!(diags.is_empty(), "{diags:?}");
    }
}

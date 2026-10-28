//! REDmod has no `new` with arguments. In redscript 0.5, `new S(a, b)` with arguments is only
//! valid for a struct `S`, with one argument per field in declaration order; REDmod writes the
//! same value as a call to the struct, `S(a, b)` (the form the game's own `.script` sources use,
//! e.g. `Vector3( 0.0, 0.0, 0.0 )` or `QuickSlotUIStructure( index, success )`).
//!
//! The arguments keep their order, so nothing needs hoisting. The struct's fields come from the
//! type information, or else from the structs declared in the module. When they are unknown, or
//! do not match the arguments, the expression is left as it is and the diagnostics report it as
//! `new-with-args`.

use redscript_ast::{Expr, SourceExpr, SourceModule};

use crate::index::ModuleIndex;
use crate::walk::{MutVisitor, SExpr, for_each_function, ident, type_name, walk_body, walk_expr};
use crate::{Cx, TypeQuery};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            walk_body(
                &mut StructConstructors {
                    index: &index,
                    cx: &mut *cx,
                },
                body,
            );
        }
    });
}

/// The number of fields of the struct `name`, when it is known to be a struct.
fn field_count(
    index: &ModuleIndex<'_>,
    types: Option<&dyn TypeQuery>,
    name: &str,
) -> Option<usize> {
    if let Some(fields) = types.and_then(|types| types.struct_fields(name)) {
        return Some(fields.len());
    }
    index.struct_fields(name).map(<[_]>::len)
}

/// Whether `expr` is a `new S(..)` with arguments that becomes a call `S(..)`: `S` is a struct
/// with as many fields as there are arguments.
pub(crate) fn is_struct_constructor(
    expr: &SourceExpr<'_>,
    index: &ModuleIndex<'_>,
    types: Option<&dyn TypeQuery>,
) -> bool {
    let Expr::New { typ, args } = expr else {
        return false;
    };
    !args.is_empty()
        && type_name(&typ.0)
            .and_then(|name| field_count(index, types, name))
            .is_some_and(|count| count == args.len())
}

struct StructConstructors<'a, 'b, 'src> {
    index: &'a ModuleIndex<'src>,
    cx: &'a mut Cx<'b, 'src>,
}

impl<'src> MutVisitor<'src> for StructConstructors<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        if !is_struct_constructor(&expr.0, self.index, self.cx.opts.types) {
            return;
        }
        let Expr::New { typ, args } = &mut expr.0 else {
            return;
        };
        let Some(name) = type_name(&typ.0) else {
            return;
        };
        let callee = ident(name, typ.1);
        let args = std::mem::take(args);
        expr.0 = Expr::Call {
            expr: Box::new(callee),
            type_args: Box::new([]),
            args,
        };
        self.cx.info(
            expr.1,
            "struct-constructor",
            format!(
                "REDmod has no `new` with arguments, `new {name}(..)` was written as \
                 `{name}(..)`, with one argument per field in declaration order"
            ),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn game_structs_with_known_fields() {
        let types = MapTypeQuery::new().with_struct("Vector3", &["X", "Y", "Z"]);
        let (out, diags) = to_redmod_with_types(
            "func F(i: ref<StateGameScriptInterface>) {\n  i.Set(ToVariant(new Vector3(0.0, 0.0, 0.0)));\n  let v: Vector3 = new Vector3(1.0, F(), new Vector3(1.0, 2.0, 3.0).X);\n}\n",
            &types,
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains("i.Set(ToVariant(Vector3(0.0, 0.0, 0.0)));"),
            "{out}"
        );
        assert!(
            out.contains("v: Vector3 = Vector3(1.0, F(), Vector3(1.0, 2.0, 3.0).X);"),
            "{out}"
        );
        assert_eq!(
            codes(&diags),
            [
                "struct-constructor",
                "struct-constructor",
                "struct-constructor"
            ]
        );
    }

    #[test]
    fn structs_declared_in_the_module() {
        let (out, diags) = to_redmod(
            "struct Pair {\n  let a: Int32;\n  let b: String;\n}\n\nfunc F() {\n  let p = new Pair(1, \"x\");\n}\n",
            run,
        );
        assert!(squash(&out).contains("p = Pair(1, \"x\");"), "{out}");
        assert_eq!(codes(&diags), ["struct-constructor"]);
    }

    #[test]
    fn unknown_or_mismatched_fields_are_left_alone() {
        let types = MapTypeQuery::new().with_struct("Vector2", &["X", "Y"]);
        let src = "class Own {}\n\nstruct One {\n  let a: Int32;\n}\n\nfunc F() {\n  let a = new Vector3(1.0, 2.0, 3.0);\n  let b = new Vector2(1.0, 2.0, 3.0);\n  let c = new Own(1);\n  let d = new One(1, 2);\n  let e = new One();\n}\n";
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(out.contains("new Vector3(1.0, 2.0, 3.0)"), "{out}");
        assert!(out.contains("new Vector2(1.0, 2.0, 3.0)"), "{out}");
        assert!(out.contains("new Own(1)"), "{out}");
        assert!(out.contains("new One(1, 2)"), "{out}");
        assert!(out.contains("new One;"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn diagnostics_only_report_the_constructors_left() {
        let types = MapTypeQuery::new().with_struct("Vector3", &["X", "Y", "Z"]);
        let (_, diags) = to_redmod_with_types(
            "func F() {\n  let a = new Vector3(1.0, 2.0, 3.0);\n  let b = new Vector4(1.0, 2.0, 3.0, 4.0);\n}\n",
            &types,
            |m, cx| crate::diagnostics::community(m, cx),
        );
        assert_eq!(codes(&diags), ["new-with-args"]);
    }
}

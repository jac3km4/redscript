//! Diagnostic-only checks: constructs that are not rewritten, either because the right rewrite
//! depends on type information or because they need a manual decision.

use redscript_ast::{
    AstNode, AstVisitor, BinOp, Constant, Expr, Item, ItemQualifiers, SourceAstNode, SourceModule,
    WithSpan,
};

use crate::Cx;
use crate::index::ModuleIndex;
use crate::to_redmod::to_string;
use crate::walk::{PATCH_ANNOTATIONS, REDMOD_RESERVED, SExpr, free_call_name};

const UNSUPPORTED_MATH: [&str; 5] = [
    "RoundF",
    "AngleNormalize",
    "AngleDistance",
    "AngleApproach",
    "LerpAngleF",
];

/// Checks community code about to be converted to REDmod.
pub fn community<'src>(module: &SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    let mut visitor = Community { cx, index: &index };
    visitor.visit_module(module).ok();
}

struct Community<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    index: &'a ModuleIndex<'src>,
}

impl<'src> Community<'_, '_, 'src> {
    fn check_expr(&mut self, (expr, span): &SExpr<'src>) {
        let span = *span;
        match expr {
            Expr::Call { args, .. } => match free_call_name(expr) {
                Some("IsDefined") => self.cx.warn(
                    span,
                    "is-defined",
                    "IsDefined(..) depends on the type of its argument in REDmod: `if (obj)` for \
                     objects, `EntityID.IsDefined(..)`, `GlobalNodeRef.IsDefined(..)` or \
                     `TDBID.IsValid(..)`; convert it manually or with type information",
                ),
                Some("ToString") => {
                    let arg = match &args[..] {
                        [arg] => Some(arg.1),
                        _ => None,
                    };
                    let message = to_string::call_warning(self.cx.opts.types, arg);
                    self.cx.warn(span, "to-string", message);
                }
                Some("Deref") => self.cx.warn(
                    span,
                    "deref",
                    "REDmod dereferences script_ref values implicitly, remove Deref(..)",
                ),
                Some(name) if UNSUPPORTED_MATH.contains(&name) => self.cx.warn(
                    span,
                    "unsupported-math",
                    format!("{name} is not available in REDmod"),
                ),
                _ => {}
            },
            Expr::New { args, .. } if !args.is_empty() => self.cx.warn(
                span,
                "new-with-args",
                "REDmod has no constructor arguments, create the object with `new X` and \
                 assign the fields",
            ),
            // only parsed in the 0.5.x compatibility mode
            Expr::Assign { rhs, .. } | Expr::BinOp { rhs, .. }
                if is_assignment(expr) && is_assignment(&rhs.0) =>
            {
                self.cx.warn(
                    span,
                    "chained-assignment",
                    "REDmod has no chained assignments, split this into one assignment per \
                     statement",
                );
            }
            Expr::Lambda { .. } => self.cx.warn(
                span,
                "lambda",
                "REDmod has no lambdas, rewrite this with a class or a callback",
            ),
            Expr::Constant(Constant::String(s))
                if s.contains(|c: char| c == '"' || (c.is_control() && !"\n\r\t".contains(c))) =>
            {
                self.cx.info(
                    span,
                    "string-escape",
                    "this string contains characters REDmod cannot escape with `\\\"` or `\\x`, \
                     they were written as `\\u{..}` escapes",
                );
            }
            Expr::ArrayLit(elems) => {
                for (elem, span) in &elems[..] {
                    let is_cast = matches!(elem, Expr::DynCast { .. })
                        || matches!(
                            free_call_name(elem),
                            Some("EnumInt" | "IntEnum" | "ToVariant" | "FromVariant" | "Cast")
                        );
                    if is_cast {
                        self.cx.info(
                            *span,
                            "cast-in-array-literal",
                            "REDmod rejects `(T)x` casts inside array literals, keep the `as` \
                             form or build the array with PushBack",
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

fn is_assignment(expr: &Expr<'_, WithSpan>) -> bool {
    matches!(
        expr,
        Expr::Assign { .. }
            | Expr::BinOp {
                op: BinOp::AssignAdd
                    | BinOp::AssignSub
                    | BinOp::AssignMul
                    | BinOp::AssignDiv
                    | BinOp::AssignBitOr
                    | BinOp::AssignBitAnd,
                ..
            }
    )
}

impl<'src> AstVisitor<'src, WithSpan> for Community<'_, '_, 'src> {
    type Error = ();

    fn visit_node(&mut self, node: SourceAstNode<'_, 'src>) -> Result<(), Self::Error> {
        match node {
            AstNode::Expr(expr) => self.check_expr(expr),
            AstNode::ItemDecl((decl, span)) => {
                let span = *span;
                for (ann, _) in &decl.annotations {
                    if !PATCH_ANNOTATIONS.contains(&ann.name) {
                        continue;
                    }
                    if let Some((Expr::Ident(target), _)) = ann.args.first()
                        && self.index.is_declared_class(target)
                    {
                        self.cx.warn(
                            span,
                            "patch-own-class",
                            format!(
                                "@{} on `{target}`, a class declared in this file: patching a \
                                 mod's own class crashes REDmod at load, change the class \
                                 directly",
                                ann.name
                            ),
                        );
                    }
                }
                match &decl.item {
                    Item::Let(field) if decl.qualifiers.contains(ItemQualifiers::PERSISTENT) => {
                        self.cx.warn(
                            span,
                            "persistent-field",
                            format!(
                                "persistent field `{}`: REDmod persistence works differently \
                                 (see ModdingSystem), and fields read from outside their class \
                                 need accessor methods",
                                field.name.0
                            ),
                        );
                    }
                    Item::Let(field) if REDMOD_RESERVED.contains(&field.name.0) => self.cx.warn(
                        span,
                        "reserved-field-name",
                        format!(
                            "`{}` is a reserved word in REDmod, rename this field",
                            field.name.0
                        ),
                    ),
                    Item::Class(_) | Item::Struct(_) | Item::Enum(_)
                        if decl.annotations.iter().any(|(ann, _)| ann.name == "if") =>
                    {
                        self.cx.warn(
                            span,
                            "conditional-type",
                            "REDmod only allows @if on functions, make this type unconditional \
                             or move the condition to the functions using it",
                        );
                    }
                    Item::Function(func)
                        if func.name.0 == "ScriptLog"
                            && decl.qualifiers.contains(ItemQualifiers::NATIVE) =>
                    {
                        self.cx.warn(
                            span,
                            "script-log-import",
                            "only one installed REDmod mod may import ScriptLog, remove it \
                             before releasing",
                        );
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, to_redmod};

    #[test]
    fn community_diagnostics() {
        let (_, diags) = to_redmod(
            "class Own {\n  persistent let p: Int32;\n  let name: String;\n}\n\n@addField(Own)\nlet extra: Int32;\n\nnative func ScriptLog(text: String);\n\nfunc F(o: ref<Own>, r: script_ref<Int32>) {\n  let a = IsDefined(o);\n  let b: String = ToString(1);\n  let c: Int32 = Deref(r);\n  let d: Float = RoundF(1.5);\n  let e: Int32 = Cast<Int32>(1 + 2);\n  let f: ref<Own> = new Own(1);\n  let g: String = \"say \\\"hi\\\"\";\n  let h: [Int32] = [EnumInt(E.A), 1];\n  let l = (x) -> x;\n}\n",
            |m, cx| community(m, cx),
        );
        let mut codes = codes(&diags);
        codes.sort_unstable();
        assert_eq!(
            codes,
            [
                "cast-in-array-literal",
                "deref",
                "is-defined",
                "lambda",
                "new-with-args",
                "patch-own-class",
                "persistent-field",
                "reserved-field-name",
                "script-log-import",
                "string-escape",
                "to-string",
                "unsupported-math",
            ]
        );
    }

    #[test]
    fn reserved_field_names_and_conditional_types() {
        let (_, diags) = to_redmod(
            "class C {\n  let event: Int32;\n  let hint: Int32;\n  let ok: Int32;\n}\n\n@if(ModuleExists(\"A\"))\nclass D {}\n\n@if(ModuleExists(\"A\"))\nstruct S {}\n\n@if(ModuleExists(\"A\"))\nenum E {\n  A = 0,\n}\n\n@if(ModuleExists(\"A\"))\nfunc F() {}\n",
            |m, cx| community(m, cx),
        );
        assert_eq!(
            codes(&diags),
            [
                "reserved-field-name",
                "reserved-field-name",
                "conditional-type",
                "conditional-type",
                "conditional-type",
            ]
        );
        assert!(diags[0].message.contains("`event`"), "{}", diags[0]);
    }

    #[test]
    fn chained_assignments() {
        let (_, diags) = to_redmod(
            "func F() {
  let a: Int32;
  let b: Int32;
  a = b = 1;
  a += b -= 1;
  a = b + 1;
}
",
            |m, cx| community(m, cx),
        );
        assert_eq!(codes(&diags), ["chained-assignment", "chained-assignment"]);
    }
}

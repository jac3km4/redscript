//! Diagnostic-only checks: constructs that are not rewritten, either because the right rewrite
//! depends on type information or because they need a manual decision.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{
    AstNode, AstVisitor, Constant, Expr, Item, ItemQualifiers, SourceAstNode, SourceExpr,
    SourceModule, Span, UnOp, WithSpan,
};

use crate::Cx;
use crate::index::ModuleIndex;
use crate::to_redmod::{fields, to_string};
use crate::walk::{PATCH_ANNOTATIONS, REDMOD_RESERVED, SExpr, free_call_name};

const UNSUPPORTED_MATH: [&str; 5] = [
    "RoundF",
    "AngleNormalize",
    "AngleDistance",
    "AngleApproach",
    "LerpAngleF",
];

/// Annotations adding a member to a class, which REDmod silently drops on `importonly` classes.
const ADDING_ANNOTATIONS: [&str; 2] = ["addField", "addMethod"];

/// Checks community code about to be converted to REDmod.
pub fn community<'src>(module: &SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    conditional_limits(module, cx);
    let index = ModuleIndex::new(module);
    let renamed_fields = fields::renamed_field_spans(module);
    let mut visitor = Community {
        cx,
        index: &index,
        renamed_fields: &renamed_fields,
    };
    visitor.visit_module(module).ok();
}

struct Community<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    index: &'a ModuleIndex<'src>,
    /// The declarations of the fields renamed by [`fields`].
    renamed_fields: &'a HashSet<Span>,
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
                    if let Some((Expr::Ident(target), _)) = ann.args.first()
                        && ADDING_ANNOTATIONS.contains(&ann.name)
                        && self
                            .cx
                            .opts
                            .types
                            .and_then(|types| types.is_import_only(target))
                            == Some(true)
                    {
                        self.cx.warn(
                            span,
                            "importonly-target",
                            format!(
                                "@{} on `{target}`, an `importonly` class: REDmod silently drops \
                                 this member (every use of it then fails with `is not a member`), \
                                 add it to another class",
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
                    Item::Let(field)
                        if REDMOD_RESERVED.contains(&field.name.0)
                            && !self.renamed_fields.contains(&span) =>
                    {
                        self.cx.warn(
                            span,
                            "reserved-field-name",
                            format!(
                                "`{}` is a reserved word in REDmod, rename this field",
                                field.name.0
                            ),
                        );
                    }
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

/// The limits of REDmod on the versions of a conditional function, each of which aborts the
/// compile without an error message:
/// - at most two versions of a conditional function,
/// - not two versions with a positive `ModuleExists(..)` condition,
/// - a single version of a conditional `@addMethod`.
///
/// The versions of a function are the functions of the module with the same name, parameter
/// types and, for `@addMethod`, target class, when one of them at least has a condition. Only
/// the versions declared in the same file are seen. `@if` on imports and types is reported
/// elsewhere.
fn conditional_limits<'src>(module: &SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    type Key<'src> = (Option<&'src str>, &'src str, String);
    let mut groups: HashMap<Key<'src>, Vec<(Span, Option<&SourceExpr<'src>>)>> = HashMap::new();
    let mut order = vec![];
    for (decl, span) in &module.items {
        let Item::Function(func) = &decl.item else {
            continue;
        };
        let patch = decl
            .annotations
            .iter()
            .find(|(ann, _)| PATCH_ANNOTATIONS.contains(&ann.name));
        let target = match patch {
            None => None,
            Some((ann, _)) if ann.name == "addMethod" => match ann.args.first() {
                Some((Expr::Ident(target), _)) => Some(*target),
                _ => continue,
            },
            // wraps chain and replacements supersede each other: they are not versions
            Some(_) => continue,
        };
        let params = func
            .params
            .iter()
            .map(|(param, _)| format!("{:?}", param.typ.clone().map(|t| t.0.unwrapped())))
            .collect::<Vec<_>>()
            .join(",");
        let condition = decl
            .annotations
            .iter()
            .find(|(ann, _)| ann.name == "if")
            .and_then(|(ann, _)| ann.args.first())
            .map(|(cond, _)| cond);
        let key = (target, func.name.0, params);
        let versions = groups.entry(key.clone()).or_default();
        if versions.is_empty() {
            order.push(key);
        }
        versions.push((*span, condition));
    }

    for key in order {
        let versions = &groups[&key];
        if versions.iter().all(|(_, cond)| cond.is_none()) {
            continue;
        }
        let (target, name, _) = key;
        if let Some(target) = target {
            for (span, _) in versions.iter().skip(1) {
                cx.warn(
                    *span,
                    "conditional-limit",
                    format!(
                        "REDmod allows a single version of a conditional @addMethod, this other \
                         version of `{target}.{name}` aborts the REDmod compile without an \
                         error: keep one version"
                    ),
                );
            }
            continue;
        }
        for (span, _) in versions.iter().skip(2) {
            cx.warn(
                *span,
                "conditional-limit",
                format!(
                    "REDmod allows at most two versions of a conditional function, this third \
                     version of `{name}` aborts the REDmod compile without an error: merge the \
                     versions"
                ),
            );
        }
        let second_positive = versions
            .iter()
            .filter(|(_, cond)| cond.is_some_and(has_positive_module_exists))
            .nth(1);
        if let Some((span, _)) = second_positive {
            cx.warn(
                *span,
                "conditional-limit",
                format!(
                    "REDmod rejects two versions of a conditional function that both use a \
                     positive ModuleExists(..), this version of `{name}` aborts the REDmod \
                     compile without an error: pair a positive condition with a negated one \
                     (`!ModuleExists(..)`)"
                ),
            );
        }
    }
}

/// Whether `cond` contains a `ModuleExists(..)` call that is not negated.
fn has_positive_module_exists(cond: &SourceExpr<'_>) -> bool {
    match cond {
        Expr::UnOp { op: UnOp::Not, .. } => false,
        Expr::BinOp { lhs, rhs, .. } => {
            has_positive_module_exists(&lhs.0) || has_positive_module_exists(&rhs.0)
        }
        expr => free_call_name(expr) == Some("ModuleExists"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use crate::test_util::{MapTypeQuery, codes, to_redmod, to_redmod_with_types};

    #[test]
    fn community_diagnostics() {
        let (_, diags) = to_redmod(
            "class Own {\n  persistent let p: Int32;\n  let name: String;\n}\n\n@addField(Own)\nlet extra: Int32;\n\n@addField(GameObject)\nlet name: CName;\n\nnative func ScriptLog(text: String);\n\nfunc F(o: ref<Own>, r: script_ref<Int32>) {\n  let a = IsDefined(o);\n  let b: String = ToString(1);\n  let c: Int32 = Deref(r);\n  let d: Float = RoundF(1.5);\n  let e: Int32 = Cast<Int32>(1 + 2);\n  let f: ref<Own> = new Own(1);\n  let g: String = \"say \\\"hi\\\"\";\n  let h: [Int32] = [EnumInt(E.A), 1];\n  let l = (x) -> x;\n}\n",
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
            "native class C {\n  native let event: Int32;\n  let hint: Int32;\n  let ok: Int32;\n}\n\nclass Renamed {\n  let event: Int32;\n}\n\n@if(ModuleExists(\"A\"))\nclass D {}\n\n@if(ModuleExists(\"A\"))\nstruct S {}\n\n@if(ModuleExists(\"A\"))\nenum E {\n  A = 0,\n}\n\n@if(ModuleExists(\"A\"))\nfunc F() {}\n",
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
    fn members_added_to_importonly_classes() {
        let src = "@addField(inkISystemRequestsHandler)
let counter: Int32;

@addMethod(inkISystemRequestsHandler)
func Added() {}

@wrapMethod(inkISystemRequestsHandler)
func Wrapped() { wrappedMethod(); }

@addField(ModdingSystem)
let key: Int32;

@addMethod(Unknown)
func Other() {}
";
        let types = MapTypeQuery::new()
            .with_import_only("inkISystemRequestsHandler", true)
            .with_import_only("ModdingSystem", false);
        let (_, diags) = to_redmod_with_types(src, &types, |m, cx| community(m, cx));
        assert_eq!(codes(&diags), ["importonly-target", "importonly-target"]);
        assert!(diags[0].message.contains("@addField"), "{}", diags[0]);
        assert!(diags[1].message.contains("@addMethod"), "{}", diags[1]);
        assert!(diags.iter().all(|d| d.severity == Severity::Warning));

        // nothing is known without type information
        let (_, diags) = to_redmod(src, |m, cx| community(m, cx));
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn at_most_two_versions_of_a_conditional_function() {
        let src = "@if(ModuleExists(\"A\"))
func F() {}

@if(!ModuleExists(\"A\") && ModuleExists(\"B\"))
func F() {}

@if(!ModuleExists(\"A\") && !ModuleExists(\"B\"))
func F() {}

@if(ModuleExists(\"A\"))
func G(x: Int32) {}

@if(!ModuleExists(\"A\"))
func G(x: Int32) {}

@if(!ModuleExists(\"A\"))
func G(x: Float) {}
";
        let (_, diags) = to_redmod(src, |m, cx| community(m, cx));
        // `F` also has two versions with a positive condition, the second one
        assert_eq!(codes(&diags), ["conditional-limit"; 2]);
        assert!(diags[0].message.contains("third"), "{}", diags[0]);
        assert!(diags[0].message.contains("`F`"), "{}", diags[0]);
        assert!(diags[1].message.contains("positive"), "{}", diags[1]);
        assert!(diags[0].span.start > diags[1].span.start);
    }

    #[test]
    fn two_positive_conditions() {
        let src = "@if(ModuleExists(\"A\"))
func F() {}

@if(ModuleExists(\"B\"))
func F() {}

@if(ModuleExists(\"A\"))
func G() {}

@if(!ModuleExists(\"A\"))
func G() {}

@if(!ModuleExists(\"A\"))
func H() {}

@if(!ModuleExists(\"B\"))
func H() {}

@if(ModuleExists(\"A\"))
func K() {}

func K() {}
";
        let (_, diags) = to_redmod(src, |m, cx| community(m, cx));
        assert_eq!(codes(&diags), ["conditional-limit"]);
        assert!(diags[0].message.contains("positive"), "{}", diags[0]);
        assert!(diags[0].message.contains("`F`"), "{}", diags[0]);
    }

    #[test]
    fn a_single_version_of_a_conditional_add_method() {
        let src = "@if(ModuleExists(\"A\"))
@addMethod(PlayerPuppet)
func M() {}

@if(!ModuleExists(\"A\"))
@addMethod(PlayerPuppet)
func M() {}

@if(ModuleExists(\"A\"))
@addMethod(GameObject)
func M() {}

@if(ModuleExists(\"A\"))
@wrapMethod(PlayerPuppet)
func W() { wrappedMethod(); }

@if(!ModuleExists(\"A\"))
@wrapMethod(PlayerPuppet)
func W() { wrappedMethod(); }

@addMethod(PlayerPuppet)
func N() {}

@addMethod(PlayerPuppet)
func N(x: Int32) {}
";
        let (_, diags) = to_redmod(src, |m, cx| community(m, cx));
        assert_eq!(codes(&diags), ["conditional-limit"]);
        assert!(
            diags[0].message.contains("`PlayerPuppet.M`"),
            "{}",
            diags[0]
        );
        assert!(diags[0].message.contains("@addMethod"), "{}", diags[0]);
    }
}

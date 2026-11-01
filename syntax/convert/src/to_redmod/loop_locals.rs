//! REDmod hoists locals to the top of the function, so a local declared without an initializer
//! inside a loop keeps its value from the previous iteration, while in the community dialect it
//! starts over from its default value. Such locals get an explicit reset where they are declared:
//!
//! - arrays: `let a: array<T>; a.Clear();`
//! - `Int32`, `Float`, `Bool`, `String`, `CName`: the default literal (`0`, `0.0`, `false`,
//!   `""`, `n"None"`), other integer types: `Cast<T>(0)`
//! - references and classes declared in the file: `NULL`
//!
//! With type information, the kind of other named types is taken from the type recorded for a
//! later use of the local: classes become `NULL` and enums `0 as E`. Locals of other types
//! (structs, unknown types) are reported.

use std::borrow::Cow;

use redscript_ast::{
    AstNode, AstVisitor, Constant, Expr, SourceAstNode, SourceFunctionBody, SourceModule,
    SourceType, Span, Stmt, Type, WithSpan,
};

use crate::index::ModuleIndex;
use crate::walk::{
    MutVisitor, SExpr, SParam, SStmt, SType, call, elem_type, expand_stmts, for_each_function,
    ident, int, method_call, plain_type, synth, type_name, walk_body, walk_stmt, walk_stmts,
};
use crate::{Cx, RefKind, TypeHint, TypeQuery};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            walk_body(
                &mut LoopLocals {
                    cx: &mut *cx,
                    index: &index,
                    depth: 0,
                },
                body,
            );
        }
    });
}

struct LoopLocals<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    index: &'a ModuleIndex<'src>,
    /// Number of loops around the statements being visited.
    depth: usize,
}

enum Reset<'src> {
    Value(SExpr<'src>),
    Clear,
}

impl<'src> LoopLocals<'_, '_, 'src> {
    fn reset(&self, typ: &SourceType<'src>, span: Span) -> Option<Reset<'src>> {
        if elem_type(typ).is_some() && !matches!(typ, Type::StaticArray(..)) {
            return Some(Reset::Clear);
        }
        if let Type::Named {
            name: "ref" | "wref",
            ..
        } = typ
        {
            return Some(Reset::Value((Expr::Null, span)));
        }
        let constant = |c| Some(Reset::Value((Expr::Constant(c), span)));
        match type_name(typ)? {
            "Int32" => Some(Reset::Value(int(0, span))),
            name @ ("Int8" | "Int16" | "Int64" | "Uint8" | "Uint16" | "Uint32" | "Uint64") => {
                Some(Reset::Value(call(
                    ident("Cast", span),
                    vec![plain_type(name, span)],
                    vec![int(0, span)],
                    span,
                )))
            }
            "Float" => constant(Constant::F32(0.0)),
            "Bool" => constant(Constant::Bool(false)),
            "String" => constant(Constant::String(Cow::Borrowed(""))),
            "CName" => constant(Constant::CName(Cow::Borrowed("None"))),
            name if self
                .index
                .classes
                .get(name)
                .is_some_and(|c| c.declared && !c.is_struct) =>
            {
                Some(Reset::Value((Expr::Null, span)))
            }
            _ => None,
        }
    }

    /// The reset value of a local of type `typ` named `name` from the type recorded for one of
    /// its uses in `rest`, the statements following its declaration.
    fn typed_reset(
        &self,
        name: &str,
        typ: &SType<'src>,
        rest: &[SStmt<'src>],
    ) -> Option<Reset<'src>> {
        let types = self.cx.opts.types?;
        let type_name = type_name(&typ.0)?;
        let simple = type_name.rsplit_once('.').map_or(type_name, |(_, n)| n);
        let mut finder = UseType {
            name,
            simple,
            types,
            found: None,
        };
        for stmt in rest {
            if finder.visit_stmt(stmt).is_err() {
                break;
            }
        }
        let span = synth(typ.1);
        match finder.found? {
            TypeHint::Enum(_) => Some(Reset::Value((
                Expr::DynCast {
                    expr: Box::new(int(0, span)),
                    typ: Box::new((typ.0.clone(), span)),
                },
                span,
            ))),
            TypeHint::Class(_) | TypeHint::Ref(RefKind::Strong | RefKind::Weak, _) => {
                Some(Reset::Value((Expr::Null, span)))
            }
            _ => None,
        }
    }

    fn expand(&mut self, stmt: SStmt<'src>, out: &mut Vec<SStmt<'src>>, rest: &[SStmt<'src>]) {
        let (
            Stmt::Let {
                name,
                typ,
                value: None,
            },
            span,
        ) = stmt
        else {
            out.push(stmt);
            return;
        };
        let syn = synth(span);
        let reset = typ.as_deref().and_then(|t| {
            self.reset(&t.0, syn)
                .or_else(|| self.typed_reset(name.0, t, rest))
        });
        match reset {
            Some(Reset::Value(value)) => out.push((
                Stmt::Let {
                    name,
                    typ,
                    value: Some(Box::new(value)),
                },
                span,
            )),
            Some(Reset::Clear) => {
                let local = name.0;
                out.push((
                    Stmt::Let {
                        name,
                        typ,
                        value: None,
                    },
                    span,
                ));
                let clear = method_call(ident(local, syn), "Clear", vec![], syn);
                out.push((Stmt::Expr(Box::new(clear)), syn));
            }
            None => {
                self.cx.warn(
                    span,
                    "loop-local-reset",
                    format!(
                        "REDmod keeps the value of `{}` from the previous iteration of the loop, \
                         reset it explicitly",
                        name.0
                    ),
                );
                out.push((
                    Stmt::Let {
                        name,
                        typ,
                        value: None,
                    },
                    span,
                ));
            }
        }
    }
}

impl<'src> MutVisitor<'src> for LoopLocals<'_, '_, 'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        if self.depth > 0 {
            // the following statements are only needed to look up the types of their uses
            let original = if self.cx.opts.types.is_some() {
                stmts.clone()
            } else {
                Box::default()
            };
            let mut index = 0;
            expand_stmts(stmts, |stmt, out| {
                index += 1;
                self.expand(stmt, out, original.get(index..).unwrap_or_default());
            });
        }
        walk_stmts(self, stmts);
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        let is_loop = matches!(
            stmt.0,
            Stmt::While(_) | Stmt::ForIn { .. } | Stmt::For { .. }
        );
        self.depth += usize::from(is_loop);
        walk_stmt(self, stmt);
        self.depth -= usize::from(is_loop);
    }

    fn visit_lambda(&mut self, _: &mut Box<[SParam<'src>]>, body: &mut SourceFunctionBody<'src>) {
        let saved = std::mem::replace(&mut self.depth, 0);
        walk_body(self, body);
        self.depth = saved;
    }
}

/// Finds the type recorded for a use of a local, when it is the named type the local is declared
/// with.
struct UseType<'a> {
    name: &'a str,
    simple: &'a str,
    types: &'a dyn TypeQuery,
    found: Option<TypeHint>,
}

impl<'src> AstVisitor<'src, WithSpan> for UseType<'_> {
    /// Stops the traversal once a type is found.
    type Error = ();

    fn visit_node(&mut self, node: SourceAstNode<'_, 'src>) -> Result<(), Self::Error> {
        if let AstNode::Expr((Expr::Ident(name), span)) = node
            && *name == self.name
            && let Some(hint) = self.types.type_of(*span)
            && hint.simple_name() == Some(self.simple)
            && !matches!(hint, TypeHint::Ref(RefKind::Script, _))
        {
            self.found = Some(hint);
            return Err(());
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, span_in, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn locals_in_loops_are_reset() {
        let (out, diags) = to_redmod(
            "class C {}\nstruct S {}\n\nfunc F(xs: [String]) {\n  let outside: Int32;\n  while true {\n    let found: [String];\n    let n: Int32;\n    let u: Uint32;\n    let f: Float;\n    let b: Bool;\n    let s: String;\n    let c: CName;\n    let r: ref<C>;\n    let o: C;\n    let st: S;\n    let kept: Int32 = 5;\n    if b {\n      let nested: Int32;\n    }\n    ArrayPush(found, s);\n  }\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains("let outside: Int32;"), "{out}");
        for expected in [
            "let found: array<String>; found.Clear();",
            "let n: Int32 = 0;",
            "let u: Uint32 = Cast<Uint32>(0);",
            "let f: Float = 0.0;",
            "let b: Bool = false;",
            "let s: String = \"\";",
            "let c: CName = n\"None\";",
            "let r: C = NULL;",
            "let o: C = NULL;",
            "let st: S;",
            "let kept: Int32 = 5;",
            "let nested: Int32 = 0;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["loop-local-reset"]);
    }

    #[test]
    fn locals_typed_by_their_uses() {
        let src = "func F(p: ref<PlayerPuppet>) {
  while true {
    let o: PlayerPuppet;
    let e: gamedataItemType;
    let v: Vector4;
    let u: Unknown;
    let w: Mismatch;
    o = p;
    e = gamedataItemType.Clo_Face;
    v = p.GetWorldPosition();
    Use(o, e, v, u, w);
  }
}
";
        let types = MapTypeQuery::new()
            .with_type(
                span_in(src, "o = p", 0, "o"),
                TypeHint::Class("PlayerPuppet".to_owned()),
            )
            .with_type(
                span_in(src, "e = g", 0, "e"),
                TypeHint::Enum("gamedataItemType".to_owned()),
            )
            .with_type(
                span_in(src, "v = p", 0, "v"),
                TypeHint::Struct("Vector4".to_owned()),
            )
            // the name of the recorded type has to match the declared one
            .with_type(
                span_in(src, "w)", 0, "w"),
                TypeHint::Class("Other".to_owned()),
            );
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        for expected in [
            "let o: PlayerPuppet = NULL;",
            "let e: gamedataItemType = 0 as gamedataItemType;",
            "let v: Vector4;",
            "let u: Unknown;",
            "let w: Mismatch;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        assert_eq!(codes(&diags), ["loop-local-reset"; 3]);

        let (out, diags) = to_redmod(src, run);
        assert!(squash(&out).contains("let o: PlayerPuppet;"), "{out}");
        assert_eq!(codes(&diags), ["loop-local-reset"; 5]);
    }
}

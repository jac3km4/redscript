//! Local variable names REDmod rejects:
//! - parameters and locals named with a word reserved in REDmod (e.g. `name`, `event`, `out`)
//!   are renamed,
//! - parameters and locals cannot shadow a field of the enclosing class in REDmod, they are
//!   renamed (fields declared outside the module are only known with type information),
//! - locals are hoisted to the top of the function in REDmod, so a name declared twice in one
//!   function (even in sibling blocks) is an error: later declarations are renamed.
//!
//! References are updated following the community scoping rules. Field accesses through `this`
//! are left untouched.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{
    Condition, Expr, LetCondition, SourceConditionalBlock, SourceFunctionBody, SourceModule,
    SourcePattern, Stmt,
};

use crate::Cx;
use crate::index::{ModuleIndex, Names};
use crate::walk::{
    MutVisitor, REDMOD_RESERVED, SExpr, SParam, SStmt, for_each_function, pattern_bindings_mut,
    walk_body, walk_expr, walk_stmt, walk_stmts,
};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let index = ModuleIndex::new(module);
    for_each_function(module, |info, func| {
        let names = Names::of(func, cx.arena);
        let mut renamer = Renamer {
            cx: &mut *cx,
            index: &index,
            class: info.class,
            names,
            scopes: vec![HashMap::new()],
            declared: HashSet::new(),
        };
        for (param, span) in &mut func.params {
            param.name = renamer.declare(param.name, *span);
        }
        if let Some(body) = &mut func.body {
            walk_body(&mut renamer, body);
        }
    });
}

struct Renamer<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    index: &'a ModuleIndex<'src>,
    /// The class whose fields the variables of the function cannot shadow.
    class: Option<&'src str>,
    names: Names<'src>,
    scopes: Vec<HashMap<&'src str, &'src str>>,
    declared: HashSet<&'src str>,
}

impl<'src> Renamer<'_, '_, 'src> {
    /// Declares a variable in the innermost scope and returns the name it should have.
    fn declare(&mut self, name: &'src str, span: redscript_ast::Span) -> &'src str {
        let reason = if REDMOD_RESERVED.contains(&name) {
            Some(format!("`{name}` is a reserved word in REDmod"))
        } else if let Some(class) = self.class
            && self.shadows_field(name)
        {
            Some(format!(
                "REDmod does not allow `{name}` to shadow a field of `{class}`"
            ))
        } else if self.declared.contains(name) {
            Some(format!(
                "REDmod does not allow declaring `{name}` twice in one function"
            ))
        } else {
            None
        };
        let renamed = match reason {
            Some(reason) => {
                let base = if REDMOD_RESERVED.contains(&name) {
                    format!("{name}_")
                } else {
                    name.to_owned()
                };
                let renamed = self.fresh(&base);
                self.cx.info(
                    span,
                    "renamed-local",
                    format!("{reason}, renamed to `{renamed}`"),
                );
                renamed
            }
            None => name,
        };
        self.declared.insert(renamed);
        self.scopes
            .last_mut()
            .expect("there should be a scope")
            .insert(name, renamed);
        renamed
    }

    /// A fresh name derived from `base` that is neither reserved nor shadowing a field.
    fn fresh(&mut self, base: &str) -> &'src str {
        loop {
            let name = self.names.fresh(base);
            if !REDMOD_RESERVED.contains(&name) && !self.shadows_field(name) {
                return name;
            }
        }
    }

    /// Whether `name` is a field of the class of the function, declared in the module or, with
    /// type information, anywhere.
    fn shadows_field(&self, name: &str) -> bool {
        let Some(class) = self.class else {
            return false;
        };
        self.index.has_field(class, name)
            || self
                .cx
                .opts
                .types
                .and_then(|types| types.class_has_field(class, name))
                == Some(true)
    }

    fn lookup(&self, name: &str) -> Option<&'src str> {
        self.scopes.iter().rev().find_map(|s| s.get(name).copied())
    }

    /// Pattern bindings are scoped to the guarded block.
    fn visit_conditional(&mut self, block: &mut SourceConditionalBlock<'src>) {
        match &mut block.condition {
            LetCondition::Expr(expr) => {
                self.visit_expr(expr);
                self.visit_stmts(&mut block.body.stmts);
            }
            LetCondition::LetPattern((pattern, _), value) => {
                self.visit_expr(value);
                self.scoped(|this| {
                    this.declare_pattern(pattern);
                    this.visit_stmts(&mut block.body.stmts);
                });
            }
        }
    }

    fn declare_pattern(&mut self, pattern: &mut SourcePattern<'src>) {
        pattern_bindings_mut(pattern, &mut |name| name.0 = self.declare(name.0, name.1));
    }

    fn scoped(&mut self, f: impl FnOnce(&mut Self)) {
        self.scopes.push(HashMap::new());
        f(self);
        self.scopes.pop();
    }
}

impl<'src> MutVisitor<'src> for Renamer<'_, '_, 'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        self.scoped(|this| walk_stmts(this, stmts));
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &mut stmt.0 {
            Stmt::Let { name, value, .. } => {
                // the initializer cannot refer to the variable being declared
                if let Some(value) = value {
                    self.visit_expr(value);
                }
                name.0 = self.declare(name.0, name.1);
            }
            Stmt::ForIn { name, iter, body } => {
                self.visit_expr(iter);
                self.scoped(|this| {
                    name.0 = this.declare(name.0, name.1);
                    this.visit_stmts(&mut body.stmts);
                });
            }
            Stmt::For { .. } => self.scoped(|this| walk_stmt(this, stmt)),
            Stmt::If { blocks, else_ } => {
                for block in blocks.iter_mut() {
                    self.visit_conditional(block);
                }
                if let Some(else_) = else_ {
                    self.visit_stmts(&mut else_.stmts);
                }
            }
            Stmt::While(block) => self.visit_conditional(block),
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                self.visit_expr(expr);
                for case in cases.iter_mut() {
                    self.scoped(|this| {
                        match &mut case.condition {
                            Condition::Expr(expr) => this.visit_expr(expr),
                            Condition::Pattern((pattern, _)) => this.declare_pattern(pattern),
                        }
                        this.visit_stmts(&mut case.body);
                    });
                }
                if let Some(default) = default {
                    self.visit_stmts(default);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        if let Expr::Ident(name) = &mut expr.0
            && let Some(renamed) = self.lookup(name)
        {
            *name = renamed;
        }
        walk_expr(self, expr);
    }

    fn visit_lambda(
        &mut self,
        params: &mut Box<[SParam<'src>]>,
        body: &mut SourceFunctionBody<'src>,
    ) {
        self.scoped(|this| {
            // lambda parameters only shadow, they are not function locals
            for (param, _) in params.iter_mut() {
                this.scopes
                    .last_mut()
                    .expect("there should be a scope")
                    .insert(param.name, param.name);
            }
            walk_body(this, body);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{MapTypeQuery, codes, squash, to_redmod, to_redmod_with_types};

    #[test]
    fn reserved_names_are_renamed() {
        let (out, diags) = to_redmod(
            "func F(name: String) -> String {\n  let name_ = 1;\n  return name + this.name;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "function F(name__1: String) -> String { let name_ = 1; return name__1 + this.name; }"
            ),
            "{out}"
        );
        assert_eq!(codes(&diags), ["renamed-local"]);
    }

    #[test]
    fn duplicate_locals_are_renamed() {
        let (out, diags) = to_redmod(
            "func F(a: Int32) -> Int32 {\n  let x = 1;\n  if a > 0 {\n    let x = x + 1;\n    a = x;\n  } else {\n    let x = 3;\n    a = x;\n  }\n  let y = x;\n  while a > 0 {\n    let a = 2;\n    a -= 1;\n  }\n  return y + x + a;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "let x = 1; if (a > 0) { let x_1 = x + 1; a = x_1; } else { let x_2 = 3; a = x_2; } let y = x; while (a > 0) { let a_1 = 2; a_1 -= 1; } return y + x + a;"
            ),
            "{out}"
        );
        assert_eq!(codes(&diags), ["renamed-local"; 3]);
    }

    #[test]
    fn pattern_bindings_are_scoped_declarations() {
        let (out, _) = to_redmod(
            "func F(name: CName, o: ref<A>) {\n  if let name as B = o {\n    name.Foo();\n  }\n  Log(name);\n}\n\nfunc G(o: ref<A>) {\n  let x = 1;\n  let x = 2;\n  if let x as B = o {\n    x.Foo();\n  }\n  Log(x);\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "function F(name_: CName, o: A) { if (let name__1 as B = o) { name__1.Foo(); } Log(name_); }"
            ),
            "{out}"
        );
        assert!(
            out.contains("let x = 1; let x_1 = 2; if (let x_2 as B = o) { x_2.Foo(); } Log(x_1);"),
            "{out}"
        );
    }

    #[test]
    fn all_reserved_words_are_renamed() {
        let (out, diags) = to_redmod(
            "func F(event: Int32, array: Int32) -> Int32 {\n  let hint = event + array;\n  let static = hint;\n  return static;\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "function F(event_: Int32, array_: Int32) -> Int32 { let hint_ = event_ + array_; let static_ = hint_; return static_; }"
            ),
            "{out}"
        );
        assert_eq!(codes(&diags), ["renamed-local"; 4]);
    }

    #[test]
    fn locals_shadowing_own_fields_are_renamed() {
        let (out, diags) = to_redmod(
            "class Base {\n  let player: Int32;\n}\n\nclass C extends Base {\n  let count: Int32;\n  let count_1: Int32;\n\n  func Set(count: Int32, player: Int32) {\n    this.count = count;\n    this.player = player;\n  }\n\n  func Get() -> Int32 {\n    let count = this.count;\n    return count + this.count;\n  }\n\n  static func Make(count: Int32) {}\n}\n\n@addField(Base)\nlet extra: Int32;\n\n@wrapMethod(Base)\nfunc W(extra: Int32) {}\n\nfunc Free(count: Int32) {}\n",
            run,
        );
        let out = squash(&out);
        assert!(
            out.contains(
                "function Set(count_2: Int32, player_1: Int32) { this.count = count_2; this.player = player_1; }"
            ),
            "{out}"
        );
        assert!(
            out.contains("let count_2 = this.count; return count_2 + this.count;"),
            "{out}"
        );
        assert!(
            out.contains("static function Make(count_2: Int32)"),
            "{out}"
        );
        assert!(out.contains("function W(extra_1: Int32)"), "{out}");
        assert!(out.contains("function Free(count: Int32)"), "{out}");
        assert_eq!(codes(&diags), ["renamed-local"; 5]);
        assert!(
            diags[0].message.contains("shadow a field of `C`"),
            "{}",
            diags[0]
        );
    }

    #[test]
    fn locals_shadowing_inherited_fields_are_renamed_with_types() {
        let src = "class C extends GameBase {\n  func F(player: Int32, other: Int32) -> Int32 {\n    let owner = player + other;\n    return owner + this.owner;\n  }\n}\n\n@wrapMethod(PlayerPuppet)\nfunc G(health: Int32) {}\n";
        let types = MapTypeQuery::new()
            .with_class_field("C", "player")
            .with_class_field("C", "owner")
            .with_class_field("C", "player_1")
            .with_class_field("PlayerPuppet", "health");
        let (out, diags) = to_redmod_with_types(src, &types, run);
        let out = squash(&out);
        assert!(
            out.contains(
                "function F(player_2: Int32, other: Int32) -> Int32 { let owner_1 = player_2 + other; return owner_1 + this.owner; }"
            ),
            "{out}"
        );
        assert!(out.contains("function G(health_1: Int32)"), "{out}");
        assert_eq!(codes(&diags), ["renamed-local"; 3]);

        // without types, inherited fields are unknown
        let (out, diags) = to_redmod(src, run);
        assert!(
            squash(&out).contains("function F(player: Int32, other: Int32)"),
            "{out}"
        );
        assert!(diags.is_empty(), "{diags:?}");
    }
}

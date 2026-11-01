//! Moving subexpressions into locals declared right before their statement, for the passes that
//! need a value held in a variable (REDmod calls array methods and reads struct fields only on
//! variables).
//!
//! An expression can only be moved when this does not change when it is evaluated: it must be
//! evaluated exactly once, where the statement starts (not in a loop condition, an `else if`
//! condition, the right-hand side of `&&`/`||` or a ternary branch), and nothing other than
//! constants may be evaluated earlier in the statement: the moved expression could change the
//! variables, fields or array elements read there, or depend on the side effects of calls made
//! there.

use redscript_ast::{
    BinOp, Condition, Expr, LetCondition, SourceExpr, SourceFunctionBody, SourceType, Stmt,
};

use crate::index::Names;
use crate::walk::{
    MutVisitor, SExpr, SParam, SStmt, expand_stmts, ident, let_stmt, synth, walk_body, walk_expr,
    walk_stmt,
};

/// A rewrite applied to every expression, after its subexpressions.
pub trait Rewrite<'src> {
    fn rewrite(&mut self, expr: &mut SExpr<'src>, hoist: &mut Hoist<'src>);
}

/// What a [`Rewrite`] needs to know to move an expression before the statement.
pub struct Hoist<'src> {
    names: Names<'src>,
    /// Locals to declare before the statement being visited.
    hoisted: Vec<SStmt<'src>>,
    /// Whether the expression being visited is evaluated exactly once, right where the
    /// statement starts.
    can_hoist: bool,
    /// Whether something other than a constant is evaluated before the expression being
    /// rewritten.
    effects_before: bool,
}

impl<'src> Hoist<'src> {
    /// Whether the expression being rewritten stays where it is evaluated.
    pub fn in_place(&self) -> bool {
        self.can_hoist
    }

    /// Whether moving the expression being rewritten would change the order in which it is
    /// evaluated relative to the reads and side effects before it.
    pub fn effects_before(&self) -> bool {
        self.effects_before
    }

    /// Declares a local of type `typ` named after `base` initialized with `value` before the
    /// statement, and returns a reference to it. The caller checks that the expression can move.
    pub fn hoist(&mut self, base: &str, typ: SourceType<'src>, value: SExpr<'src>) -> SExpr<'src> {
        let syn = synth(value.1);
        let local = self.names.fresh(base);
        self.hoisted
            .push(let_stmt(local, Some((typ, syn)), Some(value), syn));
        ident(local, syn)
    }

    /// Moves `expr` into a statement of its own before the statement, for its side effects. The
    /// caller checks that the expression can move.
    pub fn hoist_stmt(&mut self, expr: SExpr<'src>) {
        let syn = synth(expr.1);
        self.hoisted.push((Stmt::Expr(Box::new(expr)), syn));
    }
}

/// Whether evaluating `expr` reads or changes state, beyond what its subexpressions do: the
/// value of a variable, a field or an array element can change when a call is moved before it,
/// and a call, `new` or an assignment can change what is evaluated after it. Constants and the
/// operators combining other expressions do not.
fn reads_state(expr: &SourceExpr<'_>) -> bool {
    matches!(
        expr,
        Expr::Ident(_)
            | Expr::Member { .. }
            | Expr::Index { .. }
            | Expr::Call { .. }
            | Expr::New { .. }
            | Expr::Assign { .. }
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

/// Whether `expr` is a place whose location cannot depend on anything evaluated after it: a local
/// or a field of `this`.
fn is_fixed_place(expr: &SourceExpr<'_>) -> bool {
    match expr {
        Expr::Ident(_) => true,
        Expr::Member { expr, .. } => matches!(expr.0, Expr::This),
        _ => false,
    }
}

/// A visitor applying a [`Rewrite`] to every expression of a function body.
pub struct Hoisting<'src, R> {
    rewrite: R,
    hoist: Hoist<'src>,
    /// Whether something other than a constant was evaluated earlier in the statement.
    effects: bool,
}

impl<'src, R: Rewrite<'src>> Hoisting<'src, R> {
    pub fn new(rewrite: R, names: Names<'src>) -> Self {
        Self {
            rewrite,
            hoist: Hoist {
                names,
                hoisted: vec![],
                can_hoist: false,
                effects_before: false,
            },
            effects: false,
        }
    }

    fn with_hoisting(&mut self, can_hoist: bool, f: impl FnOnce(&mut Self)) {
        let saved = std::mem::replace(&mut self.hoist.can_hoist, can_hoist);
        f(self);
        self.hoist.can_hoist = saved;
    }
}

impl<'src, R: Rewrite<'src>> MutVisitor<'src> for Hoisting<'src, R> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        let saved = std::mem::take(&mut self.hoist.hoisted);
        expand_stmts(stmts, |mut stmt, out| {
            self.visit_stmt(&mut stmt);
            // the first hoisted local takes the span of the statement, so that comments above
            // the statement stay above the local
            if let Some(first) = self.hoist.hoisted.first_mut() {
                first.1 = stmt.1;
            }
            out.append(&mut self.hoist.hoisted);
            out.push(stmt);
        });
        self.hoist.hoisted = saved;
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        self.effects = false;
        match &mut stmt.0 {
            Stmt::Let {
                value: Some(expr), ..
            }
            | Stmt::Return(Some(expr))
            | Stmt::Expr(expr) => self.with_hoisting(true, |this| this.visit_expr(expr)),
            Stmt::If { blocks, else_ } => {
                for (i, block) in blocks.iter_mut().enumerate() {
                    match &mut block.condition {
                        LetCondition::Expr(cond) => {
                            self.with_hoisting(i == 0, |this| this.visit_expr(cond));
                        }
                        LetCondition::LetPattern(_, cond) => {
                            self.with_hoisting(false, |this| this.visit_expr(cond));
                        }
                    }
                    self.visit_stmts(&mut block.body.stmts);
                }
                if let Some(else_) = else_ {
                    self.visit_stmts(&mut else_.stmts);
                }
            }
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                self.with_hoisting(true, |this| this.visit_expr(expr));
                self.with_hoisting(false, |this| {
                    for case in cases.iter_mut() {
                        if let Condition::Expr(e) = &mut case.condition {
                            this.visit_expr(e);
                        }
                        this.visit_stmts(&mut case.body);
                    }
                    if let Some(default) = default {
                        this.visit_stmts(default);
                    }
                });
            }
            _ => self.with_hoisting(false, |this| walk_stmt(this, stmt)),
        }
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        let effects_before = self.effects;
        match &mut expr.0 {
            Expr::BinOp {
                lhs,
                op: BinOp::And | BinOp::Or,
                rhs,
            } => {
                self.visit_expr(lhs);
                self.with_hoisting(false, |this| this.visit_expr(rhs));
            }
            Expr::Conditional { cond, then, else_ } => {
                self.visit_expr(cond);
                self.with_hoisting(false, |this| {
                    this.visit_expr(then);
                    this.visit_expr(else_);
                });
            }
            // the callee is not a value: a function name is not read, only the receiver of a
            // method is
            Expr::Call {
                expr: callee, args, ..
            } => {
                match &mut callee.0 {
                    Expr::Ident(_) => {}
                    Expr::Member { expr: receiver, .. } => self.visit_expr(receiver),
                    _ => self.visit_expr(callee),
                }
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            // assigning to a local or a field of `this` does not read anything that a moved
            // expression could change
            Expr::Assign { lhs, rhs } => {
                let before = self.effects;
                self.visit_expr(lhs);
                if is_fixed_place(&lhs.0) {
                    self.effects = before;
                }
                self.visit_expr(rhs);
            }
            _ => walk_expr(self, expr),
        }
        self.hoist.effects_before = effects_before;
        self.rewrite.rewrite(expr, &mut self.hoist);
        if reads_state(&expr.0) {
            self.effects = true;
        }
    }

    fn visit_lambda(
        &mut self,
        _params: &mut Box<[SParam<'src>]>,
        body: &mut SourceFunctionBody<'src>,
    ) {
        // the body is not evaluated here, and walking its statements resets the flag
        let effects = self.effects;
        self.with_hoisting(false, |this| walk_body(this, body));
        self.effects = effects;
    }
}

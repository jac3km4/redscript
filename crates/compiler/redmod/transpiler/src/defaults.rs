//! Elide only first-execution defaults whose values are overwritten before use.

use std::collections::BTreeSet;

use redscript_community_ast::Span;
use redscript_compiler_frontend::ir::{self, Visitor};

#[derive(Default)]
struct Reads(BTreeSet<ir::Local>);

impl<'ctx> Visitor<'ctx> for Reads {
    fn visit_local(&mut self, local: ir::Local, _: redscript_community_ast::Span) {
        self.0.insert(local);
    }
}

#[derive(Default)]
struct Analysis {
    overwritten: BTreeSet<ir::Local>,
    omitted: BTreeSet<(ir::Local, Span)>,
}

/// `first` contains declarations with no earlier occurrence, outside branches and
/// loops. Restricting elision to these sites avoids extending an earlier value's
/// lifetime or changing a reset observed through an escaped script reference.
pub(crate) fn overwritten_defaults(
    block: &ir::Block<'_>,
    first: &BTreeSet<ir::Local>,
) -> BTreeSet<(ir::Local, Span)> {
    fn collect(
        block: &ir::Block<'_>,
        first: &mut BTreeSet<ir::Local>,
        sites: &mut BTreeSet<(ir::Local, Span)>,
    ) {
        for stmt in &block.stmts {
            match stmt {
                ir::Stmt::Block(block, _) => collect(block, first, sites),
                ir::Stmt::InitDefault { local, span, .. } if first.remove(local) => {
                    sites.insert((*local, *span));
                }
                ir::Stmt::Expr(expr) => {
                    if let ir::Expr::Assign { place, .. } = &**expr
                        && let ir::Expr::Local(local, _) = &**place
                    {
                        first.remove(local);
                    }
                }
                _ => {}
            }
        }
    }
    let mut sites = BTreeSet::new();
    collect(block, &mut first.clone(), &mut sites);
    if sites.is_empty() {
        return sites;
    }
    let mut analysis = Analysis::default();
    analysis.block(
        block,
        BTreeSet::new(),
        &BTreeSet::new(),
        &BTreeSet::new(),
        &sites,
    );
    analysis.omitted
}

impl Analysis {
    fn block(
        &mut self,
        block: &ir::Block<'_>,
        mut live: BTreeSet<ir::Local>,
        on_break: &BTreeSet<ir::Local>,
        on_continue: &BTreeSet<ir::Local>,
        first: &BTreeSet<(ir::Local, Span)>,
    ) -> BTreeSet<ir::Local> {
        let end = block
            .stmts
            .iter()
            .position(|stmt| {
                matches!(
                    stmt,
                    ir::Stmt::Return(..) | ir::Stmt::Break(_) | ir::Stmt::Continue(_)
                )
            })
            .map_or(block.stmts.len(), |index| index + 1);
        for stmt in block.stmts.iter().take(end).rev() {
            match stmt {
                ir::Stmt::Expr(expr) => {
                    if let ir::Expr::Assign { place, expr, .. } = &**expr
                        && let ir::Expr::Local(local, _) = &**place
                    {
                        live.remove(local);
                        self.overwritten.insert(*local);
                        live = Reads(live).with_expr(expr);
                    } else {
                        live = Reads(live).with_expr(expr);
                    }
                }
                ir::Stmt::Block(block, _) => {
                    live = self.block(block, live, on_break, on_continue, first);
                }
                ir::Stmt::Branches {
                    branches, default, ..
                } => {
                    let after = live;
                    live = default.as_ref().map_or_else(
                        || after.clone(),
                        |block| self.block(block, after.clone(), on_break, on_continue, first),
                    );
                    for branch in branches.iter().rev() {
                        live.extend(self.block(
                            &branch.block,
                            after.clone(),
                            on_break,
                            on_continue,
                            first,
                        ));
                        live = Reads(live).with_expr(&branch.condition);
                    }
                }
                ir::Stmt::While(branch, _) => {
                    let after = live;
                    let mut head = Reads(after.clone()).with_expr(&branch.condition);
                    loop {
                        let mut next =
                            self.block(&branch.block, head.clone(), &after, &head, first);
                        next.extend(after.iter().copied());
                        next = Reads(next).with_expr(&branch.condition);
                        if next == head {
                            break;
                        }
                        head = next;
                    }
                    live = head;
                }
                ir::Stmt::Switch { .. } => {
                    // Conservatively keep every occurrence: fallthrough and the
                    // switch's distinct break target need no special CFG here.
                    let mut reads = Reads(live);
                    reads.visit_stmt(stmt);
                    reads.0.extend(on_continue.iter().copied());
                    live = reads.0;
                }
                ir::Stmt::InitDefault { local, span, .. } => {
                    if first.contains(&(*local, *span))
                        && !live.contains(local)
                        && self.overwritten.contains(local)
                    {
                        self.omitted.insert((*local, *span));
                    }
                    live.remove(local);
                }
                ir::Stmt::InitArray {
                    local, elements, ..
                } => {
                    // InitArray resizes before evaluating elements; preserve any
                    // references to its storage rather than treating it as a kill.
                    let mut reads = Reads(live);
                    for element in elements {
                        reads.visit_expr(element);
                    }
                    reads.0.insert(*local);
                    live = reads.0;
                }
                ir::Stmt::Return(expr, _) => {
                    live = expr
                        .as_ref()
                        .map_or_else(BTreeSet::new, |expr| Reads::default().with_expr(expr));
                }
                ir::Stmt::Break(_) => live = on_break.clone(),
                ir::Stmt::Continue(_) => live = on_continue.clone(),
            }
        }
        live
    }
}

impl Reads {
    fn with_expr(mut self, expr: &ir::Expr<'_>) -> BTreeSet<ir::Local> {
        self.visit_expr(expr);
        self.0
    }
}

//! Diagnose changed array evaluation while retaining legacy lowering for runtime compatibility.
use std::collections::HashMap;

use redscript_community_ast::Span;

use super::{Error, LowerReporter};
use crate::{Param, QualifiedName, Symbols, ir};

pub(super) fn check_block<'ctx>(
    block: &ir::Block<'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
) {
    let mut pending = HashMap::new();
    check_block_with(block, &mut pending, symbols, reporter);
}

struct ArrayEntry {
    info: ExprInfo,
    span: Span,
    reported: bool,
}

#[derive(Clone, Copy, Default)]
pub struct ArrayPrefixInfo {
    pub has_array: bool,
    pub array_effects: bool,
    pub array_reads_state: bool,
    pub has_side_effects: bool,
    pub reads_state: bool,
}

impl ArrayPrefixInfo {
    pub fn combine_prefixes(self, other: Self) -> Self {
        Self {
            has_array: self.has_array || other.has_array,
            array_effects: self.array_effects || other.array_effects,
            array_reads_state: self.array_reads_state || other.array_reads_state,
            has_side_effects: self.has_side_effects || other.has_side_effects,
            reads_state: self.reads_state || other.reads_state,
        }
    }

    pub fn is_reordering_unsafe(before: Self, after: Self) -> bool {
        if !after.has_array {
            return false;
        }
        (before.has_side_effects && (after.array_reads_state || after.array_effects))
            || (after.array_effects && (before.reads_state || before.has_side_effects))
    }
}

type ExprInfo = ArrayPrefixInfo;

fn is_reordering_unsafe(before: ExprInfo, after: ExprInfo) -> bool {
    ArrayPrefixInfo::is_reordering_unsafe(before, after)
}

fn check_block_with<'ctx>(
    block: &ir::Block<'ctx>,
    pending: &mut HashMap<ir::Local, ArrayEntry>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
) {
    for stmt in &block.stmts {
        match stmt {
            ir::Stmt::InitArray {
                local,
                source_binding: None,
                elements,
                span,
                ..
            } => {
                let info = siblings(elements.iter(), pending, symbols, reporter, false);
                pending.insert(
                    *local,
                    ArrayEntry {
                        info,
                        span: *span,
                        reported: false,
                    },
                );
            }
            ir::Stmt::InitArray {
                source_binding: Some(_),
                elements,
                ..
            } => {
                siblings(elements.iter(), pending, symbols, reporter, false);
            }
            ir::Stmt::Block(inner, _) => {
                check_block_with(inner, pending, symbols, reporter);
            }
            ir::Stmt::Expr(expr) | ir::Stmt::Return(Some(expr), _) => {
                visit(expr, pending, symbols, reporter, false);
            }
            ir::Stmt::While(cb, _) => {
                visit(&cb.condition, pending, symbols, reporter, false);
                check_block(&cb.block, symbols, reporter);
            }
            ir::Stmt::If {
                branches, default, ..
            } => {
                for (index, branch) in branches.iter().enumerate() {
                    visit(&branch.condition, pending, symbols, reporter, index > 0);
                }
                for branch in branches {
                    check_block(&branch.block, symbols, reporter);
                }
                if let Some(default) = default {
                    check_block(default, symbols, reporter);
                }
            }
            ir::Stmt::Switch {
                scrutinee,
                branches,
                default,
                ..
            } => {
                visit(scrutinee, pending, symbols, reporter, false);
                for branch in branches {
                    visit(&branch.matcher, pending, symbols, reporter, true);
                }
                for branch in branches {
                    check_block(&branch.block, symbols, reporter);
                }
                if let Some(default) = default {
                    check_block(default, symbols, reporter);
                }
            }
            ir::Stmt::InitDefault { .. }
            | ir::Stmt::Break(_)
            | ir::Stmt::Continue(_)
            | ir::Stmt::Return(None, _) => {}
        }
    }
}

fn is_pure_intrinsic(intrinsic: ir::Intrinsic) -> bool {
    !matches!(
        intrinsic,
        ir::Intrinsic::ArrayClear
            | ir::Intrinsic::ArrayResize
            | ir::Intrinsic::ArrayPush
            | ir::Intrinsic::ArrayPop
            | ir::Intrinsic::ArrayInsert
            | ir::Intrinsic::ArrayRemove
            | ir::Intrinsic::ArrayGrow
            | ir::Intrinsic::ArrayErase
            | ir::Intrinsic::ArraySort
            | ir::Intrinsic::ArraySortByPredicate
    )
}

/// Operators are lowered to ordinary native calls, but their operands are
/// already evaluated before the call runs, so arithmetic, comparison, logical,
/// and unary operators cannot change observable evaluation order. Compound
/// assignments mutate their first operand, and `OperatorArray` reads a place,
/// so those keep the conservative treatment.
fn is_pure_free_function(func: &crate::FreeFunction<'_>, name: &QualifiedName<'_>) -> bool {
    if func.intrinsic().is_some_and(is_pure_intrinsic) {
        return true;
    }
    if !func.flags().is_operator() {
        return false;
    }
    let Some(name) = name.as_ref().last().copied() else {
        return false;
    };
    !name.starts_with("OperatorAssign") && name != "OperatorArray"
}

fn report(reporter: &mut LowerReporter<'_>, span: Span) {
    reporter.report(Error::UnsafeArrayEvaluation(span));
}

fn siblings<'a, 'ctx: 'a>(
    exprs: impl IntoIterator<Item = &'a ir::Expr<'ctx>>,
    pending: &mut HashMap<ir::Local, ArrayEntry>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
    lazy: bool,
) -> ExprInfo {
    let mut earlier = ExprInfo::default();
    for expr in exprs {
        let info = visit(expr, pending, symbols, reporter, lazy);
        if is_reordering_unsafe(earlier, info) {
            report(reporter, expr.span());
        }
        earlier = earlier.combine_prefixes(info);
    }
    earlier
}

fn call_args<'ctx>(
    args: &[ir::Expr<'ctx>],
    params: &[Param<'ctx>],
    pending: &mut HashMap<ir::Local, ArrayEntry>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
    lazy: bool,
) -> ExprInfo {
    let mut earlier = ExprInfo::default();
    for (index, arg) in args.iter().enumerate() {
        let is_skip = params
            .get(index)
            .is_some_and(|p| p.flags().is_short_circuit());
        let info = visit(arg, pending, symbols, reporter, lazy || is_skip);
        if is_reordering_unsafe(earlier, info) {
            report(reporter, arg.span());
        }
        earlier = earlier.combine_prefixes(info);
    }
    earlier
}

fn pair<'ctx>(
    before: &ir::Expr<'ctx>,
    after: &ir::Expr<'ctx>,
    pending: &mut HashMap<ir::Local, ArrayEntry>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
    lazy: bool,
) -> ExprInfo {
    let first = visit(before, pending, symbols, reporter, lazy);
    let second = visit(after, pending, symbols, reporter, lazy);
    if is_reordering_unsafe(first, second) {
        report(reporter, after.span());
    }
    first.combine_prefixes(second)
}

fn visit<'ctx>(
    expr: &ir::Expr<'ctx>,
    pending: &mut HashMap<ir::Local, ArrayEntry>,
    symbols: &Symbols<'ctx>,
    reporter: &mut LowerReporter<'ctx>,
    lazy: bool,
) -> ExprInfo {
    match expr {
        ir::Expr::Local(local, _) => {
            if let Some(entry) = pending.get_mut(local) {
                if lazy && entry.info.has_side_effects && !entry.reported {
                    entry.reported = true;
                    let span = entry.span;
                    report(reporter, span);
                }
                ExprInfo {
                    has_array: true,
                    array_effects: entry.info.has_side_effects,
                    array_reads_state: entry.info.reads_state,
                    has_side_effects: entry.info.has_side_effects,
                    reads_state: entry.info.reads_state,
                }
            } else {
                ExprInfo {
                    reads_state: true,
                    ..ExprInfo::default()
                }
            }
        }
        ir::Expr::Capture(_, _) => ExprInfo {
            reads_state: true,
            ..ExprInfo::default()
        },
        ir::Expr::Construct { args, .. } => siblings(args.iter(), pending, symbols, reporter, lazy),
        ir::Expr::Call { call, .. } => {
            let (callee_info, args_info, callee_span, is_pure) = match &**call {
                ir::Call::FreeFunction { function, args, .. } => {
                    let func = &symbols[*function];
                    let params = func.type_().params();
                    let args_info = call_args(args, params, pending, symbols, reporter, lazy);
                    let is_pure = symbols
                        .get_free_function(*function)
                        .is_some_and(|(name, _)| is_pure_free_function(func, name));
                    (None, args_info, None, is_pure)
                }
                ir::Call::Static { method, args, .. } => {
                    let params = symbols[*method].type_().params();
                    let args_info = call_args(args, params, pending, symbols, reporter, lazy);
                    (None, args_info, None, false)
                }
                ir::Call::Instance {
                    receiver,
                    method,
                    args,
                    ..
                } => {
                    let recv = visit(receiver, pending, symbols, reporter, lazy);
                    let params = symbols[*method].type_().params();
                    let args_info = call_args(args, params, pending, symbols, reporter, lazy);
                    (Some(recv), args_info, Some(receiver.span()), false)
                }
                ir::Call::Closure { closure, args, .. } => {
                    let recv = visit(closure, pending, symbols, reporter, lazy);
                    let args_info = siblings(args.iter(), pending, symbols, reporter, lazy);
                    (Some(recv), args_info, Some(closure.span()), false)
                }
            };
            if let (Some(callee), Some(span)) = (callee_info, callee_span)
                && is_reordering_unsafe(callee, args_info)
            {
                report(reporter, span);
            }
            let combined =
                callee_info.map_or(args_info, |callee| callee.combine_prefixes(args_info));
            ExprInfo {
                has_array: combined.has_array,
                array_effects: combined.array_effects,
                array_reads_state: combined.array_reads_state,
                has_side_effects: !is_pure || combined.has_side_effects,
                reads_state: !is_pure || combined.reads_state,
            }
        }
        ir::Expr::Assign { place, expr, .. } => {
            let first = visit(place, pending, symbols, reporter, lazy);
            let second = visit(expr, pending, symbols, reporter, lazy);
            let mut info = first.combine_prefixes(second);
            info.has_side_effects = true;
            info.reads_state = true;
            info
        }
        ir::Expr::Field { receiver, .. } => {
            let mut info = visit(receiver, pending, symbols, reporter, lazy);
            info.reads_state = true;
            info
        }
        ir::Expr::Index { array, index, .. } => {
            let mut info = pair(array, index, pending, symbols, reporter, lazy);
            info.reads_state = true;
            info
        }
        ir::Expr::Conditional {
            condition,
            then,
            else_,
            ..
        } => {
            let cond_info = visit(condition, pending, symbols, reporter, lazy);
            let then_info = visit(then, pending, symbols, reporter, true);
            let else_info = visit(else_, pending, symbols, reporter, true);
            cond_info
                .combine_prefixes(then_info)
                .combine_prefixes(else_info)
        }
        ir::Expr::DynCast { expr, .. } => visit(expr, pending, symbols, reporter, lazy),
        ir::Expr::NewClosure { closure, .. } => {
            check_block(&closure.block, symbols, reporter);
            ExprInfo::default()
        }
        ir::Expr::NewInstance { .. }
        | ir::Expr::Nop(_)
        | ir::Expr::Const(_, _)
        | ir::Expr::Null { .. } => ExprInfo::default(),
    }
}

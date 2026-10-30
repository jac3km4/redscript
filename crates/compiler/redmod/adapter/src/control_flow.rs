//! REDmod-only typed control-flow fixups. Headers are lowered exactly once.
//!
//! A guarded header at the loop head runs on every back edge, including continue
//! through a switch. The first-entry flag is reset at the loop's execution site,
//! not in the function prologue (important for loops nested in other loops).
use std::collections::HashMap;

use redscript_community_ast::Span;
use redscript_compiler_frontend::{LoweredCompilationUnit, LoweredFunction, PolyType, ir, predef};

use crate::{CapabilityError, Error};

#[derive(Debug, Clone, Copy)]
pub(crate) enum LoopKind {
    For,
    DoWhile,
}

#[derive(Debug, Clone)]
pub(crate) struct LoopPlan {
    pub span: Span,
    pub kind: LoopKind,
}

/// Generated expressions have no written source identity. Structural nodes keep
/// their source envelope so queries can still reach their written children.
pub(crate) fn generated_span(source: Span) -> Span {
    Span::new(source.start, source.start, source.file)
}

pub(crate) fn apply(
    unit: &mut LoweredCompilationUnit<'_>,
    plans: &[LoopPlan],
    errors: &mut Vec<Error>,
) {
    let mut indexed = HashMap::with_capacity(plans.len());
    for plan in plans {
        if indexed.insert(plan.span, plan.kind).is_some() {
            provenance_error(plan.span, errors);
        }
    }
    for function in unit
        .classes
        .values_mut()
        .flat_map(|class| class.methods.values_mut())
        .chain(unit.functions.values_mut())
        .chain(unit.added_methods.values_mut().flatten())
        .chain(unit.method_replacements.values_mut())
        .chain(unit.method_wrappers.values_mut().flatten())
    {
        fix_function(function, &indexed, errors);
    }
}

fn provenance_error(span: Span, errors: &mut Vec<Error>) {
    errors.push(Error::Capability {
        kind: CapabilityError::ControlFlowProvenance,
        span,
    });
}

fn fix_function(
    function: &mut LoweredFunction<'_>,
    plans: &HashMap<Span, LoopKind>,
    errors: &mut Vec<Error>,
) {
    let mut locals = function.locals.to_vec();
    let mut next_local = locals
        .iter()
        .filter_map(|info| match info.id {
            ir::Local::Var(id) => Some(u32::from(id) + 1),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    fix_block(
        &mut function.block,
        &mut locals,
        &mut next_local,
        plans,
        errors,
    );
    function.locals = locals.into();
}

fn assign<'ctx>(local: ir::Local, value: bool, source: Span) -> ir::Stmt<'ctx> {
    let span = generated_span(source);
    ir::Expr::Assign {
        place: Box::new(ir::Expr::Local(local, span)),
        expr: Box::new(ir::Expr::Const(ir::Const::Bool(value), span)),
        span,
    }
    .into()
}

fn header_matches(stmt: &ir::Stmt<'_>, source: Span) -> bool {
    matches!(stmt, ir::Stmt::Branches { branches, default: None, span }
        if *span == source && matches!(&**branches, [header]
            if matches!(&*header.condition, ir::Expr::Const(ir::Const::Bool(true), span)
                if *span == generated_span(source))))
}

fn fix_block<'ctx>(
    block: &mut ir::Block<'ctx>,
    locals: &mut Vec<ir::LocalInfo<'ctx>>,
    next_local: &mut u32,
    plans: &HashMap<Span, LoopKind>,
    errors: &mut Vec<Error>,
) {
    let mut result = std::collections::VecDeque::new();
    for mut stmt in std::mem::take(&mut block.stmts) {
        match &mut stmt {
            ir::Stmt::While(while_, span) => {
                fix_block(&mut while_.block, locals, next_local, plans, errors);
                if let Some(kind) = plans.get(span) {
                    let header = match kind {
                        LoopKind::DoWhile => while_.block.stmts.back_mut(),
                        LoopKind::For => while_.block.stmts.front_mut(),
                    };
                    let valid_condition = matches!(&*while_.condition,
                        ir::Expr::Const(ir::Const::Bool(true), condition_span)
                            if *condition_span == generated_span(*span));
                    if !valid_condition
                        || !header
                            .as_deref()
                            .is_some_and(|header| header_matches(header, *span))
                    {
                        provenance_error(*span, errors);
                        result.push_back(stmt);
                        continue;
                    }
                    // Local::to_index reserves index zero, so u16::MAX is not usable.
                    if *next_local >= u32::from(u16::MAX) {
                        errors.push(Error::Capability {
                            kind: CapabilityError::ControlFlowLocalLimit,
                            span: *span,
                        });
                        result.push_back(stmt);
                        continue;
                    }
                    // Validation above leaves the original header intact on failure.
                    if let Some(ir::Stmt::Branches { branches, .. }) = header {
                        let local = ir::Local::Var(*next_local as u16);
                        *next_local += 1;
                        locals.push(ir::LocalInfo::generated(
                            local,
                            ir::TemporaryPurpose::LoopEntered,
                            PolyType::nullary(predef::BOOL),
                            Some(generated_span(*span)),
                        ));
                        *branches[0].condition = ir::Expr::Local(local, generated_span(*span));
                        if matches!(kind, LoopKind::DoWhile)
                            && let Some(header) = while_.block.stmts.pop_back()
                        {
                            while_.block.stmts.push_front(header);
                        }
                        // Mark first entry before executing the source body, but after
                        // testing the previous iteration's guarded header.
                        while_.block.stmts.insert(1, assign(local, true, *span));
                        result.push_back(assign(local, false, *span));
                    }
                }
            }
            ir::Stmt::Block(block, _) => fix_block(block, locals, next_local, plans, errors),
            ir::Stmt::Branches {
                branches, default, ..
            } => {
                for branch in branches {
                    fix_block(&mut branch.block, locals, next_local, plans, errors);
                }
                if let Some(block) = default {
                    fix_block(block, locals, next_local, plans, errors);
                }
            }
            ir::Stmt::Switch {
                branches, default, ..
            } => {
                for branch in branches {
                    fix_block(&mut branch.block, locals, next_local, plans, errors);
                }
                if let Some(block) = default {
                    fix_block(block, locals, next_local, plans, errors);
                }
            }
            _ => {}
        }
        result.push_back(stmt);
    }
    block.stmts = result;
}

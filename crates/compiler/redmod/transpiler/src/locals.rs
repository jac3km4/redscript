//! Conservative declaration placement without moving initializer execution.

use std::collections::BTreeSet;

use redscript_compiler_frontend::{Symbols, Type, ir, predef};

use crate::{Result, error};

#[derive(Default)]
struct Occurrences(BTreeSet<ir::Local>);

impl<'ctx> ir::Visitor<'ctx> for Occurrences {
    fn visit_local(&mut self, local: ir::Local, _: redscript_community_ast::Span) {
        self.0.insert(local);
    }
}

/// Only unconditional statements (including flattened plain blocks) qualify.
/// Every earlier occurrence counts, even in a branch, receiver, or out argument.
/// A local is selected at most once; emission consumes it at its first initializer.
pub(crate) fn inline_declarations<'ctx>(
    block: &ir::Block<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<BTreeSet<ir::Local>> {
    fn collect<'ctx>(
        block: &ir::Block<'ctx>,
        symbols: &Symbols<'ctx>,
        seen: &mut Occurrences,
        selected: &mut BTreeSet<ir::Local>,
    ) -> Result<()> {
        use ir::Visitor;

        for stmt in &block.stmts {
            if let ir::Stmt::Block(block, _) = stmt {
                collect(block, symbols, seen, selected)?;
                continue;
            }
            match stmt {
                ir::Stmt::Expr(expr) => {
                    if let ir::Expr::Assign { place, expr, .. } = &**expr
                        && let ir::Expr::Local(local @ ir::Local::Var(_), _) = &**place
                    {
                        // Read the RHS first to exclude same-identity self-reference.
                        seen.visit_expr(expr);
                        if !seen.0.contains(local) {
                            selected.insert(*local);
                        }
                    }
                }
                ir::Stmt::InitDefault {
                    local: local @ ir::Local::Var(_),
                    typ,
                    span,
                } if !seen.0.contains(local) => {
                    let typ = typ
                        .coalesced(symbols)
                        .map_err(|e| error(e.to_string(), Some(*span)))?;
                    if !matches!(typ, Type::Data(app) if app.id() == predef::ARRAY) {
                        selected.insert(*local);
                    }
                }
                _ => {}
            }
            seen.visit_stmt(stmt);
        }
        Ok(())
    }

    let mut selected = BTreeSet::new();
    collect(block, symbols, &mut Occurrences::default(), &mut selected)?;
    Ok(selected)
}

#[cfg(test)]
mod tests {
    use redscript_community_ast::{SourceMap, Span};
    use redscript_compiler_frontend::{
        FreeFunction, FunctionType, LoweredFunction, Param, ParamFlags, PolyType, TypeApp,
    };

    use super::*;

    fn assign<'ctx>(local: ir::Local, value: ir::Expr<'ctx>, span: Span) -> ir::Stmt<'ctx> {
        ir::Expr::Assign {
            place: ir::Expr::Local(local, span).into(),
            expr: value.into(),
            span,
        }
        .into()
    }

    #[test]
    fn prior_occurrences_and_self_reference_prevent_fusion() {
        let sources = SourceMap::new();
        let span = Span::new(0, 1, sources.push_back("test.reds", "x"));
        let symbols = Symbols::with_default_types();
        let local = ir::Local::Var(0);
        let read = || ir::Expr::Local(local, span);
        let value = || ir::Expr::Const(ir::Const::I32(1), span);
        for block in [
            ir::Block::new([assign(local, read(), span)]),
            ir::Block::new([read().into(), assign(local, value(), span)]),
            ir::Block::new([
                ir::Stmt::Branches {
                    branches: [ir::ConditionalBlock::new(
                        ir::Expr::Const(ir::Const::Bool(true), span),
                        ir::Block::new([assign(local, value(), span)]),
                    )]
                    .into(),
                    default: None,
                    span,
                },
                assign(local, value(), span),
            ]),
            ir::Block::new([
                ir::Stmt::While(
                    ir::ConditionalBlock::new(
                        ir::Expr::Const(ir::Const::Bool(true), span),
                        ir::Block::new([assign(local, value(), span)]),
                    ),
                    span,
                ),
                assign(local, value(), span),
            ]),
        ] {
            assert!(inline_declarations(&block, &symbols).unwrap().is_empty());
        }
    }

    #[test]
    fn receivers_out_arguments_indexed_places_and_nested_rhs_prevent_fusion() {
        let sources = SourceMap::new();
        let span = Span::new(0, 1, sources.push_back("test.reds", "x"));
        let mut symbols = Symbols::with_default_types();
        let function = symbols.add_free_function(
            "Write",
            FreeFunction::new(
                redscript_compiler_frontend::FreeFunctionFlags::default(),
                FunctionType::default().with_params([Param::new(
                    "value",
                    ParamFlags::default().with_is_out(true),
                    Type::nullary(predef::INT32),
                    None,
                )]),
                [],
                None,
            ),
        );
        let local = ir::Local::Var(0);
        let read = || ir::Expr::Local(local, span);
        let value = || ir::Expr::Const(ir::Const::I32(1), span);
        let indexed_write = |array, index| ir::Expr::Assign {
            place: ir::Expr::Index {
                array: ir::Expr::Local(array, span).into(),
                array_type: PolyType::from_type(&Type::app(
                    predef::ARRAY,
                    [Type::nullary(predef::INT32)],
                ))
                .into(),
                index: ir::Expr::Local(index, span).into(),
                span,
            }
            .into(),
            expr: value().into(),
            span,
        };
        for (name, prior) in [
            (
                "call receiver",
                ir::Expr::call(
                    ir::Call::Closure {
                        closure: read().into(),
                        closure_type: TypeApp::new(
                            predef::FUNCTION0,
                            [PolyType::nullary(predef::VOID)],
                        )
                        .into(),
                        args: Box::new([]),
                    },
                    span,
                ),
            ),
            (
                "out argument",
                ir::Expr::call(
                    ir::Call::FreeFunction {
                        function,
                        type_args: Box::new([]),
                        args: [read()].into(),
                    },
                    span,
                ),
            ),
            (
                "indexed receiver",
                indexed_write(local, ir::Local::Param(0)),
            ),
            (
                "indexed subscript",
                indexed_write(ir::Local::Param(0), local),
            ),
        ] {
            let block = ir::Block::new([prior.into(), assign(local, value(), span)]);
            assert!(
                inline_declarations(&block, &symbols).unwrap().is_empty(),
                "{name}"
            );
        }

        let rhs = ir::Expr::Conditional {
            condition: ir::Expr::Const(ir::Const::Bool(true), span).into(),
            then: value().into(),
            else_: ir::Expr::Index {
                array: ir::Expr::Local(ir::Local::Param(0), span).into(),
                array_type: PolyType::from_type(&Type::app(
                    predef::ARRAY,
                    [Type::nullary(predef::INT32)],
                ))
                .into(),
                index: read().into(),
                span,
            }
            .into(),
            span,
        };
        let block = ir::Block::new([assign(local, rhs, span), assign(local, value(), span)]);
        assert!(inline_declarations(&block, &symbols).unwrap().is_empty());
    }

    #[test]
    fn conditions_and_switch_occurrences_prevent_later_fusion() {
        let sources = SourceMap::new();
        let span = Span::new(0, 1, sources.push_back("test.reds", "x"));
        let symbols = Symbols::with_default_types();
        let local = ir::Local::Var(0);
        let read = || ir::Expr::Local(local, span);
        let value = || ir::Expr::Const(ir::Const::I32(1), span);
        for occurrence in 0..6 {
            let prior = match occurrence {
                0 => ir::Stmt::Branches {
                    branches: [ir::ConditionalBlock::new(read(), ir::Block::default())].into(),
                    default: None,
                    span,
                },
                1 => ir::Stmt::While(
                    ir::ConditionalBlock::new(read(), ir::Block::default()),
                    span,
                ),
                _ => ir::Stmt::Switch {
                    scrutinee: if occurrence == 2 { read() } else { value() }.into(),
                    scrutinee_type: PolyType::nullary(predef::INT32).into(),
                    branches: [ir::Case::new(
                        if occurrence == 3 { read() } else { value() },
                        if occurrence == 4 {
                            ir::Block::new([assign(local, value(), span)])
                        } else {
                            ir::Block::default()
                        },
                    )]
                    .into(),
                    default: (occurrence == 5)
                        .then(|| ir::Block::new([assign(local, value(), span)])),
                    span,
                },
            };
            let block = ir::Block::new([prior, assign(local, value(), span)]);
            assert!(
                inline_declarations(&block, &symbols).unwrap().is_empty(),
                "occurrence {occurrence}"
            );
        }
    }

    #[test]
    fn nested_plain_blocks_share_order_and_select_each_local_once() {
        let sources = SourceMap::new();
        let span = Span::new(0, 1, sources.push_back("test.reds", "x"));
        let symbols = Symbols::with_default_types();
        let before = ir::Local::Var(0);
        let inside = ir::Local::Var(1);
        let fresh = ir::Local::Var(2);
        let value = || ir::Expr::Const(ir::Const::I32(1), span);
        let block = ir::Block::new([
            ir::Expr::Local(before, span).into(),
            ir::Stmt::Block(
                ir::Block::new([
                    assign(before, value(), span),
                    ir::Stmt::Block(
                        ir::Block::new([
                            ir::Expr::Local(inside, span).into(),
                            assign(fresh, value(), span),
                        ]),
                        span,
                    ),
                    assign(fresh, value(), span),
                ]),
                span,
            ),
            assign(inside, value(), span),
            assign(fresh, value(), span),
        ]);
        assert_eq!(
            inline_declarations(&block, &symbols).unwrap(),
            BTreeSet::from([fresh])
        );
        let function = LoweredFunction {
            block,
            locals: [(before, "before"), (inside, "inside"), (fresh, "fresh")]
                .map(|(id, name)| {
                    ir::LocalInfo::new(id, Some(name), PolyType::nullary(predef::INT32), Some(span))
                })
                .into(),
            span,
        };
        let signature = FunctionType::default();
        let (text, _) = crate::body::Body::new(&function, &symbols, &signature, false, None)
            .unwrap()
            .emit()
            .unwrap();
        assert!(text.contains("var before: Int32;"), "{text}");
        assert!(text.contains("var inside: Int32;"), "{text}");
        assert!(text.contains("var fresh: Int32 = 1;"), "{text}");
        assert_eq!(text.matches("var fresh:").count(), 1, "{text}");
        assert_eq!(
            text.lines()
                .filter(|line| line.trim() == "fresh = 1;")
                .count(),
            2,
            "{text}"
        );
    }

    #[test]
    fn arrays_retain_init_operations_and_parameters_are_not_declared() {
        let sources = SourceMap::new();
        let span = Span::new(0, 1, sources.push_back("test.reds", "x"));
        let symbols = Symbols::with_default_types();
        let array = ir::Local::Var(0);
        let empty = ir::Local::Var(1);
        let scalar = ir::Local::Var(2);
        let block = ir::Block::new([
            ir::Stmt::InitArray {
                local: array,
                elements: Box::new([]),
                element_type: PolyType::nullary(predef::INT32).into(),
                span,
            },
            ir::Stmt::InitDefault {
                local: empty,
                typ: PolyType::from_type(&Type::app(predef::ARRAY, [Type::nullary(predef::INT32)]))
                    .into(),
                span,
            },
            assign(array, ir::Expr::Local(empty, span), span),
            assign(
                ir::Local::Param(0),
                ir::Expr::Const(ir::Const::I32(1), span),
                span,
            ),
            ir::Stmt::InitDefault {
                local: scalar,
                typ: PolyType::nullary(predef::INT32).into(),
                span,
            },
            assign(scalar, ir::Expr::Const(ir::Const::I32(2), span), span),
        ]);
        assert_eq!(
            inline_declarations(&block, &symbols).unwrap(),
            BTreeSet::from([scalar])
        );
    }
}

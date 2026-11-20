//! Statement, expression, and loop-plan lowering for the REDmod bridge.
use redscript_community_ast::{self as ast, Span, Spanned};
use redscript_redmod_ast as native;

use super::{Bridge, CapabilityError, HeaderCondition, Result};

impl<'src> Bridge<'src> {
    pub(super) fn block(&mut self, block: native::Block<'src>) -> Result<ast::SourceBlock<'src>> {
        let mut stmts = Vec::new();
        for (stmt, span) in block {
            let stmt = match stmt {
                native::Statement::Empty => continue,
                // REDmod has function-scope locals, not lexical block bindings.
                // Flattening preserves execution order and break/continue targets.
                native::Statement::Block(block) => {
                    stmts.extend(self.block(block)?.stmts);
                    continue;
                }
                native::Statement::Local(local) => {
                    if local.names.len() > 1 && local.value.is_some() {
                        return Err((CapabilityError::InitializedLocalGroup, span));
                    }
                    let ty = typ(local.typ);
                    let value = local
                        .value
                        .map(|expr| self.expr(expr).map(Box::new))
                        .transpose()?;
                    let grouped = local.names.len() > 1;
                    for name in local.names {
                        let own_span = if grouped { name.1 } else { span };
                        stmts.push((
                            ast::Stmt::Let {
                                is_const: local.const_.is_some(),
                                name,
                                typ: Some(Box::new(ty.clone())),
                                value: value.clone(),
                            },
                            own_span,
                        ));
                    }
                    continue;
                }
                native::Statement::Expr(expr) => {
                    stmts.extend(self.discard_expr(*expr)?);
                    continue;
                }
                native::Statement::Return(expr) => ast::Stmt::Return(
                    expr.map(|expr| self.expr(*expr).map(Box::new))
                        .transpose()?,
                ),
                native::Statement::Break => ast::Stmt::Break,
                native::Statement::Continue => ast::Stmt::Continue,
                native::Statement::If { blocks, else_ }
                    if blocks.iter().all(|(condition, _)| {
                        !matches!(condition.0, native::Expr::Sequence(_))
                    }) =>
                {
                    ast::Stmt::If {
                        blocks: blocks
                            .into_iter()
                            .map(|(condition, body)| self.conditional(condition, body))
                            .collect::<Result<Vec<_>>>()?
                            .into(),
                        else_: else_.map(|body| self.block(body)).transpose()?,
                    }
                }
                native::Statement::If { blocks, else_ } => {
                    stmts.extend(self.if_blocks(blocks, else_, span)?.stmts);
                    continue;
                }
                native::Statement::While { condition, body }
                    if !matches!(condition.0, native::Expr::Sequence(_)) =>
                {
                    ast::Stmt::While(Box::new(self.conditional(condition, body)?))
                }
                native::Statement::While { condition, body } => {
                    let body = self.block(body)?;
                    let checked = self.condition_check(condition, body, span)?;
                    Self::forever(checked, span)
                }
                native::Statement::For {
                    init,
                    condition,
                    step,
                    body,
                } => {
                    if let Some(init) = init {
                        let (init, init_span) = *init;
                        if let native::Statement::Expr(expr) = init {
                            stmts.extend(self.discard_expr(*expr)?);
                        } else {
                            // Declaration initializers retain ordinary value semantics.
                            stmts.extend(self.block(vec![(init, init_span)])?.stmts);
                        }
                    }
                    let mut step_body = Vec::new();
                    if let Some(step) = step {
                        step_body = self.discard_expr(step)?;
                    }
                    let body = self.block(body)?;
                    let checked = if let Some(condition) = condition {
                        self.condition_check(condition, body, span)?
                    } else {
                        body
                    };
                    let mut loop_body = vec![Self::marker(ast::Block::new(step_body), span)];
                    loop_body.extend(checked.stmts);
                    self.control_flow.push(crate::control_flow::LoopPlan {
                        span,
                        kind: crate::control_flow::LoopKind::For,
                    });
                    Self::forever(ast::Block::new(loop_body), span)
                }
                native::Statement::DoWhile { body, condition } => {
                    let mut body = self.block(body)?.stmts.into_vec();
                    let checked = self.condition_check(condition, ast::Block::new(vec![]), span)?;
                    body.push(Self::marker(checked, span));
                    self.control_flow.push(crate::control_flow::LoopPlan {
                        span,
                        kind: crate::control_flow::LoopKind::DoWhile,
                    });
                    Self::forever(ast::Block::new(body), span)
                }
                native::Statement::Switch { expr, labels } => {
                    let mut cases = Vec::new();
                    let mut default = None;
                    let mut default_index = 0;
                    for (label, span) in labels {
                        let body = self.block(label.body)?.stmts;
                        if let Some(value) = label.value {
                            cases.push(ast::Case::new(
                                ast::Condition::Expr(self.expr(value)?),
                                body,
                            ));
                        } else {
                            if default.is_some() {
                                return Err((CapabilityError::SwitchLabelOrder, span));
                            }
                            default_index = cases.len();
                            default = Some(body);
                        }
                    }
                    ast::Stmt::Switch {
                        expr: Box::new(self.expr(expr)?),
                        cases: cases.into(),
                        default_index,
                        default,
                    }
                }
            };
            stmts.push((stmt, span));
        }
        Ok(ast::Block::new(stmts))
    }

    fn forever(body: ast::SourceBlock<'src>, span: Span) -> ast::Stmt<'src, ast::WithSpan> {
        ast::Stmt::While(Box::new(ast::ConditionalBlock::new(
            ast::LetCondition::Expr((
                ast::Expr::Constant(ast::Constant::Bool(true)),
                crate::control_flow::generated_span(span),
            )),
            body,
        )))
    }

    // A real, transparent conditional carries one header through type checking.
    // The target-owned plan identifies it by the containing loop's source span.
    fn marker(body: ast::SourceBlock<'src>, span: Span) -> Spanned<ast::Stmt<'src, ast::WithSpan>> {
        (
            ast::Stmt::If {
                blocks: vec![ast::ConditionalBlock::new(
                    ast::LetCondition::Expr((
                        ast::Expr::Constant(ast::Constant::Bool(true)),
                        crate::control_flow::generated_span(span),
                    )),
                    body,
                )]
                .into(),
                else_: None,
            },
            span,
        )
    }

    fn discard_expr(
        &mut self,
        expr: Spanned<native::Expr<'src>>,
    ) -> Result<Vec<Spanned<ast::Stmt<'src, ast::WithSpan>>>> {
        let (expr, span) = expr;
        if let native::Expr::Sequence(exprs) = expr {
            if exprs.len() < 2 {
                return Err((CapabilityError::SequenceExpression, span));
            }
            let mut stmts = Vec::new();
            for expr in exprs {
                stmts.extend(self.discard_expr(expr)?);
            }
            Ok(stmts)
        } else {
            Ok(vec![(
                ast::Stmt::Expr(Box::new(self.expr((expr, span))?)),
                span,
            )])
        }
    }

    fn header_condition(
        &mut self,
        expr: Spanned<native::Expr<'src>>,
    ) -> Result<HeaderCondition<'src>> {
        let (expr, span) = expr;
        if let native::Expr::Sequence(exprs) = expr {
            let mut exprs = exprs.into_vec();
            let last = exprs
                .pop()
                .ok_or((CapabilityError::SequenceExpression, span))?;
            let mut prefix = Vec::new();
            for expr in exprs {
                prefix.extend(self.discard_expr(expr)?);
            }
            let (rest, value) = self.header_condition(last)?;
            prefix.extend(rest);
            Ok((prefix, value))
        } else {
            Ok((vec![], self.expr((expr, span))?))
        }
    }

    fn condition_check(
        &mut self,
        condition: Spanned<native::Expr<'src>>,
        body: ast::SourceBlock<'src>,
        span: Span,
    ) -> Result<ast::SourceBlock<'src>> {
        let (mut prefix, condition) = self.header_condition(condition)?;
        prefix.push((
            ast::Stmt::If {
                blocks: vec![ast::ConditionalBlock::new(
                    ast::LetCondition::Expr(condition),
                    body,
                )]
                .into(),
                else_: Some(ast::Block::new(vec![(
                    ast::Stmt::Break,
                    crate::control_flow::generated_span(span),
                )])),
            },
            span,
        ));
        Ok(ast::Block::new(prefix))
    }

    fn if_blocks(
        &mut self,
        blocks: Vec<(Spanned<native::Expr<'src>>, native::Block<'src>)>,
        else_: Option<native::Block<'src>>,
        span: Span,
    ) -> Result<ast::SourceBlock<'src>> {
        let mut rest = else_.map(|body| self.block(body)).transpose()?;
        for (condition, body) in blocks.into_iter().rev() {
            let (mut prefix, condition) = self.header_condition(condition)?;
            let body = self.block(body)?;
            prefix.push((
                ast::Stmt::If {
                    blocks: vec![ast::ConditionalBlock::new(
                        ast::LetCondition::Expr(condition),
                        body,
                    )]
                    .into(),
                    else_: rest,
                },
                span,
            ));
            rest = Some(ast::Block::new(prefix));
        }
        Ok(rest.unwrap_or_else(|| ast::Block::new(vec![])))
    }

    fn conditional(
        &mut self,
        condition: Spanned<native::Expr<'src>>,
        body: native::Block<'src>,
    ) -> Result<ast::SourceConditionalBlock<'src>> {
        Ok(ast::ConditionalBlock::new(
            ast::LetCondition::Expr(self.expr(condition)?),
            self.block(body)?,
        ))
    }

    pub(super) fn expr(
        &self,
        expr: Spanned<native::Expr<'src>>,
    ) -> Result<Spanned<ast::SourceExpr<'src>>> {
        self.lower_expr(expr)
    }

    fn lower_expr(
        &self,
        (expr, span): Spanned<native::Expr<'src>>,
    ) -> Result<Spanned<ast::SourceExpr<'src>>> {
        let expr = match expr {
            native::Expr::Ident(name) => ast::Expr::Ident(name),
            native::Expr::Constant(value) => ast::Expr::Constant(value),
            native::Expr::Null => ast::Expr::Null,
            native::Expr::This => ast::Expr::This,
            native::Expr::Super => ast::Expr::Super,
            native::Expr::ArrayLit(items) => ast::Expr::ArrayLit(
                items
                    .into_vec()
                    .into_iter()
                    .map(|expr| self.lower_expr(expr))
                    .collect::<Result<Vec<_>>>()?
                    .into(),
            ),
            native::Expr::New { typ: ty } => ast::Expr::New {
                typ: Box::new(typ(*ty)),
                args: vec![].into(),
            },
            native::Expr::Call { expr, args } => {
                let mut expr = self.lower_expr(*expr)?;
                if matches!(expr.0, ast::Expr::Ident(name) if Some(name) == self.wrapper) {
                    expr.0 = ast::Expr::Ident("wrappedMethod");
                }
                ast::Expr::Call {
                    expr: Box::new(expr),
                    type_args: vec![].into(),
                    args: args
                        .into_vec()
                        .into_iter()
                        .map(|expr| match expr {
                            (native::Expr::Omitted, span) => Ok((ast::Expr::Omitted, span)),
                            expr => self.lower_expr(expr),
                        })
                        .collect::<Result<Vec<_>>>()?
                        .into(),
                }
            }
            native::Expr::Cast { typ: ty, expr, .. } => ast::Expr::Call {
                // A synthetic callee must not hide the written target type from find_at.
                expr: Box::new((
                    ast::Expr::Ident("Cast"),
                    Span::new(span.start, ty.1.start, span.file),
                )),
                type_args: vec![typ(*ty)].into(),
                args: vec![self.lower_expr(*expr)?].into(),
            },
            native::Expr::DynCast { expr, typ: ty } => ast::Expr::DynCast {
                expr: Box::new(self.lower_expr(*expr)?),
                typ: Box::new(typ(*ty)),
            },
            native::Expr::Member { expr, member } => ast::Expr::Member {
                expr: Box::new(self.lower_expr(*expr)?),
                member,
            },
            native::Expr::Index { expr, index } => ast::Expr::Index {
                expr: Box::new(self.lower_expr(*expr)?),
                index: Box::new(self.lower_expr(*index)?),
            },
            native::Expr::UnOp { op, expr } => ast::Expr::UnOp {
                op,
                expr: Box::new(self.lower_expr(*expr)?),
            },
            native::Expr::BinOp { lhs, op, rhs } => ast::Expr::BinOp {
                lhs: Box::new(self.lower_expr(*lhs)?),
                op,
                rhs: Box::new(self.lower_expr(*rhs)?),
            },
            native::Expr::Assign { lhs, rhs } => ast::Expr::Assign {
                lhs: Box::new(self.lower_expr(*lhs)?),
                rhs: Box::new(self.lower_expr(*rhs)?),
            },
            native::Expr::Conditional { cond, then, else_ } => ast::Expr::Conditional {
                cond: Box::new(self.lower_expr(*cond)?),
                then: Box::new(self.lower_expr(*then)?),
                else_: Box::new(self.lower_expr(*else_)?),
            },
            native::Expr::Sequence(_) => return Err((CapabilityError::SequenceExpression, span)),
            native::Expr::Omitted => return Err((CapabilityError::OmittedArgument, span)),
            native::Expr::Error => return Err((CapabilityError::RecoveredExpression, span)),
        };
        Ok((expr, span))
    }
}

pub(super) fn operator_name(
    operator: native::Operator,
    arity: usize,
    span: Span,
) -> Result<&'static str> {
    use native::Operator as Op;

    if !(1..=2).contains(&arity) {
        return Err((
            CapabilityError::InvalidOperatorSignature("operators require one or two parameters"),
            span,
        ));
    }
    if operator == Op::Index && arity != 2 {
        return Err((
            CapabilityError::InvalidOperatorSignature("index operators require two parameters"),
            span,
        ));
    }
    let name = match operator {
        Op::Add => ast::BinOp::Add.name(),
        Op::Subtract if arity == 1 => ast::UnOp::Neg.name(),
        Op::Subtract => ast::BinOp::Sub.name(),
        Op::Multiply => ast::BinOp::Mul.name(),
        Op::Divide => ast::BinOp::Div.name(),
        Op::Modulo => ast::BinOp::Mod.name(),
        Op::Equal => ast::BinOp::Eq.name(),
        Op::NotEqual => ast::BinOp::Ne.name(),
        Op::Less => ast::BinOp::Lt.name(),
        Op::LessEqual => ast::BinOp::Le.name(),
        Op::Greater => ast::BinOp::Gt.name(),
        Op::GreaterEqual => ast::BinOp::Ge.name(),
        Op::And => ast::BinOp::And.name(),
        Op::Or => ast::BinOp::Or.name(),
        Op::BitAnd => ast::BinOp::BitAnd.name(),
        Op::BitOr => ast::BinOp::BitOr.name(),
        Op::BitXor => ast::BinOp::BitXor.name(),
        Op::Not => ast::UnOp::Not.name(),
        Op::BitNot => ast::UnOp::BitNot.name(),
        Op::AssignAdd => ast::BinOp::AssignAdd.name(),
        Op::AssignSubtract => ast::BinOp::AssignSub.name(),
        Op::AssignMultiply => ast::BinOp::AssignMul.name(),
        Op::AssignDivide => ast::BinOp::AssignDiv.name(),
        Op::AssignBitAnd => ast::BinOp::AssignBitAnd.name(),
        Op::AssignBitOr => ast::BinOp::AssignBitOr.name(),
        Op::Index => "OperatorArray",
    };
    Ok(name)
}

pub(super) fn mutable_array(typ: &native::Type<'_>) -> bool {
    matches!(
        typ,
        native::Type::Array(_) | native::Type::StaticArray(_, _)
    )
}

pub(super) fn typ((ty, span): Spanned<native::Type<'_>>) -> Spanned<ast::SourceType<'_>> {
    let ty = match ty {
        native::Type::Named { name, args } => ast::Type::Named {
            name: match name {
                "name" => "CName",
                "weak" => "wref",
                // Native ref is a value reference, not the community strong handle.
                "ref" => "script_ref",
                other => other,
            },
            args: args.into_iter().map(typ).collect(),
        },
        native::Type::Array(elem) => ast::Type::Array(Box::new(typ(*elem))),
        native::Type::StaticArray(elem, size) => ast::Type::StaticArray(Box::new(typ(*elem)), size),
    };
    (ty, span)
}

pub(super) fn binds(block: &native::Block<'_>, name: &str) -> bool {
    block.iter().any(|(stmt, _)| binds_stmt(stmt, name))
}

fn binds_stmt(stmt: &native::Statement<'_>, name: &str) -> bool {
    match stmt {
        native::Statement::Local(local) => local.names.iter().any(|(binding, _)| *binding == name),
        native::Statement::Block(body)
        | native::Statement::While { body, .. }
        | native::Statement::DoWhile { body, .. } => binds(body, name),
        native::Statement::For { init, body, .. } => {
            init.as_ref().is_some_and(|stmt| binds_stmt(&stmt.0, name)) || binds(body, name)
        }
        native::Statement::If { blocks, else_ } => {
            blocks.iter().any(|(_, body)| binds(body, name))
                || else_.as_ref().is_some_and(|body| binds(body, name))
        }
        native::Statement::Switch { labels, .. } => {
            labels.iter().any(|(label, _)| binds(&label.body, name))
        }
        _ => false,
    }
}

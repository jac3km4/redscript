//! Checked native REDmod -> shared compiler syntax boundary.
use std::fmt;

use redscript_community_ast::{self as ast, Span, Spanned};
use redscript_redmod_ast as native;

/// Shared syntax plus REDmod intent which has no shared syntax representation.
/// Pass this value intact to REDmod `process_sources`: extracting `module` alone
/// discards the typed loop-header fixup plan.
#[derive(Debug, Clone)]
pub struct ParsedModule<'src> {
    pub module: ast::SourceModule<'src>,
    pub overrides: Vec<Span>,
    pub(crate) control_flow: Vec<crate::control_flow::LoopPlan>,
}

/// Syntax accepted by the native parser but not faithfully supported by this bridge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityError {
    ForLoop,
    DoWhileLoop,
    SequenceExpression,
    OmittedArgument,
    AssignmentValue,
    UnsafeArrayEvaluation,
    ConstLocal,
    InitializedLocalGroup,
    FieldGroup,
    OperatorDeclaration,
    ConversionDeclaration,
    EnumStorage,
    Metadata,
    DefaultDirective,
    HintDirective,
    ConstReturn,
    Modifier(native::Modifier),
    SkipParameter,
    SwitchLabelOrder,
    RecoveredExpression,
    ControlFlowLocalLimit,
    ControlFlowProvenance,
}

impl fmt::Display for CapabilityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            Self::ForLoop => {
                "for loops require REDmod metadata processing; use parse_file_with_metadata and process_sources"
            }
            Self::DoWhileLoop => {
                "do-while loops require REDmod metadata processing; use parse_file_with_metadata and process_sources"
            }
            Self::SequenceExpression => {
                "comma expressions have no shared value/ordering representation"
            }
            Self::OmittedArgument => {
                "omitted argument slots are not supported; provide explicit arguments"
            }
            Self::AssignmentValue => {
                "value-producing assignments are unsupported by the shared Void assignment semantics"
            }
            Self::UnsafeArrayEvaluation => {
                "array literal prefixes may change conditional evaluation or operand order; bind the array explicitly at its intended execution point"
            }
            Self::ConstLocal => "const local enforcement is not supported",
            Self::InitializedLocalGroup => {
                "initialized local groups require explicit per-binding initializers; split the declaration"
            }
            Self::FieldGroup => {
                "initialized field groups require explicit per-field initializers; split the declaration"
            }
            Self::OperatorDeclaration => {
                "operator declarations require REDmod overload registration that is not implemented"
            }
            Self::ConversionDeclaration => {
                "conversion declarations require REDmod conversion registration that is not implemented"
            }
            Self::EnumStorage => "explicit enum storage types are not supported",
            Self::Metadata => "bracket metadata requires a field and string literal values",
            Self::DefaultDirective => {
                "default directives (including inherited defaults) require target-specific initialization that is not implemented"
            }
            Self::HintDirective => {
                "hint directives require a same-aggregate field target and string value"
            }
            Self::ConstReturn => "const return semantics are not supported",
            Self::Modifier(modifier) => {
                return write!(
                    f,
                    "REDmod modifier {modifier:?} has no implemented semantic mapping"
                );
            }
            Self::SkipParameter => {
                "skip parameters require short-circuit operator lowering that is not implemented"
            }
            Self::SwitchLabelOrder => {
                "switch defaults must be unique and last; shared syntax cannot preserve this label order"
            }
            Self::ControlFlowLocalLimit => "loop lowering exceeds the typed local limit",
            Self::ControlFlowProvenance => {
                "loop lowering provenance was changed or lost; retain the parsed REDmod module and its control-flow plan together"
            }
            Self::RecoveredExpression => {
                "recovered syntax cannot be lowered; fix the syntax errors first"
            }
        };
        f.write_str(message)
    }
}

impl std::error::Error for CapabilityError {}

type Result<T> = std::result::Result<T, (CapabilityError, Span)>;
type HeaderCondition<'src> = (
    Vec<Spanned<ast::SourceStmt<'src>>>,
    Spanned<ast::SourceExpr<'src>>,
);

/// No partial module or recovery placeholders escape a failed conversion.
pub fn lower(module: native::Module<'_>) -> Result<ParsedModule<'_>> {
    let mut bridge = Bridge {
        overrides: vec![],
        wrapper: None,
        control_flow: vec![],
    };
    let items = bridge.items(module.items, false)?;
    Ok(ParsedModule {
        module: ast::Module::new(module.path.map(|path| ast::Path::new(path.segments)), items),
        overrides: bridge.overrides,
        control_flow: bridge.control_flow,
    })
}

struct Bridge<'src> {
    overrides: Vec<Span>,
    wrapper: Option<&'src str>,
    control_flow: Vec<crate::control_flow::LoopPlan>,
}

impl<'src> Bridge<'src> {
    fn items(
        &mut self,
        items: Vec<Spanned<native::Declaration<'src>>>,
        aggregate: bool,
    ) -> Result<Vec<Spanned<ast::SourceItemDecl<'src>>>> {
        // Hints are metadata on an existing field, never synthetic fields. Resolve
        // against typed declarations in this exact aggregate, not inherited names.
        let mut lowered = Vec::new();
        let mut hints = Vec::new();
        for (mut decl, span) in items {
            if let native::Item::Field(field) = &mut decl.item
                && field.names.len() > 1
            {
                if field.default.is_some() {
                    return Err((CapabilityError::FieldGroup, span));
                }
                // Remove the full name list before cloning common declaration
                // metadata, so expanding a large group is linear in its size.
                let names = std::mem::take(&mut field.names);
                for name in names {
                    let mut member = decl.clone();
                    if let native::Item::Field(field) = &mut member.item {
                        field.names.push(name);
                    }
                    lowered.push((self.declaration(member, span)?, name.1));
                }
                continue;
            }
            if let native::Item::Directive {
                kind: native::DirectiveKind::Hint,
                target,
                value,
            } = &decl.item
            {
                if !aggregate
                    || !decl.annotations.is_empty()
                    || !decl.modifiers.is_empty()
                    || !decl.metadata.is_empty()
                    || decl.visibility.is_some()
                {
                    return Err((CapabilityError::HintDirective, span));
                }
                hints.push((*target, value.clone(), span));
            } else {
                lowered.push((self.declaration(decl, span)?, span));
            }
        }
        for (target, value, span) in hints {
            let native::Expr::Constant(ast::Constant::String(_)) = &value.0 else {
                return Err((CapabilityError::HintDirective, value.1));
            };
            let mut targets = lowered.iter_mut().filter(
                |(decl, _)| matches!(&decl.item, ast::Item::Let(field) if field.name.0 == target.0),
            );
            let Some((decl, _)) = targets.next() else {
                return Err((CapabilityError::HintDirective, target.1));
            };
            if targets.next().is_some() {
                return Err((CapabilityError::HintDirective, target.1));
            }
            decl.annotations = decl
                .annotations
                .iter()
                .cloned()
                .chain(std::iter::once((
                    ast::Annotation::new("hint", vec![self.expr(value, false)?]),
                    span,
                )))
                .collect();
        }
        Ok(lowered)
    }

    fn declaration(
        &mut self,
        decl: native::Declaration<'src>,
        span: Span,
    ) -> Result<ast::SourceItemDecl<'src>> {
        let mut annotations = crate::metadata::annotations(
            decl.metadata,
            matches!(decl.item, native::Item::Field(_)),
        )?;
        annotations.extend(
            decl.annotations
                .into_iter()
                .map(|(ann, span)| {
                    Ok((
                        ast::Annotation::new(
                            ann.name.0,
                            ann.args
                                .into_iter()
                                .map(|expr| self.expr(expr, false))
                                .collect::<Result<Vec<_>>>()?,
                        ),
                        span,
                    ))
                })
                .collect::<Result<Vec<_>>>()?,
        );
        let mut qualifiers = ast::ItemQualifiers::empty();
        let mut explicit_override = false;
        for (modifier, span) in decl.modifiers {
            use ast::ItemQualifiers as Q;
            qualifiers |= match modifier {
                native::Modifier::Abstract => Q::ABSTRACT,
                native::Modifier::Const => Q::CONST,
                native::Modifier::Exec => Q::EXEC,
                native::Modifier::Final => Q::FINAL,
                native::Modifier::ImportOnly => Q::IMPORT_ONLY,
                native::Modifier::Import => Q::NATIVE,
                native::Modifier::Persistent => Q::PERSISTENT,
                native::Modifier::Quest => Q::QUEST,
                native::Modifier::Static => Q::STATIC,
                native::Modifier::Editable => Q::EDITABLE,
                native::Modifier::InstanceEditable => Q::INSTANCE_EDITABLE,
                native::Modifier::Inlined => Q::INLINE,
                native::Modifier::Replicated => Q::REPLICATED,
                native::Modifier::TestOnly => Q::TEST_ONLY,
                native::Modifier::Override => {
                    explicit_override = true;
                    Q::empty()
                }
                other => return Err((CapabilityError::Modifier(other), span)),
            };
        }
        let item = match decl.item {
            native::Item::Import(path) => {
                ast::Item::Import(ast::Import::All(ast::Path::new(path.segments)))
            }
            native::Item::Field(field) => {
                if field.names.len() != 1 {
                    return Err((CapabilityError::FieldGroup, span));
                }
                ast::Item::Let(ast::Field::new(
                    field.names[0],
                    Box::new(typ(field.typ)),
                    field
                        .default
                        .map(|expr| self.expr(expr, false).map(Box::new))
                        .transpose()?,
                ))
            }
            native::Item::Aggregate {
                is_struct,
                name,
                extends,
                items,
            } => {
                let aggregate = ast::Aggregate::new(
                    name,
                    vec![],
                    extends.map(|ty| Box::new(typ(ty))),
                    self.items(items, true)?,
                );
                if is_struct {
                    ast::Item::Struct(aggregate)
                } else {
                    ast::Item::Class(aggregate)
                }
            }
            native::Item::Function {
                kind,
                params,
                return_type,
                const_return,
                body,
            } => {
                if let Some(span) = const_return {
                    return Err((CapabilityError::ConstReturn, span));
                }
                let name = match kind {
                    native::FunctionKind::Function(name) => name,
                    native::FunctionKind::Event(name) => {
                        qualifiers |= ast::ItemQualifiers::CALLBACK;
                        name
                    }
                    native::FunctionKind::Operator((_, span)) => {
                        return Err((CapabilityError::OperatorDeclaration, span));
                    }
                    native::FunctionKind::Cast { span, .. } => {
                        return Err((CapabilityError::ConversionDeclaration, span));
                    }
                };
                if explicit_override {
                    self.overrides.push(name.1);
                }
                let mut lowered_params = Vec::new();
                for (param, group_span) in params {
                    let mut qualifiers = ast::ParamQualifiers::empty();
                    for (modifier, span) in param.modifiers {
                        qualifiers |= match modifier {
                            native::ParamModifier::Optional => ast::ParamQualifiers::OPTIONAL,
                            native::ParamModifier::Out => ast::ParamQualifiers::OUT,
                            native::ParamModifier::Const => ast::ParamQualifiers::CONST,
                            native::ParamModifier::Skip => {
                                return Err((CapabilityError::SkipParameter, span));
                            }
                        };
                    }
                    let ty = typ(param.typ);
                    let grouped = param.names.len() > 1;
                    for (name, span) in param.names {
                        let span = if grouped { span } else { group_span };
                        lowered_params
                            .push((ast::Param::new(name, Some(ty.clone()), qualifiers), span));
                    }
                }
                // The hoisting policy makes every local visible throughout the
                // function, including before its declaration and in its initializer.
                let shadowed = lowered_params.iter().any(|(param, _)| param.name == name.0)
                    || body.as_ref().is_some_and(|body| binds(body, name.0));
                let wrapper =
                    annotations.iter().any(|(ann, _)| ann.name == "wrapMethod") && !shadowed;
                let previous = self.wrapper;
                self.wrapper = wrapper.then_some(name.0);
                let body = body
                    .map(|body| self.block(body).map(ast::FunctionBody::Block))
                    .transpose()?;
                self.wrapper = previous;
                ast::Item::Function(ast::Function::new(
                    name,
                    vec![],
                    lowered_params,
                    return_type.map(|ty| Box::new(typ(ty))),
                    body,
                ))
            }
            native::Item::Enum {
                name,
                variants,
                storage,
            } => {
                if let Some((_, span)) = storage {
                    return Err((CapabilityError::EnumStorage, span));
                }
                ast::Item::Enum(ast::Enum::new(
                    name,
                    variants
                        .into_iter()
                        .map(|(variant, span)| {
                            (
                                ast::EnumVariant::new(
                                    variant.name.0,
                                    variant.value.map(|(value, _)| i64::from(value)),
                                ),
                                span,
                            )
                        })
                        .collect::<Vec<_>>(),
                ))
            }
            native::Item::Directive { kind, .. } => {
                return Err((
                    match kind {
                        native::DirectiveKind::Default => CapabilityError::DefaultDirective,
                        native::DirectiveKind::Hint => CapabilityError::HintDirective,
                    },
                    span,
                ));
            }
        };
        if explicit_override && !matches!(item, ast::Item::Function(_)) {
            return Err((CapabilityError::Modifier(native::Modifier::Override), span));
        }
        Ok(ast::ItemDecl::new(
            annotations,
            decl.visibility,
            qualifiers,
            decl.docs,
            item,
        ))
    }

    fn block(&mut self, block: native::Block<'src>) -> Result<ast::SourceBlock<'src>> {
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
                    if let Some(span) = local.const_ {
                        return Err((CapabilityError::ConstLocal, span));
                    }
                    if local.names.len() > 1 && local.value.is_some() {
                        return Err((CapabilityError::InitializedLocalGroup, span));
                    }
                    let ty = typ(local.typ);
                    let value = local
                        .value
                        .map(|expr| self.expr(expr, false).map(Box::new))
                        .transpose()?;
                    let grouped = local.names.len() > 1;
                    for name in local.names {
                        let own_span = if grouped { name.1 } else { span };
                        stmts.push((
                            ast::Stmt::Let {
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
                    expr.map(|expr| self.expr(*expr, false).map(Box::new))
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
                    for (label, span) in labels {
                        if default.is_some() {
                            return Err((CapabilityError::SwitchLabelOrder, span));
                        }
                        let body = self.block(label.body)?.stmts;
                        if let Some(value) = label.value {
                            // Matcher prefixes must not run before the switch.
                            crate::array_guard::ArrayGuard::check_lazy(&value)?;
                            cases.push(ast::Case::new(
                                ast::Condition::Expr(self.expr(value, false)?),
                                body,
                            ));
                        } else {
                            default = Some(body);
                        }
                    }
                    ast::Stmt::Switch {
                        expr: Box::new(self.expr(expr, false)?),
                        cases: cases.into(),
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
                ast::Stmt::Expr(Box::new(self.expr((expr, span), true)?)),
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
            Ok((vec![], self.expr((expr, span), false)?))
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
            ast::LetCondition::Expr(self.expr(condition, false)?),
            self.block(body)?,
        ))
    }

    fn expr(
        &self,
        expr: Spanned<native::Expr<'src>>,
        discarded: bool,
    ) -> Result<Spanned<ast::SourceExpr<'src>>> {
        crate::array_guard::ArrayGuard::check(&expr)?;
        self.lower_expr(expr, discarded)
    }

    fn lower_expr(
        &self,
        (expr, span): Spanned<native::Expr<'src>>,
        discarded: bool,
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
                    .map(|expr| self.lower_expr(expr, false))
                    .collect::<Result<Vec<_>>>()?
                    .into(),
            ),
            native::Expr::New { typ: ty } => ast::Expr::New {
                typ: Box::new(typ(*ty)),
                args: vec![].into(),
            },
            native::Expr::Call { expr, args } => {
                let mut expr = self.lower_expr(*expr, false)?;
                if matches!(expr.0, ast::Expr::Ident(name) if Some(name) == self.wrapper) {
                    expr.0 = ast::Expr::Ident("wrappedMethod");
                }
                ast::Expr::Call {
                    expr: Box::new(expr),
                    type_args: vec![].into(),
                    args: args
                        .into_vec()
                        .into_iter()
                        .map(|expr| self.lower_expr(expr, false))
                        .collect::<Result<Vec<_>>>()?
                        .into(),
                }
            }
            native::Expr::Cast { typ: ty, expr, .. } => ast::Expr::Call {
                // A synthetic callee must not hide the written target type from find_at.
                expr: Box::new((
                    ast::Expr::Ident("Cast"),
                    Span::new(span.start, span.start, span.file),
                )),
                type_args: vec![typ(*ty)].into(),
                args: vec![self.lower_expr(*expr, false)?].into(),
            },
            native::Expr::DynCast { expr, typ: ty } => ast::Expr::DynCast {
                expr: Box::new(self.lower_expr(*expr, false)?),
                typ: Box::new(typ(*ty)),
            },
            native::Expr::Member { expr, member } => ast::Expr::Member {
                expr: Box::new(self.lower_expr(*expr, false)?),
                member,
            },
            native::Expr::Index { expr, index } => ast::Expr::Index {
                expr: Box::new(self.lower_expr(*expr, false)?),
                index: Box::new(self.lower_expr(*index, false)?),
            },
            native::Expr::UnOp { op, expr } => ast::Expr::UnOp {
                op,
                expr: Box::new(self.lower_expr(*expr, false)?),
            },
            native::Expr::BinOp { lhs, op, rhs } => {
                if !discarded
                    && matches!(
                        op,
                        ast::BinOp::AssignAdd
                            | ast::BinOp::AssignSub
                            | ast::BinOp::AssignMul
                            | ast::BinOp::AssignDiv
                            | ast::BinOp::AssignBitAnd
                            | ast::BinOp::AssignBitOr
                    )
                {
                    return Err((CapabilityError::AssignmentValue, span));
                }
                ast::Expr::BinOp {
                    lhs: Box::new(self.lower_expr(*lhs, false)?),
                    op,
                    rhs: Box::new(self.lower_expr(*rhs, false)?),
                }
            }
            native::Expr::Assign { lhs, rhs } => {
                if !discarded {
                    return Err((CapabilityError::AssignmentValue, span));
                }
                ast::Expr::Assign {
                    lhs: Box::new(self.lower_expr(*lhs, false)?),
                    rhs: Box::new(self.lower_expr(*rhs, false)?),
                }
            }
            native::Expr::Conditional { cond, then, else_ } => ast::Expr::Conditional {
                cond: Box::new(self.lower_expr(*cond, false)?),
                then: Box::new(self.lower_expr(*then, discarded)?),
                else_: Box::new(self.lower_expr(*else_, discarded)?),
            },
            native::Expr::Sequence(_) => return Err((CapabilityError::SequenceExpression, span)),
            native::Expr::Omitted => return Err((CapabilityError::OmittedArgument, span)),
            native::Expr::Error => return Err((CapabilityError::RecoveredExpression, span)),
        };
        Ok((expr, span))
    }
}

fn typ((ty, span): Spanned<native::Type<'_>>) -> Spanned<ast::SourceType<'_>> {
    let ty = match ty {
        native::Type::Named { name, args } => ast::Type::Named {
            name: match name {
                "name" => "CName",
                "weak" => "wref",
                other => other,
            },
            args: args.into_iter().map(typ).collect(),
        },
        native::Type::Array(elem) => ast::Type::Array(Box::new(typ(*elem))),
        native::Type::StaticArray(elem, size) => ast::Type::StaticArray(Box::new(typ(*elem)), size),
    };
    (ty, span)
}

fn binds(block: &native::Block<'_>, name: &str) -> bool {
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

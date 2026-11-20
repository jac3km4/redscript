//! Checked native REDmod -> shared compiler syntax boundary.
mod body;

use std::fmt;

use body::{binds, mutable_array, operator_name, typ};
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

    UnsafeArrayEvaluation,

    InitializedLocalGroup,
    FieldGroup,
    OperatorDeclaration,

    InvalidOperatorSignature(&'static str),
    InvalidConversionSignature(&'static str),
    EnumStorage,
    Metadata,
    DefaultDirective,
    HintDirective,
    ConstReturn,
    BodylessValueMethod,
    Modifier(native::Modifier),

    SwitchLabelOrder,
    RecoveredExpression,
    ControlFlowLocalLimit,
    ControlFlowProvenance,
}

impl fmt::Display for CapabilityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            Self::ForLoop => {
                "this tool entry point does not support for loops; integrations must use parse_file_with_metadata and process_sources"
            }
            Self::DoWhileLoop => {
                "this tool entry point does not support do-while loops; integrations must use parse_file_with_metadata and process_sources"
            }
            Self::SequenceExpression => "comma expressions are not supported here",
            Self::OmittedArgument => {
                "omitted arguments are not supported here; supply a value only if it preserves the parameter's default behavior"
            }

            Self::UnsafeArrayEvaluation => {
                "this array literal cannot be evaluated here without changing when expressions run"
            }

            Self::InitializedLocalGroup => {
                "initializing multiple locals in one declaration is not supported; declare each separately with its intended initial value"
            }
            Self::FieldGroup => {
                "initializing multiple fields in one declaration is not supported; declare each separately with its intended initial value"
            }
            Self::OperatorDeclaration => "this operator declaration is not supported",

            Self::InvalidOperatorSignature(message) | Self::InvalidConversionSignature(message) => {
                message
            }
            Self::EnumStorage => "this enum storage type is not supported",
            Self::Metadata => {
                "use [key = \"value\"] on a field or [profile = \"label\"] on a function"
            }
            Self::DefaultDirective => {
                "default requires a class or struct field and a literal or enum member value"
            }
            Self::HintDirective => {
                "invalid hint directive: target a field declared in the same class or struct and use a quoted string value"
            }
            Self::ConstReturn => "const return values are not supported",
            Self::BodylessValueMethod => {
                "this method returns a value and needs a body; add its implementation"
            }
            Self::Modifier(modifier) => {
                return write!(
                    f,
                    "modifier '{}' is not supported on this declaration",
                    format!("{modifier:?}").to_ascii_lowercase()
                );
            }

            Self::SwitchLabelOrder => {
                "a switch can have only one default label; combine the default cases"
            }
            Self::ControlFlowLocalLimit => {
                "processing this loop exceeds the local variable limit; split the function into smaller functions"
            }
            Self::ControlFlowProvenance => {
                "internal error: loop information was lost; integrations must pass the complete ParsedModule to process_sources"
            }
            Self::RecoveredExpression => {
                "fix the reported syntax errors before analyzing this code"
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
        scalar_mutable: false,
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
    scalar_mutable: bool,
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
            if let native::Item::Directive {
                kind: native::DirectiveKind::Default,
                target,
                value,
            } = &decl.item
            {
                if !decl.metadata.is_empty() {
                    crate::metadata::annotations(decl.metadata.clone(), false, false)?;
                }
                if !aggregate
                    || !decl.annotations.is_empty()
                    || !decl.modifiers.is_empty()
                    || decl.visibility.is_some()
                {
                    return Err((CapabilityError::DefaultDirective, span));
                }
                let value = self.expr(value.clone())?;
                let default = crate::defaults::plan(*target, value)?;
                lowered.push((
                    ast::ItemDecl::new(
                        vec![],
                        None,
                        ast::ItemQualifiers::empty(),
                        decl.docs,
                        ast::Item::Default(default),
                    ),
                    span,
                ));
                continue;
            }
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
                    lowered.push((self.declaration(member, span, aggregate)?, name.1));
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
                lowered.push((self.declaration(decl, span, aggregate)?, span));
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
                    ast::Annotation::new("hint", vec![self.expr(value)?]),
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
        aggregate: bool,
    ) -> Result<ast::SourceItemDecl<'src>> {
        let mut annotations = crate::metadata::annotations(
            decl.metadata,
            matches!(decl.item, native::Item::Field(_)),
            matches!(decl.item, native::Item::Function { .. }),
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
                                .map(|expr| self.expr(expr))
                                .collect::<Result<Vec<_>>>()?,
                        ),
                        span,
                    ))
                })
                .collect::<Result<Vec<_>>>()?,
        );
        let has_modifier = |expected| {
            decl.modifiers
                .iter()
                .any(|(modifier, _)| *modifier == expected)
        };
        let scalar_mutable = self.scalar_mutable
            && has_modifier(native::Modifier::InstanceEditable)
            && has_modifier(native::Modifier::Inlined)
            && decl.modifiers.iter().all(|(modifier, _)| {
                matches!(
                    modifier,
                    native::Modifier::Mutable
                        | native::Modifier::Const
                        | native::Modifier::InstanceEditable
                        | native::Modifier::Inlined
                )
            });
        let imported = has_modifier(native::Modifier::Import);
        let mut qualifiers = ast::ItemQualifiers::empty();
        let mut explicit_override = false;
        let mut virtual_method = false;
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
                native::Modifier::Mutable
                    if aggregate
                        && matches!(
                            &decl.item,
                            native::Item::Field(field) if mutable_array(&field.typ.0)
                                                            || scalar_mutable && matches!(&field.typ.0,
                                                                native::Type::Named { name: "Float", args } if args.is_empty())
                        ) =>
                {
                    // These observed forms have no extra flag in official output.
                    // Explicit const is independent and must not be cleared.
                    Q::empty()
                }
                native::Modifier::Override => {
                    explicit_override = true;
                    Q::empty()
                }
                native::Modifier::Virtual
                    if aggregate
                        && matches!(
                            decl.item,
                            native::Item::Function {
                                kind: native::FunctionKind::Function(_),
                                ..
                            }
                        ) =>
                {
                    virtual_method = true;
                    Q::empty()
                }
                native::Modifier::Export if matches!(decl.item, native::Item::Function { .. }) => {
                    Q::empty()
                }
                native::Modifier::Constexpr
                    if matches!(
                        decl.item,
                        native::Item::Function {
                            kind: native::FunctionKind::Function(_),
                            ..
                        }
                    ) =>
                {
                    Q::empty()
                }
                other => return Err((CapabilityError::Modifier(other), span)),
            };
        }
        if virtual_method && qualifiers.contains(ast::ItemQualifiers::STATIC) {
            return Err((CapabilityError::Modifier(native::Modifier::Virtual), span));
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
                        .map(|expr| self.expr(expr).map(Box::new))
                        .transpose()?,
                ))
            }
            native::Item::Aggregate {
                is_struct,
                name,
                extends,
                items,
            } => {
                let previous = self.scalar_mutable;
                self.scalar_mutable = !is_struct && !imported;
                let items = self.items(items, true)?;
                self.scalar_mutable = previous;
                let aggregate =
                    ast::Aggregate::new(name, vec![], extends.map(|ty| Box::new(typ(ty))), items);
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
                let conversion = match &kind {
                    native::FunctionKind::Cast { implicit, .. } => Some(if implicit.is_some() {
                        ast::ConversionKind::Implicit
                    } else {
                        ast::ConversionKind::Explicit
                    }),
                    _ => None,
                };
                let name = match kind {
                    native::FunctionKind::Function(name) => name,
                    native::FunctionKind::Event(name) => {
                        qualifiers |= ast::ItemQualifiers::CALLBACK;
                        name
                    }
                    native::FunctionKind::Operator((operator, span)) => {
                        let arity = params.iter().map(|(param, _)| param.names.len()).sum();
                        let name = operator_name(operator, arity, span)?;
                        if aggregate {
                            return Err((
                                CapabilityError::InvalidOperatorSignature(
                                    "operators must be declared in global scope",
                                ),
                                span,
                            ));
                        }
                        (name, span)
                    }
                    native::FunctionKind::Cast { span, .. } => {
                        if aggregate {
                            return Err((
                                CapabilityError::InvalidConversionSignature(
                                    "conversions must be declared in global scope",
                                ),
                                span,
                            ));
                        }
                        if params
                            .iter()
                            .map(|(param, _)| param.names.len())
                            .sum::<usize>()
                            != 1
                        {
                            return Err((
                                CapabilityError::InvalidConversionSignature(
                                    "conversions require exactly one parameter",
                                ),
                                span,
                            ));
                        }
                        if return_type.as_ref().is_none_or(|(typ, _)| {
                            matches!(typ, native::Type::Named { name: "Void", args } if args.is_empty())
                        }) {
                            return Err((
                                CapabilityError::InvalidConversionSignature(
                                    "conversions require an explicit non-Void return type",
                                ),
                                span,
                            ));
                        }
                        ("Cast", span)
                    }
                };
                if explicit_override {
                    self.overrides.push(name.1);
                }
                if aggregate
                    && !virtual_method
                    && !explicit_override
                    && !qualifiers
                        .intersects(ast::ItemQualifiers::STATIC | ast::ItemQualifiers::CALLBACK)
                    && decl.visibility.unwrap_or(ast::Visibility::Private)
                        == ast::Visibility::Private
                {
                    qualifiers |= ast::ItemQualifiers::FINAL;
                }
                let body = if body.is_none()
                    && (aggregate
                        || return_type.as_ref().is_none_or(|(typ, _)| {
                            matches!(typ, native::Type::Named { name: "Void", args } if args.is_empty())
                        }))
                    && !qualifiers
                        .intersects(ast::ItemQualifiers::NATIVE | ast::ItemQualifiers::CALLBACK)
                {
                    if return_type.as_ref().is_some_and(|(typ, _)| {
                        !matches!(typ, native::Type::Named { name: "Void", args } if args.is_empty())
                    }) {
                        return Err((CapabilityError::BodylessValueMethod, name.1));
                    }
                    Some(Vec::new())
                } else {
                    body
                };
                let mut lowered_params = Vec::new();
                for (param, group_span) in params {
                    let mut qualifiers = ast::ParamQualifiers::empty();
                    for (modifier, _) in param.modifiers {
                        qualifiers |= match modifier {
                            native::ParamModifier::Optional => ast::ParamQualifiers::OPTIONAL,
                            native::ParamModifier::Out => ast::ParamQualifiers::OUT,
                            native::ParamModifier::Const => ast::ParamQualifiers::CONST,
                            native::ParamModifier::Skip => ast::ParamQualifiers::SKIP,
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
                ast::Item::Function(
                    ast::Function::new(
                        name,
                        vec![],
                        lowered_params,
                        return_type.map(|ty| Box::new(typ(ty))),
                        body,
                    )
                    .with_conversion(conversion)
                    .with_const_return(const_return.is_some()),
                )
            }
            native::Item::Enum {
                name,
                variants,
                storage,
            } => ast::Item::Enum(
                ast::Enum::new(
                    name,
                    variants
                        .into_iter()
                        .map(|(variant, span)| {
                            (
                                ast::EnumVariant::new(
                                    variant.name.0,
                                    variant.value.map(|(value, _)| value),
                                ),
                                span,
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .with_storage(storage.map(typ)),
            ),
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
}

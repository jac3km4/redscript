use std::collections::HashSet;
use std::convert::Infallible;

use redscript_community_ast::{self as ast, AstNode, AstVisitor, FileId, Span};
use redscript_compiler_frontend::{FieldId, LoweredCompilationUnit, MethodId, Symbols, Visibility};

use crate::{Error, SemanticError};

pub fn validate(module: &ast::SourceModule<'_>) -> Vec<Error> {
    let mut validator = Validator {
        errors: vec![],
        locals: HashSet::new(),
        aggregates: Vec::new(),
    };
    let Ok(()) = validator.visit_module(module);
    validator.errors
}

struct Validator<'src> {
    errors: Vec<Error>,
    locals: HashSet<&'src str>,
    aggregates: Vec<bool>,
}

impl<'src> Validator<'src> {
    fn reject(&mut self, kind: SemanticError, span: Span) {
        self.errors.push(Error::semantic(kind, span));
    }

    fn typ(&mut self, (typ, span): &ast::Spanned<ast::SourceType<'src>>) {
        match typ {
            ast::Type::Named { name, args } => {
                if *name == "ref"
                    || (!args.is_empty() && !matches!(*name, "array" | "wref" | "script_ref"))
                {
                    self.reject(SemanticError::UnsupportedType, *span);
                }
                for arg in args {
                    self.typ(arg);
                }
            }
            ast::Type::Array(elem) | ast::Type::StaticArray(elem, _) => self.typ(elem),
            ast::Type::Fn { .. } => self.reject(SemanticError::UnsupportedFunctionType, *span),
        }
    }
}

impl<'src> AstVisitor<'src, ast::WithSpan> for Validator<'src> {
    type Error = Infallible;

    fn visit_function(&mut self, function: &ast::SourceFunction<'src>) -> Result<(), Self::Error> {
        let old = std::mem::take(&mut self.locals);
        if !function.type_params.is_empty() {
            self.reject(SemanticError::UnsupportedGenericFunction, function.name.1);
        }
        for (param, span) in &function.params {
            if !self.locals.insert(param.name) {
                self.reject(SemanticError::DuplicateBinding, *span);
            }
            if let Some(typ) = &param.typ {
                self.typ(typ);
            }
        }
        if let Some(typ) = &function.return_type {
            self.typ(typ);
        }
        match &function.body {
            Some(ast::FunctionBody::Block(block)) => self.visit_block(block)?,
            Some(ast::FunctionBody::Inline(expr)) => {
                self.reject(SemanticError::UnsupportedExpressionBody, expr.1);
            }
            None => {}
        }
        self.locals = old;
        Ok(())
    }

    fn visit_node(&mut self, node: AstNode<'_, 'src, ast::WithSpan>) -> Result<(), Self::Error> {
        match node {
            AstNode::ItemDecl((decl, span)) => {
                if decl.qualifiers.contains(ast::ItemQualifiers::PERSISTENT)
                    && (!matches!(decl.item, ast::Item::Let(_)) || self.aggregates.is_empty())
                {
                    self.reject(SemanticError::PersistentRequiresAggregateField, *span);
                }
                if decl.qualifiers.contains(ast::ItemQualifiers::CALLBACK) {
                    if self.aggregates.last() != Some(&true)
                        && !decl.annotations.iter().any(|(ann, _)| {
                            matches!(ann.name, "addMethod" | "wrapMethod" | "replaceMethod")
                        })
                    {
                        self.reject(SemanticError::EventRequiresClassOrMethodTarget, *span);
                    }
                    if decl
                        .visibility
                        .is_some_and(|visibility| visibility != ast::Visibility::Protected)
                    {
                        self.reject(SemanticError::EventVisibility, *span);
                    }
                    if decl.qualifiers.intersects(
                        ast::ItemQualifiers::STATIC
                            | ast::ItemQualifiers::FINAL
                            | ast::ItemQualifiers::CONST,
                    ) {
                        self.reject(SemanticError::EventQualifiers, *span);
                    }
                    if let ast::Item::Function(function) = &decl.item
                        && function.return_type.is_some()
                    {
                        self.reject(SemanticError::EventReturnType, function.name.1);
                    }
                }
                for (annotation, ann_span) in &decl.annotations {
                    if !matches!(
                        annotation.name,
                        "addField"
                            | "addMethod"
                            | "wrapMethod"
                            | "replaceMethod"
                            | "runtimeProperty"
                            | "hint"
                            | "if"
                    ) {
                        self.reject(SemanticError::UnsupportedAnnotation, *ann_span);
                    } else if matches!(
                        annotation.name,
                        "addField" | "addMethod" | "wrapMethod" | "replaceMethod"
                    ) && decl.visibility.is_some()
                    {
                        self.reject(SemanticError::AnnotatedMemberVisibility, *span);
                    }
                }
                for (annotation, ann_span) in &decl.annotations {
                    match annotation.name {
                        "runtimeProperty"
                            if !matches!(decl.item, ast::Item::Let(_))
                                || !matches!(
                                    &*annotation.args,
                                    [
                                        (ast::Expr::Constant(ast::Constant::String(_)), _),
                                        (ast::Expr::Constant(ast::Constant::String(_)), _)
                                    ]
                                ) =>
                        {
                            self.reject(SemanticError::InvalidRuntimeProperty, *ann_span);
                        }
                        "hint"
                            if !matches!(decl.item, ast::Item::Let(_))
                                || !matches!(
                                    &*annotation.args,
                                    [(ast::Expr::Constant(ast::Constant::String(_)), _)]
                                ) =>
                        {
                            self.reject(SemanticError::InvalidHint, *ann_span);
                        }
                        "if" if annotation.args.len() != 1
                            || !matches!(decl.item, ast::Item::Function(_)) =>
                        {
                            self.reject(SemanticError::InvalidConditionAnnotation, *ann_span);
                        }
                        _ => {}
                    }
                }
                if decl
                    .annotations
                    .iter()
                    .filter(|(ann, _)| ann.name == "if")
                    .count()
                    > 1
                {
                    self.reject(SemanticError::DuplicateConditionAnnotation, *span);
                }
                match &decl.item {
                    ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) => {
                        if !aggregate.type_params.is_empty() {
                            self.reject(SemanticError::UnsupportedGenericType, aggregate.name.1);
                        }
                        if let Some(typ) = &aggregate.extends {
                            self.typ(typ);
                        }
                        self.aggregates
                            .push(matches!(decl.item, ast::Item::Class(_)));
                    }
                    ast::Item::Let(field) => self.typ(&field.typ),
                    _ => {}
                }
            }
            AstNode::Stmt((ast::Stmt::Let { name, typ, .. }, _)) => {
                if !self.locals.insert(name.0) {
                    self.reject(SemanticError::DuplicateBinding, name.1);
                }
                if let Some(typ) = typ {
                    self.typ(typ);
                } else {
                    self.reject(SemanticError::MissingLocalType, name.1);
                }
            }
            AstNode::Pattern((_, span)) => {
                self.reject(SemanticError::UnsupportedPatternBinding, *span);
            }
            AstNode::Stmt((ast::Stmt::ForIn { .. }, span)) => {
                self.reject(SemanticError::UnsupportedForIn, *span);
            }
            AstNode::Expr((expr, span)) => match expr {
                ast::Expr::Lambda { .. } | ast::Expr::InterpolatedString(_) => {
                    self.reject(SemanticError::UnsupportedCommunityExpression, *span);
                }
                ast::Expr::ArrayLit(items) if items.is_empty() => {
                    self.reject(SemanticError::UnsupportedEmptyArrayLiteral, *span);
                }
                ast::Expr::DynCast { typ, .. } | ast::Expr::New { typ, .. } => self.typ(typ),
                ast::Expr::Call {
                    expr, type_args, ..
                } => {
                    if !type_args.is_empty() && !matches!(expr.0, ast::Expr::Ident("Cast")) {
                        self.reject(SemanticError::UnsupportedGenericCall, *span);
                    }
                    if matches!(expr.0, ast::Expr::Ident("Cast")) && type_args.len() != 1 {
                        self.reject(SemanticError::InvalidCastTypeArguments, *span);
                    }
                    for typ in type_args {
                        self.typ(typ);
                    }
                }
                _ => {}
            },
            _ => {}
        }
        Ok(())
    }

    fn post_visit_node(
        &mut self,
        node: AstNode<'_, 'src, ast::WithSpan>,
    ) -> Result<(), Self::Error> {
        if matches!(
            node,
            AstNode::ItemDecl((
                ast::ItemDecl {
                    item: ast::Item::Class(_) | ast::Item::Struct(_),
                    ..
                },
                _
            ))
        ) {
            self.aggregates.pop();
        }
        Ok(())
    }
}

pub fn overrides<'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    files: &HashSet<FileId>,
    overrides: &HashSet<Span>,
    errors: &mut Vec<Error>,
) {
    for &id in unit.classes.keys() {
        let Some(definition) = symbols[id].schema().as_aggregate() else {
            continue;
        };
        for entry in definition.methods().iter() {
            let method = entry.func();
            let Some(span) = method.span().filter(|span| files.contains(&span.file)) else {
                continue;
            };
            let explicit = overrides.contains(&span);
            if method.flags().is_callback() {
                if explicit {
                    errors.push(Error::semantic(SemanticError::EventOverride, span));
                }
                let incompatible = symbols
                    .base_iter_with_self(id)
                    .skip(1)
                    .flat_map(|(_, base)| base.methods().by_name::<str>(entry.name()))
                    .any(|base| {
                        base.func().flags().is_callback()
                            && base
                                .func()
                                .type_()
                                .param_types()
                                .eq(method.type_().param_types())
                            && base.func().type_().return_type() != method.type_().return_type()
                    });
                if incompatible {
                    errors.push(Error::semantic(
                        SemanticError::IncompatibleBaseCallback,
                        span,
                    ));
                }
                continue;
            }
            let base = symbols
                .base_iter_with_self(id)
                .skip(1)
                .flat_map(|(_, base)| base.methods().by_name::<str>(entry.name()))
                .find(|entry| {
                    !entry.func().flags().is_static()
                        && !entry.func().flags().is_callback()
                        && entry.func().flags().visibility() != Visibility::Private
                        && entry
                            .func()
                            .type_()
                            .param_types()
                            .eq(method.type_().param_types())
                });
            match base {
                Some(base) if !method.flags().is_static() => {
                    if !explicit {
                        errors.push(Error::semantic(SemanticError::MissingOverride, span));
                    }
                    if base
                        .func()
                        .type_()
                        .params()
                        .iter()
                        .zip(method.type_().params())
                        .any(|(base, method)| {
                            let base = base.flags();
                            let method = method.flags();
                            base.is_out() != method.is_out()
                                || base.is_const() != method.is_const()
                                || base.is_optional() != method.is_optional()
                        })
                    {
                        errors.push(Error::semantic(
                            SemanticError::OverrideParameterFlagsMismatch,
                            span,
                        ));
                    }
                    if base.func().flags().is_final() {
                        errors.push(Error::semantic(SemanticError::OverrideFinalMethod, span));
                    }
                    if base.func().type_().return_type() != method.type_().return_type() {
                        errors.push(Error::semantic(
                            SemanticError::OverrideReturnTypeMismatch,
                            span,
                        ));
                    }
                }
                _ if explicit => {
                    errors.push(Error::semantic(SemanticError::OverrideWithoutBase, span));
                }
                _ => {}
            }
        }
    }
    let extensions = unit
        .added_fields
        .keys()
        .map(|id| (FieldId::parent(id), symbols[*id].span()))
        .chain(
            unit.added_methods
                .keys()
                .map(|id| (MethodId::parent(id), symbols[*id].span())),
        );
    for (id, span) in extensions {
        if let Some(span) = span.filter(|span| files.contains(&span.file))
            && symbols[id]
                .schema()
                .as_aggregate()
                .is_some_and(|agg| agg.flags().is_import_only())
        {
            errors.push(Error::semantic(SemanticError::ExtendImportOnlyTarget, span));
        }
    }
    let patches = unit
        .method_wrappers
        .iter()
        .flat_map(|(id, functions)| functions.iter().map(move |function| (*id, function.span)))
        .chain(
            unit.method_replacements
                .iter()
                .map(|(id, function)| (*id, function.span)),
        );
    for (id, span) in patches {
        if files.contains(&span.file)
            && symbols[id].flags().is_callback()
            && symbols[id].type_().return_type()
                != &redscript_compiler_frontend::Type::nullary(
                    redscript_compiler_frontend::predef::VOID,
                )
        {
            errors.push(Error::semantic(
                SemanticError::UnsupportedCallbackPatchReturn,
                span,
            ));
        }
        if files.contains(&span.file)
            && symbols[id.parent()]
                .span()
                .is_some_and(|span| files.contains(&span.file))
        {
            errors.push(Error::semantic(SemanticError::PatchModDefinedClass, span));
        }
    }
}

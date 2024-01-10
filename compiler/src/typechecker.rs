use std::iter;
use std::str::FromStr;

use itertools::{izip, Itertools};
use redscript::ast::{Constant, Expr, Ident, Literal, NameKind, Seq, SourceAst, Span, SwitchCase, TypeName};
use redscript::bundle::{ConstantPool, PoolError, PoolIndex};
use redscript::bytecode::IntrinsicOp;
use redscript::definition::{Class, Definition, Enum, Field, Function, Local, LocalFlags};
use redscript::Ref;
use thiserror::Error;

use crate::diagnostics::{Deprecation, Diagnostic};
use crate::error::{Cause, Error, FunctionMatchError, ResultSpan};
use crate::scope::{FunctionCandidates, Reference, Scope, TypeId, Value};
use crate::symbol::Symbol;

pub struct TypeChecker<'a> {
    pool: &'a mut ConstantPool,
    locals: Vec<PoolIndex<Local>>,
    diagnostics: Vec<Diagnostic>,
    permissive: bool,
}

impl<'a> TypeChecker<'a> {
    pub fn new(pool: &'a mut ConstantPool, permissive: bool) -> TypeChecker<'a> {
        TypeChecker {
            pool,
            locals: vec![],
            diagnostics: vec![],
            permissive,
        }
    }

    pub fn check(
        &mut self,
        expr: &Expr<SourceAst>,
        expected: Option<&TypeId>,
        scope: &mut Scope,
    ) -> Result<Expr<TypedAst>, Error> {
        let res = match expr {
            Expr::Ident(name, span) => match scope.resolve_reference(name.clone()) {
                Ok(reference) => Expr::Ident(reference, *span),
                Err(err) if self.permissive => {
                    self.report(err.with_span(*span))?;
                    Expr::Null(*span)
                }
                Err(err) => return Err(err.with_span(*span)),
            },
            Expr::Constant(cons, pos) => {
                let cons = match expected {
                    Some(type_) => {
                        let type_name = type_.pretty(self.pool)?;
                        match cons {
                            Constant::I32(i) if type_name == TypeName::FLOAT.name() => Constant::F32(*i as f32),
                            Constant::I32(i) if type_name == TypeName::DOUBLE.name() => Constant::F64(f64::from(*i)),
                            Constant::I32(i) if type_name == TypeName::INT64.name() => Constant::I64((*i).into()),
                            Constant::U32(i) if type_name == TypeName::UINT64.name() => Constant::U64((*i).into()),
                            Constant::F32(i) if type_name == TypeName::DOUBLE.name() => Constant::F64((*i).into()),
                            other => other.clone(),
                        }
                    }
                    None => cons.clone(),
                };
                Expr::Constant(cons, *pos)
            }
            Expr::ArrayLit(exprs, _, span) => {
                if exprs.is_empty() {
                    match expected {
                        Some(TypeId::Array(inner)) => Expr::ArrayLit([].into(), Some(inner.clone()), *span),
                        Some(type_) => {
                            let cause = Cause::TypeError(Ident::from_static("array"), type_.pretty(self.pool)?);
                            return Err(cause.with_span(*span));
                        }
                        None => return Err(Cause::TypeAnnotationRequired.with_span(*span)),
                    }
                } else {
                    let mut checked = Vec::with_capacity(exprs.len());
                    for expr in exprs.iter() {
                        let val = match expected {
                            Some(TypeId::Array(elem)) => self.check_and_convert(expr, elem, scope)?,
                            _ => self.check(expr, None, scope)?,
                        };
                        checked.push(val);
                    }

                    let typ: Result<TypeId, Error> = checked
                        .iter()
                        .skip(1)
                        .try_fold(type_of(&checked[0], scope, self.pool)?, |acc, expr| {
                            lub(acc, type_of(expr, scope, self.pool)?, self.pool).with_span(*span)
                        });

                    Expr::ArrayLit(checked.into_boxed_slice(), Some(typ?.into()), *span)
                }
            }
            Expr::InterpolatedString(prefix, parts, pos) => {
                let mut checked = Vec::with_capacity(parts.len());
                for (part, str) in parts {
                    checked.push((self.check(part, None, scope)?, str.clone()));
                }
                Expr::InterpolatedString(prefix.clone(), checked, *pos)
            }
            Expr::Declare(name, type_, init, span) => {
                let (initializer, type_) = match (type_, init) {
                    (None, None) => return Err(Cause::TypeAnnotationRequired.with_span(*span)),
                    (None, Some(expr)) => {
                        let checked = self.check(expr, None, scope)?;
                        let type_ = type_of(&checked, scope, self.pool)?;
                        (Some(checked), type_)
                    }
                    (Some(type_name), None) => (None, scope.resolve_type(type_name, self.pool).with_span(*span)?),
                    (Some(type_name), Some(expr)) => {
                        let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                        let checked = self.check_and_convert(expr, &type_, scope)?;
                        (Some(checked), type_)
                    }
                };
                let local = self.add_local(name.clone(), &type_, scope).with_span(*span)?;
                Expr::Declare(local, Some(type_.into()), initializer.map(Box::new), *span)
            }
            Expr::Cast(type_name, expr, span) => {
                let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                let checked = self.check(expr, None, scope)?;
                if let TypeId::WeakRef(inner) = type_of(&checked, scope, self.pool)? {
                    let converted = insert_conversion(checked, &TypeId::Ref(inner), Conversion::WeakRefToRef);
                    Expr::Cast(type_, Box::new(converted), *span)
                } else {
                    Expr::Cast(type_, Box::new(checked), *span)
                }
            }
            Expr::Assign(lhs, rhs, span) => {
                let lhs_typed = self.check(lhs, None, scope)?;
                let type_ = type_of(&lhs_typed, scope, self.pool)?;
                let rhs_typed = self.check_and_convert(rhs, &type_, scope)?;
                Expr::Assign(Box::new(lhs_typed), Box::new(rhs_typed), *span)
            }
            Expr::Call(name, type_args, args, span) => {
                let expected = match &type_args[..] {
                    [target] => Some(scope.resolve_type(target, self.pool).with_span(*span)?),
                    _ => expected.cloned(),
                };
                if let Ok(intrinsic) = IntrinsicOp::from_str(name.as_ref()) {
                    self.check_intrinsic(intrinsic, args, expected.as_ref(), scope, *span)?
                } else {
                    let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                    let match_ =
                        self.resolve_overload(name.clone(), candidates, args.iter(), expected.as_ref(), scope, *span)?;
                    Expr::Call(
                        Callable::Function(match_.index),
                        [].into(),
                        match_.args.into_boxed_slice(),
                        *span,
                    )
                }
            }
            Expr::MethodCall(context, name, args, span) => {
                let checked_context = self.check(context, None, scope)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;
                let class = match type_.unwrapped() {
                    TypeId::Struct(class) | TypeId::Class(class) => *class,
                    type_ => return Err(Cause::InvalidMemberAccess(type_.pretty(self.pool)?).with_span(*span)),
                };
                let candidates = scope.resolve_method(name.clone(), class, self.pool).with_span(*span)?;
                let match_ = self.resolve_overload(name.clone(), candidates, args.iter(), expected, scope, *span)?;

                let converted_context = if let TypeId::WeakRef(inner) = type_ {
                    insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef)
                } else {
                    checked_context
                };
                Expr::MethodCall(Box::new(converted_context), match_.index, match_.args, *span)
            }
            Expr::BinOp(lhs, rhs, op, span) => {
                let name = Ident::from_static(op.into());
                let args = IntoIterator::into_iter([lhs.as_ref(), rhs.as_ref()]);
                let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, *span)?;
                Expr::Call(
                    Callable::Function(match_.index),
                    [].into(),
                    match_.args.into_boxed_slice(),
                    *span,
                )
            }
            Expr::UnOp(expr, op, span) => {
                let name = Ident::from_static(op.into());
                let args = iter::once(expr.as_ref());
                let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, *span)?;
                Expr::Call(
                    Callable::Function(match_.index),
                    [].into(),
                    match_.args.into_boxed_slice(),
                    *span,
                )
            }
            Expr::Member(context, name, span) => {
                let checked_context = self.check(context, None, scope)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;

                let member = match type_.unwrapped() {
                    TypeId::Class(class) => {
                        let field = Scope::resolve_field(name.clone(), *class, self.pool).with_span(*span)?;
                        Member::ClassField(field)
                    }
                    TypeId::Struct(class) => {
                        let field = Scope::resolve_field(name.clone(), *class, self.pool).with_span(*span)?;
                        Member::StructField(field)
                    }
                    TypeId::Enum(enum_) => {
                        let member = scope
                            .resolve_enum_member(name.clone(), *enum_, self.pool)
                            .with_span(*span)?;
                        Member::EnumMember(*enum_, member)
                    }
                    type_ => return Err(Cause::InvalidMemberAccess(type_.pretty(self.pool)?).with_span(*span)),
                };
                let converted_context = if let TypeId::WeakRef(inner) = type_ {
                    insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef)
                } else {
                    checked_context
                };
                Expr::Member(Box::new(converted_context), member, *span)
            }
            Expr::ArrayElem(expr, idx, span) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(*span)?;
                let checked_expr = self.check(expr, None, scope)?;
                let checked_idx = self.check_and_convert(idx, &idx_type, scope)?;
                Expr::ArrayElem(Box::new(checked_expr), Box::new(checked_idx), *span)
            }
            Expr::New(type_name, args, span) => {
                let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                match type_ {
                    TypeId::Class(class_idx) => {
                        if self.pool.class(class_idx)?.flags.is_abstract() {
                            return Err(Cause::InstantiatingAbstract(type_name.pretty()).with_span(*span));
                        }
                        if !args.is_empty() {
                            return Err(Cause::InvalidArgCount(type_name.pretty(), 0).with_span(*span));
                        }
                        Expr::New(type_, [].into(), *span)
                    }
                    TypeId::Struct(class_idx) => {
                        let fields = self.pool.class(class_idx)?.fields.clone();
                        if fields.len() != args.len() {
                            return Err(Cause::InvalidArgCount(type_name.pretty(), fields.len()).with_span(*span));
                        }
                        let mut checked_args = Vec::with_capacity(args.len());
                        for (arg, field_idx) in args.iter().zip(&fields) {
                            let field = self.pool.field(*field_idx)?;
                            let field_type = scope.resolve_type_from_pool(field.type_, self.pool).with_span(*span)?;
                            let checked_arg = self.check_and_convert(arg, &field_type, scope)?;
                            checked_args.push(checked_arg);
                        }
                        Expr::New(type_, checked_args.into_boxed_slice(), *span)
                    }
                    _ => {
                        return Err(Cause::UnsupportedOperation("constructing", type_name.pretty()).with_span(*span));
                    }
                }
            }
            Expr::Return(None, span) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                match fun.return_type {
                    Some(type_idx) => {
                        let type_ = scope.resolve_type_from_pool(type_idx, self.pool).with_span(*span)?;
                        return Err(Cause::UnexpectedVoidReturn(type_.pretty(self.pool)?).with_span(*span));
                    }
                    None => Expr::Return(None, *span),
                }
            }
            Expr::Return(Some(expr), span) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let expected = scope.resolve_type_from_pool(ret_type, self.pool).with_span(*span)?;
                    let checked = self.check_and_convert(expr, &expected, scope)?;
                    Expr::Return(Some(Box::new(checked)), *span)
                } else {
                    return Err(Cause::UnexpectedValueReturn.with_span(*span));
                }
            }
            Expr::Seq(seq) => Expr::Seq(self.check_seq(seq, scope)?),
            Expr::Switch(matched, cases, default, span) => {
                let checked_matched = self.check(matched, None, scope)?;
                let matched_type = type_of(&checked_matched, scope, self.pool).ok();
                let mut checked_cases = Vec::with_capacity(cases.len());
                for case in cases {
                    let matcher = self.check(&case.matcher, matched_type.as_ref(), scope)?;
                    let body = self.check_seq(&case.body, &mut scope.clone())?;
                    checked_cases.push(SwitchCase { matcher, body });
                }
                let default = default
                    .as_ref()
                    .map_or_else(|| Ok(None), |body| self.check_seq(body, &mut scope.clone()).map(Some))?;

                Expr::Switch(Box::new(checked_matched), checked_cases, default, *span)
            }
            Expr::Goto(target, span) => Expr::Goto(*target, *span),
            Expr::If(cond, if_, else_, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope)?;
                let checked_if = self.check_seq(if_, &mut scope.clone())?;
                let checked_else = else_
                    .as_ref()
                    .map_or_else(|| Ok(None), |body| self.check_seq(body, &mut scope.clone()).map(Some))?;

                Expr::If(Box::new(checked_cond), checked_if, checked_else, *span)
            }
            Expr::Conditional(cond, true_, false_, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope)?;

                let checked_true = self.check(true_, None, scope)?;
                let if_type = type_of(&checked_true, scope, self.pool)?;
                let checked_false = self.check(false_, Some(&if_type), scope)?;

                Expr::Conditional(
                    Box::new(checked_cond),
                    Box::new(checked_true),
                    Box::new(checked_false),
                    *span,
                )
            }
            Expr::While(cond, body, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope)?;
                let checked_body = self.check_seq(body, &mut scope.clone())?;

                Expr::While(Box::new(checked_cond), checked_body, *span)
            }
            Expr::ForIn(name, array, body, span) => {
                let array = self.check(array, None, scope)?;
                match type_of(&array, scope, self.pool)? {
                    TypeId::Array(inner) => {
                        let mut local_scope = scope.clone();
                        let local = self
                            .add_local(name.clone(), &inner, &mut local_scope)
                            .with_span(*span)?;
                        let body = self.check_seq(body, &mut local_scope)?;
                        Expr::ForIn(local, Box::new(array), body, *span)
                    }
                    other => {
                        let cause = Cause::TypeError(other.pretty(self.pool)?, Ident::from_static("array"));
                        return Err(cause.with_span(*span));
                    }
                }
            }
            Expr::This(span) => Expr::This(*span),
            Expr::Super(span) => Expr::Super(*span),
            Expr::Break(span) => Expr::Break(*span),
            Expr::Null(span) => Expr::Null(*span),
        };
        Ok(res)
    }

    pub fn check_seq(&mut self, seq: &Seq<SourceAst>, scope: &mut Scope) -> Result<Seq<TypedAst>, Error> {
        let mut exprs = Vec::with_capacity(seq.exprs.len());
        for expr in &seq.exprs {
            exprs.push(self.check(expr, None, scope)?);
        }
        Ok(Seq { exprs })
    }

    #[allow(clippy::match_same_arms)]
    fn check_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: &[Expr<SourceAst>],
        expected: Option<&TypeId>,
        scope: &mut Scope,
        span: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        if args.len() != intrinsic.arg_count().into() {
            let cause = Cause::InvalidArgCount(Ident::from_static(intrinsic.into()), intrinsic.arg_count() as usize);
            return Err(cause.with_span(span));
        }
        let first_arg = self.check(&args[0], None, scope)?;
        let first_arg_type = type_of(&first_arg, scope, self.pool)?;
        let mut checked_args = vec![];
        let type_ = match (intrinsic, first_arg_type) {
            (IntrinsicOp::Equals | IntrinsicOp::NotEquals, arg_type) => {
                let snd_arg = self.check(&args[1], Some(&arg_type), scope)?;
                if lub(arg_type, type_of(&snd_arg, scope, self.pool)?, self.pool).is_err() {
                    self.diagnostics
                        .push(Diagnostic::Deprecation(Deprecation::UnrelatedTypeEquals, span));
                };
                checked_args.push(first_arg);
                checked_args.push(snd_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayClear, TypeId::Array(_)) => {
                checked_args.push(first_arg);
                TypeId::Void
            }
            (IntrinsicOp::ArraySize, TypeId::Array(_)) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayResize, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                *elem
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                *elem
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                *elem
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope)?);
                checked_args.push(self.check_and_convert(&args[2], &elem, scope)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(_)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayLast, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                *elem
            }
            (IntrinsicOp::ToString, _) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::STRING, self.pool).with_span(span)?
            }
            (IntrinsicOp::EnumInt, TypeId::Enum(_)) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?
            }
            (IntrinsicOp::IntEnum, _) if expected.is_some() => {
                checked_args.push(first_arg);
                if let Some(TypeId::Enum(idx)) = expected {
                    TypeId::Enum(*idx)
                } else {
                    let cause = Cause::TypeError(Ident::from_static("enum"), expected.unwrap().pretty(self.pool)?);
                    return Err(cause.with_span(span));
                }
            }
            (IntrinsicOp::ToVariant, _) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::VARIANT, self.pool).with_span(span)?
            }
            (IntrinsicOp::FromVariant, TypeId::Variant) if expected.is_some() => {
                checked_args.push(first_arg);
                expected.unwrap().clone()
            }
            (IntrinsicOp::VariantTypeName, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::CNAME, self.pool).with_span(span)?
            }
            (IntrinsicOp::VariantIsRef, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::VariantIsArray, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::AsRef, type_) => {
                checked_args.push(first_arg);
                TypeId::ScriptRef(Box::new(type_))
            }
            (IntrinsicOp::Deref, TypeId::ScriptRef(inner)) => {
                checked_args.push(first_arg);
                *inner
            }
            (IntrinsicOp::IsDefined, TypeId::Ref(_) | TypeId::WeakRef(_) | TypeId::Null | TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (_, type_) => return Err(Cause::InvalidIntrinsicUse(intrinsic, type_.pretty(self.pool)?).with_span(span)),
        };

        Ok(Expr::Call(
            Callable::Intrinsic(intrinsic, type_),
            [].into(),
            checked_args.into_boxed_slice(),
            span,
        ))
    }

    pub fn check_and_convert(
        &mut self,
        expr: &Expr<SourceAst>,
        to: &TypeId,
        scope: &mut Scope,
    ) -> Result<Expr<TypedAst>, Error> {
        let checked = self.check(expr, Some(to), scope)?;
        let from = type_of(&checked, scope, self.pool)?;
        if let Some(conversion) = find_conversion(&from, to, self.pool)? {
            Ok(insert_conversion(checked, to, conversion))
        } else {
            self.report(Cause::TypeError(from.pretty(self.pool)?, to.pretty(self.pool)?).with_span(expr.span()))?;
            Ok(checked)
        }
    }

    fn resolve_overload<'b>(
        &mut self,
        name: Ident,
        overloads: FunctionCandidates,
        args: impl ExactSizeIterator<Item = &'b Expr<SourceAst>> + Clone,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        span: Span,
    ) -> Result<FunctionMatch, Error> {
        let arg_count = args.len();
        let mut eligible = vec![];
        let mut overload_errors = vec![];

        for overload in &overloads.functions {
            match Self::validate_call(*overload, arg_count, scope, self.pool, span) {
                Ok(res) => eligible.push(res),
                Err(MatcherError::MatchError(err)) => overload_errors.push(err),
                Err(MatcherError::Other(err)) => return Err(err),
            }
        }

        let match_ = match eligible.into_iter().exactly_one() {
            Ok((fun_index, types)) => {
                let checked_args: Vec<_> = args
                    .clone()
                    .zip(&types)
                    .map(|(arg, typ)| self.check(arg, Some(typ), scope))
                    .try_collect()?;

                match Self::validate_args(fun_index, &checked_args, &types, expected, scope, self.pool, span) {
                    Ok(conversions) => Ok(FunctionMatch::new(fun_index, checked_args, conversions)),
                    Err(MatcherError::MatchError(err)) => {
                        overload_errors.push(err);
                        Err(Cause::NoMatchingOverload(name, overload_errors.into_boxed_slice()).with_span(span))
                    }
                    Err(MatcherError::Other(err)) => Err(err),
                }
            }
            Err(eligible) => {
                let checked_args: Vec<_> = args.clone().map(|expr| self.check(expr, None, scope)).try_collect()?;

                let mut matches = vec![];
                for (fun_index, types) in eligible {
                    match Self::validate_args(fun_index, &checked_args, &types, expected, scope, self.pool, span) {
                        Ok(convs) => matches.push((fun_index, convs)),
                        Err(MatcherError::MatchError(err)) => overload_errors.push(err),
                        Err(MatcherError::Other(err)) => return Err(err),
                    }
                }

                let mut it = matches.into_iter();
                match it.next() {
                    None => Err(Cause::NoMatchingOverload(name, overload_errors.into_boxed_slice()).with_span(span)),
                    Some((fun_index, convs)) => Ok(FunctionMatch::new(fun_index, checked_args, convs)),
                }
            }
        };
        match match_ {
            Ok(match_) => Ok(match_),
            Err(err) if self.permissive => {
                self.report(err)?;

                let dummy_args: Vec<_> = args.map(|expr| self.check(expr, None, scope)).try_collect()?;
                let convs = iter::repeat(ArgConversion::identity()).take(dummy_args.len()).collect();
                Ok(FunctionMatch::new(overloads.functions[0], dummy_args, convs))
            }
            Err(err) => Err(err),
        }
    }

    fn validate_call(
        fun_index: PoolIndex<Function>,
        arg_count: usize,
        scope: &Scope,
        pool: &ConstantPool,
        span: Span,
    ) -> Result<(PoolIndex<Function>, Vec<TypeId>), MatcherError> {
        let fun = pool.function(fun_index)?;
        let params = fun
            .parameters
            .iter()
            .map(|idx| pool.parameter(*idx).map_err(Error::PoolError));
        let min_params = params.clone().filter_ok(|param| !param.flags.is_optional()).count();

        if arg_count < min_params || arg_count > fun.parameters.len() {
            let err = FunctionMatchError::ArgumentCountMismatch {
                given: arg_count,
                min: min_params,
                max: fun.parameters.len(),
            };
            return Err(err.into());
        }

        let types = params
            .map(|res| res.and_then(|param| scope.resolve_type_from_pool(param.type_, pool).with_span(span)))
            .try_collect()?;
        Ok((fun_index, types))
    }

    fn validate_args(
        fun_index: PoolIndex<Function>,
        args: &[Expr<TypedAst>],
        param_types: &[TypeId],
        wanted_ret_type: Option<&TypeId>,
        scope: &Scope,
        pool: &ConstantPool,
        span: Span,
    ) -> Result<Vec<ArgConversion>, MatcherError> {
        let fun = pool.function(fun_index)?;
        if fun.flags.is_cast() {
            if let Some(wanted_ret_type) = wanted_ret_type {
                let type_idx = fun.return_type.ok_or(Cause::VoidCannotBeUsed).with_span(span)?;
                let ret_type = scope.resolve_type_from_pool(type_idx, pool).with_span(span)?;

                if find_conversion(&ret_type, wanted_ret_type, pool)?.is_none() {
                    let given = wanted_ret_type.pretty(pool)?;
                    let expected = ret_type.pretty(pool)?;
                    return Err(FunctionMatchError::ReturnMismatch { given, expected }.into());
                }
            }
        }

        izip!(args, param_types, 0..args.len())
            .map(|(arg, param_typ, index)| {
                let arg_typ = type_of(arg, scope, pool)?;

                if let Some(conv) = find_conversion(&arg_typ, param_typ, pool)? {
                    Ok(ArgConversion::new(conv, param_typ.clone()))
                } else {
                    let given = arg_typ.pretty(pool)?;
                    let expected = param_typ.pretty(pool)?;
                    Err(FunctionMatchError::ParameterMismatch { given, expected, index }.into())
                }
            })
            .collect()
    }

    fn add_local(&mut self, name: Ident, type_: &TypeId, scope: &mut Scope) -> Result<PoolIndex<Local>, Cause> {
        let idx = self.locals.len();
        let name_mangled = Ref::from(format!("{name}$local${idx}"));
        let name_idx = self.pool.names.add(name_mangled);

        let local = Local::new(scope.get_type_index(type_, self.pool)?, LocalFlags::new());
        let local_def = Definition::local(name_idx, scope.function.unwrap().cast(), local);
        let local_idx = self.pool.add_definition(local_def);
        scope.add_local(name, local_idx);
        self.locals.push(local_idx);
        Ok(local_idx)
    }

    fn report(&mut self, err: Error) -> Result<(), Error> {
        self.diagnostics.push(Diagnostic::from_error(err)?);
        Ok(())
    }

    pub fn into_inner(self) -> (Vec<Diagnostic>, Vec<PoolIndex<Local>>) {
        (self.diagnostics, self.locals)
    }

    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}

pub fn type_of(expr: &Expr<TypedAst>, scope: &Scope, pool: &ConstantPool) -> Result<TypeId, Error> {
    let res = match expr {
        Expr::Ident(reference, span) => match reference {
            Reference::Value(Value::Local(idx)) => scope
                .resolve_type_from_pool(pool.local(*idx)?.type_, pool)
                .with_span(*span)?,
            Reference::Value(Value::Parameter(idx)) => scope
                .resolve_type_from_pool(pool.parameter(*idx)?.type_, pool)
                .with_span(*span)?,
            Reference::Symbol(Symbol::Class(idx, _)) => TypeId::Class(*idx),
            Reference::Symbol(Symbol::Struct(idx, _)) => TypeId::Struct(*idx),
            Reference::Symbol(Symbol::Enum(idx)) => TypeId::Enum(*idx),
            Reference::Symbol(Symbol::Functions(_)) => return Err(Cause::UnexpectedToken("function").with_span(*span)),
        },
        Expr::Constant(cons, span) => match cons {
            Constant::String(Literal::String, _) => scope.resolve_type(&TypeName::STRING, pool).with_span(*span)?,
            Constant::String(Literal::Name, _) => scope.resolve_type(&TypeName::CNAME, pool).with_span(*span)?,
            Constant::String(Literal::Resource, _) => scope.resolve_type(&TypeName::RESOURCE, pool).with_span(*span)?,
            Constant::String(Literal::TweakDbId, _) => {
                scope.resolve_type(&TypeName::TWEAKDB_ID, pool).with_span(*span)?
            }
            Constant::F32(_) => scope.resolve_type(&TypeName::FLOAT, pool).with_span(*span)?,
            Constant::F64(_) => scope.resolve_type(&TypeName::DOUBLE, pool).with_span(*span)?,
            Constant::I32(_) => scope.resolve_type(&TypeName::INT32, pool).with_span(*span)?,
            Constant::I64(_) => scope.resolve_type(&TypeName::INT64, pool).with_span(*span)?,
            Constant::U32(_) => scope.resolve_type(&TypeName::UINT32, pool).with_span(*span)?,
            Constant::U64(_) => scope.resolve_type(&TypeName::UINT64, pool).with_span(*span)?,
            Constant::Bool(_) => scope.resolve_type(&TypeName::BOOL, pool).with_span(*span)?,
        },
        Expr::ArrayLit(_, type_, _) => TypeId::Array(type_.clone().unwrap()),
        Expr::InterpolatedString(_, _, span) => scope.resolve_type(&TypeName::STRING, pool).with_span(*span)?,
        Expr::Cast(type_, expr, _) => match type_of(expr, scope, pool)? {
            TypeId::WeakRef(_) | TypeId::Ref(_) => TypeId::Ref(Box::new(type_.clone())),
            TypeId::ScriptRef(_) => TypeId::ScriptRef(Box::new(type_.clone())),
            _ => type_.clone(),
        },
        Expr::Call(Callable::Function(index), _, _, span) => match pool.function(*index)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool).with_span(*span)?,
        },
        Expr::Call(Callable::Intrinsic(_, type_), _, _, _) => type_.clone(),
        Expr::MethodCall(_, fun, _, span) => match pool.function(*fun)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool).with_span(*span)?,
        },
        Expr::Member(_, member, span) => match member {
            Member::StructField(field) | Member::ClassField(field) => scope
                .resolve_type_from_pool(pool.field(*field)?.type_, pool)
                .with_span(*span)?,
            Member::EnumMember(enum_, _) => TypeId::Enum(*enum_),
        },
        Expr::ArrayElem(expr, _, span) => match type_of(expr, scope, pool)? {
            TypeId::StaticArray(inner, _) | TypeId::Array(inner) => *inner,
            type_ => return Err(Cause::UnsupportedOperation("indexing", type_.pretty(pool)?).with_span(*span)),
        },
        Expr::New(type_, _, span) => match type_ {
            TypeId::Struct(s) => TypeId::Struct(*s),
            TypeId::Class(s) => TypeId::Ref(Box::new(TypeId::Class(*s))),
            type_ => return Err(Cause::UnsupportedOperation("constructing", type_.pretty(pool)?).with_span(*span)),
        },
        Expr::Conditional(_, lhs, rhs, span) => {
            let lt = type_of(lhs, scope, pool)?;
            let rt = type_of(rhs, scope, pool)?;
            return lub(lt, rt, pool).with_span(*span);
        }
        Expr::This(span) => match scope.this {
            Some(class_idx) => TypeId::Ref(Box::new(TypeId::Class(class_idx))),
            None => return Err(Cause::UnexpectedThis.with_span(*span)),
        },
        Expr::Super(span) => match scope.this {
            Some(class_idx) => {
                let base_idx = pool.class(class_idx)?.base;
                TypeId::Ref(Box::new(TypeId::Class(base_idx)))
            }
            None => return Err(Cause::UnexpectedThis.with_span(*span)),
        },
        Expr::Null(_) => TypeId::Null,
        Expr::BinOp(_, _, _, span) => return Err(Cause::UnsupportedFeature("BinOp").with_span(*span)),
        Expr::UnOp(_, _, span) => return Err(Cause::UnsupportedFeature("UnOp").with_span(*span)),
        Expr::Declare(_, _, _, _)
        | Expr::Assign(_, _, _)
        | Expr::Seq(_)
        | Expr::If(_, _, _, _)
        | Expr::Switch(_, _, _, _)
        | Expr::While(_, _, _)
        | Expr::ForIn(_, _, _, _)
        | Expr::Return(_, _)
        | Expr::Goto(_, _)
        | Expr::Break(_) => TypeId::Void,
    };
    Ok(res)
}

pub fn collect_supertypes(
    class_idx: PoolIndex<Class>,
    pool: &ConstantPool,
) -> Result<Vec<PoolIndex<Class>>, PoolError> {
    if class_idx.is_undefined() {
        Ok(vec![])
    } else {
        let class = pool.class(class_idx)?;
        let mut res = collect_supertypes(class.base, pool)?;
        res.push(class_idx);
        Ok(res)
    }
}

// least upper type bound
pub fn lub(a: TypeId, b: TypeId, pool: &ConstantPool) -> Result<TypeId, Cause> {
    fn lub_class(a: PoolIndex<Class>, b: PoolIndex<Class>, pool: &ConstantPool) -> Result<PoolIndex<Class>, Cause> {
        let subs_a = collect_supertypes(a, pool)?;
        let subs_b = collect_supertypes(b, pool)?;
        let res = subs_a.into_iter().zip(subs_b).take_while(|(a, b)| a == b).last();
        res.map(|(i, _)| i).ok_or_else(|| {
            Cause::UnificationFailed(
                Ident::from_heap(pool.def_name(a).unwrap()),
                Ident::from_heap(pool.def_name(b).unwrap()),
            )
        })
    }

    if a == b {
        Ok(a)
    } else {
        match (a, b) {
            (TypeId::Ref(a), TypeId::Null) | (TypeId::Null, TypeId::Ref(a)) => Ok(TypeId::Ref(a)),
            (TypeId::WeakRef(a), TypeId::Null) | (TypeId::Null, TypeId::WeakRef(a)) => Ok(TypeId::WeakRef(a)),
            (TypeId::Ref(a), TypeId::Ref(b)) => Ok(TypeId::Ref(Box::new(lub(*a, *b, pool)?))),
            (TypeId::WeakRef(a), TypeId::WeakRef(b)) => Ok(TypeId::WeakRef(Box::new(lub(*a, *b, pool)?))),
            (TypeId::Class(a), TypeId::Class(b)) => Ok(TypeId::Class(lub_class(a, b, pool)?)),
            (TypeId::Struct(a), TypeId::Struct(b)) => Ok(TypeId::Struct(lub_class(a, b, pool)?)),
            (a, b) => Err(Cause::UnificationFailed(a.pretty(pool)?, b.pretty(pool)?)),
        }
    }
}

fn find_conversion(from: &TypeId, to: &TypeId, pool: &ConstantPool) -> Result<Option<Conversion>, Error> {
    let result = if from == to {
        Some(Conversion::Identity)
    } else {
        match (from, to) {
            (TypeId::Null, TypeId::Ref(_)) => Some(Conversion::Identity),
            (TypeId::Null, TypeId::WeakRef(_)) => Some(Conversion::RefToWeakRef),
            (TypeId::Class(from), TypeId::Class(_)) => {
                let class = pool.class(*from)?;
                if class.base != PoolIndex::UNDEFINED {
                    find_conversion(&TypeId::Class(class.base), to, pool)?
                } else {
                    None
                }
            }
            (TypeId::Struct(from), TypeId::Struct(_)) => {
                let class = pool.class(*from)?;
                if class.base != PoolIndex::UNDEFINED {
                    find_conversion(&TypeId::Struct(class.base), to, pool)?
                } else {
                    None
                }
            }
            (TypeId::Ref(from), TypeId::Ref(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
            }
            (TypeId::WeakRef(from), TypeId::WeakRef(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
            }
            (TypeId::WeakRef(from), TypeId::Ref(to))
                if find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
            {
                Some(Conversion::WeakRefToRef)
            }
            (TypeId::Ref(from), TypeId::WeakRef(to))
                if find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
            {
                Some(Conversion::RefToWeakRef)
            }
            (from, TypeId::ScriptRef(to)) if find_conversion(from, to, pool)? == Some(Conversion::Identity) => {
                Some(Conversion::ToScriptRef)
            }
            (_, TypeId::Variant) => Some(Conversion::ToVariant),
            _ => None,
        }
    };
    Ok(result)
}

fn insert_conversion(expr: Expr<TypedAst>, type_: &TypeId, conversion: Conversion) -> Expr<TypedAst> {
    let span = expr.span();
    match conversion {
        Conversion::Identity => expr,
        Conversion::RefToWeakRef => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::RefToWeakRef, type_.clone()),
            [].into(),
            [expr].into(),
            span,
        ),
        Conversion::WeakRefToRef => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::WeakRefToRef, type_.clone()),
            [].into(),
            [expr].into(),
            span,
        ),
        Conversion::ToScriptRef => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::AsRef, type_.clone()),
            [].into(),
            [expr].into(),
            span,
        ),
        Conversion::ToVariant => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::ToVariant, type_.clone()),
            [].into(),
            [expr].into(),
            span,
        ),
    }
}

#[derive(Debug, Error)]
enum MatcherError {
    #[error("{0}")]
    MatchError(#[from] FunctionMatchError),
    #[error("{0}")]
    Other(#[from] Error),
}

impl From<PoolError> for MatcherError {
    fn from(err: PoolError) -> Self {
        Self::Other(err.into())
    }
}

#[derive(Debug, Clone)]
struct ArgConversion {
    conversion: Conversion,
    target: TypeId,
}

impl ArgConversion {
    fn identity() -> Self {
        Self::new(Conversion::Identity, TypeId::Void)
    }

    fn new(conversion: Conversion, target: TypeId) -> Self {
        Self { conversion, target }
    }
}

#[derive(Debug)]
pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub args: Vec<Expr<TypedAst>>,
}

impl FunctionMatch {
    fn new(index: PoolIndex<Function>, args: Vec<Expr<TypedAst>>, conversions: Vec<ArgConversion>) -> Self {
        let args = args
            .into_iter()
            .zip(conversions)
            .map(|(expr, conv)| insert_conversion(expr, &conv.target, conv.conversion))
            .collect();
        Self { index, args }
    }
}

#[derive(Debug)]
pub struct TypedAst;

impl NameKind for TypedAst {
    type Callable = Callable;
    type Function = PoolIndex<Function>;
    type Local = PoolIndex<Local>;
    type Member = Member;
    type Reference = Reference;
    type Type = TypeId;
}

pub type TypedExpr = Expr<TypedAst>;

pub trait TypedExprExt {
    fn is_rvalue(&self) -> bool;
}

impl TypedExprExt for TypedExpr {
    fn is_rvalue(&self) -> bool {
        match self {
            Expr::Constant(_, _)
            | Expr::Ident(_, _)
            | Expr::This(_)
            | Expr::Super(_)
            | Expr::Call(Callable::Intrinsic(IntrinsicOp::Deref, _), _, _, _) => false,
            Expr::Member(inner, _, _) | Expr::ArrayElem(inner, _, _) => inner.is_rvalue(),
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Function(PoolIndex<Function>),
    Intrinsic(IntrinsicOp, TypeId),
}

#[derive(Debug)]
pub enum Member {
    ClassField(PoolIndex<Field>),
    StructField(PoolIndex<Field>),
    EnumMember(PoolIndex<Enum>, PoolIndex<i64>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Conversion {
    Identity,
    RefToWeakRef,
    WeakRefToRef,
    ToScriptRef,
    ToVariant,
}

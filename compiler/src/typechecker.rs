use std::iter;
use std::str::FromStr;

use redscript::ast::{Constant, Expr, Ident, Literal, NameKind, Seq, SourceAst, Span, SwitchCase, TypeName};
use redscript::bundle::{ConstantPool, PoolError, PoolIndex};
use redscript::bytecode::IntrinsicOp;
use redscript::definition::{Class, Definition, Enum, Field, Function, Local, LocalFlags};
use redscript::Ref;

use crate::error::{Cause, Error, FunctionResolutionError, ResultSpan};
use crate::scope::{FunctionCandidates, FunctionMatch, Reference, Scope, TypeId, Value};
use crate::symbol::Symbol;
use crate::unit::Diagnostic;

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
        silent: bool,
    ) -> Result<Expr<TypedAst>, Error> {
        let res = match expr {
            Expr::Ident(name, span) => match scope.resolve_reference(name.clone()) {
                Ok(reference) => Expr::Ident(reference, *span),
                Err(err) if self.permissive => {
                    if !silent {
                        self.report(err.with_span(*span))?;
                    }
                    Expr::Null(*span)
                }
                Err(err) => return Err(err.with_span(*span)),
            },
            Expr::Constant(cons, pos) => {
                let cons = match expected {
                    Some(type_) => {
                        let type_name = type_.pretty(self.pool)?;
                        match cons {
                            Constant::I32(i) if type_name == TypeName::FLOAT.pretty() => Constant::F32(*i as f32),
                            Constant::I32(i) if type_name == TypeName::DOUBLE.pretty() => Constant::F64(*i as f64),
                            Constant::I32(i) if type_name == TypeName::INT64.pretty() => Constant::I64((*i).into()),
                            Constant::U32(i) if type_name == TypeName::UINT64.pretty() => Constant::U64((*i).into()),
                            Constant::F32(i) if type_name == TypeName::DOUBLE.pretty() => Constant::F64((*i).into()),
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
                        Some(TypeId::Array(inner)) => Expr::ArrayLit(vec![], Some(*inner.clone()), *span),
                        Some(type_) => {
                            return Err(Cause::type_error("array", type_.pretty(self.pool)?).with_span(*span))
                        }
                        None => return Err(Cause::type_annotation_required().with_span(*span)),
                    }
                } else {
                    let mut checked = Vec::with_capacity(exprs.len());
                    for expr in exprs {
                        let val = match expected {
                            Some(TypeId::Array(elem)) => self.check_and_convert(expr, elem, scope, silent)?,
                            _ => self.check(expr, None, scope, silent)?,
                        };
                        checked.push(val);
                    }

                    let typ: Result<TypeId, Error> = checked
                        .iter()
                        .skip(1)
                        .try_fold(type_of(&checked[0], scope, self.pool)?, |acc, expr| {
                            lub(acc, type_of(expr, scope, self.pool)?, self.pool).with_span(*span)
                        });

                    Expr::ArrayLit(checked, Some(typ?), *span)
                }
            }
            Expr::InterpolatedString(prefix, parts, pos) => {
                let mut checked = Vec::with_capacity(parts.len());
                for (part, str) in parts {
                    checked.push((self.check(part, None, scope, silent)?, str.clone()));
                }
                Expr::InterpolatedString(prefix.clone(), checked, *pos)
            }
            Expr::Declare(name, type_, init, span) => {
                let (initializer, type_) = match (type_, init) {
                    (None, None) => return Err(Cause::type_annotation_required().with_span(*span)),
                    (None, Some(expr)) => {
                        let checked = self.check(expr, None, scope, silent)?;
                        let type_ = type_of(&checked, scope, self.pool)?;
                        (Some(checked), type_)
                    }
                    (Some(type_name), None) => (None, scope.resolve_type(type_name, self.pool).with_span(*span)?),
                    (Some(type_name), Some(expr)) => {
                        let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                        let checked = self.check_and_convert(expr, &type_, scope, silent)?;
                        (Some(checked), type_)
                    }
                };
                let local = self.add_local(name.clone(), &type_, scope).with_span(*span)?;
                Expr::Declare(local, Some(type_), initializer.map(Box::new), *span)
            }
            Expr::Cast(type_name, expr, span) => {
                let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                let checked = self.check(expr, None, scope, silent)?;
                if let TypeId::WeakRef(inner) = type_of(&checked, scope, self.pool)? {
                    let converted = insert_conversion(checked, &TypeId::Ref(inner), Conversion::WeakRefToRef);
                    Expr::Cast(type_, Box::new(converted), *span)
                } else {
                    Expr::Cast(type_, Box::new(checked), *span)
                }
            }
            Expr::Assign(lhs, rhs, span) => {
                let lhs_typed = self.check(lhs, None, scope, silent)?;
                let type_ = type_of(&lhs_typed, scope, self.pool)?;
                let rhs_typed = self.check_and_convert(rhs, &type_, scope, silent)?;
                Expr::Assign(Box::new(lhs_typed), Box::new(rhs_typed), *span)
            }
            Expr::Call(name, args, span) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(name.as_ref()) {
                    self.check_intrinsic(intrinsic, args, expected, scope, silent, *span)?
                } else {
                    let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                    let match_ =
                        self.resolve_overload(name.clone(), candidates, args.iter(), expected, scope, silent, *span)?;
                    Expr::Call(Callable::Function(match_.index), match_.args, *span)
                }
            }
            Expr::MethodCall(context, name, args, span) => {
                let checked_context = self.check(context, None, scope, silent)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;
                let class = match type_.unwrapped() {
                    TypeId::Class(class) => *class,
                    TypeId::Struct(class) => *class,
                    type_ => return Err(Cause::invalid_context(type_.pretty(self.pool)?).with_span(*span)),
                };
                let candidates = scope.resolve_method(name.clone(), class, self.pool).with_span(*span)?;
                let match_ =
                    self.resolve_overload(name.clone(), candidates, args.iter(), expected, scope, silent, *span)?;

                let converted_context = if let TypeId::WeakRef(inner) = type_ {
                    insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef)
                } else {
                    checked_context
                };
                Expr::MethodCall(Box::new(converted_context), match_.index, match_.args, *span)
            }
            Expr::BinOp(lhs, rhs, op, span) => {
                let name = Ident::Static(op.into());
                let args = IntoIterator::into_iter([lhs.as_ref(), rhs.as_ref()]);
                let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, silent, *span)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *span)
            }
            Expr::UnOp(expr, op, span) => {
                let name = Ident::Static(op.into());
                let args = iter::once(expr.as_ref());
                let candidates = scope.resolve_function(name.clone()).with_span(*span)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, silent, *span)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *span)
            }
            Expr::Member(context, name, span) => {
                let checked_context = self.check(context, None, scope, silent)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;

                let member = match type_.unwrapped() {
                    TypeId::Class(class) => {
                        let field = scope.resolve_field(name.clone(), *class, self.pool).with_span(*span)?;
                        Member::ClassField(field)
                    }
                    TypeId::Struct(class) => {
                        let field = scope.resolve_field(name.clone(), *class, self.pool).with_span(*span)?;
                        Member::StructField(field)
                    }
                    TypeId::Enum(enum_) => {
                        let member = scope
                            .resolve_enum_member(name.clone(), *enum_, self.pool)
                            .with_span(*span)?;
                        Member::EnumMember(*enum_, member)
                    }
                    type_ => return Err(Cause::invalid_context(type_.pretty(self.pool)?).with_span(*span)),
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
                let checked_expr = self.check(expr, None, scope, silent)?;
                let checked_idx = self.check_and_convert(idx, &idx_type, scope, silent)?;
                Expr::ArrayElem(Box::new(checked_expr), Box::new(checked_idx), *span)
            }
            Expr::New(type_name, args, span) => {
                let type_ = scope.resolve_type(type_name, self.pool).with_span(*span)?;
                match type_ {
                    TypeId::Class(class_idx) => {
                        if self.pool.class(class_idx)?.flags.is_abstract() {
                            return Err(Cause::class_is_abstract(type_name.pretty()).with_span(*span));
                        }
                        if !args.is_empty() {
                            return Err(Cause::invalid_arg_count(type_name.pretty(), 0).with_span(*span));
                        }
                        Expr::New(type_, vec![], *span)
                    }
                    TypeId::Struct(class_idx) => {
                        let fields = self.pool.class(class_idx)?.fields.clone();
                        if fields.len() != args.len() {
                            return Err(Cause::invalid_arg_count(type_name.mangled(), fields.len()).with_span(*span));
                        }
                        let mut checked_args = Vec::with_capacity(args.len());
                        for (arg, field_idx) in args.iter().zip(fields.iter()) {
                            let field = self.pool.field(*field_idx)?;
                            let field_type = scope.resolve_type_from_pool(field.type_, self.pool).with_span(*span)?;
                            let checked_arg = self.check_and_convert(arg, &field_type, scope, false)?;
                            checked_args.push(checked_arg)
                        }
                        Expr::New(type_, checked_args, *span)
                    }
                    _ => {
                        return Err(Cause::invalid_op(type_name.mangled(), "Constructing").with_span(*span));
                    }
                }
            }
            Expr::Return(None, span) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                match fun.return_type {
                    Some(type_idx) => {
                        let type_ = scope.resolve_type_from_pool(type_idx, self.pool).with_span(*span)?;
                        return Err(Cause::return_type_mismatch(type_.pretty(self.pool)?).with_span(*span));
                    }
                    None => Expr::Return(None, *span),
                }
            }
            Expr::Return(Some(expr), span) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let expected = scope.resolve_type_from_pool(ret_type, self.pool).with_span(*span)?;
                    let checked = self.check_and_convert(expr, &expected, scope, silent)?;
                    Expr::Return(Some(Box::new(checked)), *span)
                } else {
                    return Err(Cause::return_type_mismatch("Void").with_span(*span));
                }
            }
            Expr::Seq(seq) => Expr::Seq(self.check_seq(seq, scope)?),
            Expr::Switch(matched, cases, default, span) => {
                let checked_matched = self.check(matched, None, scope, silent)?;
                let matched_type = type_of(&checked_matched, scope, self.pool).ok();
                let mut checked_cases = Vec::with_capacity(cases.len());
                for case in cases {
                    let matcher = self.check(&case.matcher, matched_type.as_ref(), scope, silent)?;
                    let body = self.check_seq(&case.body, &mut scope.clone())?;
                    checked_cases.push(SwitchCase { matcher, body })
                }
                let default = default
                    .iter()
                    .try_fold(None, |_, body| self.check_seq(body, scope).map(Some))?;

                Expr::Switch(Box::new(checked_matched), checked_cases, default, *span)
            }
            Expr::Goto(target, span) => Expr::Goto(target.clone(), *span),
            Expr::If(cond, if_, else_, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent)?;
                let checked_if = self.check_seq(if_, &mut scope.clone())?;
                let checked_else = else_
                    .iter()
                    .try_fold(None, |_, body| self.check_seq(body, &mut scope.clone()).map(Some))?;

                Expr::If(Box::new(checked_cond), checked_if, checked_else, *span)
            }
            Expr::Conditional(cond, true_, false_, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent)?;

                let checked_true = self.check(true_, None, scope, silent)?;
                let if_type = type_of(&checked_true, scope, self.pool)?;
                let checked_false = self.check(false_, Some(&if_type), scope, silent)?;

                Expr::Conditional(
                    Box::new(checked_cond),
                    Box::new(checked_true),
                    Box::new(checked_false),
                    *span,
                )
            }
            Expr::While(cond, body, span) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool).with_span(*span)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent)?;
                let checked_body = self.check_seq(body, &mut scope.clone())?;

                Expr::While(Box::new(checked_cond), checked_body, *span)
            }
            Expr::ForIn(name, array, body, span) => {
                let array = self.check(array, None, scope, silent)?;
                match type_of(&array, scope, self.pool)? {
                    TypeId::Array(inner) => {
                        let mut local_scope = scope.clone();
                        let local = self
                            .add_local(name.clone(), &inner, &mut local_scope)
                            .with_span(*span)?;
                        let body = self.check_seq(body, &mut local_scope)?;
                        Expr::ForIn(local, Box::new(array), body, *span)
                    }
                    other => return Err(Cause::type_error(other.pretty(self.pool)?, "array").with_span(*span)),
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
            exprs.push(self.check(expr, None, scope, false)?);
        }
        Ok(Seq { exprs })
    }

    fn check_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: &[Expr<SourceAst>],
        expected: Option<&TypeId>,
        scope: &mut Scope,
        silent: bool,
        span: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        if args.len() != intrinsic.arg_count().into() {
            return Err(Cause::invalid_arg_count(intrinsic, intrinsic.arg_count() as usize)).with_span(span);
        }
        let first_arg = self.check(&args[0], None, scope, silent)?;
        let first_arg_type = type_of(&first_arg, scope, self.pool)?;
        let mut checked_args = vec![];
        let type_ = match (intrinsic, first_arg_type) {
            (IntrinsicOp::Equals, arg_type) => {
                checked_args.push(first_arg);
                checked_args.push(self.check(&args[1], Some(&arg_type), scope, silent)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::NotEquals, arg_type) => {
                checked_args.push(first_arg);
                checked_args.push(self.check(&args[1], Some(&arg_type), scope, silent)?);
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
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, silent)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                *elem
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                *elem
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                *elem
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, silent)?);
                checked_args.push(self.check_and_convert(&args[2], &elem, scope, silent)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent)?);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, silent)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(_)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, silent)?);
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
                    return Err(Cause::type_error("Enum", expected.unwrap().pretty(self.pool)?).with_span(span));
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
            (IntrinsicOp::IsDefined, TypeId::Ref(_) | TypeId::WeakRef(_) | TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool).with_span(span)?
            }
            (_, type_) => return Err(Cause::invalid_intrinsic(intrinsic, type_.pretty(self.pool)?).with_span(span)),
        };

        Ok(Expr::Call(Callable::Intrinsic(intrinsic, type_), checked_args, span))
    }

    fn check_and_convert_faily(
        &mut self,
        expr: &Expr<SourceAst>,
        to: &TypeId,
        scope: &mut Scope,
        span: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        let checked = self.check(expr, Some(to), scope, true)?;
        let from = type_of(&checked, scope, self.pool)?;
        let conversion = find_conversion(&from, to, self.pool)?.ok_or_else(|| {
            Error::arg_type_error(from.pretty(self.pool).unwrap(), to.pretty(self.pool).unwrap(), span)
        })?;
        Ok(insert_conversion(checked, to, conversion))
    }

    // version of check that recovers from type errors and logs them as diagnostics instead
    fn check_and_convert(
        &mut self,
        expr: &Expr<SourceAst>,
        to: &TypeId,
        scope: &mut Scope,
        silent: bool,
    ) -> Result<Expr<TypedAst>, Error> {
        let checked = self.check(expr, Some(to), scope, silent)?;
        let from = type_of(&checked, scope, self.pool)?;
        match find_conversion(&from, to, self.pool)? {
            Some(conversion) => Ok(insert_conversion(checked, to, conversion)),
            None => {
                if !silent {
                    let err = Cause::type_error(from.pretty(self.pool)?, to.pretty(self.pool)?).with_span(expr.span());
                    self.report(err)?;
                }
                Ok(checked)
            }
        }
    }

    fn resolve_overload_faily<'b>(
        &mut self,
        name: Ident,
        overloads: FunctionCandidates,
        args: impl ExactSizeIterator<Item = &'b Expr<SourceAst>> + Clone,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        span: Span,
    ) -> Result<FunctionMatch, Error> {
        let mut overload_errors = Vec::new();
        let mut inner_error = None;

        for fun_idx in overloads.functions {
            match self.try_overload(fun_idx, args.clone(), expected, scope, span) {
                Ok(Ok(res)) => return Ok(res),
                Ok(Err(err)) => overload_errors.push(err),
                Err(err @ Error::ResolutionError(_, _)) => inner_error = Some(err),
                Err(other) => return Err(other),
            }
        }
        if let Some(inner) = inner_error {
            Err(inner)
        } else {
            Err(Error::no_matching_overload(name, &overload_errors, span))
        }
    }

    fn resolve_overload<'b>(
        &mut self,
        name: Ident,
        overloads: FunctionCandidates,
        args: impl ExactSizeIterator<Item = &'b Expr<SourceAst>> + Clone,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        silent: bool,
        span: Span,
    ) -> Result<FunctionMatch, Error> {
        let fst = overloads.functions.first().cloned();
        match self.resolve_overload_faily(name, overloads, args.clone(), expected, scope, span) {
            Ok(res) => Ok(res),
            Err(err) if !self.permissive => Err(err),
            Err(err) => {
                if !silent {
                    self.report(err)?;
                }
                let args = args
                    .map(|e| self.check(e, None, scope, false))
                    .collect::<Result<_, Error>>()?;
                let dummy = FunctionMatch {
                    index: fst.unwrap(),
                    args,
                };
                Ok(dummy)
            }
        }
    }

    fn try_overload<'b>(
        &mut self,
        fun_idx: PoolIndex<Function>,
        args: impl ExactSizeIterator<Item = &'b Expr<SourceAst>>,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        span: Span,
    ) -> Result<Result<FunctionMatch, FunctionResolutionError>, Error> {
        let fun = self.pool.function(fun_idx)?;
        let params = fun.parameters.clone();
        let ret_type = fun.return_type;

        if args.len() > params.len() {
            return Ok(Err(FunctionResolutionError::too_many_args(params.len(), args.len())));
        }

        if fun.flags.is_cast() || fun.flags.is_operator() {
            if let Some(expected) = expected {
                let ret_type_idx = ret_type.ok_or_else(|| Cause::void_cannot_be_used().with_span(span))?;
                let ret_type = scope.resolve_type_from_pool(ret_type_idx, self.pool).with_span(span)?;
                if find_conversion(&ret_type, expected, self.pool)?.is_none() {
                    let err = FunctionResolutionError::return_mismatch(
                        expected.pretty(self.pool)?,
                        ret_type.pretty(self.pool)?,
                    );
                    return Ok(Err(err));
                }
            }
        }

        let mut compiled_args = Vec::new();
        for (idx, arg) in args.enumerate() {
            let param = self.pool.parameter(params[idx])?;
            let param_type = scope.resolve_type_from_pool(param.type_, self.pool).with_span(span)?;
            match self.check_and_convert_faily(arg, &param_type, scope, span) {
                Ok(converted) => compiled_args.push(converted),
                Err(Error::ArgumentError(err, _)) => {
                    return Ok(Err(FunctionResolutionError::parameter_mismatch(&err, idx)))
                }
                Err(err) => return Err(err),
            }
        }

        let opt_param_count = params
            .iter()
            .filter_map(|idx| self.pool.parameter(*idx).ok())
            .filter(|param| param.flags.is_optional())
            .count();

        let min_params = params.len() - opt_param_count;
        if compiled_args.len() >= min_params {
            Ok(Ok(FunctionMatch {
                index: fun_idx,
                args: compiled_args,
            }))
        } else {
            let err = FunctionResolutionError::invalid_arg_count(compiled_args.len(), min_params, params.len());
            Ok(Err(err))
        }
    }

    fn add_local(&mut self, name: Ident, type_: &TypeId, scope: &mut Scope) -> Result<PoolIndex<Local>, Cause> {
        let idx = self.locals.len();
        let name_mangled = Ref::new(format!("{}$local${}", name, idx));
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

    pub fn finish(self) -> (Vec<Diagnostic>, Vec<PoolIndex<Local>>) {
        (self.diagnostics, self.locals)
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
            Reference::Symbol(Symbol::Functions(_)) => return Err(Cause::value_expected("function").with_span(*span)),
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
        Expr::ArrayLit(_, type_, _) => TypeId::Array(Box::new(type_.clone().unwrap())),
        Expr::InterpolatedString(_, _, span) => scope.resolve_type(&TypeName::STRING, pool).with_span(*span)?,
        Expr::Declare(_, _, _, _) => TypeId::Void,
        Expr::Cast(type_, expr, _) => match type_of(expr, scope, pool)? {
            TypeId::Ref(_) => TypeId::Ref(Box::new(type_.clone())),
            TypeId::WeakRef(_) => TypeId::Ref(Box::new(type_.clone())),
            TypeId::ScriptRef(_) => TypeId::ScriptRef(Box::new(type_.clone())),
            _ => type_.clone(),
        },
        Expr::Assign(_, _, _) => TypeId::Void,
        Expr::Call(Callable::Function(index), _, span) => match pool.function(*index)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool).with_span(*span)?,
        },
        Expr::Call(Callable::Intrinsic(_, type_), _, _) => type_.clone(),
        Expr::MethodCall(_, fun, _, span) => match pool.function(*fun)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool).with_span(*span)?,
        },
        Expr::Member(_, member, span) => match member {
            Member::ClassField(field) => scope
                .resolve_type_from_pool(pool.field(*field)?.type_, pool)
                .with_span(*span)?,
            Member::StructField(field) => scope
                .resolve_type_from_pool(pool.field(*field)?.type_, pool)
                .with_span(*span)?,
            Member::EnumMember(enum_, _) => TypeId::Enum(*enum_),
        },
        Expr::ArrayElem(expr, _, span) => match type_of(expr, scope, pool)? {
            TypeId::Array(inner) => *inner,
            TypeId::StaticArray(inner, _) => *inner,
            type_ => return Err(Cause::invalid_op(type_.pretty(pool)?, "Indexing").with_span(*span)),
        },
        Expr::New(type_, _, span) => match type_ {
            TypeId::Struct(s) => TypeId::Struct(*s),
            TypeId::Class(s) => TypeId::Ref(Box::new(TypeId::Class(*s))),
            type_ => return Err(Cause::invalid_op(type_.pretty(pool)?, "Constructing").with_span(*span)),
        },
        Expr::Return(_, _) => TypeId::Void,
        Expr::Seq(_) => TypeId::Void,
        Expr::Switch(_, _, _, _) => TypeId::Void,
        Expr::Goto(_, _) => TypeId::Void,
        Expr::If(_, _, _, _) => TypeId::Void,
        Expr::Conditional(_, lhs, rhs, span) => {
            let lt = type_of(lhs, scope, pool)?;
            let rt = type_of(rhs, scope, pool)?;
            return lub(lt, rt, pool).with_span(*span);
        }
        Expr::While(_, _, _) => TypeId::Void,
        Expr::ForIn(_, _, _, _) => TypeId::Void,
        Expr::This(span) => match scope.this {
            Some(class_idx) => TypeId::Ref(Box::new(TypeId::Class(class_idx))),
            None => return Err(Cause::no_this_in_static_context().with_span(*span)),
        },
        Expr::Super(span) => match scope.this {
            Some(class_idx) => {
                let base_idx = pool.class(class_idx)?.base;
                TypeId::Ref(Box::new(TypeId::Class(base_idx)))
            }
            None => return Err(Cause::no_this_in_static_context().with_span(*span)),
        },
        Expr::Break(_) => TypeId::Void,
        Expr::Null(_) => TypeId::Null,
        Expr::BinOp(_, _, _, span) => return Err(Cause::unsupported("BinOp").with_span(*span)),
        Expr::UnOp(_, _, span) => return Err(Cause::unsupported("UnOp").with_span(*span)),
    };
    Ok(res)
}

pub fn collect_subtypes(class_idx: PoolIndex<Class>, pool: &ConstantPool) -> Result<Vec<PoolIndex<Class>>, PoolError> {
    if class_idx.is_undefined() {
        Ok(vec![])
    } else {
        let class = pool.class(class_idx)?;
        let mut res = collect_subtypes(class.base, pool)?;
        res.push(class_idx);
        Ok(res)
    }
}

// least upper type bound
pub fn lub(a: TypeId, b: TypeId, pool: &ConstantPool) -> Result<TypeId, Cause> {
    fn lub_class(a: PoolIndex<Class>, b: PoolIndex<Class>, pool: &ConstantPool) -> Result<PoolIndex<Class>, Cause> {
        let subs_a = collect_subtypes(a, pool)?;
        let subs_b = collect_subtypes(b, pool)?;
        let res = subs_a.into_iter().zip(subs_b).take_while(|(a, b)| a == b).last();
        res.map(|x| x.0)
            .ok_or_else(|| Cause::unification_failed(pool.def_name(a).unwrap(), pool.def_name(b).unwrap()))
    }

    if a == b {
        Ok(a)
    } else {
        match (a, b) {
            (TypeId::Ref(a), TypeId::Ref(b)) => Ok(TypeId::Ref(Box::new(lub(*a, *b, pool)?))),
            (TypeId::Ref(a), TypeId::Null) => Ok(TypeId::Ref(a)),
            (TypeId::Null, TypeId::Ref(a)) => Ok(TypeId::Ref(a)),
            (TypeId::WeakRef(a), TypeId::WeakRef(b)) => Ok(TypeId::Ref(Box::new(lub(*a, *b, pool)?))),
            (TypeId::WeakRef(a), TypeId::Null) => Ok(TypeId::WeakRef(a)),
            (TypeId::Null, TypeId::WeakRef(a)) => Ok(TypeId::WeakRef(a)),
            (TypeId::Class(a), TypeId::Class(b)) => Ok(TypeId::Class(lub_class(a, b, pool)?)),
            (TypeId::Struct(a), TypeId::Struct(b)) => Ok(TypeId::Struct(lub_class(a, b, pool)?)),
            (a, b) => Err(Cause::unification_failed(a.pretty(pool)?, b.pretty(pool)?)),
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
            (from @ TypeId::Class(_), TypeId::Ref(to)) => {
                find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
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
            vec![expr],
            span,
        ),
        Conversion::WeakRefToRef => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::WeakRefToRef, type_.clone()),
            vec![expr],
            span,
        ),
        Conversion::ToScriptRef => Expr::Call(Callable::Intrinsic(IntrinsicOp::AsRef, type_.clone()), vec![expr], span),
        Conversion::ToVariant => Expr::Call(
            Callable::Intrinsic(IntrinsicOp::ToVariant, type_.clone()),
            vec![expr],
            span,
        ),
    }
}

#[derive(Debug)]
pub struct TypedAst;

impl NameKind for TypedAst {
    type Reference = Reference;
    type Callable = Callable;
    type Local = PoolIndex<Local>;
    type Function = PoolIndex<Function>;
    type Member = Member;
    type Type = TypeId;
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

#[derive(Debug, PartialEq, Eq)]
pub enum Conversion {
    Identity,
    RefToWeakRef,
    WeakRefToRef,
    ToScriptRef,
    ToVariant,
}

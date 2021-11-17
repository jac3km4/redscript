use std::iter;
use std::str::FromStr;

use redscript::ast::{Constant, Expr, Ident, Literal, NameKind, Seq, SourceAst, Span, SwitchCase, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::IntrinsicOp;
use redscript::definition::{Definition, Enum, Field, Function, Local, LocalFlags};
use redscript::error::{Error, FunctionResolutionError};
use redscript::Ref;

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
            Expr::Ident(name, pos) => match scope.resolve_reference(name.clone(), *pos) {
                Ok(reference) => Expr::Ident(reference, *pos),
                Err(err) if self.permissive => {
                    if !silent {
                        self.report(err)?;
                    }
                    Expr::Null(*pos)
                }
                Err(err) => return Err(err),
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
            Expr::ArrayLit(exprs, _, pos) => match exprs.split_first() {
                Some((head, tail)) => {
                    let head = self.check(head, None, scope, silent)?;
                    let type_ = type_of(&head, scope, self.pool)?;
                    let mut checked = vec![head];
                    for expr in tail {
                        checked.push(self.check_and_convert(expr, &type_, scope, silent, *pos)?);
                    }
                    Expr::ArrayLit(checked, Some(type_), *pos)
                }
                None => match expected {
                    Some(TypeId::Array(inner)) => Expr::ArrayLit(vec![], Some(*inner.clone()), *pos),
                    Some(type_) => return Err(Error::type_error("array", type_.pretty(self.pool)?, *pos)),
                    None => return Err(Error::type_annotation_required(*pos)),
                },
            },
            Expr::InterpolatedString(prefix, parts, pos) => {
                let mut checked = Vec::with_capacity(parts.len());
                for (part, str) in parts {
                    checked.push((self.check(part, None, scope, silent)?, str.clone()));
                }
                Expr::InterpolatedString(prefix.clone(), checked, *pos)
            }
            Expr::Declare(name, type_, init, pos) => {
                let (initializer, type_) = match (type_, init) {
                    (None, None) => return Err(Error::type_annotation_required(*pos)),
                    (None, Some(expr)) => {
                        let checked = self.check(expr, None, scope, silent)?;
                        let type_ = type_of(&checked, scope, self.pool)?;
                        (Some(checked), type_)
                    }
                    (Some(type_name), None) => (None, scope.resolve_type(type_name, self.pool, *pos)?),
                    (Some(type_name), Some(expr)) => {
                        let type_ = scope.resolve_type(type_name, self.pool, *pos)?;
                        let checked = self.check_and_convert(expr, &type_, scope, silent, *pos)?;
                        (Some(checked), type_)
                    }
                };
                let local = self.add_local(name.clone(), &type_, scope)?;
                Expr::Declare(local, Some(type_), initializer.map(Box::new), *pos)
            }
            Expr::Cast(type_name, expr, pos) => {
                let type_ = scope.resolve_type(type_name, self.pool, *pos)?;
                let checked = self.check(expr, None, scope, silent)?;
                if let TypeId::WeakRef(inner) = type_of(&checked, scope, self.pool)? {
                    let converted = insert_conversion(checked, &TypeId::Ref(inner), Conversion::WeakRefToRef);
                    Expr::Cast(type_, Box::new(converted), *pos)
                } else {
                    Expr::Cast(type_, Box::new(checked), *pos)
                }
            }
            Expr::Assign(lhs, rhs, pos) => {
                let lhs_typed = self.check(lhs, None, scope, silent)?;
                let type_ = type_of(&lhs_typed, scope, self.pool)?;
                let rhs_typed = self.check_and_convert(rhs, &type_, scope, silent, *pos)?;
                Expr::Assign(Box::new(lhs_typed), Box::new(rhs_typed), *pos)
            }
            Expr::Call(name, args, pos) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(name.as_ref()) {
                    self.check_intrinsic(intrinsic, args, expected, scope, silent, *pos)?
                } else {
                    let candidates = scope.resolve_function(name.clone(), *pos)?;
                    let match_ =
                        self.resolve_overload(name.clone(), candidates, args.iter(), expected, scope, silent, *pos)?;
                    Expr::Call(Callable::Function(match_.index), match_.args, *pos)
                }
            }
            Expr::MethodCall(context, name, args, pos) => {
                let checked_context = self.check(context, None, scope, silent)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;
                let class = match type_.unwrapped() {
                    TypeId::Class(class) => *class,
                    TypeId::Struct(class) => *class,
                    type_ => return Err(Error::invalid_context(type_.pretty(self.pool)?, *pos)),
                };
                let candidates = scope.resolve_method(name.clone(), class, self.pool, *pos)?;
                let match_ =
                    self.resolve_overload(name.clone(), candidates, args.iter(), expected, scope, silent, *pos)?;

                let converted_context = if let TypeId::WeakRef(inner) = type_ {
                    insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef)
                } else {
                    checked_context
                };
                Expr::MethodCall(Box::new(converted_context), match_.index, match_.args, *pos)
            }
            Expr::BinOp(lhs, rhs, op, pos) => {
                let name = Ident::Static(op.into());
                let args = IntoIterator::into_iter([lhs.as_ref(), rhs.as_ref()]);
                let candidates = scope.resolve_function(name.clone(), *pos)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, silent, *pos)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *pos)
            }
            Expr::UnOp(expr, op, pos) => {
                let name = Ident::Static(op.into());
                let args = iter::once(expr.as_ref());
                let candidates = scope.resolve_function(name.clone(), *pos)?;
                let match_ = self.resolve_overload(name, candidates, args, expected, scope, silent, *pos)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *pos)
            }
            Expr::Member(context, name, pos) => {
                let checked_context = self.check(context, None, scope, silent)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;

                let member = match type_.unwrapped() {
                    TypeId::Class(class) => {
                        let field = scope.resolve_field(name.clone(), *class, self.pool, *pos)?;
                        Member::ClassField(field)
                    }
                    TypeId::Struct(class) => {
                        let field = scope.resolve_field(name.clone(), *class, self.pool, *pos)?;
                        Member::StructField(field)
                    }
                    TypeId::Enum(enum_) => {
                        let member = scope.resolve_enum_member(name.clone(), *enum_, self.pool, *pos)?;
                        Member::EnumMember(*enum_, member)
                    }
                    type_ => return Err(Error::invalid_context(type_.pretty(self.pool)?.as_ref(), *pos)),
                };
                let converted_context = if let TypeId::WeakRef(inner) = type_ {
                    insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef)
                } else {
                    checked_context
                };
                Expr::Member(Box::new(converted_context), member, *pos)
            }
            Expr::ArrayElem(expr, idx, pos) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, *pos)?;
                let checked_expr = self.check(expr, None, scope, silent)?;
                let checked_idx = self.check_and_convert(idx, &idx_type, scope, silent, *pos)?;
                Expr::ArrayElem(Box::new(checked_expr), Box::new(checked_idx), *pos)
            }
            Expr::New(type_name, args, pos) => {
                let type_ = scope.resolve_type(type_name, self.pool, *pos)?;
                match type_ {
                    TypeId::Class(class_idx) => {
                        if self.pool.class(class_idx)?.flags.is_abstract() {
                            return Err(Error::class_is_abstract(type_name.mangled(), *pos));
                        }
                        if !args.is_empty() {
                            return Err(Error::invalid_arg_count(type_name.mangled(), 0, *pos));
                        }
                        Expr::New(type_, vec![], *pos)
                    }
                    TypeId::Struct(class_idx) => {
                        let fields = self.pool.class(class_idx)?.fields.clone();
                        if fields.len() != args.len() {
                            return Err(Error::invalid_arg_count(type_name.mangled(), fields.len(), *pos));
                        }
                        let mut checked_args = Vec::with_capacity(args.len());
                        for (arg, field_idx) in args.iter().zip(fields.iter()) {
                            let field = self.pool.field(*field_idx)?;
                            let field_type = scope.resolve_type_from_pool(field.type_, self.pool, *pos)?;
                            let checked_arg = self.check_and_convert(arg, &field_type, scope, silent, *pos)?;
                            checked_args.push(checked_arg)
                        }
                        Expr::New(type_, checked_args, *pos)
                    }
                    _ => {
                        return Err(Error::invalid_op(type_name.mangled(), "Constructing", *pos));
                    }
                }
            }
            Expr::Return(None, pos) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                match fun.return_type {
                    Some(type_idx) => {
                        let type_ = scope.resolve_type_from_pool(type_idx, self.pool, *pos)?;
                        return Err(Error::return_type_mismatch(type_.pretty(self.pool)?, *pos));
                    }
                    None => Expr::Return(None, *pos),
                }
            }
            Expr::Return(Some(expr), pos) => {
                let fun = self.pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let expected = scope.resolve_type_from_pool(ret_type, self.pool, *pos)?;
                    let checked = self.check_and_convert(expr, &expected, scope, silent, *pos)?;
                    Expr::Return(Some(Box::new(checked)), *pos)
                } else {
                    return Err(Error::return_type_mismatch("Void", *pos));
                }
            }
            Expr::Seq(seq) => Expr::Seq(self.check_seq(seq, scope)?),
            Expr::Switch(matched, cases, default, pos) => {
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

                Expr::Switch(Box::new(checked_matched), checked_cases, default, *pos)
            }
            Expr::Goto(target, pos) => Expr::Goto(target.clone(), *pos),
            Expr::If(cond, if_, else_, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent, *pos)?;
                let checked_if = self.check_seq(if_, &mut scope.clone())?;
                let checked_else = else_
                    .iter()
                    .try_fold(None, |_, body| self.check_seq(body, &mut scope.clone()).map(Some))?;

                Expr::If(Box::new(checked_cond), checked_if, checked_else, *pos)
            }
            Expr::Conditional(cond, true_, false_, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent, *pos)?;
                let checked_true = self.check(true_, None, scope, silent)?;
                let if_type = type_of(&checked_true, scope, self.pool)?;
                let checked_false = self.check_and_convert(false_, &if_type, scope, silent, *pos)?;

                Expr::Conditional(
                    Box::new(checked_cond),
                    Box::new(checked_true),
                    Box::new(checked_false),
                    *pos,
                )
            }
            Expr::While(cond, body, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, silent, *pos)?;
                let checked_body = self.check_seq(body, &mut scope.clone())?;

                Expr::While(Box::new(checked_cond), checked_body, *pos)
            }
            Expr::ForIn(name, array, body, pos) => {
                let array = self.check(array, None, scope, silent)?;
                match type_of(&array, scope, self.pool)? {
                    TypeId::Array(inner) => {
                        let mut local_scope = scope.clone();
                        let local = self.add_local(name.clone(), &inner, &mut local_scope)?;
                        let body = self.check_seq(body, &mut local_scope)?;
                        Expr::ForIn(local, Box::new(array), body, *pos)
                    }
                    other => return Err(Error::type_error(other.pretty(self.pool)?, "array", *pos)),
                }
            }
            Expr::This(pos) => Expr::This(*pos),
            Expr::Super(pos) => Expr::Super(*pos),
            Expr::Break(pos) => Expr::Break(*pos),
            Expr::Null(pos) => Expr::Null(*pos),
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
        pos: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        if args.len() != intrinsic.arg_count().into() {
            return Err(Error::invalid_arg_count(intrinsic, intrinsic.arg_count() as usize, pos));
        }
        let first_arg = self.check(&args[0], None, scope, silent)?;
        let first_arg_type = type_of(&first_arg, scope, self.pool)?;
        let mut checked_args = vec![];
        let type_ = match (intrinsic, first_arg_type) {
            (IntrinsicOp::Equals, arg_type) => {
                checked_args.push(first_arg);
                checked_args.push(self.check(&args[1], Some(&arg_type), scope, silent)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::NotEquals, arg_type) => {
                checked_args.push(first_arg);
                checked_args.push(self.check(&args[1], Some(&arg_type), scope, silent)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayClear, TypeId::Array(_)) => {
                checked_args.push(first_arg);
                TypeId::Void
            }
            (IntrinsicOp::ArraySize, TypeId::Array(_)) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::ArrayResize, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, silent, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                *elem
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                *elem
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                *elem
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, silent, pos)?);
                checked_args.push(self.check_and_convert(&args[2], &elem, scope, silent, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, silent, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, silent, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(_)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(first_arg);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, silent, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayLast, TypeId::Array(elem)) => {
                checked_args.push(first_arg);
                *elem
            }
            (IntrinsicOp::ToString, _) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::STRING, self.pool, pos)?
            }
            (IntrinsicOp::EnumInt, TypeId::Enum(_)) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::IntEnum, _) if expected.is_some() => {
                checked_args.push(first_arg);
                if let Some(TypeId::Enum(idx)) = expected {
                    TypeId::Enum(*idx)
                } else {
                    return Err(Error::type_error("Enum", expected.unwrap().pretty(self.pool)?, pos));
                }
            }
            (IntrinsicOp::ToVariant, _) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::VARIANT, self.pool, pos)?
            }
            (IntrinsicOp::FromVariant, TypeId::Variant) if expected.is_some() => {
                checked_args.push(first_arg);
                expected.unwrap().clone()
            }
            (IntrinsicOp::VariantTypeName, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::CNAME, self.pool, pos)?
            }
            (IntrinsicOp::VariantIsRef, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::VariantIsArray, TypeId::Variant) => {
                checked_args.push(first_arg);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
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
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (_, type_) => return Err(Error::invalid_intrinsic(intrinsic, type_.pretty(self.pool)?, pos)),
        };

        Ok(Expr::Call(Callable::Intrinsic(intrinsic, type_), checked_args, pos))
    }

    fn check_and_convert_faily(
        &mut self,
        expr: &Expr<SourceAst>,
        to: &TypeId,
        scope: &mut Scope,
        pos: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        let checked = self.check(expr, Some(to), scope, true)?;
        let from = type_of(&checked, scope, self.pool)?;
        let conversion = find_conversion(&from, to, self.pool)?
            .ok_or_else(|| Error::type_error(from.pretty(self.pool).unwrap(), to.pretty(self.pool).unwrap(), pos))?;
        Ok(insert_conversion(checked, to, conversion))
    }

    // version of check that recovers from type errors and logs them as diagnostics instead
    fn check_and_convert(
        &mut self,
        expr: &Expr<SourceAst>,
        to: &TypeId,
        scope: &mut Scope,
        silent: bool,
        pos: Span,
    ) -> Result<Expr<TypedAst>, Error> {
        let checked = self.check(expr, Some(to), scope, silent)?;
        let from = type_of(&checked, scope, self.pool)?;
        match find_conversion(&from, to, self.pool)? {
            Some(conversion) => Ok(insert_conversion(checked, to, conversion)),
            None => {
                if !silent {
                    let err = Error::type_error(from.pretty(self.pool).unwrap(), to.pretty(self.pool).unwrap(), pos);
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
        pos: Span,
    ) -> Result<FunctionMatch, Error> {
        let mut overload_errors = Vec::new();
        let mut inner_error = None;

        for fun_idx in overloads.functions {
            match self.try_overload(fun_idx, args.clone(), expected, scope, pos) {
                Ok(Ok(res)) => return Ok(res),
                Ok(Err(err)) => overload_errors.push(err),
                Err(err @ Error::ResolutionError(_, _)) => inner_error = Some(err),
                Err(other) => return Err(other),
            }
        }
        if let Some(inner) = inner_error {
            Err(inner)
        } else {
            Err(Error::no_matching_overload(name, &overload_errors, pos))
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
        pos: Span,
    ) -> Result<FunctionMatch, Error> {
        let fst = overloads.functions.first().cloned();
        match self.resolve_overload_faily(name, overloads, args.clone(), expected, scope, pos) {
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
        pos: Span,
    ) -> Result<Result<FunctionMatch, FunctionResolutionError>, Error> {
        let fun = self.pool.function(fun_idx)?;
        let params = fun.parameters.clone();
        let ret_type = fun.return_type;

        if args.len() > params.len() {
            return Ok(Err(FunctionResolutionError::too_many_args(params.len(), args.len())));
        }

        if fun.flags.is_cast() || fun.flags.is_operator() {
            if let Some(expected) = expected {
                let ret_type_idx = ret_type.ok_or_else(|| Error::void_cannot_be_used(pos))?;
                let ret_type = scope.resolve_type_from_pool(ret_type_idx, self.pool, pos)?;
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
            let param_type = scope.resolve_type_from_pool(param.type_, self.pool, pos)?;
            match self.check_and_convert_faily(arg, &param_type, scope, pos) {
                Ok(converted) => compiled_args.push(converted),
                Err(Error::TypeError(err, _)) => {
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

    fn add_local(&mut self, name: Ident, type_: &TypeId, scope: &mut Scope) -> Result<PoolIndex<Local>, Error> {
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
        Expr::Ident(reference, pos) => match reference {
            Reference::Value(Value::Local(idx)) => scope.resolve_type_from_pool(pool.local(*idx)?.type_, pool, *pos)?,
            Reference::Value(Value::Parameter(idx)) => {
                scope.resolve_type_from_pool(pool.parameter(*idx)?.type_, pool, *pos)?
            }
            Reference::Symbol(Symbol::Class(idx, _)) => TypeId::Class(*idx),
            Reference::Symbol(Symbol::Struct(idx, _)) => TypeId::Struct(*idx),
            Reference::Symbol(Symbol::Enum(idx)) => TypeId::Enum(*idx),
            Reference::Symbol(Symbol::Functions(_)) => return Err(Error::value_expected("function", *pos)),
        },
        Expr::Constant(cons, pos) => match cons {
            Constant::String(Literal::String, _) => scope.resolve_type(&TypeName::STRING, pool, *pos)?,
            Constant::String(Literal::Name, _) => scope.resolve_type(&TypeName::CNAME, pool, *pos)?,
            Constant::String(Literal::Resource, _) => scope.resolve_type(&TypeName::RESOURCE, pool, *pos)?,
            Constant::String(Literal::TweakDbId, _) => scope.resolve_type(&TypeName::TWEAKDB_ID, pool, *pos)?,
            Constant::F32(_) => scope.resolve_type(&TypeName::FLOAT, pool, *pos)?,
            Constant::F64(_) => scope.resolve_type(&TypeName::DOUBLE, pool, *pos)?,
            Constant::I32(_) => scope.resolve_type(&TypeName::INT32, pool, *pos)?,
            Constant::I64(_) => scope.resolve_type(&TypeName::INT64, pool, *pos)?,
            Constant::U32(_) => scope.resolve_type(&TypeName::UINT32, pool, *pos)?,
            Constant::U64(_) => scope.resolve_type(&TypeName::UINT64, pool, *pos)?,
            Constant::Bool(_) => scope.resolve_type(&TypeName::BOOL, pool, *pos)?,
        },
        Expr::ArrayLit(_, type_, _) => TypeId::Array(Box::new(type_.clone().unwrap())),
        Expr::InterpolatedString(_, _, pos) => scope.resolve_type(&TypeName::STRING, pool, *pos)?,
        Expr::Declare(_, _, _, _) => TypeId::Void,
        Expr::Cast(type_, expr, _) => match type_of(expr, scope, pool)? {
            TypeId::Ref(_) => TypeId::Ref(Box::new(type_.clone())),
            TypeId::WeakRef(_) => TypeId::Ref(Box::new(type_.clone())),
            TypeId::ScriptRef(_) => TypeId::ScriptRef(Box::new(type_.clone())),
            _ => type_.clone(),
        },
        Expr::Assign(_, _, _) => TypeId::Void,
        Expr::Call(Callable::Function(index), _, pos) => match pool.function(*index)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool, *pos)?,
        },
        Expr::Call(Callable::Intrinsic(_, type_), _, _) => type_.clone(),
        Expr::MethodCall(_, fun, _, pos) => match pool.function(*fun)?.return_type {
            None => TypeId::Void,
            Some(return_type) => scope.resolve_type_from_pool(return_type, pool, *pos)?,
        },
        Expr::Member(_, member, pos) => match member {
            Member::ClassField(field) => scope.resolve_type_from_pool(pool.field(*field)?.type_, pool, *pos)?,
            Member::StructField(field) => scope.resolve_type_from_pool(pool.field(*field)?.type_, pool, *pos)?,
            Member::EnumMember(enum_, _) => TypeId::Enum(*enum_),
        },
        Expr::ArrayElem(expr, _, pos) => match type_of(expr, scope, pool)? {
            TypeId::Array(inner) => *inner,
            TypeId::StaticArray(inner, _) => *inner,
            type_ => return Err(Error::invalid_op(type_.pretty(pool)?, "Indexing", *pos)),
        },
        Expr::New(type_, _, pos) => match type_ {
            TypeId::Struct(s) => TypeId::Struct(*s),
            TypeId::Class(s) => TypeId::Ref(Box::new(TypeId::Class(*s))),
            type_ => return Err(Error::invalid_op(type_.pretty(pool)?, "Constructing", *pos)),
        },
        Expr::Return(_, _) => TypeId::Void,
        Expr::Seq(_) => TypeId::Void,
        Expr::Switch(_, _, _, _) => TypeId::Void,
        Expr::Goto(_, _) => TypeId::Void,
        Expr::If(_, _, _, _) => TypeId::Void,
        Expr::Conditional(_, lhs, _, _) => type_of(lhs, scope, pool)?,
        Expr::While(_, _, _) => TypeId::Void,
        Expr::ForIn(_, _, _, _) => TypeId::Void,
        Expr::This(pos) => match scope.this {
            Some(class_idx) => TypeId::Ref(Box::new(TypeId::Class(class_idx))),
            None => return Err(Error::no_this_in_static_context(*pos)),
        },
        Expr::Super(pos) => match scope.this {
            Some(class_idx) => {
                let base_idx = pool.class(class_idx)?.base;
                TypeId::Ref(Box::new(TypeId::Class(base_idx)))
            }
            None => return Err(Error::no_this_in_static_context(*pos)),
        },
        Expr::Break(_) => TypeId::Void,
        Expr::Null(_) => TypeId::Null,
        Expr::BinOp(_, _, _, pos) => return Err(Error::unsupported("BinOp", *pos)),
        Expr::UnOp(_, _, pos) => return Err(Error::unsupported("UnOp", *pos)),
    };
    Ok(res)
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
        Conversion::ToVariant => Expr::Call(Callable::Intrinsic(IntrinsicOp::ToVariant, type_.clone()), vec![expr], span),
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

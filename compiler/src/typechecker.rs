use std::iter;
use std::str::FromStr;

use redscript::ast::{Constant, Expr, Ident, Literal, NameKind, Pos, Seq, Source, SwitchCase, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Definition, Enum, Field, Function, Local, LocalFlags};
use redscript::error::{Error, FunctionResolutionError};
use strum::{Display, EnumString};

use crate::scope::{Conversion, FunctionMatch, FunctionName, FunctionOverloads, Scope};
use crate::{Reference, TypeId};

#[derive(Debug)]
pub struct Typed;

impl NameKind for Typed {
    type Reference = Reference;
    type Callable = Callable;
    type Local = PoolIndex<Local>;
    type Function = PoolIndex<Function>;
    type Member = Member;
    type Type = TypeId;
}

#[derive(Debug)]
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

pub struct Typechecker<'a> {
    pub locals: Vec<PoolIndex<Local>>,
    pool: &'a mut ConstantPool,
}

impl<'a> Typechecker<'a> {
    pub fn new(pool: &'a mut ConstantPool) -> Typechecker<'a> {
        Typechecker { locals: vec![], pool }
    }

    pub fn check(
        &mut self,
        expr: &Expr<Source>,
        expected: Option<&TypeId>,
        scope: &mut Scope,
    ) -> Result<Expr<Typed>, Error> {
        let res = match expr {
            Expr::Ident(name, pos) => {
                let reference = scope.resolve_reference(name.clone(), *pos)?;
                Expr::Ident(reference, *pos)
            }
            Expr::Constant(cons, pos) => Expr::Constant(cons.clone(), *pos),
            Expr::ArrayLit(exprs, _, pos) => match exprs.split_first() {
                Some((head, tail)) => {
                    let head = self.check(head, None, scope)?;
                    let type_ = type_of(&head, scope, self.pool)?;
                    let mut checked = vec![head];
                    for expr in tail {
                        checked.push(self.check_and_convert(expr, &type_, scope, *pos)?);
                    }
                    Expr::ArrayLit(checked, Some(type_), *pos)
                }
                None => {
                    if let Some(TypeId::Array(inner)) = expected {
                        Expr::ArrayLit(vec![], Some(*inner.clone()), *pos)
                    } else {
                        return Err(Error::type_annotation_required(*pos));
                    }
                }
            },
            Expr::Declare(name, type_, init, pos) => {
                let name_idx = self.pool.names.add(name.to_owned());
                let (initializer, type_) = match (type_, init) {
                    (None, None) => return Err(Error::type_annotation_required(*pos)),
                    (None, Some(expr)) => {
                        let checked = self.check(expr, None, scope)?;
                        let type_ = type_of(&checked, scope, self.pool)?;
                        (Some(checked), type_)
                    }
                    (Some(type_name), None) => (None, scope.resolve_type(&type_name, self.pool, *pos)?),
                    (Some(type_name), Some(expr)) => {
                        let type_ = scope.resolve_type(&type_name, self.pool, *pos)?;
                        let checked = self.check_and_convert(expr, &type_, scope, *pos)?;
                        (Some(checked), type_)
                    }
                };
                let local = Local::new(scope.get_type_index(&type_, self.pool)?, LocalFlags::new());
                let local_def = Definition::local(name_idx, scope.function.unwrap().cast(), local);
                let local_idx = self.pool.push_definition(local_def).cast();
                self.locals.push(local_idx);
                scope.push_local(name.clone(), local_idx);
                Expr::Declare(local_idx, Some(type_), initializer.map(Box::new), *pos)
            }
            Expr::Cast(type_name, expr, pos) => {
                let type_ = scope.resolve_type(&type_name, self.pool, *pos)?;
                let checked = self.check(expr, None, scope)?;
                if let TypeId::WeakRef(inner) = type_of(&checked, scope, self.pool)? {
                    let converted =
                        Self::insert_conversion(checked, &TypeId::Ref(inner), Conversion::WeakRefToRef, *pos);
                    Expr::Cast(type_, Box::new(converted), *pos)
                } else {
                    Expr::Cast(type_, Box::new(checked), *pos)
                }
            }
            Expr::Assign(lhs, rhs, pos) => {
                let lhs_typed = self.check(lhs, None, scope)?;
                let type_ = type_of(&lhs_typed, scope, self.pool)?;
                let rhs_typed = self.check(rhs, Some(&type_), scope)?;
                Expr::Assign(Box::new(lhs_typed), Box::new(rhs_typed), *pos)
            }
            Expr::Call(name, args, pos) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(name.as_ref()) {
                    self.check_intrinsic(intrinsic, args, expected, scope, *pos)?
                } else {
                    let fun_name = FunctionName::global(name.clone());
                    let candidates = scope.resolve_function(fun_name, self.pool, *pos)?;
                    let match_ =
                        self.resolve_overload(name.clone(), candidates.clone(), args.iter(), expected, scope, *pos)?;
                    Expr::Call(Callable::Function(match_.index), match_.args, *pos)
                }
            }
            Expr::MethodCall(context, name, args, pos) => {
                let checked_context = self.check(&context, None, scope)?;
                let type_ = type_of(&checked_context, scope, self.pool)?;
                let class = match type_.unwrapped() {
                    TypeId::Class(class) => *class,
                    TypeId::Struct(class) => *class,
                    type_ => return Err(Error::invalid_context(type_.pretty(self.pool)?.as_ref(), *pos)),
                };
                let candidates = scope.resolve_method(name.clone(), class, self.pool, *pos)?;
                let match_ =
                    self.resolve_overload(name.clone(), candidates.clone(), args.iter(), expected, scope, *pos)?;

                if let TypeId::WeakRef(inner) = type_ {
                    let converted =
                        Self::insert_conversion(checked_context, &TypeId::Ref(inner), Conversion::WeakRefToRef, *pos);
                    Expr::MethodCall(Box::new(converted), match_.index, match_.args, *pos)
                } else {
                    Expr::MethodCall(Box::new(checked_context), match_.index, match_.args, *pos)
                }
            }
            Expr::BinOp(lhs, rhs, op, pos) => {
                let name = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let candidates = scope.resolve_function(FunctionName::global(name.clone()), self.pool, *pos)?;
                let match_ = self.resolve_overload(name, candidates.clone(), args, expected, scope, *pos)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *pos)
            }
            Expr::UnOp(expr, op, pos) => {
                let name = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let candidates = scope.resolve_function(FunctionName::global(name.clone()), self.pool, *pos)?;
                let match_ = self.resolve_overload(name, candidates.clone(), args, expected, scope, *pos)?;
                Expr::Call(Callable::Function(match_.index), match_.args, *pos)
            }
            Expr::Member(context, name, pos) => {
                let checked_context = self.check(&context, None, scope)?;
                let member = match type_of(&checked_context, scope, self.pool)?.unwrapped() {
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
                Expr::Member(Box::new(checked_context), member, *pos)
            }
            Expr::ArrayElem(expr, idx, pos) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, *pos)?;
                let checked_expr = self.check(expr, None, scope)?;
                let checked_idx = self.check_and_convert(idx, &idx_type, scope, *pos)?;
                Expr::ArrayElem(Box::new(checked_expr), Box::new(checked_idx), *pos)
            }
            Expr::New(type_name, args, pos) => {
                let type_ = scope.resolve_type(type_name, self.pool, *pos)?;
                match type_ {
                    TypeId::Class(_) => {
                        if args.is_empty() {
                            Expr::New(type_, vec![], *pos)
                        } else {
                            return Err(Error::invalid_arg_count(type_name.mangled(), 0, *pos));
                        }
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
                            let checked_arg = self.check_and_convert(arg, &field_type, scope, *pos)?;
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
                    let checked = self.check_and_convert(expr, &expected, scope, *pos)?;
                    Expr::Return(Some(Box::new(checked)), *pos)
                } else {
                    return Err(Error::return_type_mismatch("Void", *pos));
                }
            }
            Expr::Seq(seq) => Expr::Seq(self.check_seq(seq, scope)?),
            Expr::Switch(matched, cases, default) => {
                let checked_matched = self.check(matched, None, scope)?;
                let mut checked_cases = Vec::with_capacity(cases.len());
                for case in cases {
                    let matcher = self.check(&case.matcher, None, scope)?;
                    let body = self.check_seq(&case.body, &mut scope.clone())?;
                    checked_cases.push(SwitchCase { matcher, body })
                }
                let default = default
                    .iter()
                    .try_fold(None, |_, body| self.check_seq(body, scope).map(Some))?;

                Expr::Switch(Box::new(checked_matched), checked_cases, default)
            }
            Expr::Goto(target, pos) => Expr::Goto(target.clone(), *pos),
            Expr::If(cond, if_, else_, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, *pos)?;
                let checked_if = self.check_seq(if_, &mut scope.clone())?;
                let checked_else = else_
                    .iter()
                    .try_fold(None, |_, body| self.check_seq(body, &mut scope.clone()).map(Some))?;

                Expr::If(Box::new(checked_cond), checked_if, checked_else, *pos)
            }
            Expr::Conditional(cond, true_, false_, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, *pos)?;
                let checked_true = self.check(true_, None, scope)?;
                let if_type = type_of(&checked_true, scope, self.pool)?;
                let checked_false = self.check_and_convert(false_, &if_type, scope, *pos)?;

                Expr::Conditional(
                    Box::new(checked_cond),
                    Box::new(checked_true),
                    Box::new(checked_false),
                    *pos,
                )
            }
            Expr::While(cond, body, pos) => {
                let cond_type = scope.resolve_type(&TypeName::BOOL, self.pool, *pos)?;
                let checked_cond = self.check_and_convert(cond, &cond_type, scope, *pos)?;
                let checked_body = self.check_seq(body, &mut scope.clone())?;

                Expr::While(Box::new(checked_cond), checked_body, *pos)
            }
            Expr::This(pos) => Expr::This(*pos),
            Expr::Super(pos) => Expr::Super(*pos),
            Expr::Break(pos) => Expr::Break(*pos),
            Expr::Null => Expr::Null,
        };
        Ok(res)
    }

    pub fn check_seq(&mut self, seq: &Seq<Source>, scope: &mut Scope) -> Result<Seq<Typed>, Error> {
        let mut exprs = Vec::with_capacity(seq.exprs.len());
        for expr in &seq.exprs {
            exprs.push(self.check(&expr, None, scope)?);
        }
        Ok(Seq { exprs })
    }

    fn check_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: &[Expr<Source>],
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<Expr<Typed>, Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            return Err(Error::CompileError(err, pos));
        }
        let first = self.check(&args[0], None, scope)?;
        let arg_type = type_of(&first, scope, self.pool)?;
        let mut checked_args = vec![];
        let type_ = match (intrinsic, arg_type) {
            (IntrinsicOp::Equals, _) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check(&args[1], None, scope)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::NotEquals, _) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check(&args[1], None, scope)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayClear, TypeId::Array(_)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                TypeId::Void
            }
            (IntrinsicOp::ArraySize, TypeId::Array(_)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::ArrayResize, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                *elem
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                *elem
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                *elem
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(elem)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, pos)?);
                checked_args.push(self.check_and_convert(&args[2], &elem, scope, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &elem, scope, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(_)) => {
                let size_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &size_type, scope, pos)?);
                TypeId::Void
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(_)) => {
                let idx_type = scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
                checked_args.push(self.check(&args[0], None, scope)?);
                checked_args.push(self.check_and_convert(&args[1], &idx_type, scope, pos)?);
                scope.resolve_type(&TypeName::BOOL, self.pool, pos)?
            }
            (IntrinsicOp::ArrayLast, TypeId::Array(elem)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                *elem
            }
            (IntrinsicOp::ToString, _) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                scope.resolve_type(&TypeName::STRING, self.pool, pos)?
            }
            (IntrinsicOp::EnumInt, _) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                scope.resolve_type(&TypeName::INT32, self.pool, pos)?
            }
            (IntrinsicOp::IntEnum, _) if expected.is_some() => {
                checked_args.push(self.check(&args[0], None, scope)?);
                expected.unwrap().clone()
            }
            (IntrinsicOp::ToVariant, _) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                scope.resolve_type(&TypeName::VARIANT, self.pool, pos)?
            }
            (IntrinsicOp::FromVariant, _) if expected.is_some() => {
                let param_type = scope.resolve_type(&TypeName::VARIANT, self.pool, pos)?;
                checked_args.push(self.check_and_convert(&args[0], &param_type, scope, pos)?);
                expected.unwrap().clone()
            }
            (IntrinsicOp::AsRef, type_) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                TypeId::ScriptRef(Box::new(type_))
            }
            (IntrinsicOp::Deref, TypeId::ScriptRef(inner)) => {
                checked_args.push(self.check(&args[0], None, scope)?);
                *inner
            }
            (_, type_) => {
                let err = format!("Invalid intrinsic {} call: {}", intrinsic, type_.pretty(self.pool)?);
                return Err(Error::CompileError(err, pos));
            }
        };

        Ok(Expr::Call(Callable::Intrinsic(intrinsic, type_), checked_args, pos))
    }

    fn check_and_convert(
        &mut self,
        expr: &Expr<Source>,
        to: &TypeId,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<Expr<Typed>, Error> {
        let checked = self.check(expr, Some(to), scope)?;
        let from = type_of(&checked, scope, self.pool)?;
        let conversion = Self::find_conversion(&from, &to, self.pool)?
            .ok_or_else(|| Error::type_error(from.pretty(self.pool).unwrap(), to.pretty(self.pool).unwrap(), pos))?;
        Ok(Self::insert_conversion(checked, to, conversion, pos))
    }

    fn insert_conversion(expr: Expr<Typed>, type_: &TypeId, conversion: Conversion, pos: Pos) -> Expr<Typed> {
        match conversion {
            Conversion::Identity => expr,
            Conversion::RefToWeakRef => Expr::Call(
                Callable::Intrinsic(IntrinsicOp::RefToWeakRef, type_.clone()),
                vec![expr],
                pos,
            ),
            Conversion::WeakRefToRef => Expr::Call(
                Callable::Intrinsic(IntrinsicOp::WeakRefToRef, type_.clone()),
                vec![expr],
                pos,
            ),
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
                        Self::find_conversion(&TypeId::Class(class.base), to, pool)?
                    } else {
                        None
                    }
                }
                (TypeId::Struct(from), TypeId::Struct(_)) => {
                    let class = pool.class(*from)?;
                    if class.base != PoolIndex::UNDEFINED {
                        Self::find_conversion(&TypeId::Struct(class.base), to, pool)?
                    } else {
                        None
                    }
                }
                (from @ TypeId::Class(_), TypeId::Ref(to)) => {
                    Self::find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
                }
                (TypeId::Ref(from), TypeId::Ref(to)) => {
                    Self::find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
                }
                (TypeId::WeakRef(from), TypeId::WeakRef(to)) => {
                    Self::find_conversion(from, to, pool)?.filter(|conv| *conv == Conversion::Identity)
                }
                (TypeId::WeakRef(from), TypeId::Ref(to))
                    if Self::find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::WeakRefToRef)
                }
                (TypeId::Ref(from), TypeId::WeakRef(to))
                    if Self::find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::RefToWeakRef)
                }
                _ => None,
            }
        };
        Ok(result)
    }

    fn resolve_overload<'b>(
        &mut self,
        name: Ident,
        overloads: FunctionOverloads,
        args: impl Iterator<Item = &'b Expr<Source>> + Clone,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<FunctionMatch, Error> {
        let mut overload_errors = Vec::new();

        for fun_idx in overloads.functions {
            match self.try_overload(fun_idx, args.clone(), expected, scope, pos) {
                Ok(Ok(res)) => return Ok(res),
                Ok(Err(err)) => overload_errors.push(err),
                Err(other) => return Err(other),
            }
        }
        Err(Error::no_matching_overload(name, &overload_errors, pos))
    }

    fn try_overload<'b>(
        &mut self,
        fun_idx: PoolIndex<Function>,
        arg_iter: impl Iterator<Item = &'b Expr<Source>>,
        expected: Option<&TypeId>,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<Result<FunctionMatch, FunctionResolutionError>, Error> {
        let fun = self.pool.function(fun_idx)?;
        let params = fun.parameters.clone();

        if let Some(expected) = expected {
            let ret_type_idx = fun.return_type.ok_or_else(|| Error::void_cannot_be_used(pos))?;
            let ret_type = scope.resolve_type_from_pool(ret_type_idx, self.pool, pos)?;
            if Self::find_conversion(&ret_type, &expected, self.pool)?.is_none() {
                let err =
                    FunctionResolutionError::return_mismatch(expected.pretty(self.pool)?, ret_type.pretty(self.pool)?);
                return Ok(Err(err));
            }
        }

        let mut args = Vec::new();
        for (idx, arg) in arg_iter.enumerate() {
            let param_idx = match params.get(idx) {
                Some(val) => val,
                None => return Ok(Err(FunctionResolutionError::too_many_args(params.len()))),
            };
            let param = self.pool.parameter(*param_idx)?;
            let param_type = scope.resolve_type_from_pool(param.type_, self.pool, pos)?;
            match self.check_and_convert(arg, &param_type, scope, pos) {
                Ok(converted) => args.push(converted),
                Err(Error::CompileError(err, _)) => {
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
        if args.len() >= min_params {
            Ok(Ok(FunctionMatch { index: fun_idx, args }))
        } else {
            let err = FunctionResolutionError::invalid_arg_count(args.len(), min_params, params.len());
            Ok(Err(err))
        }
    }
}

pub fn type_of(expr: &Expr<Typed>, scope: &Scope, pool: &ConstantPool) -> Result<TypeId, Error> {
    let res = match expr {
        Expr::Ident(reference, pos) => match reference {
            Reference::Local(idx) => scope.resolve_type_from_pool(pool.local(*idx)?.type_, pool, *pos)?,
            Reference::Parameter(idx) => scope.resolve_type_from_pool(pool.parameter(*idx)?.type_, pool, *pos)?,
            Reference::Class(idx) => TypeId::Class(*idx),
            Reference::Enum(idx) => TypeId::Enum(*idx),
        },
        Expr::Constant(cons, pos) => match cons {
            Constant::String(Literal::String, _) => scope.resolve_type(&TypeName::STRING, pool, *pos)?,
            Constant::String(Literal::Name, _) => scope.resolve_type(&TypeName::CNAME, pool, *pos)?,
            Constant::String(Literal::Resource, _) => scope.resolve_type(&TypeName::RESOURCE, pool, *pos)?,
            Constant::String(Literal::TweakDbId, _) => scope.resolve_type(&TypeName::TWEAKDB_ID, pool, *pos)?,
            Constant::Float(_) => scope.resolve_type(&TypeName::FLOAT, pool, *pos)?,
            Constant::Int(_) => scope.resolve_type(&TypeName::INT32, pool, *pos)?,
            Constant::Uint(_) => scope.resolve_type(&TypeName::UINT32, pool, *pos)?,
            Constant::Bool(_) => scope.resolve_type(&TypeName::BOOL, pool, *pos)?,
        },
        Expr::ArrayLit(_, type_, _) => TypeId::Array(Box::new(type_.clone().unwrap())),
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
        Expr::Switch(_, _, _) => TypeId::Void,
        Expr::Goto(_, _) => TypeId::Void,
        Expr::If(_, _, _, _) => TypeId::Void,
        Expr::Conditional(_, lhs, _, _) => type_of(lhs, scope, pool)?,
        Expr::While(_, _, _) => TypeId::Void,
        Expr::This(pos) => match scope.this {
            Some(class_idx) => TypeId::Ref(Box::new(TypeId::Class(class_idx))),
            None => return Err(Error::CompileError("No 'this' in static context".to_owned(), *pos)),
        },
        Expr::Super(pos) => match scope.this {
            Some(class_idx) => {
                let base_idx = pool.class(class_idx)?.base;
                TypeId::Ref(Box::new(TypeId::Class(base_idx)))
            }
            None => return Err(Error::CompileError("No 'super' in static context".to_owned(), *pos)),
        },
        Expr::Break(_) => TypeId::Void,
        Expr::Null => TypeId::Null,
        Expr::BinOp(_, _, _, pos) => return Err(Error::CompileError("BinOp not supported here".to_owned(), *pos)),
        Expr::UnOp(_, _, pos) => return Err(Error::CompileError("UnOp not supported here".to_owned(), *pos)),
    };
    Ok(res)
}

#[derive(Debug, Clone, Copy, EnumString, Display)]
pub enum IntrinsicOp {
    Equals,
    NotEquals,
    ArrayClear,
    ArraySize,
    ArrayResize,
    ArrayFindFirst,
    ArrayFindLast,
    ArrayContains,
    ArrayCount,
    ArrayPush,
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArrayGrow,
    ArrayErase,
    ArrayLast,
    ToString,
    EnumInt,
    IntEnum,
    ToVariant,
    FromVariant,
    AsRef,
    Deref,
    RefToWeakRef,
    WeakRefToRef,
}

impl IntrinsicOp {
    pub fn arg_count(&self) -> u8 {
        match self {
            IntrinsicOp::Equals => 2,
            IntrinsicOp::NotEquals => 2,
            IntrinsicOp::ArrayClear => 1,
            IntrinsicOp::ArraySize => 1,
            IntrinsicOp::ArrayResize => 2,
            IntrinsicOp::ArrayFindFirst => 2,
            IntrinsicOp::ArrayFindLast => 2,
            IntrinsicOp::ArrayContains => 2,
            IntrinsicOp::ArrayCount => 2,
            IntrinsicOp::ArrayPush => 2,
            IntrinsicOp::ArrayPop => 1,
            IntrinsicOp::ArrayInsert => 3,
            IntrinsicOp::ArrayRemove => 2,
            IntrinsicOp::ArrayGrow => 2,
            IntrinsicOp::ArrayErase => 2,
            IntrinsicOp::ArrayLast => 1,
            IntrinsicOp::ToString => 1,
            IntrinsicOp::EnumInt => 1,
            IntrinsicOp::IntEnum => 1,
            IntrinsicOp::ToVariant => 1,
            IntrinsicOp::FromVariant => 1,
            IntrinsicOp::AsRef => 1,
            IntrinsicOp::Deref => 1,
            IntrinsicOp::RefToWeakRef => 1,
            IntrinsicOp::WeakRefToRef => 1,
        }
    }
}

use std::cell::RefCell;
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;
use std::str::FromStr;
use std::{fmt, iter, mem};

use enum_as_inner::EnumAsInner;
use hashbrown::{hash_map, HashMap, HashSet};
use itertools::{izip, Itertools};
use redscript::ast::{
    Constant, Expr, ExprKind, Ident, Literal, Param, Seq, SourceAst, Span, SwitchCase, TypeName, TypeParam, Variance,
};
use redscript::bytecode::Intrinsic;
use redscript::Str;

use crate::codegen::names;
use crate::error::{CompileError, CompileResult, TypeError, Unsupported};
use crate::scoped_map::ScopedMap;
use crate::type_repo::*;
use crate::{visit_expr, IndexMap};

pub type TypeScope<'scope, 'id> = ScopedMap<'scope, Str, TypeId<'id>>;
pub type NameScope<'scope, 'id> = ScopedMap<'scope, Str, Vec<Global<'id>>>;
pub type Vars<'scope, 'id> = ScopedMap<'scope, Str, InferType<'id>>;
type LocalMap<'scope, 'id> = ScopedMap<'scope, Str, LocalInfo<'id>>;
type RcVar<'id> = Rc<RefCell<Var<'id>>>;
type Inferred<'id> = (Expr<CheckedAst<'id>>, InferType<'id>);

#[derive(Debug)]
pub struct Typer<'ctx, 'id> {
    repo: &'ctx TypeRepo<'id>,
    names: &'ctx NameScope<'ctx, 'id>,
    env: TypeEnv<'ctx, 'ctx, 'id>,
    return_type: InferType<'id>,
    id_alloc: &'ctx mut IdAlloc,
    reporter: &'ctx mut ErrorReporter<'id>,
}

impl<'ctx, 'id> Typer<'ctx, 'id> {
    pub fn new(
        repo: &'ctx TypeRepo<'id>,
        names: &'ctx NameScope<'ctx, 'id>,
        env: TypeEnv<'ctx, 'ctx, 'id>,
        return_type: InferType<'id>,
        id_alloc: &'ctx mut IdAlloc,
        reporter: &'ctx mut ErrorReporter<'id>,
    ) -> Self {
        Self {
            repo,
            env,
            names,
            return_type,
            id_alloc,
            reporter,
        }
    }

    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn run(
        repo: &'ctx TypeRepo<'id>,
        names: &'ctx NameScope<'ctx, 'id>,
        env: TypeEnv<'ctx, 'ctx, 'id>,
        expr: &Seq<SourceAst>,
        locals: &mut LocalMap<'_, 'id>,
        return_type: InferType<'id>,
        id_alloc: &'ctx mut IdAlloc,
        reporter: &'ctx mut ErrorReporter<'id>,
    ) -> Seq<CheckedAst<'id>> {
        Self::new(repo, names, env, return_type, id_alloc, reporter).typeck_seq(expr, locals)
    }

    pub fn typeck_seq(&mut self, expr: &Seq<SourceAst>, locals: &mut LocalMap<'_, 'id>) -> Seq<CheckedAst<'id>> {
        let mut exprs = Vec::with_capacity(expr.exprs.len());
        for expr in &expr.exprs {
            let res = self.typeck(expr, locals);
            let Some((expr, _)) = self.reporter.unwrap_err(res) else {
                continue;
            };
            exprs.push(expr);
        }
        Seq::new(exprs)
    }

    fn typeck(&mut self, expr: &Expr<SourceAst>, locals: &mut LocalMap<'_, 'id>) -> CompileResult<'id, Inferred<'id>> {
        self.typeck_with(expr, locals, false)
    }

    fn typeck_with(
        &mut self,
        expr: &Expr<SourceAst>,
        locals: &mut LocalMap<'_, 'id>,
        is_out: bool,
    ) -> CompileResult<'id, Inferred<'id>> {
        match expr {
            Expr::Ident(id, span) => {
                let info = locals
                    .get(id)
                    .ok_or_else(|| CompileError::UnresolvedVar(id.clone(), *span))?;
                let typ = if !is_out && matches!(&info.typ, InferType::Mono(Mono::Data(data)) if data.id.is_function())
                {
                    info.typ.instantiate(self.id_alloc)
                } else {
                    info.typ.clone()
                };
                Ok((Expr::Ident(info.local, *span), typ))
            }
            Expr::Constant(val, span) => {
                let prim = match val {
                    Constant::String(lit, _) => match lit {
                        Literal::String => Prim::String,
                        Literal::Name => Prim::CName,
                        Literal::Resource => Prim::ResRef,
                        Literal::TweakDbId => Prim::TweakDbId,
                    },
                    Constant::F32(_) => Prim::Float,
                    Constant::F64(_) => Prim::Double,
                    Constant::I32(_) => Prim::Int32,
                    Constant::I64(_) => Prim::Int64,
                    Constant::U32(_) => Prim::Uint32,
                    Constant::U64(_) => Prim::Uint64,
                    Constant::Bool(_) => Prim::Bool,
                };
                Ok((Expr::Constant(val.clone(), *span), InferType::prim(prim)))
            }
            Expr::Declare(name, typ, expr, span) => {
                let (expr, typ) = match (typ, expr) {
                    (Some(ty), Some(expr)) => {
                        let local_ty = self.env.resolve_infer_type(ty, self.repo).with_span(*span)?;
                        let (mut expr, expr_ty) = self.typeck(expr, locals)?;
                        self.constrain(&mut expr, &expr_ty, &local_ty)?;
                        (Some(expr.into()), local_ty)
                    }
                    (None, Some(expr)) => {
                        let (expr, expr_ty) = self.typeck(expr, locals)?;
                        (Some(expr.into()), expr_ty)
                    }
                    (Some(ty), None) => (None, self.env.resolve_infer_type(ty, self.repo).with_span(*span)?),
                    (None, None) => (None, self.id_alloc.allocate_free_type()),
                };
                let info = self.id_alloc.allocate_local(typ.clone());
                let local = info.local;
                if let Some(prev) = locals.insert((*name).clone(), info) {
                    locals.insert(names::shadowed(locals.len()), prev);
                }
                Ok((Expr::Declare(local, Some(typ.into()), expr, *span), InferType::VOID))
            }
            Expr::DynCast(target, expr, span) => {
                let (expr, _) = self.typeck(expr, locals)?;
                let &id = self
                    .env
                    .types
                    .get(target.name())
                    .ok_or_else(|| CompileError::UnresolvedVar(target.name().clone(), *span))?;
                let type_args: Rc<[_]> = self.repo[id]
                    .type_vars()
                    .iter()
                    .map(|var| InferType::from_var_poly(var, self.env.vars, self.id_alloc))
                    .collect();
                let data = Data::new(id, type_args);
                let typ = InferType::data(data.clone());
                Ok((Expr::DynCast(data, expr.into(), *span), typ))
            }
            Expr::Assign(place, expr, _, span) => {
                let (mut expr, expr_ty) = self.typeck(expr, locals)?;
                let (place, place_ty) = self.typeck(place, locals)?;
                self.constrain(&mut expr, &expr_ty, &place_ty)?;
                Ok((Expr::Assign(place.into(), expr.into(), expr_ty, *span), InferType::VOID))
            }
            Expr::Call(expr, _, targs, args, _, span) => match &**expr {
                Expr::Member(expr, name, span) => self.check_overload(expr, name, args, locals, *span),
                Expr::Ident(name, span) if name.as_str() == "Cast" => self.check_cast(args, targs, locals, *span),
                Expr::Ident(name, span) if locals.get(name).is_none() => {
                    match self.names.get(name).map(Vec::as_slice) {
                        Some([Global::MethodAlias(mid)]) => self.check_wrapper(
                            &Expr::This(*span),
                            self.repo.get_method_name(mid).unwrap(),
                            args,
                            locals,
                            Callable::WrappedMethod(mid.clone()),
                            *span,
                        ),
                        Some([Global::StaticAlias(mid)]) => self.check_wrapper(
                            &Expr::Ident(mid.owner().name().into(), *span),
                            self.repo.get_static_name(mid).unwrap(),
                            args,
                            locals,
                            Callable::WrappedStatic(mid.clone()),
                            *span,
                        ),
                        Some(matches) => self.check_global(name, matches, &args[..], locals, *span),
                        _ => Err(CompileError::UnresolvedVar(name.clone(), *span)),
                    }
                }
                _ => {
                    let mut checked_args = Vec::with_capacity(args.len());
                    let mut arg_types = Vec::with_capacity(args.len());
                    for arg in args.iter() {
                        let (arg, typ) = self.typeck(arg, locals)?;
                        checked_args.push(arg);
                        arg_types.push(typ);
                    }
                    let (expr, typ) = self.typeck(expr, locals)?;
                    let ret = self.id_alloc.allocate_free_type();
                    let mut type_args = arg_types.clone();
                    type_args.push(ret.clone());
                    let id = TypeId::get_fn_by_arity(args.len())
                        .ok_or_else(|| CompileError::Unsupported(Unsupported::FunctionMaxArityExceeded, *span))?;
                    typ.constrain(&InferType::data(Data::new(id, type_args.into())), self.repo)
                        .with_span(*span)?;
                    let call = Expr::Call(
                        expr.into(),
                        Callable::Lambda.into(),
                        [].into(),
                        checked_args.into(),
                        CallMetadata::new(arg_types, ret.clone()).into(),
                        *span,
                    );
                    Ok((call, ret))
                }
            },
            Expr::Member(expr, member, span) => {
                if let Some((id, members)) = expr.as_ident().and_then(|(n, _)| {
                    let &id = self.env.types.get(n)?;
                    Some((id, self.repo[id].as_enum()?))
                }) {
                    let member = members
                        .get_member(member)
                        .ok_or_else(|| CompileError::UnresolvedMember(id, member.clone(), *span))?;
                    return Ok((
                        Expr::Member(Expr::EMPTY.into(), Member::EnumMember(FieldId::new(id, member)), *span),
                        InferType::data(Data::without_args(id)),
                    ));
                }
                let (expr, expr_ty) = self.typeck(expr, locals)?;
                let upper_bound = expr_ty
                    .force_upper_bound(self.repo)
                    .ok_or(CompileError::CannotLookupMember(*span))?;
                let (id, fty) = self
                    .repo
                    .upper_iter(upper_bound.id)
                    .find_map(|(type_id, cls)| {
                        let (idx, typ) = cls.fields.by_name(member)?;
                        Some((FieldId::new(type_id, idx), typ))
                    })
                    .ok_or_else(|| CompileError::UnresolvedMember(upper_bound.id, member.clone(), *span))?;
                let spec = upper_bound
                    .instantiate_as(id.owner(), self.repo)
                    .expect("should always match the type of the upper bound");
                let type_vars = self.repo[spec.id]
                    .type_var_names()
                    .zip(spec.args.iter().cloned())
                    .collect();
                let inferred = InferType::from_type(&fty.typ, &type_vars);
                Ok((
                    Expr::Member(expr.into(), Member::Field(id, inferred.clone()), *span),
                    inferred,
                ))
            }
            Expr::ArrayElem(arr, idx, _, span) => {
                let (arr, arr_type) = self.typeck(arr, locals)?;
                let elem = self.id_alloc.allocate_free_type();
                InferType::data(Data::array(elem.clone()))
                    .constrain(&arr_type, self.repo)
                    .with_span(*span)?;
                let (idx, idx_type) = self.typeck(idx, locals)?;
                idx_type
                    .constrain(&InferType::prim(Prim::Int32), self.repo)
                    .with_span(*span)?;
                Ok((Expr::ArrayElem(arr.into(), idx.into(), elem.clone(), *span), elem))
            }
            Expr::New(typ, args, span) => {
                let &id = self
                    .env
                    .types
                    .get(typ.name())
                    .ok_or_else(|| TypeError::UnresolvedType(typ.name().clone()))
                    .with_span(*span)?;
                let data_type = &self.repo[id];
                let type_args: Rc<[_]> = data_type
                    .type_vars()
                    .iter()
                    .map(|var| InferType::from_var_poly(var, self.env.vars, self.id_alloc))
                    .collect();
                let mut checked_args = vec![];
                let mut arg_types = vec![];
                match (&args[..], data_type) {
                    (args, DataType::Class(class)) if class.flags.is_struct() => {
                        for (arg, field) in args.iter().zip(class.fields.iter()) {
                            let (mut arg, typ) = self.typeck(arg, locals)?;
                            self.constrain(&mut arg, &typ, &InferType::from_type(&field.field.typ, self.env.vars))?;
                            checked_args.push(arg);
                            arg_types.push(typ);
                        }
                    }
                    ([], DataType::Class(_)) => {}
                    _ => return Err(CompileError::Unsupported(Unsupported::CustomClassConstructor, *span)),
                };
                let data = Data::new(id, type_args);
                let typ = InferType::Mono(Mono::Data(data.clone()));
                Ok((Expr::New(data, checked_args.into(), *span), typ))
            }
            Expr::Lambda(params, body, span) => {
                let locals = locals.push_scope(CaptureCollector::run(expr, locals.top()));
                self.check_lambda(locals, params, None::<std::iter::Empty<_>>, body, *span)
            }
            Expr::Return(expr, span) => {
                let expr = match expr {
                    Some(expr) => {
                        let (mut expr, expr_ty) = self.typeck(expr, locals)?;
                        self.constrain(&mut expr, &expr_ty, &self.return_type)?;
                        Some(expr.into())
                    }
                    None => {
                        self.return_type
                            .constrain(&InferType::VOID, self.repo)
                            .with_span(*span)?;
                        None
                    }
                };
                Ok((Expr::Return(expr, *span), InferType::VOID))
            }
            Expr::Seq(seq) => Ok((Expr::Seq(self.typeck_seq(seq, locals)), InferType::VOID)),
            Expr::Switch(scrutinee, cases, default, _, span) => {
                let (scrutinee, scrutinee_type) = self.typeck(scrutinee, locals)?;
                let cases = cases
                    .iter()
                    .map(|case| {
                        let (matcher, typ) = self.typeck(&case.matcher, locals)?;
                        typ.constrain(&scrutinee_type, self.repo).with_span(*span)?;
                        let body = self.typeck_seq(&case.body, &mut locals.introduce_scope());
                        Ok(SwitchCase { matcher, body })
                    })
                    .try_collect()?;
                let default = default
                    .as_ref()
                    .map(|body| self.typeck_seq(body, &mut locals.introduce_scope()));
                Ok((
                    Expr::Switch(scrutinee.into(), cases, default, scrutinee_type, *span),
                    InferType::VOID,
                ))
            }
            Expr::If(cond, if_, else_, span) => {
                let (cond, cond_type) = self.typeck(cond, locals)?;
                cond_type
                    .constrain(&InferType::prim(Prim::Bool), self.repo)
                    .with_span(*span)?;
                let if_ = self.typeck_seq(if_, &mut locals.introduce_scope());
                let else_ = else_
                    .as_ref()
                    .map(|e| self.typeck_seq(e, &mut locals.introduce_scope()));
                Ok((Expr::If(cond.into(), if_, else_, *span), InferType::VOID))
            }
            Expr::Conditional(cond, lhs, rhs, span) => {
                let (cond, cond_type) = self.typeck(cond, locals)?;
                cond_type
                    .constrain(&InferType::prim(Prim::Bool), self.repo)
                    .with_span(*span)?;
                let (lhs, lhs_typ) = self.typeck(lhs, locals)?;
                let (rhs, rhs_typ) = self.typeck(rhs, locals)?;
                let res = self.id_alloc.allocate_free_type();
                res.constrain(&lhs_typ, self.repo).with_span(*span)?;
                res.constrain(&rhs_typ, self.repo).with_span(*span)?;
                let expr = Expr::Conditional(cond.into(), lhs.into(), rhs.into(), *span);
                Ok((expr, res))
            }
            Expr::While(cond, body, span) => {
                let (cond, cond_type) = self.typeck(cond, locals)?;
                cond_type
                    .constrain(&InferType::prim(Prim::Bool), self.repo)
                    .with_span(*span)?;
                let body = self.typeck_seq(body, &mut locals.introduce_scope());
                Ok((Expr::While(cond.into(), body, *span), InferType::VOID))
            }
            Expr::ForIn(elem, iter, body, span) => {
                let (iter, iter_type) = self.typeck(iter, locals)?;
                let elem_type = self.id_alloc.allocate_free_type();
                iter_type
                    .constrain(&InferType::Mono(Mono::Data(Data::array(elem_type.clone()))), self.repo)
                    .with_span(*span)?;
                let mut locals = locals.introduce_scope();
                let info = self.id_alloc.allocate_local(elem_type);
                let local = info.local;
                locals.insert(elem.clone(), info);
                let body = self.typeck_seq(body, &mut locals);
                Ok((Expr::ForIn(local, iter.into(), body, *span), InferType::VOID))
            }
            Expr::BinOp(lhs, rhs, op, span) => {
                let name = <&str>::from(op);
                let matches = self
                    .names
                    .get(name)
                    .ok_or_else(|| CompileError::UnresolvedFunction(name.into(), *span))?;
                self.check_global(name, matches, [&**lhs, &**rhs], locals, *span)
            }
            Expr::UnOp(expr, op, span) => {
                let name = <&str>::from(op);
                let matches = self
                    .names
                    .get(name)
                    .ok_or_else(|| CompileError::UnresolvedFunction(name.into(), *span))?;
                self.check_global(name, matches, Some(&**expr), locals, *span)
            }
            &Expr::Break(span) => Ok((Expr::Break(span), InferType::VOID)),
            Expr::This(span) => {
                let loc = locals
                    .get("this")
                    .ok_or_else(|| CompileError::UnresolvedVar(Str::from_static("this"), *span))?;
                Ok((Expr::Ident(loc.local, *span), loc.typ.clone()))
            }
            Expr::Null(span) => Ok((Expr::Null(*span), self.id_alloc.allocate_free_type())),
            Expr::Super(_) => unimplemented!(),
            Expr::Goto(_, _) | Expr::ArrayLit(_, _, _) | Expr::InterpolatedString(_, _, _) => unreachable!(),
        }
    }

    fn check_global<'a>(
        &mut self,
        name: &str,
        overloads: &[Global<'id>],
        args: impl IntoIterator<IntoIter = impl ExactSizeIterator<Item = &'a Expr<SourceAst>>>,
        locals: &mut LocalMap<'_, 'id>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let mut type_vars = ScopedMap::default();

        let args = args.into_iter();
        let arg_count = args.len();
        let mut checked_args = Vec::with_capacity(arg_count);
        let mut arg_types = Vec::with_capacity(arg_count);
        let candidates = overloads
            .iter()
            .flat_map(|id| {
                self.repo
                    .globals()
                    .get_overloads(id.index().expect("global should always have an index"))
                    .map(move |e| (id, e))
            })
            .collect_vec();
        let (id, entry) = match candidates
            .iter()
            .filter(|(_, entry)| entry.function.typ.params.len() == arg_count)
            .exactly_one()
        {
            Ok((id, entry)) => {
                for var in &*entry.function.typ.type_vars {
                    let typ = InferType::from_var_poly(var, &type_vars, self.id_alloc);
                    type_vars.insert(var.name.clone(), typ);
                }
                for (arg, param) in args.zip(entry.function.typ.params.iter()) {
                    let (arg, typ) = self.typeck_with(arg, locals, param.is_out)?;
                    checked_args.push(arg);
                    arg_types.push(typ);
                }
                (id, entry)
            }
            Err(matches) => {
                for arg in args {
                    let (arg, typ) = self.typeck_with(arg, locals, true)?;
                    checked_args.push(arg);
                    arg_types.push(typ);
                }
                let (id, entry) = matches
                    .filter(|(_, entry)| {
                        arg_types
                            .iter()
                            .zip(entry.function.typ.params.iter())
                            .all(|(it, par)| it.is_same_shape(&par.typ))
                    })
                    .exactly_one()
                    .map_err(|m| {
                        CompileError::for_overloads(name.into(), m.count(), candidates.iter().map(|(_, e)| e), span)
                    })?;
                for var in &*entry.function.typ.type_vars {
                    let typ = InferType::from_var_poly(var, &type_vars, self.id_alloc);
                    type_vars.insert(var.name.clone(), typ);
                }
                (id, entry)
            }
        };

        for (param, arg_type, arg) in izip!(&entry.function.typ.params[..], &arg_types, &mut checked_args) {
            let expected = InferType::from_type(&param.typ, &type_vars);
            if param.is_out {
                arg_type.constrain_invariant(&expected, self.repo).with_span(span)?;
            } else {
                self.constrain(arg, arg_type, &expected)?;
            }
        }
        let ret = InferType::from_type(&entry.function.typ.ret, &type_vars);
        let targs = type_vars.pop_scope().into_iter().map(|(_, typ)| typ).collect();
        let call = if let Global::Intrinsic(_, intrinsic) = id {
            Callable::Intrinsic(*intrinsic)
        } else {
            Callable::Global(GlobalId::new(entry.index))
        };
        let meta = CallMetadata::new(arg_types, ret.clone()).into();
        let expr = Expr::Call(Expr::EMPTY.into(), call.into(), targs, checked_args.into(), meta, span);
        Ok((expr, ret))
    }

    fn check_overload(
        &mut self,
        expr: &Expr<SourceAst>,
        name: &str,
        args: &[Expr<SourceAst>],
        locals: &mut LocalMap<'_, 'id>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let (expr, upper_bound, this) = if let Some(id) = expr
            .as_ident()
            .and_then(|(name, _)| self.env.resolve_type_id(name).ok())
        {
            (
                None,
                Data::from_type(&Parameterized::without_args(id), self.env.vars),
                None,
            )
        } else {
            let (expr, expr_type) = self.typeck(expr, locals)?;
            (
                Some(expr),
                expr_type
                    .force_upper_bound(self.repo)
                    .ok_or(CompileError::CannotLookupMember(span))?,
                Some(expr_type),
            )
        };

        let candidates = if expr.is_some() {
            self.repo.resolve_methods(upper_bound.id, name).collect_vec()
        } else {
            self.repo.resolve_statics(upper_bound.id, name).collect_vec()
        };

        let mut checked_args = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        let mut type_vars = ScopedMap::default();

        let (owner, entry) = match candidates
            .iter()
            .filter(|(_, entry)| entry.function.typ.params.len() == args.len())
            .exactly_one()
        {
            Ok((owner, entry)) => {
                let data_type = &self.repo[*owner];
                if expr.is_none() && upper_bound.args.is_empty() {
                    let it = data_type.type_vars().iter().map(|var| {
                        let typ = InferType::from_var_poly(var, &ScopedMap::default(), self.id_alloc);
                        (var.name.clone(), typ)
                    });
                    type_vars.extend(it);
                } else {
                    let owner_type = upper_bound
                        .instantiate_as(*owner, self.repo)
                        .expect("should always match the type of the upper bound");
                    assert_eq!(owner_type.args.len(), data_type.type_vars().len());

                    type_vars.extend(data_type.type_var_names().zip(owner_type.args.iter().cloned()));
                };

                for var in &*entry.function.typ.type_vars {
                    let typ = InferType::from_var_poly(var, &type_vars, self.id_alloc);
                    type_vars.insert(var.name.clone(), typ);
                }
                for (arg, param) in args.iter().zip(entry.function.typ.params.iter()) {
                    let (arg, typ) = match (arg, &param.typ) {
                        (Expr::Lambda(params, body, span), Type::Data(data))
                            if TypeId::get_fn_by_arity(params.len()) == Some(data.id) =>
                        {
                            let locals = locals.push_scope(CaptureCollector::run(arg, locals.top()));
                            self.check_lambda_against(locals, params, body, data, &type_vars, *span)?
                        }
                        _ => self.typeck_with(arg, locals, param.is_out)?,
                    };
                    checked_args.push(arg);
                    arg_types.push(typ);
                }
                (owner, entry)
            }
            Err(matches) => {
                for arg in args {
                    let (arg, typ) = self.typeck_with(arg, locals, true)?;
                    checked_args.push(arg);
                    arg_types.push(typ);
                }
                let (owner, entry) = matches
                    .filter(|(_, entry)| {
                        arg_types
                            .iter()
                            .zip(entry.function.typ.params.iter())
                            .all(|(it, par)| it.is_same_shape(&par.typ))
                    })
                    .exactly_one()
                    .map_err(|matches| {
                        CompileError::for_overloads(
                            name.into(),
                            matches.count(),
                            candidates.iter().map(|(_, e)| e),
                            span,
                        )
                    })?;
                let data_type = &self.repo[*owner];
                if expr.is_none() && upper_bound.args.is_empty() {
                    let it = data_type.type_vars().iter().map(|var| {
                        let typ = InferType::from_var_poly(var, &ScopedMap::default(), self.id_alloc);
                        (var.name.clone(), typ)
                    });
                    type_vars.extend(it);
                } else {
                    let owner_type = upper_bound
                        .instantiate_as(*owner, self.repo)
                        .expect("should always match the type of the upper bound");
                    assert_eq!(owner_type.args.len(), data_type.type_vars().len());

                    type_vars.extend(data_type.type_var_names().zip(owner_type.args.iter().cloned()));
                };
                (owner, entry)
            }
        };

        for (param, arg_type, arg) in izip!(&entry.function.typ.params[..], &arg_types, &mut checked_args) {
            let expected = InferType::from_type(&param.typ, &type_vars);
            if param.is_out {
                arg_type.constrain_invariant(&expected, self.repo).with_span(span)?;
            } else {
                self.constrain(arg, arg_type, &expected)?;
            }
        }
        let ret = InferType::from_type(&entry.function.typ.ret, &type_vars);
        let meta = CallMetadata {
            arg_types: arg_types.into(),
            ret_type: ret.clone(),
            this_type: this,
        }
        .into();
        let expr = if let Some(expr) = expr {
            let call = Callable::Instance(MethodId::new(*owner, entry.index));
            Expr::Call(expr.into(), call.into(), [].into(), checked_args.into(), meta, span)
        } else {
            Expr::Call(
                Expr::EMPTY.into(),
                Callable::Static(MethodId::new(*owner, entry.index)).into(),
                [].into(),
                checked_args.into(),
                meta,
                span,
            )
        };
        Ok((expr, ret))
    }

    fn check_wrapper(
        &mut self,
        expr: &Expr<SourceAst>,
        name: &str,
        args: &[Expr<SourceAst>],
        locals: &mut LocalMap<'_, 'id>,
        wrapped: Callable<'id>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let (mut expr, typ) = self.check_overload(expr, name, args, locals, span)?;
        if let Expr::Call(_, callable, _, _, _, _) = &mut expr {
            *callable.as_mut() = wrapped;
        }
        Ok((expr, typ))
    }

    fn constrain(
        &self,
        expr: &mut Expr<CheckedAst<'id>>,
        lhs: &InferType<'id>,
        rhs: &InferType<'id>,
    ) -> CompileResult<'id, ()> {
        let span = expr.span();
        if let (Some((cons, span)), &InferType::Mono(Mono::Prim(from)), &InferType::Mono(Mono::Prim(to))) =
            (expr.as_constant_mut(), lhs, rhs)
        {
            if from == to || Self::adapt_constant(cons, to) {
                return Ok(());
            }
            return Err(TypeError::Mismatch(Mono::Prim(from), Mono::Prim(to))).with_span(*span);
        }
        let op = match lhs.constrain_top(rhs, self.repo).with_span(span)? {
            Some(Convert::From(RefType::Weak)) => Intrinsic::WeakRefToRef,
            Some(Convert::Into(RefType::Weak)) => Intrinsic::RefToWeakRef,
            Some(Convert::From(RefType::Script)) => Intrinsic::Deref,
            Some(Convert::Into(RefType::Script)) => Intrinsic::AsRef,
            None => return Ok(()),
        };
        *expr = Expr::Call(
            Expr::EMPTY.into(),
            Callable::Intrinsic(op).into(),
            [lhs.clone()].into(),
            [mem::take(expr)].into(),
            CallMetadata::empty().into(),
            span,
        );
        Ok(())
    }

    fn adapt_constant(val: &mut Constant, to: Prim) -> bool {
        let replacement = match (&*val, to) {
            (&Constant::I32(i), Prim::Float) => Constant::F32(i as f32),
            (&Constant::I64(i), Prim::Float) => Constant::F32(i as f32),
            (&Constant::U32(i), Prim::Float) => Constant::F32(i as f32),
            (&Constant::U64(i), Prim::Float) => Constant::F32(i as f32),
            (&Constant::I32(i), Prim::Double) => Constant::F64(i as f64),
            (&Constant::I64(i), Prim::Double) => Constant::F64(i as f64),
            (&Constant::U32(i), Prim::Double) => Constant::F64(i as f64),
            (&Constant::U64(i), Prim::Double) => Constant::F64(i as f64),
            _ => return false,
        };
        *val = replacement;
        true
    }

    fn check_lambda(
        &mut self,
        mut locals: LocalMap<'_, 'id>,
        lambda_params: &[Param],
        expected_params: Option<impl IntoIterator<Item = InferType<'id>>>,
        body: &Seq<SourceAst>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let mut fn_type_args = expected_params
            .into_iter()
            .flatten()
            .map(Some)
            .chain(iter::repeat(None))
            .zip(lambda_params)
            .map(|(expected, param)| match (&param.typ, expected) {
                (Some(typ), _) => self.env.resolve_infer_type(typ, self.repo),
                (_, Some(expected)) => Ok(expected.clone()),
                (None, None) => Ok(self.id_alloc.allocate_free_type()),
            })
            .collect::<Result<Vec<_>, _>>()
            .with_span(span)?;
        let param_locals = lambda_params
            .iter()
            .zip(&fn_type_args)
            .map(|(param, typ)| (param.name.clone(), self.id_alloc.allocate_param(typ.clone())));
        locals.extend(param_locals);

        let ret_type = self.id_alloc.allocate_free_type();
        let body = Typer::run(
            self.repo,
            self.names,
            self.env.clone(),
            body,
            &mut locals,
            ret_type.clone(),
            self.id_alloc,
            self.reporter,
        );
        fn_type_args.push(ret_type.clone());

        let id = TypeId::get_fn_by_arity(lambda_params.len())
            .ok_or_else(|| CompileError::Unsupported(Unsupported::FunctionMaxArityExceeded, span))?;
        let typ = InferType::data(Data::new(id, fn_type_args.into()));

        let mut params = IndexMap::default();
        let mut captures = IndexMap::default();
        for (_, loc) in locals.pop_scope() {
            match (loc.local, loc.captured) {
                (Local::Param(_), _) => {
                    params.insert(loc.local, loc.typ);
                }
                (Local::Capture(_), Some(capture)) => {
                    captures.insert(loc.local, (loc.typ, capture));
                }
                _ => {}
            }
        }
        params.sort_by(|a, _, b, _| a.cmp(b));

        let env = ClosureEnv {
            params,
            captures,
            ret_type,
        };
        Ok((Expr::Lambda(env.into(), body, span), typ))
    }

    fn check_lambda_against(
        &mut self,
        locals: LocalMap<'_, 'id>,
        lambda_params: &[Param],
        body: &Seq<SourceAst>,
        data: &Parameterized<'id>,
        type_vars: &Vars<'_, 'id>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let expected = data
            .args
            .iter()
            .dropping_back(1)
            .map(|typ| InferType::from_type(typ, type_vars).regeneralize(self.repo, type_vars));
        self.check_lambda(locals, lambda_params, Some(expected), body, span)
    }

    fn check_cast(
        &mut self,
        args: &[Expr<SourceAst>],
        targs: &[TypeName],
        locals: &mut LocalMap<'_, 'id>,
        span: Span,
    ) -> CompileResult<'id, Inferred<'id>> {
        let target = match targs {
            [name] => self.env.resolve_infer_type(name, self.repo).with_span(span)?,
            [] => self.id_alloc.allocate_free_type(),
            _ => return Err(TypeError::InvalidNumberOfFunctionTypeArgs(targs.len(), 1)).with_span(span),
        };
        let [arg] = args else {
            return Err(CompileError::UnresolvedFunction(Str::from_static("Cast"), span));
        };
        let (arg, arg_type) = self.typeck(arg, locals)?;
        let expr = Expr::Call(
            Expr::EMPTY.into(),
            Callable::Cast.into(),
            [target.clone()].into(),
            [arg].into(),
            CallMetadata::new([arg_type], target.clone()).into(),
            span,
        );
        Ok((expr, target))
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'ctx, 'scope, 'id> {
    types: &'ctx TypeScope<'scope, 'id>,
    vars: &'ctx Vars<'scope, 'id>,
}

impl<'ctx, 'scope, 'id> TypeEnv<'ctx, 'scope, 'id> {
    #[inline]
    pub fn new(types: &'ctx TypeScope<'scope, 'id>, vars: &'ctx Vars<'scope, 'id>) -> Self {
        Self { types, vars }
    }

    #[inline]
    pub fn with_vars(&self, vars: &'ctx Vars<'scope, 'id>) -> Self {
        Self {
            types: self.types,
            vars,
        }
    }

    fn resolve_infer_type(
        &self,
        name: &TypeName,
        validator: &dyn TypeValidator<'id>,
    ) -> Result<InferType<'id>, TypeError<'id>> {
        let ty = self.resolve_type(name, validator)?;
        Ok(InferType::from_type(&ty, self.vars))
    }

    pub fn resolve_type_id(&self, name: &str) -> Result<TypeId<'id>, TypeError<'id>> {
        self.types
            .get(name)
            .ok_or_else(|| TypeError::UnresolvedType(name.into()))
            .copied()
    }

    pub fn resolve_type(
        &self,
        name: &TypeName,
        validator: &dyn TypeValidator<'id>,
    ) -> Result<Type<'id>, TypeError<'id>> {
        if self.vars.get(name.name()).is_some() {
            Ok(Type::Var(VarName::Named(name.name().clone())))
        } else if name.name() == "Nothing" {
            Ok(Type::Bottom)
        } else if name.name() == "Any" {
            Ok(Type::Top)
        } else if name.name() == "ref" {
            let typ = name
                .arguments()
                .iter()
                .exactly_one()
                .map_err(|e| TypeError::InvalidNumberOfTypeArgs(e.count(), 1, predef::REF))?;
            self.resolve_type(typ, validator)
        } else if let Ok(prim) = Prim::from_str(name.name()) {
            Ok(Type::Prim(prim))
        } else {
            Ok(Type::Data(self.resolve_param_type(name, validator)?))
        }
    }

    pub fn resolve_param_type(
        &self,
        name: &TypeName,
        validator: &dyn TypeValidator<'id>,
    ) -> Result<Parameterized<'id>, TypeError<'id>> {
        let id = self.resolve_type_id(name.name())?;
        let args = name
            .arguments()
            .iter()
            .map(|arg| self.resolve_type(arg, validator))
            .try_collect()?;
        let result = Parameterized::new(id, args);
        validator.validate(&result)?;
        Ok(result)
    }

    pub fn instantiate_var(
        &self,
        var: &TypeParam,
        validator: &dyn TypeValidator<'id>,
    ) -> Result<TypeVar<'id>, TypeError<'id>> {
        let var = TypeVar {
            name: var.name.clone(),
            variance: var.variance,
            lower: None,
            upper: var
                .extends
                .as_ref()
                .map(|typ| self.resolve_param_type(typ, validator))
                .transpose()?,
        };
        Ok(var)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InferType<'id> {
    Mono(Mono<'id>),
    Poly(RcVar<'id>),
}

impl<'id> InferType<'id> {
    pub const TOP: Self = Self::Mono(Mono::Top);
    pub const VOID: Self = Self::prim(Prim::Void);

    #[inline]
    pub const fn prim(prim: Prim) -> Self {
        Self::Mono(Mono::Prim(prim))
    }

    #[inline]
    pub const fn data(data: Data<'id>) -> Self {
        Self::Mono(Mono::Data(data))
    }

    pub fn from_type(typ: &Type<'id>, vars: &Vars<'_, 'id>) -> Self {
        Self::from_type_with(typ, vars, false)
    }

    pub fn from_type_with(typ: &Type<'id>, vars: &Vars<'_, 'id>, allow_undefined: bool) -> Self {
        match typ {
            Type::Bottom => Self::Mono(Mono::Bottom),
            Type::Top => Self::Mono(Mono::Top),
            Type::Data(typ) => Self::Mono(Mono::Data(Data::from_type_with(typ, vars, allow_undefined))),
            Type::Prim(prim) => Self::Mono(Mono::Prim(*prim)),
            Type::Var(VarName::Named(var)) => vars
                .get(var)
                .cloned()
                .or_else(|| allow_undefined.then(|| Self::Mono(Mono::Var(MonoVar::with_name(var.clone()).into()))))
                .expect("unresolved var"),
            Type::Var(_) => unreachable!(),
        }
    }

    #[inline]
    fn from_var_poly(var: &TypeVar<'id>, ctx: &Vars<'_, 'id>, var_alloc: &mut IdAlloc) -> Self {
        Self::Poly(Var::from_type_var(var, ctx, var_alloc))
    }

    #[inline]
    pub fn from_var_mono(var: &TypeVar<'id>, ctx: &Vars<'_, 'id>) -> Self {
        Self::Mono(Mono::from_type_var(var, ctx))
    }

    #[inline]
    pub fn simplify(&self, repo: &TypeRepo<'id>) -> Type<'id> {
        Simplifier::run(self, repo)
    }

    #[inline]
    pub fn regeneralize(&self, repo: &TypeRepo<'id>, vars: &Vars<'_, 'id>) -> Self {
        Self::from_type(&Self::simplify(self, repo), vars)
    }

    #[inline]
    fn instantiate(&self, vars: &mut IdAlloc) -> Self {
        self.freshen(&mut HashMap::default(), vars)
    }

    fn freshen(&self, ctx: &mut HashMap<VarId, RcVar<'id>>, alloc: &mut IdAlloc) -> Self {
        match self {
            Self::Mono(mono) => Self::Mono(mono.freshen(ctx, alloc)),
            Self::Poly(var) => {
                let id = var.borrow().id;
                let res = if let Some(ex) = ctx.get(&id) {
                    ex.clone()
                } else {
                    let rep = Var::rep(var.clone());
                    let Var { upper, lower, .. } = &*rep.borrow();
                    let lower = lower.freshen(ctx, alloc);
                    let upper = upper.freshen(ctx, alloc);
                    let res = alloc.allocate_tvar(lower, upper);
                    ctx.insert(rep.borrow().id, res.clone());
                    res
                };
                Self::Poly(res)
            }
        }
    }

    fn glb(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<Self, TypeError<'id>> {
        match (self, rhs) {
            (Self::Mono(l), Self::Mono(r)) => Ok(Self::Mono(l.glb(r, type_repo)?)),
            (Self::Poly(l), Self::Poly(r)) => {
                Var::unify(l.clone(), r.clone(), type_repo)?;
                Ok(Self::Poly(r.clone()))
            }
            (Self::Mono(ty), Self::Poly(var)) | (Self::Poly(var), Self::Mono(ty)) => {
                Var::add_lower_bound(var.clone(), ty, type_repo)?;
                Ok(Self::Poly(var.clone()))
            }
        }
    }

    fn lub(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<Self, TypeError<'id>> {
        match (self, rhs) {
            (Self::Mono(l), Self::Mono(r)) => Ok(Self::Mono(l.lub(r, type_repo)?)),
            (Self::Poly(l), Self::Poly(r)) => {
                Var::unify(l.clone(), r.clone(), type_repo)?;
                Ok(Self::Poly(r.clone()))
            }
            (Self::Mono(ty), Self::Poly(var)) | (Self::Poly(var), Self::Mono(ty)) => {
                Var::add_upper_bound(var.clone(), ty, type_repo)?;
                Ok(Self::Poly(var.clone()))
            }
        }
    }

    fn constrain_top(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<Option<Convert>, TypeError<'id>> {
        match (self.ref_type(), rhs.ref_type()) {
            (Some((from, pointee)), None) => {
                pointee.constrain(rhs, type_repo)?;
                Ok(Some(Convert::From(from)))
            }
            (None, Some((to, pointee))) => {
                self.constrain(&pointee, type_repo)?;
                Ok(Some(Convert::Into(to)))
            }
            _ => {
                self.constrain(rhs, type_repo)?;
                Ok(None)
            }
        }
    }

    fn constrain(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        match (self, rhs) {
            (lhs, rhs) if lhs == rhs => Ok(()),
            (Self::Mono(lhs), Self::Mono(rhs)) => lhs.constrain(rhs, type_repo),
            (Self::Poly(lhs), Self::Poly(rhs)) => Var::unify(lhs.clone(), rhs.clone(), type_repo),
            (Self::Poly(lhs), Self::Mono(rhs)) => Var::add_upper_bound(lhs.clone(), rhs, type_repo),
            (Self::Mono(lhs), Self::Poly(rhs)) => Var::add_lower_bound(rhs.clone(), lhs, type_repo),
        }
    }

    fn constrain_invariant(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        self.constrain(rhs, type_repo)?;
        rhs.constrain(self, type_repo)
    }

    pub fn is_same_shape(&self, typ: &Type<'id>) -> bool {
        match self {
            InferType::Mono(mono) => mono.is_same_shape(typ),
            InferType::Poly(poly) => {
                let var = poly.borrow();
                var.lower.is_same_shape(typ) || var.upper.is_same_shape(typ)
            }
        }
    }

    fn force_upper_bound(&self, type_repo: &TypeRepo<'id>) -> Option<Data<'id>> {
        if let Some((_, typ)) = self.ref_type() {
            typ.force_upper_bound(type_repo)
        } else {
            match self {
                Self::Mono(mono) => mono.known_upper_bound(),
                Self::Poly(var) => {
                    let bound = Var::either_bound(var.clone())?;
                    Var::add_upper_bound(var.clone(), &bound, type_repo).ok()?;
                    bound.known_upper_bound()
                }
            }
        }
    }

    pub fn ref_type(&self) -> Option<(RefType, InferType<'id>)> {
        match self {
            Self::Mono(data) => data.ref_type(),
            Self::Poly(poly) => poly.borrow().lower.ref_type(),
        }
    }
}

impl fmt::Display for InferType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InferType::Mono(mono) => mono.fmt(f),
            InferType::Poly(_) => f.write_str("α"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Mono<'id> {
    Bottom,
    Top,
    Data(Data<'id>),
    Prim(Prim),
    Var(Rc<MonoVar<'id>>),
}

impl<'id> Mono<'id> {
    pub fn from_type_var(var: &TypeVar<'id>, ctx: &Vars<'_, 'id>) -> Self {
        Self::Var(MonoVar::from_type_var(var, ctx).into())
    }

    fn freshen(&self, ctx: &mut HashMap<VarId, RcVar<'id>>, vars: &mut IdAlloc) -> Self {
        match self {
            Self::Data(data) => {
                let data = Data::new(data.id, data.args.iter().map(|ty| ty.freshen(ctx, vars)).collect());
                Self::Data(data)
            }
            Self::Prim(_) | Self::Var(_) | Self::Top | Self::Bottom => self.clone(),
        }
    }

    fn glb(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<Self, TypeError<'id>> {
        match (self, rhs) {
            (Self::Top, _) => Ok(rhs.clone()),
            (_, Self::Top) => Ok(self.clone()),
            (Self::Bottom, _) | (_, Self::Bottom) => Ok(Self::Bottom),
            (Self::Data(lhs), Self::Data(rhs)) => {
                let target = type_repo
                    .glb(lhs.id, rhs.id)
                    .ok_or_else(|| TypeError::Unsatisfied(lhs.clone(), rhs.clone()))?;
                if target == lhs.id {
                    Ok(Self::Data(lhs.clone()))
                } else {
                    Ok(Self::Data(rhs.clone()))
                }
            }
            (&Self::Prim(n0), &Self::Prim(n1)) if n0 == n1 => Ok(Self::Prim(n0)),
            (Self::Data(data), Self::Prim(prim)) if !type_repo[data.id].is_builtin() => {
                let id = prim
                    .boxed_type()
                    .ok_or_else(|| TypeError::CannotUnify(self.clone(), rhs.clone()))?;
                self.glb(&Self::Data(Data::new(id, Rc::new([]))), type_repo)
            }
            (Self::Var(v0), l1) => v0.upper.glb(l1, type_repo),
            (l0, Self::Var(v0)) => l0.glb(&v0.upper, type_repo),
            (l, r) => Err(TypeError::CannotUnify(l.clone(), r.clone())),
        }
    }

    fn lub(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<Self, TypeError<'id>> {
        match (self, rhs) {
            (Self::Bottom, _) => Ok(rhs.clone()),
            (_, Self::Bottom) => Ok(self.clone()),
            (Self::Top, _) | (_, Self::Top) => Ok(Self::Top),
            (Self::Data(lhs), Self::Data(rhs)) => {
                let id = type_repo
                    .lub(lhs.id, rhs.id)
                    .ok_or_else(|| TypeError::Unsatisfied(lhs.clone(), rhs.clone()))?;
                let lhs = lhs
                    .clone()
                    .instantiate_as(id, type_repo)
                    .expect("should always match the lub type");
                let rhs = rhs
                    .clone()
                    .instantiate_as(id, type_repo)
                    .expect("should always match the lub type");
                let vars = type_repo[id].type_vars();
                let args = izip!(vars, lhs.args.iter(), rhs.args.iter())
                    .map(|(var, l, r)| match var.variance {
                        Variance::Co => l.lub(r, type_repo),
                        Variance::Contra => l.glb(r, type_repo),
                        Variance::In => l.constrain_invariant(r, type_repo).map(|_| l.clone()),
                    })
                    .try_collect()?;
                Ok(Self::Data(Data::new(id, args)))
            }
            (&Self::Prim(n0), &Self::Prim(n1)) if n0 == n1 => Ok(Self::Prim(n0)),
            (Self::Var(v0), u1) => v0.lower.lub(u1, type_repo),
            (u0, Self::Var(v1)) => u0.lub(&v1.lower, type_repo),
            (l, r) => Err(TypeError::CannotUnify(l.clone(), r.clone())),
        }
    }

    fn constrain(&self, rhs: &Self, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        match (self, rhs) {
            (Self::Bottom, _) | (_, Self::Top) => Ok(()),
            (Self::Prim(lhs), Self::Prim(rhs)) if lhs == rhs => Ok(()),
            (Self::Data(l), Self::Data(r)) => {
                let lhs = l
                    .clone()
                    .instantiate_as(r.id, type_repo)
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), rhs.clone()))?;
                let vars = type_repo[r.id].type_vars();
                for (var, l, r) in izip!(vars, lhs.args.iter(), r.args.iter()) {
                    match var.variance {
                        Variance::Co => l.constrain(r, type_repo)?,
                        Variance::Contra => r.constrain(l, type_repo)?,
                        Variance::In => {
                            l.constrain_invariant(r, type_repo)?;
                        }
                    }
                }
                Ok(())
            }
            (Self::Var(lhs), Self::Var(rhs)) if lhs.name == rhs.name => Ok(()),
            (Self::Var(lhs), rhs) if lhs.upper != Mono::Top => lhs.upper.constrain(rhs, type_repo),
            (lhs, Self::Var(rhs)) if rhs.lower != Mono::Bottom => lhs.constrain(&rhs.upper, type_repo),
            (Self::Prim(prim), Self::Data(data)) if !type_repo[data.id].is_builtin() => {
                let id = prim
                    .boxed_type()
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), rhs.clone()))?;
                Self::Data(Data::new(id, Rc::new([]))).constrain(rhs, type_repo)
            }
            _ => Err(TypeError::Mismatch(self.clone(), rhs.clone())),
        }
    }

    fn is_same_shape(&self, typ: &Type<'id>) -> bool {
        match (self, typ) {
            (Self::Bottom, _) | (_, Type::Top | Type::Var(_)) => true,
            (Self::Prim(lhs), Type::Prim(rhs)) => lhs == rhs,
            (Self::Data(lhs), Type::Data(rhs)) if lhs.id == rhs.id => true,
            (lhs, Type::Data(rhs)) => match (lhs.ref_type(), rhs.id.ref_type(), &rhs.args[..]) {
                (Some((_, lhs)), Some(_), [rhs]) => lhs.is_same_shape(rhs),
                (Some((_, lhs)), None, _) => lhs.is_same_shape(typ),
                (None, Some(_), [rhs]) => self.is_same_shape(rhs),
                _ => false,
            },
            _ => false,
        }
    }

    fn known_upper_bound(&self) -> Option<Data<'id>> {
        match self {
            Self::Data(data) => Some(data.clone()),
            Self::Var(var) => var.upper.known_upper_bound(),
            _ => None,
        }
    }

    #[inline]
    pub fn ref_type(&self) -> Option<(RefType, InferType<'id>)> {
        self.as_data().and_then(Data::ref_type)
    }
}

impl fmt::Display for Mono<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bottom => f.write_str("Nothing"),
            Self::Top => f.write_str("Any"),
            Self::Data(data) => data.fmt(f),
            Self::Prim(prim) => prim.fmt(f),
            Self::Var(var) => f.write_str(&var.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data<'id> {
    pub id: TypeId<'id>,
    pub args: Rc<[InferType<'id>]>,
}

impl<'id> Data<'id> {
    #[inline]
    pub fn new(id: TypeId<'id>, args: Rc<[InferType<'id>]>) -> Self {
        Self { id, args }
    }

    #[inline]
    pub fn without_args(id: TypeId<'id>) -> Self {
        Self { id, args: Rc::new([]) }
    }

    pub fn array(elem: InferType<'id>) -> Self {
        Self::new(predef::ARRAY, Rc::new([elem]))
    }

    #[inline]
    fn from_type(typ: &Parameterized<'id>, vars: &Vars<'_, 'id>) -> Self {
        Self::from_type_with(typ, vars, false)
    }

    fn from_type_with(typ: &Parameterized<'id>, vars: &Vars<'_, 'id>, allow_undefined: bool) -> Self {
        Self {
            id: typ.id,
            args: typ
                .args
                .iter()
                .map(|arg| InferType::from_type_with(arg, vars, allow_undefined))
                .collect(),
        }
    }

    pub fn instantiate_as(self, target: TypeId<'id>, type_repo: &TypeRepo<'id>) -> Option<Self> {
        let mut cur = self;
        while cur.id != target {
            let class = &type_repo[cur.id];
            assert_eq!(
                class.type_vars().len(),
                cur.args.len(),
                "number of type vars should always match args"
            );

            let map = class.type_var_names().zip(cur.args.iter().cloned()).collect();
            cur = Self::from_type(class.base()?, &map);
        }
        Some(cur)
    }

    pub fn ref_type(&self) -> Option<(RefType, InferType<'id>)> {
        let tp = self.id.ref_type()?;
        Some((tp, self.args.first()?.clone()))
    }
}

impl fmt::Display for Data<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.id.name())?;
        if !self.args.is_empty() {
            write!(f, "<{}>", self.args.iter().format(", "))?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Var<'id> {
    id: VarId,
    lower: Mono<'id>,
    upper: Mono<'id>,
    rep: Option<RcVar<'id>>,
}

impl<'id> Var<'id> {
    fn from_type_var(var: &TypeVar<'id>, ctx: &Vars<'_, 'id>, var_alloc: &mut IdAlloc) -> RcVar<'id> {
        var_alloc.allocate_tvar(
            var.lower
                .as_ref()
                .map(|typ| Mono::Data(Data::from_type(typ, ctx)))
                .unwrap_or(Mono::Bottom),
            var.upper
                .as_ref()
                .map(|typ| Mono::Data(Data::from_type(typ, ctx)))
                .unwrap_or(Mono::Top),
        )
    }

    fn rep(this: RcVar<'id>) -> RcVar<'id> {
        let mut borrow = this.borrow_mut();
        match &borrow.rep {
            Some(rep) => {
                let res = Self::rep(rep.clone());
                borrow.rep = Some(res.clone());
                res
            }
            None => this.clone(),
        }
    }

    fn add_upper_bound(this: RcVar<'id>, that: &Mono<'id>, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        // todo: occurs check
        let rep = Self::rep(this);
        let mut rep = rep.borrow_mut();
        rep.upper = rep.upper.glb(that, type_repo)?;
        rep.lower.constrain(that, type_repo)
    }

    fn add_lower_bound(this: RcVar<'id>, that: &Mono<'id>, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        // todo: occurs check
        let rep = Self::rep(this);
        let mut rep = rep.borrow_mut();
        rep.lower = rep.lower.lub(that, type_repo)?;
        that.constrain(&rep.upper, type_repo)
    }

    fn unify(this: RcVar<'id>, that: RcVar<'id>, type_repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        let rep0 = Self::rep(this);
        let rep1 = Self::rep(that);
        if rep0.borrow().id != rep1.borrow().id {
            // todo: occurs check
            let mut r0 = rep0.borrow_mut();
            Self::add_lower_bound(rep1.clone(), &r0.lower, type_repo)?;
            Self::add_upper_bound(rep1.clone(), &r0.upper, type_repo)?;
            r0.rep = Some(rep1);
        }
        Ok(())
    }

    pub(crate) fn either_bound(this: RcVar<'id>) -> Option<Mono<'id>> {
        let rep = Self::rep(this);
        let rep = rep.borrow();
        if !rep.lower.is_bottom() {
            Some(rep.lower.clone())
        } else if !rep.upper.is_top() {
            Some(rep.upper.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MonoVar<'id> {
    name: Str,
    lower: Mono<'id>,
    upper: Mono<'id>,
}

impl<'id> MonoVar<'id> {
    #[inline]
    fn with_name(name: Str) -> Self {
        Self {
            name,
            lower: Mono::Bottom,
            upper: Mono::Top,
        }
    }

    fn from_type_var(var: &TypeVar<'id>, ctx: &Vars<'_, 'id>) -> Self {
        Self {
            name: var.name.clone(),
            lower: var
                .lower
                .as_ref()
                .map(|typ| Mono::Data(Data::from_type(typ, ctx)))
                .unwrap_or(Mono::Bottom),
            upper: var
                .upper
                .as_ref()
                .map(|typ| Mono::Data(Data::from_type(typ, ctx)))
                .unwrap_or(Mono::Top),
        }
    }
}

#[derive(Debug)]
struct Simplifier<'ctx, 'id> {
    type_repo: &'ctx TypeRepo<'id>,
    contra: HashSet<VarId>,
    co: HashSet<VarId>,
    resolved: HashMap<VarId, Type<'id>>,
    var_count: u32,
}

impl<'ctx, 'id> Simplifier<'ctx, 'id> {
    fn run(typ: &InferType<'id>, repo: &'ctx TypeRepo<'id>) -> Type<'id> {
        let mut this = Self::new(repo);
        this.check_variance(typ, Variance::Co);
        this.simplify(typ, Variance::Co)
    }

    fn new(type_repo: &'ctx TypeRepo<'id>) -> Self {
        Self {
            type_repo,
            contra: HashSet::default(),
            co: HashSet::default(),
            resolved: HashMap::default(),
            var_count: 0,
        }
    }

    fn check_variance_mono(&mut self, mono: &Mono<'id>, variance: Variance) {
        if let Mono::Data(data) = mono {
            let vars = self.type_repo[data.id].type_vars();
            assert_eq!(
                data.args.len(),
                vars.len(),
                "number of type vars should always match args"
            );

            data.args
                .iter()
                .zip(vars)
                .for_each(|(arg, var)| self.check_variance(arg, variance.combine(var.variance)));
        }
    }

    fn check_variance(&mut self, typ: &InferType<'id>, variance: Variance) {
        match typ {
            InferType::Mono(mono) => self.check_variance_mono(mono, variance),
            InferType::Poly(var) => {
                let borrow = var.borrow();
                if variance != Variance::Co {
                    self.contra.insert(borrow.id);
                    self.check_variance_mono(&borrow.upper, variance);
                }
                if variance != Variance::Contra {
                    self.co.insert(borrow.id);
                    self.check_variance_mono(&borrow.lower, variance);
                }
            }
        }
    }

    fn simplify_mono(&mut self, mono: &Mono<'id>, variance: Variance) -> Type<'id> {
        match mono {
            Mono::Data(data) => {
                let vars = self.type_repo[data.id].type_vars();
                assert_eq!(
                    data.args.len(),
                    vars.len(),
                    "number of type vars should always match args"
                );

                let args = data
                    .args
                    .iter()
                    .zip(vars)
                    .map(|(arg, var)| self.simplify(arg, variance.combine(var.variance)))
                    .collect();
                Type::Data(Parameterized::new(data.id, args))
            }
            Mono::Prim(prim) => Type::Prim(*prim),
            Mono::Var(var) => Type::Var(VarName::Named(var.name.clone())),
            Mono::Top => Type::Top,
            Mono::Bottom => Type::Bottom,
        }
    }

    fn simplify(&mut self, typ: &InferType<'id>, variance: Variance) -> Type<'id> {
        match typ {
            InferType::Mono(mono) => self.simplify_mono(mono, variance),
            InferType::Poly(var) => {
                let var = Var::rep(var.clone());
                let var = var.borrow();
                if let Some(res) = self.resolved.get(&var.id) {
                    res.clone()
                } else {
                    let res = if var.lower == var.upper || matches!(var.lower, Mono::Prim(_)) {
                        self.simplify_mono(&var.lower, variance)
                    } else if matches!(var.upper, Mono::Prim(_)) {
                        self.simplify_mono(&var.upper, variance)
                    } else if variance == Variance::In && var.lower != Mono::Bottom
                        || variance == Variance::Co && !self.contra.contains(&var.id)
                    {
                        self.simplify_mono(&var.lower, variance)
                    } else if variance == Variance::In && var.upper != Mono::Top
                        || variance == Variance::Contra && !self.co.contains(&var.id)
                    {
                        self.simplify_mono(&var.upper, variance)
                    } else {
                        self.var_count += 1;
                        Type::Var(VarName::CompGen(VarId::new(self.var_count)))
                    };
                    self.resolved.insert(var.id, res.clone());
                    res
                }
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct IdAlloc {
    tvars: u32,
    locals: u16,
}

impl IdAlloc {
    pub fn allocate_local<'id>(&mut self, typ: InferType<'id>) -> LocalInfo<'id> {
        self.locals += 1;
        LocalInfo::new(Local::Var(self.locals), typ)
    }

    pub fn allocate_param<'id>(&mut self, typ: InferType<'id>) -> LocalInfo<'id> {
        self.locals += 1;
        LocalInfo::new(Local::Param(self.locals), typ)
    }

    fn allocate_tvar<'id>(&mut self, lower: Mono<'id>, upper: Mono<'id>) -> RcVar<'id> {
        let var = Var {
            id: self.allocate_tvar_id(),
            lower,
            upper,
            rep: None,
        };
        RefCell::new(var).into()
    }

    #[inline]
    fn allocate_tvar_id(&mut self) -> VarId {
        self.tvars += 1;
        VarId::new(self.tvars)
    }

    #[inline]
    fn allocate_free_type<'id>(&mut self) -> InferType<'id> {
        InferType::Poly(self.allocate_tvar(Mono::Bottom, Mono::Top))
    }
}

#[derive(Debug)]
pub struct CheckedAst<'id>(PhantomData<&'id ()>);

impl<'id> ExprKind for CheckedAst<'id> {
    type CallMeta = Box<CallMetadata<'id>>;
    type Callable = Box<Callable<'id>>;
    type Class = Data<'id>;
    type Closure = Box<ClosureEnv<'id>>;
    type Inferred = InferType<'id>;
    type Local = Local;
    type Member = Member<'id>;
    type Span = Span;
    type Type = InferType<'id>;
}

#[derive(Debug)]
pub struct ClosureEnv<'id> {
    pub params: IndexMap<Local, InferType<'id>>,
    pub captures: IndexMap<Local, (InferType<'id>, Local)>,
    pub ret_type: InferType<'id>,
}

#[derive(Debug)]
pub struct CallMetadata<'id> {
    pub arg_types: Box<[InferType<'id>]>,
    pub ret_type: InferType<'id>,
    pub this_type: Option<InferType<'id>>,
}

impl<'id> CallMetadata<'id> {
    #[inline]
    pub fn new(arg_types: impl Into<Box<[InferType<'id>]>>, ret_type: InferType<'id>) -> Self {
        Self {
            arg_types: arg_types.into(),
            ret_type,
            this_type: None,
        }
    }

    pub fn empty() -> Self {
        Self {
            arg_types: [].into(),
            ret_type: InferType::VOID,
            this_type: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Local {
    Var(u16),
    Param(u16),
    Capture(u16),
    This,
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Local::Var(id) => write!(f, "var${id}"),
            Local::Param(id) => write!(f, "param${id}"),
            Local::Capture(id) => write!(f, "capture${id}"),
            Local::This => write!(f, "this"),
        }
    }
}

#[derive(Debug)]
pub enum Callable<'id> {
    Static(MethodId<'id>),
    Instance(MethodId<'id>),
    Lambda,
    Global(GlobalId),
    Intrinsic(Intrinsic),
    Cast,
    WrappedMethod(MethodId<'id>),
    WrappedStatic(MethodId<'id>),
}

#[derive(Debug)]
pub enum Member<'id> {
    Field(FieldId<'id>, InferType<'id>),
    EnumMember(FieldId<'id>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalInfo<'id> {
    pub local: Local,
    pub typ: InferType<'id>,
    pub captured: Option<Local>,
}

impl<'id> LocalInfo<'id> {
    pub fn new(local: Local, typ: InferType<'id>) -> Self {
        Self {
            local,
            typ,
            captured: None,
        }
    }

    fn captured(local: Local, typ: InferType<'id>, captured: Local) -> Self {
        Self {
            local,
            typ,
            captured: Some(captured),
        }
    }
}

#[derive(Debug)]
pub enum Convert {
    From(RefType),
    Into(RefType),
}

struct CaptureCollector<'ctx, 'id> {
    env: &'ctx HashMap<Str, LocalInfo<'id>>,
    captures: HashMap<Str, LocalInfo<'id>>,
    redefined: HashSet<Str>,
    count: u16,
}

impl<'ctx, 'id> CaptureCollector<'ctx, 'id> {
    fn run(expr: &Expr<SourceAst>, locals: &'ctx HashMap<Str, LocalInfo<'id>>) -> HashMap<Str, LocalInfo<'id>> {
        let mut this = Self {
            env: locals,
            captures: HashMap::default(),
            redefined: HashSet::default(),
            count: 0,
        };
        this.collect(expr);
        this.captures
    }

    fn collect(&mut self, expr: &Expr<SourceAst>) {
        match expr {
            Expr::Ident(ident, _) if !self.redefined.contains(ident) => {
                if let (Some(info), hash_map::EntryRef::Vacant(slot)) =
                    (self.env.get(ident), self.captures.entry_ref(ident))
                {
                    let capture = LocalInfo::captured(Local::Capture(self.count), info.typ.clone(), info.local);
                    slot.insert(capture);
                    self.count += 1;
                }
            }
            &Expr::This(span) => {
                self.collect(&Expr::Ident(Ident::from_static("this"), span));
            }
            Expr::Declare(name, _, expr, _) => {
                if let Some(expr) = expr {
                    self.collect(expr);
                }
                if self.env.contains_key(name) {
                    self.redefined.insert(name.clone());
                }
            }
            Expr::Lambda(params, body, _) => {
                let mut nested = Self {
                    env: self.env,
                    captures: std::mem::take(&mut self.captures),
                    redefined: self
                        .redefined
                        .iter()
                        .cloned()
                        .chain(params.iter().map(|param| param.name.clone()))
                        .collect(),
                    count: self.count,
                };
                for expr in &body.exprs {
                    nested.collect(expr);
                }
                self.captures = nested.captures;
                self.count = nested.count;
            }
            _ => visit_expr!(self, collect, &expr),
        }
    }
}

#[derive(Debug, Default)]
pub struct ErrorReporter<'id> {
    errors: Vec<CompileError<'id>>,
}

impl<'id> ErrorReporter<'id> {
    pub fn unwrap_err<A>(&mut self, res: CompileResult<'id, A>) -> Option<A> {
        match res {
            Ok(res) => Some(res),
            Err(err) => {
                self.report(err);
                None
            }
        }
    }

    #[inline]
    pub fn report(&mut self, error: CompileError<'id>) {
        self.errors.push(error);
    }

    pub fn is_compilation_failed(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn into_errors(self) -> Vec<CompileError<'id>> {
        self.errors
    }
}

pub trait IntoTypeError {
    type Result;
    fn with_span(self, span: Span) -> Self::Result;
}

impl<'id, A> IntoTypeError for Result<A, TypeError<'id>> {
    type Result = CompileResult<'id, A>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Result {
        self.map_err(|e| CompileError::TypeError(e, span))
    }
}

pub trait TypeValidator<'id> {
    fn validate(&self, typ: &Parameterized<'id>) -> Result<(), TypeError<'id>>;
}

impl<'id> TypeValidator<'id> for TypeRepo<'id> {
    fn validate(&self, typ: &Parameterized<'id>) -> Result<(), TypeError<'id>> {
        typ.check_well_formed(self)
    }
}

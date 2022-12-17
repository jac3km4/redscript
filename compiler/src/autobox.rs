use std::mem;

use hashbrown::HashMap;
use itertools::{izip, Itertools};
use redscript::ast::{Expr, Seq};
use redscript::bytecode::Intrinsic;

use crate::type_repo::{predef, DataType, FieldId, MethodId, Prim, Type, TypeId, TypeRepo};
use crate::typer::{CallMetadata, Callable, CheckedAst, Data, InferType, Local, Member, Mono};
use crate::visit_expr;

#[derive(Debug)]
pub struct Autobox<'ctx, 'id> {
    type_repo: &'ctx TypeRepo<'id>,
    boxed_locals: HashMap<Local, Boxable<'id>>,
    poly_ret: Option<Boxable<'id>>,
}

impl<'ctx, 'id> Autobox<'ctx, 'id> {
    #[inline]
    fn new(
        type_repo: &'ctx TypeRepo<'id>,
        boxed_locals: HashMap<Local, Boxable<'id>>,
        poly_ret: Option<Boxable<'id>>,
    ) -> Self {
        Self {
            type_repo,
            boxed_locals,
            poly_ret,
        }
    }

    pub fn run(
        seq: &mut Seq<CheckedAst<'id>>,
        type_repo: &'ctx TypeRepo<'id>,
        boxed_locals: HashMap<Local, Boxable<'id>>,
        poly_ret: Option<Boxable<'id>>,
    ) {
        let mut this = Self::new(type_repo, boxed_locals, poly_ret);
        for expr in &mut seq.exprs {
            this.apply(expr);
        }
    }

    fn apply(&mut self, expr: &mut Expr<CheckedAst<'id>>) {
        match expr {
            Expr::Ident(loc, _) => {
                if let Some(prim) = self.boxed_locals.get(loc) {
                    *expr = self.unbox_primitive(prim, mem::take(expr));
                }
            }
            Expr::Assign(lhs, rhs, typ, _) => {
                self.apply(rhs);
                match (&**lhs, Boxable::from_infer_type(typ, self.type_repo)) {
                    (Expr::Member(_, Member::Field(field, _), _), Some(prim))
                        if requires_boxing(&self.type_repo.get_field(field).unwrap().typ, self.type_repo) =>
                    {
                        **rhs = self.box_primitive(&prim, mem::take(rhs));
                    }
                    _ => {}
                }
            }
            Expr::Member(obj, Member::Field(field, typ), _) => {
                self.apply(obj);
                if requires_boxing(&self.type_repo.get_field(field).unwrap().typ, self.type_repo) {
                    let typ = typ.clone();
                    self.try_unbox_expr(expr, &typ);
                }
            }
            Expr::Call(called, cb, _, args, meta, _) => {
                self.apply(called);
                for arg in args.iter_mut() {
                    self.apply(arg);
                }
                let fn_type = match &**cb {
                    Callable::Static(mid) | Callable::WrappedStatic(mid) => {
                        &self.type_repo.get_static(mid).unwrap().typ
                    }
                    Callable::Instance(mid) | Callable::WrappedMethod(mid) => {
                        &self.type_repo.get_method(mid).unwrap().typ
                    }
                    Callable::Global(gid) => &self.type_repo.get_global(gid).unwrap().typ,
                    Callable::Lambda => {
                        for (arg, typ) in args.iter_mut().zip(meta.arg_types.iter()) {
                            self.try_box_expr(arg, typ);
                        }
                        let ret = meta.ret_type.clone();
                        self.try_unbox_expr(expr, &ret);
                        return;
                    }
                    Callable::Intrinsic(_) | Callable::Cast => return,
                };
                for (arg, param, typ) in izip!(args.iter_mut(), fn_type.params.iter(), meta.arg_types.iter()) {
                    if param.is_poly || requires_boxing(&param.typ, self.type_repo) {
                        self.try_box_expr(arg, typ);
                    }
                }
                if fn_type.is_ret_poly || requires_boxing(&fn_type.ret, self.type_repo) {
                    let ret = meta.ret_type.clone();
                    self.try_unbox_expr(expr, &ret);
                }
            }
            Expr::Lambda(env, body, _) => {
                for (&loc, typ) in &env.params {
                    if let Some(prim) = Boxable::from_infer_type(typ, self.type_repo) {
                        self.boxed_locals.insert(loc, prim);
                    }
                }
                for (&loc, (_, capture)) in &env.captures {
                    if let Some(prim) = self.boxed_locals.get(capture) {
                        self.boxed_locals.insert(loc, prim.clone());
                    }
                }
                self.apply(body);
                self.try_box_expr(body, &env.ret_type);
            }
            Expr::Return(Some(expr), _) => {
                self.apply(expr);
                if let Some(boxed) = &self.poly_ret {
                    **expr = self.box_primitive(boxed, mem::take(expr));
                }
            }
            _ => visit_expr!(self, apply, expr, mut),
        }
    }

    fn try_box_expr(&self, expr: &mut Expr<CheckedAst<'id>>, from: &InferType<'id>) {
        if let Some(ty) = Boxable::from_infer_type(from, self.type_repo) {
            *expr = self.box_primitive(&ty, mem::take(expr));
        }
    }

    fn try_unbox_expr(&self, expr: &mut Expr<CheckedAst<'id>>, to: &InferType<'id>) {
        if let Some(ty) = Boxable::from_infer_type(to, self.type_repo) {
            *expr = self.unbox_primitive(&ty, mem::take(expr));
        }
    }

    fn box_primitive(&self, boxable: &Boxable<'id>, expr: Expr<CheckedAst<'id>>) -> Expr<CheckedAst<'id>> {
        if let Boxable::Prim(Prim::Unit) = boxable {
            return expr;
        }
        let boxed = boxable.boxed_type().unwrap();
        let span = expr.span();
        let (method, arg) = match boxable {
            Boxable::Prim(_) => ("New", expr),
            &Boxable::Struct(typ) => {
                let expr = Expr::Call(
                    Expr::EMPTY.into(),
                    Callable::Intrinsic(Intrinsic::ToVariant).into(),
                    [InferType::data(Data::without_args(typ))].into(),
                    [expr].into(),
                    CallMetadata::empty().into(),
                    span,
                );
                ("FromVariant", expr)
            }
            &Boxable::Enum(typ) => {
                let expr = Expr::Call(
                    Expr::EMPTY.into(),
                    Callable::Intrinsic(Intrinsic::EnumInt).into(),
                    [InferType::data(Data::without_args(typ))].into(),
                    [expr].into(),
                    CallMetadata::empty().into(),
                    span,
                );
                ("FromIntRepr", expr)
            }
        };
        let method = self
            .type_repo
            .get_type(boxed)
            .unwrap()
            .as_class()
            .unwrap()
            .statics
            .by_name(method)
            .exactly_one()
            .ok()
            .unwrap();
        Expr::Call(
            Expr::EMPTY.into(),
            Callable::Static(MethodId::new(boxed, method.index)).into(),
            [].into(),
            [arg].into(),
            CallMetadata::empty().into(),
            span,
        )
    }

    fn unbox_primitive(&self, boxable: &Boxable<'id>, expr: Expr<CheckedAst<'id>>) -> Expr<CheckedAst<'id>> {
        if let Boxable::Prim(Prim::Unit) = boxable {
            return expr;
        }
        let boxed = boxable.boxed_type().unwrap();
        let (index, _) = self
            .type_repo
            .get_type(boxed)
            .unwrap()
            .as_class()
            .unwrap()
            .fields
            .by_name("value")
            .unwrap();
        let span = expr.span();
        let get_field = Expr::Member(
            expr.into(),
            Member::Field(FieldId::new(boxed, index), InferType::TOP),
            span,
        );
        match boxable {
            Boxable::Prim(_) => get_field,
            &Boxable::Struct(typ) => Expr::Call(
                Expr::EMPTY.into(),
                Callable::Intrinsic(Intrinsic::FromVariant).into(),
                [InferType::data(Data::without_args(typ))].into(),
                [get_field].into(),
                CallMetadata::empty().into(),
                span,
            ),
            &Boxable::Enum(typ) => Expr::Call(
                Expr::EMPTY.into(),
                Callable::Intrinsic(Intrinsic::IntEnum).into(),
                [InferType::data(Data::without_args(typ))].into(),
                [get_field].into(),
                CallMetadata::empty().into(),
                span,
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Boxable<'id> {
    Prim(Prim),
    Struct(TypeId<'id>),
    Enum(TypeId<'id>),
}

impl<'id> Boxable<'id> {
    fn boxed_type(&self) -> Option<TypeId<'id>> {
        match self {
            Boxable::Prim(prim) => prim.boxed_type(),
            Boxable::Struct(_) => Some(predef::BOXED_STRUCT),
            Boxable::Enum(_) => Some(predef::BOXED_ENUM),
        }
    }

    fn from_type_id(id: TypeId<'id>, repo: &TypeRepo<'id>) -> Option<Boxable<'id>> {
        match repo.get_type(id)? {
            DataType::Class(class) if class.flags.is_struct() => Some(Boxable::Struct(id)),
            DataType::Enum(_) => Some(Boxable::Enum(id)),
            _ => None,
        }
    }

    fn from_mono(typ: &Mono<'id>, repo: &TypeRepo<'id>) -> Option<Boxable<'id>> {
        match typ {
            &Mono::Prim(prim) => Some(Boxable::Prim(prim)),
            Mono::Data(data) => Self::from_type_id(data.id, repo),
            _ => None,
        }
    }

    pub fn from_infer_type(typ: &InferType<'id>, repo: &TypeRepo<'id>) -> Option<Boxable<'id>> {
        match typ {
            InferType::Mono(mono) => Self::from_mono(mono, repo),
            InferType::Poly(poly) => Self::from_mono(poly.borrow().either_bound()?, repo),
        }
    }
}

fn requires_boxing<'id>(typ: &Type<'id>, repo: &TypeRepo<'id>) -> bool {
    match typ {
        Type::Top | Type::Var(_) => true,
        Type::Data(data) => match repo.get_type(data.id).unwrap() {
            DataType::Class(c) => !c.flags.is_struct(),
            &DataType::Builtin { is_unboxed, .. } => !is_unboxed,
            &DataType::Enum(_) => false,
        },
        _ => false,
    }
}

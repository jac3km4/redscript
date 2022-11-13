use std::mem;

use hashbrown::HashMap;
use itertools::{izip, Itertools};
use redscript::ast::{Expr, Seq};

use crate::type_repo::{predef, FieldId, MethodId, Prim, Type, TypeRepo};
use crate::typer::{CallMetadata, Callable, CheckedAst, InferType, Local, Member};
use crate::visit_expr;

#[derive(Debug)]
pub struct Autobox<'ctx, 'id> {
    type_repo: &'ctx TypeRepo<'id>,
    boxed_locals: HashMap<Local, Prim>,
}

impl<'ctx, 'id> Autobox<'ctx, 'id> {
    #[inline]
    fn new(type_repo: &'ctx TypeRepo<'id>) -> Self {
        Self {
            type_repo,
            boxed_locals: HashMap::default(),
        }
    }

    pub fn run(seq: &mut Seq<CheckedAst<'id>>, type_repo: &'ctx TypeRepo<'id>) {
        let mut this = Self::new(type_repo);
        for expr in &mut seq.exprs {
            this.apply(expr);
        }
    }

    fn apply(&mut self, expr: &mut Expr<CheckedAst<'id>>) {
        match expr {
            Expr::Ident(loc, _) => {
                if let Some(&prim) = self.boxed_locals.get(loc) {
                    *expr = self.unbox_primitive(prim, mem::take(expr));
                }
            }
            Expr::Assign(lhs, rhs, typ, _) => {
                self.apply(rhs);
                match (&**lhs, typ.into_prim()) {
                    (Expr::Member(_, Member::ClassField(field), _), Some(prim))
                        if requires_boxing(self.type_repo.get_field(field).unwrap()) =>
                    {
                        **rhs = self.box_primitive(prim, mem::take(rhs));
                    }
                    _ => {}
                }
            }
            Expr::Call(called, cb, _, args, meta, _) => {
                self.apply(called);
                for arg in args.iter_mut() {
                    self.apply(arg);
                }
                let fn_type = match &**cb {
                    Callable::Static(mid) => &self.type_repo.get_static(mid).unwrap().typ,
                    Callable::Instance(mid) => &self.type_repo.get_method(mid).unwrap().typ,
                    Callable::Global(gid) => &self.type_repo.get_global(gid).unwrap().typ,
                    Callable::Lambda => {
                        for (arg, typ) in args.iter_mut().zip(meta.arg_types.iter()) {
                            self.try_box_expr(arg, typ, &Type::Top);
                        }
                        let ret = meta.ret_type.into_prim();
                        self.try_unbox_expr(expr, &Type::Top, ret);
                        return;
                    }
                    Callable::Intrinsic(_) | Callable::Cast => return,
                };
                for (arg, param, typ) in izip!(args.iter_mut(), fn_type.params.iter(), meta.arg_types.iter()) {
                    self.try_box_expr(arg, typ, &param.typ);
                }
                let ret = meta.ret_type.into_prim();
                self.try_unbox_expr(expr, &fn_type.ret, ret);
            }
            Expr::Lambda(env, body, _) => {
                for (&loc, typ) in &env.params {
                    if let Some(prim) = typ.into_prim() {
                        self.boxed_locals.insert(loc, prim);
                    }
                }
                for (&loc, (_, capture)) in &env.captures {
                    if let Some(&prim) = self.boxed_locals.get(capture) {
                        self.boxed_locals.insert(loc, prim);
                    }
                }
                self.apply(body);
                self.try_box_expr(body, &env.ret_type, &Type::Top);
            }
            _ => visit_expr!(self, apply, expr, mut),
        }
    }

    fn try_box_expr(&self, expr: &mut Expr<CheckedAst<'id>>, from: &InferType<'id>, to: &Type<'id>) {
        match from.into_prim() {
            Some(prim) if requires_boxing(to) => {
                *expr = self.box_primitive(prim, mem::take(expr));
            }
            _ => {}
        }
    }

    fn try_unbox_expr(&self, expr: &mut Expr<CheckedAst<'id>>, from: &Type<'id>, to: Option<Prim>) {
        match to {
            Some(prim) if requires_boxing(from) => {
                *expr = self.unbox_primitive(prim, mem::take(expr));
            }
            _ => {}
        }
    }

    fn box_primitive(&self, prim: Prim, expr: Expr<CheckedAst<'id>>) -> Expr<CheckedAst<'id>> {
        if prim == Prim::Unit {
            return expr;
        }
        let boxed = prim.boxed_type().unwrap();
        let method = self
            .type_repo
            .get_type(boxed)
            .unwrap()
            .as_class()
            .unwrap()
            .statics
            .by_name("New")
            .exactly_one()
            .unwrap_or_else(|_| todo!());
        let span = expr.span();
        Expr::Call(
            Expr::EMPTY.into(),
            Callable::Static(MethodId::new(boxed, method.index)).into(),
            [].into(),
            [expr].into(),
            CallMetadata::empty().into(),
            span,
        )
    }

    fn unbox_primitive(&self, prim: Prim, expr: Expr<CheckedAst<'id>>) -> Expr<CheckedAst<'id>> {
        if prim == Prim::Unit {
            return expr;
        }
        let boxed = prim.boxed_type().unwrap();
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
        Expr::Member(expr.into(), Member::ClassField(FieldId::new(boxed, index)), span)
    }
}

fn requires_boxing(typ: &Type<'_>) -> bool {
    match typ {
        Type::Top | Type::Var(_) => true,
        Type::Data(data) => data.id != predef::SCRIPT_REF && data.id != predef::ARRAY,
        _ => false,
    }
}

use std::rc::Rc;

use indexmap::IndexMap;
use redscript_ast::{self as ast, Span};

use crate::stages::infer::FuncItem;
use crate::symbols::{FieldEntry, Visibility};
use crate::{
    CtxVar, FunctionIndex, FunctionType, Method, MethodFlags, Param, ParamFlags, Type, TypeId,
};

pub(super) fn derive_new_method<'e, 'ctx: 'e>(
    cls_id: TypeId<'ctx>,
    vars: &[Rc<CtxVar<'ctx>>],
    fields: impl IntoIterator<Item = FieldEntry<'e, 'ctx>>,
    span: Span,
) -> Method<'ctx> {
    let flags = MethodFlags::new()
        .with_visibility(Visibility::Public)
        .with_is_final(true)
        .with_is_static(true);
    let args = vars
        .iter()
        .map(|var| Type::Ctx(var.clone()))
        .collect::<Rc<[_]>>();
    let return_t = Type::app(cls_id, args);
    let params = fields
        .into_iter()
        .map(|field| {
            Param::new(
                field.name(),
                ParamFlags::new(),
                field.field().type_().clone(),
                field.field().span(),
            )
        })
        .collect::<Box<[_]>>();
    let func_t = FunctionType::new([], params, return_t);
    Method::new(flags, func_t, None, [], Some(span))
}

pub(super) fn derive_new_method_item<'e, 'ctx: 'e>(
    func_idx: FunctionIndex,
    type_name: &'ctx str,
    fields: impl IntoIterator<Item = FieldEntry<'e, 'ctx>>,
    span: Span,
) -> FuncItem<'static, 'ctx, FunctionIndex> {
    const SELF_NAME: &str = "self";

    let new = ast::Expr::New {
        typ: (
            ast::Type::Named {
                name: type_name,
                args: [].into(),
            },
            span,
        )
            .into(),
        args: [].into(),
    };

    let mut stmts = vec![(
        ast::Stmt::Let {
            name: (SELF_NAME, span),
            typ: None,
            value: Some((new, span).into()),
        },
        span,
    )];

    let mut param_names = vec![];
    for field in fields {
        param_names.push(field.name());
        let stmt = ast::Expr::Assign {
            lhs: (
                ast::Expr::Member {
                    expr: (ast::Expr::Ident(SELF_NAME), span).into(),
                    member: field.name(),
                },
                span,
            )
                .into(),
            rhs: (ast::Expr::Ident(field.name()), span).into(),
        };
        stmts.push((ast::Stmt::Expr((stmt, span).into()), span));
    }
    stmts.push((
        ast::Stmt::Return(Some((ast::Expr::Ident(SELF_NAME), span).into())),
        span,
    ));

    FuncItem::new(
        func_idx,
        span,
        span,
        param_names,
        ast::FunctionBody::Block(ast::Block::new(stmts)),
        IndexMap::default(),
    )
}

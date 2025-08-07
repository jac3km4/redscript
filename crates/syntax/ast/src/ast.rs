use std::borrow::Cow;
use std::fmt;

use bitflags::bitflags;
use derive_where::derive_where;

use crate::{Span, Spanned};

pub(super) type AnnotationT<'src, A> = <A as AstKind>::Inner<Annotation<'src, A>>;
pub(super) type ExprT<'src, A> = <A as AstKind>::Inner<Expr<'src, A>>;
pub(super) type ItemDeclT<'src, A> = <A as AstKind>::Inner<ItemDecl<'src, A>>;
pub(super) type ParamT<'src, A> = <A as AstKind>::Inner<Param<'src, A>>;
pub(super) type StmtT<'src, A> = <A as AstKind>::Inner<Stmt<'src, A>>;
pub(super) type TypeT<'src, A> = <A as AstKind>::Inner<Type<'src, A>>;
pub(super) type PatternT<'src, A> = <A as AstKind>::Inner<Pattern<'src, A>>;

#[derive_where(Debug, Clone, PartialEq)]
pub struct Module<'src, K: AstKind = Identity> {
    pub path: Option<Path<'src>>,
    pub items: Box<[ItemDeclT<'src, K>]>,
}

impl<'src, K: AstKind> Module<'src, K> {
    #[inline]
    pub fn new(path: Option<Path<'src>>, items: impl Into<Box<[ItemDeclT<'src, K>]>>) -> Self {
        Self {
            path,
            items: items.into(),
        }
    }

    pub fn unwrapped(self) -> Module<'src> {
        Module {
            path: self.path,
            items: self
                .items
                .into_vec()
                .into_iter()
                .map(|i| i.into_wrapped().unwrapped())
                .collect(),
        }
    }
}

impl<'src> Module<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        let idx = self
            .items
            .binary_search_by(|(_, sp)| sp.cmp_pos(pos))
            .ok()?;
        let (item, _) = &self.items[idx];
        Some(item.find_at(pos))
    }

    pub fn span(&self) -> Option<Span> {
        let (_, fst) = self.items.first()?;
        let (_, lst) = self.items.last()?;
        Some(fst.merge(lst))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Import<'src> {
    Exact(Path<'src>),
    Select(Path<'src>, Box<[&'src str]>),
    All(Path<'src>),
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct ItemDecl<'src, K: AstKind = Identity> {
    pub annotations: Vec<AnnotationT<'src, K>>,
    pub visibility: Option<Visibility>,
    pub qualifiers: ItemQualifiers,
    pub doc: Box<[&'src str]>,
    pub item: Item<'src, K>,
}

impl<'src, K: AstKind> ItemDecl<'src, K> {
    #[inline]
    pub fn new(
        annotations: impl Into<Vec<AnnotationT<'src, K>>>,
        visibility: Option<Visibility>,
        qualifiers: ItemQualifiers,
        doc: impl Into<Box<[&'src str]>>,
        item: Item<'src, K>,
    ) -> Self {
        Self {
            annotations: annotations.into(),
            visibility,
            qualifiers,
            doc: doc.into(),
            item,
        }
    }

    pub fn unwrapped(self) -> ItemDecl<'src> {
        ItemDecl {
            annotations: self
                .annotations
                .into_iter()
                .map(|a| a.into_wrapped().unwrapped())
                .collect(),
            visibility: self.visibility,
            qualifiers: self.qualifiers,
            doc: self.doc,
            item: self.item.into_wrapped().unwrapped(),
        }
    }
}

impl<'src> ItemDecl<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> QueryResult<'_, 'src> {
        match &self.item {
            Item::Import(_) | Item::Enum(_) => return QueryResult::ItemDecl(self),
            Item::Class(c) | Item::Struct(c) => c.find_at(pos),
            Item::Function(f) => f
                .params
                .iter()
                .filter_map(|(p, _)| p.typ.as_ref())
                .find_map(|(typ, span)| span.contains(pos).then_some(typ.find_at(pos)))
                .or_else(|| {
                    f.return_type
                        .as_deref()
                        .filter(|(_, span)| span.contains(pos))
                        .map(|(typ, _)| typ.find_at(pos))
                })
                .or_else(|| f.body.as_ref().and_then(|b| b.find_at(pos))),
            Item::Let(l) => {
                let (typ, type_span) = l.typ.as_ref();
                if type_span.contains(pos) {
                    return typ.find_at(pos);
                }
                l.default.as_ref().and_then(|d| {
                    let (def, span) = &**d;
                    span.contains(pos).then_some(def.find_at(pos))
                })
            }
        }
        .unwrap_or(QueryResult::ItemDecl(self))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Item<'src, K: AstKind = Identity> {
    Import(Import<'src>),
    Class(Aggregate<'src, K>),
    Struct(Aggregate<'src, K>),
    Function(Function<'src, K>),
    Let(Field<'src, K>),
    Enum(Enum<'src, K>),
}

impl<'src, K: AstKind> Item<'src, K> {
    pub fn unwrapped(self) -> Item<'src> {
        match self {
            Item::Import(i) => Item::Import(i),
            Item::Class(c) => Item::Class(c.unwrapped()),
            Item::Struct(s) => Item::Struct(s.unwrapped()),
            Item::Function(f) => Item::Function(f.unwrapped()),
            Item::Let(l) => Item::Let(l.unwrapped()),
            Item::Enum(e) => Item::Enum(e.unwrapped()),
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Aggregate<'src, K: AstKind = Identity> {
    pub name: K::Inner<&'src str>,
    pub type_params: Box<[TypeParam<'src, K>]>,
    pub extends: Option<Box<TypeT<'src, K>>>,
    pub items: Vec<ItemDeclT<'src, K>>,
}

impl<'src, K: AstKind> Aggregate<'src, K> {
    #[inline]
    pub fn new(
        name: K::Inner<&'src str>,
        type_params: impl Into<Box<[TypeParam<'src, K>]>>,
        extends: Option<Box<TypeT<'src, K>>>,
        items: impl Into<Vec<ItemDeclT<'src, K>>>,
    ) -> Self {
        Self {
            name,
            type_params: type_params.into(),
            extends,
            items: items.into(),
        }
    }

    pub fn unwrapped(self) -> Aggregate<'src> {
        Aggregate {
            name: self.name.into_wrapped(),
            type_params: self
                .type_params
                .into_vec()
                .into_iter()
                .map(|p| p.into_wrapped().unwrapped())
                .collect(),
            extends: self
                .extends
                .map(|typ| (*typ).into_wrapped().unwrapped().into()),
            items: self
                .items
                .into_iter()
                .map(|m| m.into_wrapped().unwrapped())
                .collect(),
        }
    }
}

impl<'src> Aggregate<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        if let Some((extends, span)) = self.extends.as_deref()
            && span.contains(pos)
        {
            return Some(extends.find_at(pos));
        }
        let idx = self
            .items
            .binary_search_by(|(_, sp)| sp.cmp_pos(pos))
            .ok()?;
        let (item, _) = &self.items[idx];
        Some(item.find_at(pos))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Field<'src, K: AstKind = Identity> {
    pub name: K::Inner<&'src str>,
    pub typ: Box<TypeT<'src, K>>,
    pub default: Option<Box<ExprT<'src, K>>>,
}

impl<'src, K: AstKind> Field<'src, K> {
    #[inline]
    pub fn new(
        name: K::Inner<&'src str>,
        typ: Box<TypeT<'src, K>>,
        default: Option<Box<ExprT<'src, K>>>,
    ) -> Self {
        Self { name, typ, default }
    }

    pub fn unwrapped(self) -> Field<'src> {
        Field {
            name: self.name.into_wrapped(),
            typ: (*self.typ).into_wrapped().unwrapped().into(),
            default: self.default.map(|d| (*d).into_wrapped().unwrapped().into()),
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Function<'src, K: AstKind = Identity> {
    pub name: K::Inner<&'src str>,
    pub type_params: Box<[TypeParam<'src, K>]>,
    pub params: Box<[ParamT<'src, K>]>,
    pub return_type: Option<Box<TypeT<'src, K>>>,
    pub body: Option<FunctionBody<'src, K>>,
}

impl<'src, K: AstKind> Function<'src, K> {
    #[inline]
    pub fn new(
        name: K::Inner<&'src str>,
        type_params: impl Into<Box<[TypeParam<'src, K>]>>,
        params: impl Into<Box<[ParamT<'src, K>]>>,
        return_type: Option<Box<TypeT<'src, K>>>,
        body: Option<FunctionBody<'src, K>>,
    ) -> Self {
        Self {
            name,
            type_params: type_params.into(),
            params: params.into(),
            return_type,
            body,
        }
    }

    pub fn unwrapped(self) -> Function<'src> {
        Function {
            name: self.name.into_wrapped(),
            params: self
                .params
                .into_vec()
                .into_iter()
                .map(|p| p.into_wrapped().unwrapped())
                .collect(),
            type_params: self
                .type_params
                .into_vec()
                .into_iter()
                .map(|p| p.into_wrapped().unwrapped())
                .collect(),
            return_type: self
                .return_type
                .map(|typ| (*typ).into_wrapped().unwrapped().into()),
            body: self.body.map(|b| b.into_wrapped().unwrapped()),
        }
    }

    pub fn param_names(&self) -> impl ExactSizeIterator<Item = &'src str> {
        self.params.iter().map(|p| p.as_wrapped().name)
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum FunctionBody<'src, K: AstKind = Identity> {
    Block(Block<'src, K>),
    Inline(Box<ExprT<'src, K>>),
}

impl<'src, K: AstKind> FunctionBody<'src, K> {
    pub fn unwrapped(self) -> FunctionBody<'src> {
        match self {
            FunctionBody::Block(b) => FunctionBody::Block(b.into_wrapped().unwrapped()),
            FunctionBody::Inline(e) => FunctionBody::Inline((*e).into_wrapped().unwrapped().into()),
        }
    }
}

impl<'src> FunctionBody<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        match self {
            FunctionBody::Block(b) => b.find_at(pos),
            FunctionBody::Inline(e) => {
                let (e, span) = &**e;
                span.contains(pos).then(|| e.find_at(pos))
            }
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Enum<'src, K: AstKind = Identity> {
    pub name: K::Inner<&'src str>,
    pub variants: Box<[K::Inner<EnumVariant<'src>>]>,
}

impl<'src, K: AstKind> Enum<'src, K> {
    #[inline]
    pub fn new(
        name: K::Inner<&'src str>,
        variants: impl Into<Box<[K::Inner<EnumVariant<'src>>]>>,
    ) -> Self {
        Self {
            name,
            variants: variants.into(),
        }
    }

    pub fn unwrapped(self) -> Enum<'src> {
        Enum {
            name: self.name.into_wrapped(),
            variants: self
                .variants
                .into_vec()
                .into_iter()
                .map(Wrapper::into_wrapped)
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant<'src> {
    pub name: &'src str,
    pub value: Option<i64>,
}

impl<'src> EnumVariant<'src> {
    #[inline]
    pub fn new(name: &'src str, value: Option<impl Into<i64>>) -> Self {
        Self {
            name,
            value: value.map(Into::into),
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Annotation<'src, K: AstKind = Identity> {
    pub name: &'src str,
    pub args: Box<[ExprT<'src, K>]>,
}

impl<'src, K: AstKind> Annotation<'src, K> {
    #[inline]
    pub fn new(name: &'src str, args: impl Into<Box<[ExprT<'src, K>]>>) -> Self {
        Self {
            name,
            args: args.into(),
        }
    }

    pub fn unwrapped(self) -> Annotation<'src> {
        Annotation {
            name: self.name,
            args: self
                .args
                .into_vec()
                .into_iter()
                .map(|a| a.into_wrapped().unwrapped())
                .collect(),
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Type<'src, K: AstKind = Identity> {
    Named {
        name: &'src str,
        args: Box<[K::Inner<Self>]>,
    },
    Array(Box<K::Inner<Self>>),
    StaticArray(Box<K::Inner<Self>>, usize),
    Fn {
        params: Box<[K::Inner<Self>]>,
        return_type: Box<K::Inner<Self>>,
    },
}

impl<'src, K: AstKind> Type<'src, K> {
    #[inline]
    pub fn plain(name: &'src str) -> Self {
        Self::Named {
            name,
            args: Box::new([]),
        }
    }

    pub fn unwrapped(self) -> Type<'src> {
        match self {
            Type::Named { name, args } => Type::Named {
                name,
                args: args
                    .into_vec()
                    .into_iter()
                    .map(|a| a.into_wrapped().unwrapped())
                    .collect(),
            },
            Type::Array(t) => Type::Array((*t).into_wrapped().unwrapped().into()),
            Type::StaticArray(t, size) => {
                Type::StaticArray((*t).into_wrapped().unwrapped().into(), size)
            }
            Type::Fn {
                params,
                return_type: return_ty,
            } => Type::Fn {
                params: params
                    .into_vec()
                    .into_iter()
                    .map(|p| p.into_wrapped().unwrapped())
                    .collect(),
                return_type: (*return_ty).into_wrapped().unwrapped().into(),
            },
        }
    }
}

impl<'src> Type<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> QueryResult<'_, 'src> {
        match self {
            Type::Named { args, .. } => args
                .iter()
                .find_map(|(typ, span)| span.contains(pos).then_some(typ.find_at(pos)))
                .unwrap_or(QueryResult::Type(self)),
            Type::Array(typ) | Type::StaticArray(typ, _) => {
                let (typ, span) = &**typ;
                if span.contains(pos) {
                    return typ.find_at(pos);
                }
                QueryResult::Type(self)
            }
            Type::Fn {
                params,
                return_type,
            } => {
                if let Some(res) = params
                    .iter()
                    .find_map(|(typ, span)| span.contains(pos).then_some(typ.find_at(pos)))
                {
                    return res;
                }
                let (return_ty, span) = &**return_type;
                if span.contains(pos) {
                    return_ty.find_at(pos)
                } else {
                    QueryResult::Type(self)
                }
            }
        }
    }
}
#[derive_where(Debug, Clone, PartialEq)]
pub struct TypeParam<'src, K: AstKind = Identity> {
    pub variance: Variance,
    pub name: K::Inner<&'src str>,
    pub upper_bound: Option<Box<TypeT<'src, K>>>,
}

impl<'src, K: AstKind> TypeParam<'src, K> {
    #[inline]
    pub fn new(
        variance: Variance,
        name: K::Inner<&'src str>,
        upper_bound: Option<Box<TypeT<'src, K>>>,
    ) -> Self {
        Self {
            variance,
            name,
            upper_bound,
        }
    }

    pub fn unwrapped(self) -> TypeParam<'src> {
        TypeParam {
            variance: self.variance,
            name: self.name.into_wrapped(),
            upper_bound: self
                .upper_bound
                .map(|typ| (*typ).into_wrapped().unwrapped().into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Param<'src, K: AstKind = Identity> {
    pub name: &'src str,
    pub typ: Option<TypeT<'src, K>>,
    pub qualifiers: ParamQualifiers,
}

impl<'src, K: AstKind> Param<'src, K> {
    #[inline]
    pub fn new(name: &'src str, typ: Option<TypeT<'src, K>>, qualifiers: ParamQualifiers) -> Self {
        Self {
            name,
            typ,
            qualifiers,
        }
    }

    pub fn unwrapped(self) -> Param<'src> {
        Param {
            name: self.name,
            typ: self.typ.map(|t| t.into_wrapped().unwrapped()),
            qualifiers: self.qualifiers,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<'src> {
    pub segments: Box<[&'src str]>,
}

impl<'src> Path<'src> {
    #[inline]
    pub fn new(segments: impl Into<Box<[&'src str]>>) -> Self {
        Self {
            segments: segments.into(),
        }
    }
}

impl<'src> AsRef<[&'src str]> for Path<'src> {
    #[inline]
    fn as_ref(&self) -> &[&'src str] {
        &self.segments
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Block<'src, K: AstKind = Identity> {
    pub stmts: Box<[StmtT<'src, K>]>,
}

impl<'src, K: AstKind> Block<'src, K> {
    #[inline]
    pub fn new(stmts: impl Into<Box<[StmtT<'src, K>]>>) -> Self {
        Self {
            stmts: stmts.into(),
        }
    }

    #[inline]
    pub fn single(stmt: StmtT<'src, K>) -> Self {
        Self {
            stmts: [stmt].into(),
        }
    }

    pub fn unwrapped(self) -> Block<'src> {
        Block {
            stmts: self
                .stmts
                .into_vec()
                .into_iter()
                .map(|s| s.into_wrapped().unwrapped())
                .collect(),
        }
    }
}

impl<'src> Block<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        let idx = self
            .stmts
            .binary_search_by(|(_, sp)| sp.cmp_pos(pos))
            .ok()?;
        let (stmt, _) = &self.stmts[idx];
        Some(stmt.find_at(pos))
    }

    pub fn bounds_span(&self) -> Option<Span> {
        let (_, fst) = self.stmts.first()?;
        let (_, lst) = self.stmts.last()?;
        Some(fst.merge(lst))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Stmt<'src, K: AstKind = Identity> {
    Let {
        name: K::Inner<&'src str>,
        typ: Option<Box<TypeT<'src, K>>>,
        value: Option<Box<ExprT<'src, K>>>,
    },
    Switch {
        expr: Box<ExprT<'src, K>>,
        cases: Box<[Case<'src, K>]>,
        default: Option<Box<[StmtT<'src, K>]>>,
    },
    If {
        blocks: Box<[ConditionalBlock<'src, K>]>,
        else_: Option<Block<'src, K>>,
    },
    While(Box<ConditionalBlock<'src, K>>),
    ForIn {
        name: K::Inner<&'src str>,
        iter: Box<ExprT<'src, K>>,
        body: Block<'src, K>,
    },
    Return(Option<Box<ExprT<'src, K>>>),
    Break,
    Continue,
    Expr(Box<ExprT<'src, K>>),
}

impl<'src, K: AstKind> Stmt<'src, K> {
    pub fn unwrapped(self) -> Stmt<'src> {
        match self {
            Stmt::Let { name, typ, value } => Stmt::Let {
                name: name.into_wrapped(),
                typ: typ.map(|typ| (*typ).into_wrapped().unwrapped().into()),
                value: value.map(|v| (*v).into_wrapped().unwrapped().into()),
            },
            Stmt::Switch {
                expr,
                cases,
                default,
            } => Stmt::Switch {
                expr: (*expr).into_wrapped().unwrapped().into(),
                cases: cases
                    .into_vec()
                    .into_iter()
                    .map(|c| c.into_wrapped().unwrapped())
                    .collect(),
                default: default.map(|d| {
                    d.into_vec()
                        .into_iter()
                        .map(|s| s.into_wrapped().unwrapped())
                        .collect()
                }),
            },
            Stmt::If { blocks, else_ } => Stmt::If {
                blocks: blocks
                    .into_vec()
                    .into_iter()
                    .map(|b| b.into_wrapped().unwrapped())
                    .collect(),
                else_: else_.map(|b| b.into_wrapped().unwrapped()),
            },
            Stmt::While(block) => Stmt::While(block.into_wrapped().unwrapped().into()),
            Stmt::ForIn { name, iter, body } => Stmt::ForIn {
                name: name.into_wrapped(),
                iter: (*iter).into_wrapped().unwrapped().into(),
                body: body.into_wrapped().unwrapped(),
            },
            Stmt::Return(v) => Stmt::Return(v.map(|v| (*v).into_wrapped().unwrapped().into())),
            Stmt::Break => Stmt::Break,
            Stmt::Continue => Stmt::Continue,
            Stmt::Expr(e) => Stmt::Expr((*e).into_wrapped().unwrapped().into()),
        }
    }
}

impl<'src> Stmt<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> QueryResult<'_, 'src> {
        let res = match self {
            Stmt::Break | Stmt::Continue => return QueryResult::Stmt(self),
            Stmt::Let { value, typ, .. } => {
                if let Some(typ) = typ {
                    let (typ, typ_span) = &**typ;
                    if typ_span.contains(pos) {
                        return typ.find_at(pos);
                    }
                }
                value.as_ref().and_then(|v| {
                    let (v, span) = &**v;
                    span.contains(pos).then_some(v.find_at(pos))
                })
            }
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                let (expr, span) = &**expr;
                if span.contains(pos) {
                    return expr.find_at(pos);
                }
                if let Some(res) = cases.iter().find_map(|c| c.find_at(pos)) {
                    return res;
                }
                default.as_deref().and_then(|d| {
                    d.iter()
                        .find_map(|(s, span)| span.contains(pos).then_some(s.find_at(pos)))
                })
            }
            Stmt::If { blocks, else_ } => blocks
                .iter()
                .find_map(|b| b.find_at(pos))
                .or_else(|| else_.as_ref().and_then(|e| e.find_at(pos))),
            Stmt::While(block) => block.find_at(pos),
            Stmt::ForIn { iter, body, .. } => {
                let (iter, iter_span) = &**iter;
                if iter_span.contains(pos) {
                    return iter.find_at(pos);
                }
                body.find_at(pos)
            }
            Stmt::Return(v) => v.as_ref().and_then(|v| {
                let (v, span) = &**v;
                span.contains(pos).then_some(v.find_at(pos))
            }),
            Stmt::Expr(e) => {
                let (e, span) = &**e;
                if span.contains(pos) {
                    return e.find_at(pos);
                }
                return QueryResult::Stmt(self);
            }
        };
        res.unwrap_or(QueryResult::Stmt(self))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct ConditionalBlock<'src, K: AstKind = Identity> {
    pub condition: LetCondition<'src, K>,
    pub body: Block<'src, K>,
}

impl<'src, K: AstKind> ConditionalBlock<'src, K> {
    #[inline]
    pub fn new(condition: LetCondition<'src, K>, body: Block<'src, K>) -> Self {
        Self { condition, body }
    }

    pub fn unwrapped(self) -> ConditionalBlock<'src> {
        ConditionalBlock {
            condition: self.condition.into_wrapped().unwrapped(),
            body: self.body.into_wrapped().unwrapped(),
        }
    }
}

impl<'src> ConditionalBlock<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        self.condition
            .find_at(pos)
            .or_else(|| self.body.find_at(pos))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub struct Case<'src, K: AstKind = Identity> {
    pub condition: Condition<'src, K>,
    pub body: Box<[StmtT<'src, K>]>,
}

impl<'src, K: AstKind> Case<'src, K> {
    #[inline]
    pub fn new(condition: Condition<'src, K>, body: impl Into<Box<[StmtT<'src, K>]>>) -> Self {
        Self {
            condition,
            body: body.into(),
        }
    }

    pub fn unwrapped(self) -> Case<'src> {
        Case {
            condition: self.condition.into_wrapped().unwrapped(),
            body: self
                .body
                .into_vec()
                .into_iter()
                .map(|s| s.into_wrapped().unwrapped())
                .collect(),
        }
    }
}

impl<'src> Case<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        self.body
            .iter()
            .find_map(|(s, span)| span.contains(pos).then_some(s.find_at(pos)))
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Condition<'src, K: AstKind = Identity> {
    Expr(ExprT<'src, K>),
    Pattern(PatternT<'src, K>),
}

impl<'src, K: AstKind> Condition<'src, K> {
    pub fn unwrapped(self) -> Condition<'src> {
        match self {
            Self::Expr(e) => Condition::Expr(e.into_wrapped().unwrapped()),
            Self::Pattern(p) => Condition::Pattern(p.into_wrapped().unwrapped()),
        }
    }
}

impl<'src> Condition<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        match self {
            Self::Expr((e, span)) => span.contains(pos).then(|| e.find_at(pos)),
            Self::Pattern((p, span)) => span.contains(pos).then(|| p.find_at(pos)).flatten(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Expr((_, span)) | Self::Pattern((_, span)) => *span,
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum LetCondition<'src, K: AstKind = Identity> {
    Expr(ExprT<'src, K>),
    LetPattern(PatternT<'src, K>, ExprT<'src, K>),
}

impl<'src, K: AstKind> LetCondition<'src, K> {
    pub fn unwrapped(self) -> LetCondition<'src> {
        match self {
            Self::Expr(e) => LetCondition::Expr(e.into_wrapped().unwrapped()),
            Self::LetPattern(pat, e) => LetCondition::LetPattern(
                pat.into_wrapped().unwrapped(),
                e.into_wrapped().unwrapped(),
            ),
        }
    }
}

impl<'src> LetCondition<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        match self {
            Self::Expr((e, span)) => span.contains(pos).then(|| e.find_at(pos)),
            Self::LetPattern(pat, e) => {
                let (pat, pat_span) = pat;
                if pat_span.contains(pos) {
                    return pat.find_at(pos);
                }
                let (e, e_span) = e;
                e_span.contains(pos).then(|| e.find_at(pos))
            }
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Expr<'src, K: AstKind = Identity> {
    Ident(&'src str),
    Constant(Constant<'src>),
    ArrayLit(Box<[ExprT<'src, K>]>),
    InterpolatedString(Box<[StrPart<'src, K>]>),
    Assign {
        lhs: Box<ExprT<'src, K>>,
        rhs: Box<ExprT<'src, K>>,
    },
    BinOp {
        lhs: Box<ExprT<'src, K>>,
        op: BinOp,
        rhs: Box<ExprT<'src, K>>,
    },
    UnOp {
        op: UnOp,
        expr: Box<ExprT<'src, K>>,
    },
    Call {
        expr: Box<ExprT<'src, K>>,
        type_args: Box<[TypeT<'src, K>]>,
        args: Box<[ExprT<'src, K>]>,
    },
    Member {
        expr: Box<ExprT<'src, K>>,
        member: &'src str,
    },
    Index {
        expr: Box<ExprT<'src, K>>,
        index: Box<ExprT<'src, K>>,
    },
    DynCast {
        expr: Box<ExprT<'src, K>>,
        typ: Box<TypeT<'src, K>>,
    },
    New {
        typ: Box<TypeT<'src, K>>,
        args: Box<[ExprT<'src, K>]>,
    },
    Conditional {
        cond: Box<ExprT<'src, K>>,
        then: Box<ExprT<'src, K>>,
        else_: Box<ExprT<'src, K>>,
    },
    Lambda {
        params: Box<[ParamT<'src, K>]>,
        body: FunctionBody<'src, K>,
    },
    This,
    Super,
    Null,

    Error,
}

impl<'src, K: AstKind> Expr<'src, K> {
    pub fn unwrapped(self) -> Expr<'src> {
        match self {
            Expr::Ident(i) => Expr::Ident(i),
            Expr::Constant(c) => Expr::Constant(c),
            Expr::ArrayLit(a) => Expr::ArrayLit(
                a.into_vec()
                    .into_iter()
                    .map(|e| e.into_wrapped().unwrapped())
                    .collect(),
            ),
            Expr::InterpolatedString(parts) => Expr::InterpolatedString(
                parts
                    .into_vec()
                    .into_iter()
                    .map(|p| p.into_wrapped().unwrapped())
                    .collect(),
            ),
            Expr::Assign { lhs, rhs } => Expr::Assign {
                lhs: (*lhs).into_wrapped().unwrapped().into(),
                rhs: (*rhs).into_wrapped().unwrapped().into(),
            },
            Expr::BinOp { lhs, op, rhs } => Expr::BinOp {
                lhs: (*lhs).into_wrapped().unwrapped().into(),
                op,
                rhs: (*rhs).into_wrapped().unwrapped().into(),
            },
            Expr::UnOp { op, expr } => Expr::UnOp {
                op,
                expr: (*expr).into_wrapped().unwrapped().into(),
            },
            Expr::Call {
                expr,
                type_args,
                args,
            } => Expr::Call {
                expr: (*expr).into_wrapped().unwrapped().into(),
                type_args: type_args
                    .into_vec()
                    .into_iter()
                    .map(|t| t.into_wrapped().unwrapped())
                    .collect(),
                args: args
                    .into_vec()
                    .into_iter()
                    .map(|a| a.into_wrapped().unwrapped())
                    .collect(),
            },
            Expr::Member { expr, member } => Expr::Member {
                expr: (*expr).into_wrapped().unwrapped().into(),
                member,
            },
            Expr::Index { expr, index } => Expr::Index {
                expr: (*expr).into_wrapped().unwrapped().into(),
                index: (*index).into_wrapped().unwrapped().into(),
            },
            Expr::DynCast { expr, typ } => Expr::DynCast {
                expr: (*expr).into_wrapped().unwrapped().into(),
                typ: (*typ).into_wrapped().unwrapped().into(),
            },
            Expr::New { typ, args } => Expr::New {
                typ: (*typ).into_wrapped().unwrapped().into(),
                args: args
                    .into_vec()
                    .into_iter()
                    .map(|a| a.into_wrapped().unwrapped())
                    .collect(),
            },
            Expr::Conditional { cond, then, else_ } => Expr::Conditional {
                cond: (*cond).into_wrapped().unwrapped().into(),
                then: (*then).into_wrapped().unwrapped().into(),
                else_: (*else_).into_wrapped().unwrapped().into(),
            },
            Expr::Lambda { params, body } => Expr::Lambda {
                params: params
                    .into_vec()
                    .into_iter()
                    .map(|p| p.into_wrapped().unwrapped())
                    .collect(),
                body: body.into_wrapped().unwrapped(),
            },
            Expr::This => Expr::This,
            Expr::Super => Expr::Super,
            Expr::Null => Expr::Null,
            Expr::Error => Expr::Error,
        }
    }
}

impl<'src> Expr<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> QueryResult<'_, 'src> {
        match self {
            Expr::Ident(_)
            | Expr::Constant(_)
            | Expr::This
            | Expr::Super
            | Expr::Null
            | Expr::Error => QueryResult::Expr(self),
            Expr::ArrayLit(e) => e
                .iter()
                .find_map(|(e, s)| s.contains(pos).then_some(e.find_at(pos)))
                .unwrap_or(QueryResult::Expr(self)),
            Expr::InterpolatedString(parts) => parts
                .iter()
                .find_map(|p| match p {
                    StrPart::Expr((e, s)) if s.contains(pos) => Some(e.find_at(pos)),
                    _ => None,
                })
                .unwrap_or(QueryResult::Expr(self)),
            Expr::BinOp { lhs, rhs, .. }
            | Expr::Assign { lhs, rhs }
            | Expr::Index {
                expr: lhs,
                index: rhs,
            } => {
                let (lhs, lhs_span) = &**lhs;
                let (rhs, rhs_span) = &**rhs;
                if lhs_span.contains(pos) {
                    lhs.find_at(pos)
                } else if rhs_span.contains(pos) {
                    rhs.find_at(pos)
                } else {
                    QueryResult::Expr(self)
                }
            }
            Expr::UnOp { expr, .. } | Expr::Member { expr, .. } => {
                let (expr, span) = &**expr;
                if span.contains(pos) {
                    expr.find_at(pos)
                } else {
                    QueryResult::Expr(self)
                }
            }
            Expr::Call {
                expr,
                type_args,
                args,
            } => {
                let (expr, span) = &**expr;
                if span.contains(pos) {
                    expr.find_at(pos)
                } else {
                    type_args
                        .iter()
                        .find_map(|(typ, s)| s.contains(pos).then_some(typ.find_at(pos)))
                        .or_else(|| {
                            args.iter()
                                .find_map(|(a, s)| s.contains(pos).then_some(a.find_at(pos)))
                        })
                        .unwrap_or(QueryResult::Expr(self))
                }
            }
            Expr::DynCast { expr, typ, .. } => {
                let (expr, span) = &**expr;
                let (typ, typ_span) = &**typ;
                if span.contains(pos) {
                    expr.find_at(pos)
                } else if typ_span.contains(pos) {
                    typ.find_at(pos)
                } else {
                    QueryResult::Expr(self)
                }
            }
            Expr::New { typ, args } => {
                let (typ, typ_span) = &**typ;
                if typ_span.contains(pos) {
                    typ.find_at(pos)
                } else {
                    args.iter()
                        .find_map(|(a, s)| s.contains(pos).then_some(a.find_at(pos)))
                        .unwrap_or(QueryResult::Expr(self))
                }
            }
            Expr::Conditional { cond, then, else_ } => {
                let (cond, span) = &**cond;
                let (then, then_span) = &**then;
                let (else_, else_span) = &**else_;

                if span.contains(pos) {
                    cond.find_at(pos)
                } else if then_span.contains(pos) {
                    then.find_at(pos)
                } else if else_span.contains(pos) {
                    else_.find_at(pos)
                } else {
                    QueryResult::Expr(self)
                }
            }
            Expr::Lambda { body, params, .. } => {
                if let Ok(idx) = params.binary_search_by(|(_, span)| span.cmp_pos(pos)) {
                    let (param, _) = &params[idx];
                    param
                        .typ
                        .as_ref()
                        .filter(|(_, s)| s.contains(pos))
                        .map(|(t, _)| t.find_at(pos))
                        .unwrap_or(QueryResult::Expr(self))
                } else {
                    body.find_at(pos).unwrap_or(QueryResult::Expr(self))
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'src> {
    String(Cow<'src, str>),
    CName(Cow<'src, str>),
    Resource(Cow<'src, str>),
    TweakDbId(Cow<'src, str>),
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    Bool(bool),
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum StrPart<'src, K: AstKind = Identity> {
    Expr(ExprT<'src, K>),
    Str(Cow<'src, str>),
}

impl<'src, K: AstKind> StrPart<'src, K> {
    pub fn unwrapped(self) -> StrPart<'src> {
        match self {
            StrPart::Expr(e) => StrPart::Expr(e.into_wrapped().unwrapped()),
            StrPart::Str(s) => StrPart::Str(s),
        }
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum Pattern<'src, K: AstKind = Identity> {
    Name(K::Inner<&'src str>),
    As(Box<K::Inner<Self>>, TypeT<'src, K>),
    Aggregate(K::Inner<&'src str>, Box<[(K::Inner<&'src str>, Self)]>),
    Nullable(Box<K::Inner<Self>>),
    Array(ArraySpread, K::Inner<Box<[Self]>>),
}

impl<'src, K: AstKind> Pattern<'src, K> {
    pub fn unwrapped(self) -> Pattern<'src> {
        match self {
            Self::Name(name) => Pattern::Name(name.into_wrapped()),
            Self::As(pat, typ) => Pattern::As(
                Box::new((*pat).into_wrapped().unwrapped()),
                typ.into_wrapped().unwrapped(),
            ),
            Self::Aggregate(name, fields) => Pattern::Aggregate(
                name.into_wrapped(),
                fields
                    .into_vec()
                    .into_iter()
                    .map(|(n, p)| (n.into_wrapped(), p.into_wrapped().unwrapped()))
                    .collect(),
            ),
            Self::Nullable(pat) => Pattern::Nullable((*pat).into_wrapped().unwrapped().into()),
            Self::Array(spread, pats) => Pattern::Array(
                spread,
                pats.into_wrapped()
                    .into_vec()
                    .into_iter()
                    .map(|p| p.into_wrapped().unwrapped())
                    .collect::<Box<_>>(),
            ),
        }
    }
}

impl<'src> Pattern<'src, WithSpan> {
    pub fn find_at(&self, pos: u32) -> Option<QueryResult<'_, 'src>> {
        match self {
            Pattern::Name(_) => None,
            Pattern::Nullable(pat) => {
                let (pat, pat_span) = &**pat;
                pat_span.contains(pos).then(|| pat.find_at(pos)).flatten()
            }
            Pattern::As(pat, typ) => {
                let (pat, pat_span) = &**pat;
                let (typ, typ_span) = &typ;
                if pat_span.contains(pos) {
                    pat.find_at(pos)
                } else if typ_span.contains(pos) {
                    Some(typ.find_at(pos))
                } else {
                    None
                }
            }
            Pattern::Aggregate((_, _), fields) => fields.iter().find_map(|(_, p)| p.find_at(pos)),
            Pattern::Array(_, (pats, _)) => pats.iter().find_map(|p| p.find_at(pos)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArraySpread {
    Start,
    End,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    BitNot,
    Not,
    Neg,
}

impl UnOp {
    pub fn name(self) -> &'static str {
        match self {
            Self::Not => "OperatorLogicNot",
            Self::Neg => "OperatorNeg",
            Self::BitNot => "OperatorBitNot",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "OperatorLogicNot" => Some(Self::Not),
            "OperatorNeg" => Some(Self::Neg),
            "OperatorBitNot" => Some(Self::BitNot),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignBitOr,
    AssignBitAnd,
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinOp {
    pub fn precedence(self) -> u8 {
        match self {
            Self::AssignAdd
            | Self::AssignSub
            | Self::AssignMul
            | Self::AssignDiv
            | Self::AssignBitOr
            | Self::AssignBitAnd => 0,
            Self::Or => 1,
            Self::And => 2,
            Self::BitOr => 3,
            Self::BitXor => 4,
            Self::BitAnd => 5,
            Self::Eq | Self::Ne => 6,
            Self::Lt | Self::Le | Self::Gt | Self::Ge => 7,
            Self::Add | Self::Sub => 8,
            Self::Mul | Self::Div | Self::Mod => 9,
        }
    }

    pub fn assoc(self) -> Assoc {
        match self {
            Self::AssignAdd
            | Self::AssignSub
            | Self::AssignMul
            | Self::AssignDiv
            | Self::AssignBitOr
            | Self::AssignBitAnd => Assoc::Right,
            Self::Or
            | Self::And
            | Self::BitOr
            | Self::BitXor
            | Self::BitAnd
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod => Assoc::Left,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::AssignAdd => "OperatorAssignAdd",
            Self::AssignSub => "OperatorAssignSubtract",
            Self::AssignMul => "OperatorAssignMultiply",
            Self::AssignDiv => "OperatorAssignDivide",
            Self::AssignBitOr => "OperatorAssignOr",
            Self::AssignBitAnd => "OperatorAssignAnd",
            Self::Or => "OperatorLogicOr",
            Self::And => "OperatorLogicAnd",
            Self::BitOr => "OperatorOr",
            Self::BitXor => "OperatorXor",
            Self::BitAnd => "OperatorAnd",
            Self::Eq => "OperatorEqual",
            Self::Ne => "OperatorNotEqual",
            Self::Lt => "OperatorLess",
            Self::Le => "OperatorLessEqual",
            Self::Gt => "OperatorGreater",
            Self::Ge => "OperatorGreaterEqual",
            Self::Add => "OperatorAdd",
            Self::Sub => "OperatorSubtract",
            Self::Mul => "OperatorMultiply",
            Self::Div => "OperatorDivide",
            Self::Mod => "OperatorModulo",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "OperatorAssignAdd" => Some(Self::AssignAdd),
            "OperatorAssignSubtract" => Some(Self::AssignSub),
            "OperatorAssignMultiply" => Some(Self::AssignMul),
            "OperatorAssignDivide" => Some(Self::AssignDiv),
            "OperatorAssignOr" => Some(Self::AssignBitOr),
            "OperatorAssignAnd" => Some(Self::AssignBitAnd),
            "OperatorLogicOr" => Some(Self::Or),
            "OperatorLogicAnd" => Some(Self::And),
            "OperatorOr" => Some(Self::BitOr),
            "OperatorXor" => Some(Self::BitXor),
            "OperatorAnd" => Some(Self::BitAnd),
            "OperatorEqual" => Some(Self::Eq),
            "OperatorNotEqual" => Some(Self::Ne),
            "OperatorLess" => Some(Self::Lt),
            "OperatorLessEqual" => Some(Self::Le),
            "OperatorGreater" => Some(Self::Gt),
            "OperatorGreaterEqual" => Some(Self::Ge),
            "OperatorAdd" => Some(Self::Add),
            "OperatorSubtract" => Some(Self::Sub),
            "OperatorMultiply" => Some(Self::Mul),
            "OperatorDivide" => Some(Self::Div),
            "OperatorModulo" => Some(Self::Mod),
            _ => None,
        }
    }
}

impl From<BinOp> for &'static str {
    fn from(op: BinOp) -> Self {
        op.name()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
}

bitflags! {
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct ItemQualifiers: u16 {
        const ABSTRACT = 1 << 0;
        const CALLBACK = 1 << 1;
        const CONST = 1 << 2;
        const EXEC = 1 << 3;
        const FINAL = 1 << 4;
        const IMPORT_ONLY = 1 << 5;
        const NATIVE = 1 << 6;
        const PERSISTENT = 1 << 7;
        const QUEST = 1 << 8;
        const STATIC = 1 << 9;
    }
}

bitflags! {
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct ParamQualifiers: u8 {
        const OPTIONAL = 1 << 0;
        const OUT = 1 << 1;
        const CONST = 1 << 2;
    }

}

pub trait AstKind {
    type Inner<A>: Wrapper<A> + fmt::Debug + Clone + PartialEq
    where
        A: fmt::Debug + Clone + PartialEq;
}

pub struct Identity;

impl AstKind for Identity {
    type Inner<A>
        = A
    where
        A: fmt::Debug + Clone + PartialEq;
}

pub struct WithSpan;

impl AstKind for WithSpan {
    type Inner<A>
        = Spanned<A>
    where
        A: fmt::Debug + Clone + PartialEq;
}

pub trait Wrapper<A> {
    fn as_wrapped(&self) -> &A;
    fn into_wrapped(self) -> A;
}

impl<A> Wrapper<A> for A {
    #[inline]
    fn as_wrapped(&self) -> &A {
        self
    }

    #[inline]
    fn into_wrapped(self) -> A {
        self
    }
}

impl<A, B> Wrapper<A> for (A, B) {
    #[inline]
    fn as_wrapped(&self) -> &A {
        &self.0
    }

    #[inline]
    fn into_wrapped(self) -> A {
        self.0
    }
}

#[derive(Debug)]
pub enum QueryResult<'a, 'src> {
    ItemDecl(&'a ItemDecl<'src, WithSpan>),
    Stmt(&'a Stmt<'src, WithSpan>),
    Expr(&'a Expr<'src, WithSpan>),
    Type(&'a Type<'src, WithSpan>),
}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::*;

    #[test]
    fn sizes() {
        assert_eq!(mem::size_of::<Expr<'_>>(), 48);
        assert_eq!(mem::size_of::<Stmt<'_>>(), 48);
        assert_eq!(mem::size_of::<Item<'_>>(), 80);
    }
}

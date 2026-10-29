use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::VecDeque;

use redscript_ast::{self as ast, Span};

use crate::lower::{InferredTypeApp, PolyType};
use crate::symbols::{FieldId, FreeFunctionIndex, MethodId};
use crate::types::{self, TypeId, predef};
use crate::{CoalesceError, RefType, Symbols};

mod intrinsic;

pub use intrinsic::{Intrinsic, UnknownIntrinsic};

#[derive(Debug)]
pub enum Stmt<'ctx> {
    Expr(Box<Expr<'ctx>>),
    Block(Block<'ctx>, Span),
    While(ConditionalBlock<'ctx>, Span),
    Branches {
        branches: Box<[ConditionalBlock<'ctx>]>,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    Switch {
        scrutinee: Box<Expr<'ctx>>,
        scrutinee_type: Type<'ctx>,
        branches: Box<[Case<'ctx>]>,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    InitArray {
        local: Local,
        elements: Box<[Expr<'ctx>]>,
        element_type: Type<'ctx>,
        span: Span,
    },
    InitDefault {
        local: Local,
        typ: Type<'ctx>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Return(Option<Box<Expr<'ctx>>>, Span),
}

impl<'ctx> Stmt<'ctx> {
    pub fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::Block(_, span)
            | Self::While(_, span)
            | Self::Branches { span, .. }
            | Self::Switch { span, .. }
            | Self::InitArray { span, .. }
            | Self::InitDefault { span, .. }
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Return(_, span) => *span,
        }
    }

    pub fn find_at(&self, pos: u32) -> Option<&Expr<'ctx>> {
        match self {
            Stmt::Return(Some(expr), _) | Stmt::Expr(expr) if expr.span().contains(pos) => {
                Some(expr.find_at(pos))
            }
            Stmt::Block(block, span) if span.contains(pos) => block.find_at(pos),
            Stmt::While(cond_block, span) if span.contains(pos) => cond_block.find_at(pos),
            Stmt::Branches {
                branches,
                default,
                span,
            } if span.contains(pos) => branches
                .binary_search_by(|branch| {
                    branch
                        .block
                        .span()
                        .map_or(branch.condition.span(), |s| {
                            branch.condition.span().merge(&s)
                        })
                        .cmp_pos(pos)
                })
                .ok()
                .and_then(|idx| branches[idx].find_at(pos))
                .or_else(|| default.as_ref()?.find_at(pos)),
            Stmt::Switch {
                scrutinee,
                branches,
                default,
                span,
                ..
            } if span.contains(pos) => {
                if scrutinee.span().contains(pos) {
                    Some(scrutinee.find_at(pos))
                } else {
                    branches
                        .iter()
                        .find(|branch| branch.block.span().is_some_and(|span| span.contains(pos)))
                        .and_then(|branch| branch.block.find_at(pos))
                        .or_else(|| default.as_ref()?.find_at(pos))
                }
            }
            Stmt::InitArray { elements, span, .. } if span.contains(pos) => elements
                .binary_search_by(|element| element.span().cmp_pos(pos))
                .ok()
                .map(|idx| elements[idx].find_at(pos)),
            _ => None,
        }
    }
}

impl<'ctx> From<Expr<'ctx>> for Stmt<'ctx> {
    #[inline]
    fn from(expr: Expr<'ctx>) -> Self {
        Self::Expr(Box::new(expr))
    }
}

#[derive(Debug)]
pub struct ConditionalBlock<'ctx> {
    pub condition: Box<Expr<'ctx>>,
    pub block: Block<'ctx>,
}

impl<'ctx> ConditionalBlock<'ctx> {
    #[inline]
    pub fn new(condition: impl Into<Box<Expr<'ctx>>>, block: Block<'ctx>) -> Self {
        Self {
            condition: condition.into(),
            block,
        }
    }

    pub fn find_at(&self, pos: u32) -> Option<&Expr<'ctx>> {
        if self.condition.span().contains(pos) {
            Some(self.condition.find_at(pos))
        } else {
            self.block.find_at(pos)
        }
    }
}

#[derive(Debug, Default)]
pub struct Block<'ctx> {
    pub stmts: VecDeque<Stmt<'ctx>>,
}

impl<'ctx> Block<'ctx> {
    #[inline]
    pub fn new(stmts: impl Into<VecDeque<Stmt<'ctx>>>) -> Self {
        Self {
            stmts: stmts.into(),
        }
    }

    pub fn span(&self) -> Option<Span> {
        let fst = self.stmts.front()?;
        let lst = self.stmts.back()?;
        Some(fst.span().merge(&lst.span()))
    }

    pub fn find_at(&self, pos: u32) -> Option<&Expr<'ctx>> {
        let idx = self
            .stmts
            .binary_search_by(|stmt| stmt.span().cmp_pos(pos))
            .ok()?;
        self.stmts[idx].find_at(pos)
    }

    pub(crate) fn push_prologue(
        &mut self,
        iter: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = Stmt<'ctx>>>,
    ) {
        for stmt in iter.into_iter().rev() {
            self.stmts.push_front(stmt);
        }
    }
}

#[derive(Debug)]
pub struct Case<'ctx> {
    pub matcher: Expr<'ctx>,
    pub block: Block<'ctx>,
}

impl<'ctx> Case<'ctx> {
    #[inline]
    pub fn new(matcher: Expr<'ctx>, block: Block<'ctx>) -> Self {
        Self { matcher, block }
    }
}

#[derive(Debug)]
pub enum Expr<'ctx> {
    NewInstance {
        class_type: TypeApp<'ctx>,
        span: Span,
    },
    Construct {
        struct_type: TypeApp<'ctx>,
        args: Box<[Self]>,
        span: Span,
    },
    NewClosure {
        closure: Box<Closure<'ctx>>,
        span: Span,
    },
    Call {
        call: Box<Call<'ctx>>,
        span: Span,
    },
    Assign {
        place: Box<Self>,
        expr: Box<Self>,
        span: Span,
    },
    Field {
        receiver: Box<Self>,
        receiver_type: TypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
        span: Span,
    },
    Index {
        array: Box<Self>,
        array_type: Type<'ctx>,
        index: Box<Self>,
        span: Span,
    },
    Conditional {
        condition: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
        span: Span,
    },
    DynCast {
        expr: Box<Self>,
        expr_type: Type<'ctx>,
        target_type: TypeApp<'ctx>,
        span: Span,
    },
    Local(Local, Span),
    Capture(Local, Span),
    Const(Const<'ctx>, Span),
    Null {
        is_weak: bool,
        span: Span,
    },
}

impl<'ctx> Expr<'ctx> {
    pub fn call(call: impl Into<Call<'ctx>>, span: Span) -> Self {
        Self::Call {
            call: Box::new(call.into()),
            span,
        }
    }

    pub fn null(span: Span) -> Self {
        Self::Null {
            is_weak: false,
            span,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::NewInstance { span, .. }
            | Self::Construct { span, .. }
            | Self::NewClosure { span, .. }
            | Self::Call { span, .. }
            | Self::Assign { span, .. }
            | Self::Field { span, .. }
            | Self::Index { span, .. }
            | Self::Conditional { span, .. }
            | Self::DynCast { span, .. }
            | Self::Local(_, span)
            | Self::Capture(_, span)
            | Self::Const(_, span)
            | Self::Null { span, .. } => *span,
        }
    }

    pub fn is_prvalue(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Local(_, _) | Self::Capture(_, _) | Self::Field { .. } | Self::Index { .. } => {
                false
            }
            Self::Call { call, .. } => !matches!(
                &**call,
                Call::FreeFunction { function, .. }
                    if symbols[*function].intrinsic().is_some_and(|i| i == Intrinsic::Deref)
            ),
            _ => true,
        }
    }

    pub fn is_prvalue_ref(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Call { call, .. } => matches!(
                &**call,
                Call::FreeFunction { function, args, .. }
                    if symbols[*function].intrinsic().is_some_and(|i| i == Intrinsic::AsRef)
                        && matches!(&args[..], [arg] if arg.is_prvalue(symbols))
            ),
            _ => false,
        }
    }

    pub fn find_at(&self, pos: u32) -> &Expr<'ctx> {
        match self {
            Expr::Construct { args, .. } => args
                .binary_search_by(|arg| arg.span().cmp_pos(pos))
                .ok()
                .map(|i| args[i].find_at(pos))
                .unwrap_or(self),
            Expr::NewClosure { closure, .. } => closure.block.find_at(pos).unwrap_or(self),
            Expr::Call { call, .. } => {
                if let Some(receiver) = call.receiver()
                    && receiver.span().contains(pos)
                {
                    return receiver.find_at(pos);
                };
                let args = call.args();
                args.binary_search_by(|arg| arg.span().cmp_pos(pos))
                    .ok()
                    .map(|i| args[i].find_at(pos))
                    .unwrap_or(self)
            }
            Expr::Assign { place, expr, .. } => {
                if place.span().contains(pos) {
                    place.find_at(pos)
                } else if expr.span().contains(pos) {
                    expr.find_at(pos)
                } else {
                    self
                }
            }
            Expr::Field { receiver, .. } => {
                if receiver.span().contains(pos) {
                    receiver.find_at(pos)
                } else {
                    self
                }
            }
            Expr::Index { array, index, .. } => {
                if array.span().contains(pos) {
                    array.find_at(pos)
                } else if index.span().contains(pos) {
                    index.find_at(pos)
                } else {
                    self
                }
            }
            Expr::Conditional {
                condition,
                then,
                else_,
                ..
            } => {
                if condition.span().contains(pos) {
                    condition.find_at(pos)
                } else if then.span().contains(pos) {
                    then.find_at(pos)
                } else if else_.span().contains(pos) {
                    else_.find_at(pos)
                } else {
                    self
                }
            }
            Expr::DynCast { expr, .. } => {
                if expr.span().contains(pos) {
                    expr.find_at(pos)
                } else {
                    self
                }
            }
            _ => self,
        }
    }
}

#[derive(Debug)]
pub enum Call<'ctx> {
    FreeFunction {
        function: FreeFunctionIndex,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
    },
    Static {
        parent_id: TypeId<'ctx>,
        parent_type_args: Box<[Type<'ctx>]>,
        method: MethodId<'ctx>,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
    },
    Instance {
        receiver: Box<Expr<'ctx>>,
        receiver_type: TypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        method: MethodId<'ctx>,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
        mode: CallMode,
    },
    Closure {
        closure: Box<Expr<'ctx>>,
        closure_type: TypeApp<'ctx>,
        args: Box<[Expr<'ctx>]>,
    },
}

impl<'ctx> Call<'ctx> {
    pub fn args(&self) -> &[Expr<'ctx>] {
        match self {
            Self::FreeFunction { args, .. }
            | Self::Static { args, .. }
            | Self::Instance { args, .. }
            | Self::Closure { args, .. } => args,
        }
    }

    pub fn receiver(&self) -> Option<&Expr<'ctx>> {
        match self {
            Self::Instance { receiver, .. } => Some(receiver),
            Self::Closure { closure, .. } => Some(closure),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Const<'ctx> {
    Str(Cow<'ctx, str>),
    CName(Cow<'ctx, str>),
    ResRef(Cow<'ctx, str>),
    TweakDbId(Cow<'ctx, str>),
    EnumVariant(FieldId<'ctx>),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Bool(bool),
}

impl<'ctx> Const<'ctx> {
    pub fn type_id(&self) -> TypeId<'ctx> {
        match self {
            Self::Str(_) => predef::STRING,
            Self::CName(_) => predef::CNAME,
            Self::ResRef(_) => predef::RES_REF,
            Self::TweakDbId(_) => predef::TWEAK_DB_ID,
            Self::EnumVariant(variant) => variant.parent(),
            Self::F32(_) => predef::FLOAT,
            Self::F64(_) => predef::DOUBLE,
            Self::I8(_) => predef::INT8,
            Self::I16(_) => predef::INT16,
            Self::I32(_) => predef::INT32,
            Self::I64(_) => predef::INT64,
            Self::U8(_) => predef::UINT8,
            Self::U16(_) => predef::UINT16,
            Self::U32(_) => predef::UINT32,
            Self::U64(_) => predef::UINT64,
            Self::Bool(_) => predef::BOOL,
        }
    }
}

impl<'ctx> From<&ast::Constant<'ctx>> for Const<'ctx> {
    fn from(ast: &ast::Constant<'ctx>) -> Self {
        match ast {
            ast::Constant::String(s) => Self::Str(s.clone()),
            ast::Constant::CName(s) => Self::CName(s.clone()),
            ast::Constant::Resource(s) => Self::ResRef(s.clone()),
            ast::Constant::TweakDbId(s) => Self::TweakDbId(s.clone()),
            &ast::Constant::F32(f) => Self::F32(f),
            &ast::Constant::F64(f) => Self::F64(f),
            &ast::Constant::I32(i) => Self::I32(i),
            &ast::Constant::I64(i) => Self::I64(i),
            &ast::Constant::U32(u) => Self::U32(u),
            &ast::Constant::U64(u) => Self::U64(u),
            &ast::Constant::Bool(b) => Self::Bool(b),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Local {
    This,
    Param(u16),
    Var(u16),
}

impl Local {
    pub fn to_index(&self) -> u16 {
        match self {
            Self::This => 0,
            Self::Var(i) | Local::Param(i) => *i + 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalInfo<'ctx> {
    pub id: Local,
    pub name: Option<&'ctx str>,
    pub typ: PolyType<'ctx>,
    pub span: Option<Span>,
}

impl<'ctx> LocalInfo<'ctx> {
    pub fn new(
        id: Local,
        name: Option<&'ctx str>,
        typ: PolyType<'ctx>,
        span: Option<Span>,
    ) -> Self {
        Self {
            id,
            name,
            typ,
            span,
        }
    }
}

#[derive(Debug)]
pub struct Closure<'ctx> {
    pub typ: TypeApp<'ctx>,
    pub locals: Box<[LocalInfo<'ctx>]>,
    pub captures: Box<[Local]>,
    pub block: Block<'ctx>,
}

impl<'ctx> Closure<'ctx> {
    pub fn new(
        typ: impl Into<TypeApp<'ctx>>,
        locals: impl Into<Box<[LocalInfo<'ctx>]>>,
        captures: impl Into<Box<[Local]>>,
        block: Block<'ctx>,
    ) -> Self {
        Self {
            typ: typ.into(),
            locals: locals.into(),
            captures: captures.into(),
            block,
        }
    }
}

#[derive(Debug)]
pub struct Type<'ctx>(Box<RefCell<TypeState<'ctx>>>);

impl<'ctx> Type<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::Type<'ctx>, CoalesceError<'ctx>> {
        let Self(state) = self;
        let typ = match &*state.borrow() {
            TypeState::Uninit(poly) => poly.coalesce(symbols)?,
            TypeState::Init(t) => return Ok(t.clone()),
        };
        *state.borrow_mut() = TypeState::Init(typ.clone());
        Ok(typ)
    }
}

impl<'ctx> From<PolyType<'ctx>> for Type<'ctx> {
    #[inline]
    fn from(poly: PolyType<'ctx>) -> Self {
        Self(Box::new(RefCell::new(TypeState::Uninit(poly))))
    }
}

#[derive(Debug)]
enum TypeState<'ctx> {
    Uninit(PolyType<'ctx>),
    Init(types::Type<'ctx>),
}

#[derive(Debug)]
pub struct TypeApp<'ctx>(Box<RefCell<TypeAppState<'ctx>>>);

impl<'ctx> TypeApp<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::TypeApp<'ctx>, CoalesceError<'ctx>> {
        let Self(state) = self;
        let typ = match &*state.borrow() {
            TypeAppState::Uninit(inferred) => inferred.coalesce(symbols)?,
            TypeAppState::Init(t) => return Ok(t.clone()),
        };
        *state.borrow_mut() = TypeAppState::Init(typ.clone());
        Ok(typ)
    }
}

impl<'ctx> From<InferredTypeApp<'ctx>> for TypeApp<'ctx> {
    #[inline]
    fn from(inferred: InferredTypeApp<'ctx>) -> Self {
        Self(Box::new(RefCell::new(TypeAppState::Uninit(inferred))))
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum CallMode {
    #[default]
    Normal,
    ForceStatic,
}

#[derive(Debug)]
enum TypeAppState<'ctx> {
    Uninit(InferredTypeApp<'ctx>),
    Init(types::TypeApp<'ctx>),
}

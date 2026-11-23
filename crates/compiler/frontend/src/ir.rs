use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt;

use redscript_community_ast::{self as ast, Span};

use crate::lower::{InferredTypeApp, PolyType};
use crate::symbols::{FieldId, FreeFunctionIndex, MethodId};
use crate::sync::OnceCell;
use crate::types::{self, TypeId, predef};
use crate::{CoalesceError, RefType, Symbols};

mod intrinsic;

pub use intrinsic::{Intrinsic, UnknownIntrinsic};

pub use crate::visitor::Visitor;

#[derive(Debug)]
pub enum Stmt<'ctx> {
    Expr(Box<Expr<'ctx>>),
    Block(Block<'ctx>, Span),
    While(ConditionalBlock<'ctx>, Span),
    If {
        branches: Box<[ConditionalBlock<'ctx>]>,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    Switch {
        scrutinee: Box<Expr<'ctx>>,
        scrutinee_type: Type<'ctx>,
        branches: Box<[Case<'ctx>]>,
        /// Number of case labels preceding the default label.
        default_index: usize,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    InitArray {
        local: Local,
        /// Query-only declaration binding, never evaluated or visited.
        /// When present, must be `Expr::Local(local, declaration_name_span)`.
        source_binding: Option<Box<Expr<'ctx>>>,
        elements: Box<[Expr<'ctx>]>,
        element_type: Type<'ctx>,
        span: Span,
    },
    InitDefault {
        local: Local,
        /// Query-only declaration binding, never evaluated or visited.
        /// When present, must be `Expr::Local(local, declaration_name_span)`.
        source_binding: Option<Box<Expr<'ctx>>>,
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
            | Self::If { span, .. }
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
            Stmt::If {
                branches,
                default,
                span,
            } if span.contains(pos) => branches
                .iter()
                .find_map(|branch| branch.find_at(pos))
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
            Stmt::InitArray {
                source_binding,
                elements,
                span,
                ..
            } if span.contains(pos) => source_binding
                .as_deref()
                .filter(|binding| binding.span().contains(pos))
                .or_else(|| {
                    elements
                        .binary_search_by(|element| element.span().cmp_pos(pos))
                        .ok()
                        .map(|idx| elements[idx].find_at(pos))
                }),
            Stmt::InitDefault {
                source_binding,
                span,
                ..
            } if span.contains(pos) => source_binding
                .as_deref()
                .filter(|binding| binding.span().contains(pos)),
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
        self.stmts
            .iter()
            .map(Stmt::span)
            .reduce(|envelope, span| envelope.merge(&span))
    }

    pub fn find_at(&self, pos: u32) -> Option<&Expr<'ctx>> {
        // Lowering can lift prefixes or reorder statements into execution order.
        // Their source spans may overlap; a containing structural statement that
        // has no expression at this position must not hide later statements.
        self.stmts.iter().find_map(|stmt| stmt.find_at(pos))
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

    /// Store and yield the coerced value without reading the place again.
    /// The place is evaluated once, before the value; the result is not a place.
    Assign {
        place: Box<Self>,
        expr: Box<Self>,
        typ: Type<'ctx>,
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
        target_type: Type<'ctx>,
        span: Span,
    },
    /// An absent optional call slot. This has no value or evaluation.
    Nop(Span),
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
            Self::Nop(span)
            | Self::NewInstance { span, .. }
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
            Self::Local(_, _) | Self::Capture(_, _) | Self::Index { .. } => false,
            Self::Field {
                receiver, field, ..
            } => {
                symbols[field.parent()]
                    .schema()
                    .as_aggregate()
                    .is_some_and(|agg| agg.flags().is_struct())
                    && receiver.is_prvalue(symbols)
            }
            Self::Call { call, .. } => match &**call {
                Call::FreeFunction { function, args, .. }
                    if symbols[*function]
                        .intrinsic()
                        .is_some_and(|i| i == Intrinsic::Deref) =>
                {
                    matches!(&args[..], [arg] if arg.is_prvalue_ref(symbols))
                }
                _ => true,
            },
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

/// Target-independent reason for a compiler-generated temporary.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TemporaryPurpose {
    ArrayLiteral,
    IterationIndex,
    IterationIterable,
    PatternScrutinee,
    SwitchScrutinee,
    LoopEntered,
}

/// Source binding associated with a generated temporary, independent of its name.
/// Local identities are scoped to the containing function or closure's local table.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TemporaryOwner {
    Destination(Local),
    IterationBinding(Local),
}

/// Lexical scope identity, unique within a function and its nested closures.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ScopeId(pub u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum InitializationKind {
    AuthoredValue,
    Default,
    Pattern,
    Iteration,
}

/// An initialization operation, identified by the destination local and IR span.
/// This describes the source kind: an authored empty array remains `AuthoredValue`
/// even when lowered to `InitDefault`. Pattern/iteration sites execute per match/step.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Initialization {
    pub kind: InitializationKind,
    pub site: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LocalStorage {
    /// Storage and visibility follow the declaration's lexical scope.
    Lexical,
    /// Function-wide visibility and an additional default initialization at entry.
    /// For declarations without a value, this is also `initialization.site`.
    Hoisted { scope: ScopeId, default_site: Span },
}

/// Source declaration provenance. The containing `LocalInfo::id` is its identity;
/// parameters and compiler-generated temporaries have no declaration record.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct LocalDeclaration {
    pub scope: ScopeId,
    /// Full let statement, root pattern, or for-in statement span.
    pub span: Span,
    /// Source initialization, or the entry default for a valueless hoisted let.
    /// Later assignments are not declaration initializations.
    pub initialization: Initialization,
    pub storage: LocalStorage,
}

#[derive(Debug, Clone)]
pub struct LocalInfo<'ctx> {
    pub id: Local,
    /// Source const qualification, independent of whether handle storage can rebind.
    pub is_const: bool,
    /// Exact declaration-initializer assignment site, not a later binding write.
    pub initializer_span: Option<Span>,
    pub declaration: Option<LocalDeclaration>,
    pub name: Option<&'ctx str>,
    pub temporary_purpose: Option<TemporaryPurpose>,
    pub temporary_owner: Option<TemporaryOwner>,
    pub typ: PolyType<'ctx>,
    /// Source span of the local's name, when it has one.
    pub name_span: Option<Span>,
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
            is_const: false,
            initializer_span: None,
            declaration: None,
            name,
            temporary_purpose: None,
            temporary_owner: None,
            typ,
            name_span: span,
        }
    }

    pub fn generated(
        id: Local,
        purpose: TemporaryPurpose,
        typ: PolyType<'ctx>,
        span: Option<Span>,
    ) -> Self {
        Self {
            temporary_purpose: Some(purpose),
            ..Self::new(id, None, typ, span)
        }
    }
}

#[derive(Debug)]
pub struct Closure<'ctx> {
    pub typ: TypeApp<'ctx>,
    /// Sorted by local ID, as in the enclosing function's local table.
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

pub struct Type<'ctx>(Box<TypeCell<'ctx>>);

struct TypeCell<'ctx> {
    poly: PolyType<'ctx>,
    coalesced: OnceCell<types::Type<'ctx>>,
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Keep the historical `Type(RefCell { value: Uninit/Init(..) })` shape
        // so diagnostics and snapshots do not change.
        f.debug_tuple("Type")
            .field(&TypeCellDebug(&self.0))
            .finish()
    }
}

impl<'ctx> Type<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::Type<'ctx>, CoalesceError<'ctx>> {
        // Compute outside the cell; concurrent callers race harmlessly because
        // coalescing is deterministic and the first stored value wins.
        if let Some(typ) = self.0.coalesced.get() {
            return Ok(typ.clone());
        }
        let typ = self.0.poly.coalesce(symbols)?;
        match self.0.coalesced.set(typ.clone()) {
            Ok(()) => Ok(typ),
            Err(_) => Ok(self
                .0
                .coalesced
                .get()
                .expect("coalesced value stored by a racing or re-entrant caller")
                .clone()),
        }
    }

    pub fn arg_ref_type(&self, symbols: &Symbols<'ctx>, index: usize) -> Option<crate::RefType> {
        if let Some(typ) = self.0.coalesced.get() {
            return match typ {
                types::Type::Data(app) => app.args().get(index).and_then(types::Type::ref_type),
                _ => None,
            };
        }
        self.0
            .poly
            .upper_bound(symbols)
            .and_then(|app| app.args().get(index)?.ref_type(symbols))
    }
}

impl<'ctx> From<PolyType<'ctx>> for Type<'ctx> {
    #[inline]
    fn from(poly: PolyType<'ctx>) -> Self {
        Self(Box::new(TypeCell {
            poly,
            coalesced: OnceCell::new(),
        }))
    }
}

pub struct TypeApp<'ctx>(Box<TypeAppCell<'ctx>>);

struct TypeAppCell<'ctx> {
    inferred: InferredTypeApp<'ctx>,
    coalesced: OnceCell<types::TypeApp<'ctx>>,
}

impl fmt::Debug for TypeApp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Keep the historical `TypeApp(RefCell { value: Uninit/Init(..) })` shape.
        f.debug_tuple("TypeApp")
            .field(&TypeAppCellDebug(&self.0))
            .finish()
    }
}

impl<'ctx> TypeApp<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::TypeApp<'ctx>, CoalesceError<'ctx>> {
        if let Some(typ) = self.0.coalesced.get() {
            return Ok(typ.clone());
        }
        let typ = self.0.inferred.coalesce(symbols)?;
        match self.0.coalesced.set(typ.clone()) {
            Ok(()) => Ok(typ),
            Err(_) => Ok(self
                .0
                .coalesced
                .get()
                .expect("coalesced value stored by a racing or re-entrant caller")
                .clone()),
        }
    }

    pub fn last_arg_ref_type(&self, symbols: &Symbols<'ctx>) -> Option<crate::RefType> {
        if let Some(typ) = self.0.coalesced.get() {
            return typ.args().last().and_then(types::Type::ref_type);
        }
        self.0
            .inferred
            .args()
            .last()
            .and_then(|t| t.ref_type(symbols))
    }
}

impl<'ctx> From<InferredTypeApp<'ctx>> for TypeApp<'ctx> {
    #[inline]
    fn from(inferred: InferredTypeApp<'ctx>) -> Self {
        Self(Box::new(TypeAppCell {
            inferred,
            coalesced: OnceCell::new(),
        }))
    }
}

/// Renders an IR type cell in the pre-`OnceLock` `RefCell` shape.
struct TypeCellDebug<'a, 'ctx>(&'a TypeCell<'ctx>);

impl fmt::Debug for TypeCellDebug<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self.0.coalesced.get() {
            Some(typ) => CellStateDebug::Init(typ),
            None => CellStateDebug::Uninit(&self.0.poly),
        };
        f.debug_struct("RefCell").field("value", &value).finish()
    }
}

struct TypeAppCellDebug<'a, 'ctx>(&'a TypeAppCell<'ctx>);

impl fmt::Debug for TypeAppCellDebug<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self.0.coalesced.get() {
            Some(typ) => CellStateDebug::InitApp(typ),
            None => CellStateDebug::Inferred(&self.0.inferred),
        };
        f.debug_struct("RefCell").field("value", &value).finish()
    }
}

enum CellStateDebug<'a, 'ctx> {
    Uninit(&'a PolyType<'ctx>),
    Inferred(&'a InferredTypeApp<'ctx>),
    Init(&'a types::Type<'ctx>),
    InitApp(&'a types::TypeApp<'ctx>),
}

impl fmt::Debug for CellStateDebug<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Uninit(poly) => f.debug_tuple("Uninit").field(poly).finish(),
            Self::Inferred(inferred) => f.debug_tuple("Uninit").field(inferred).finish(),
            Self::Init(typ) => f.debug_tuple("Init").field(typ).finish(),
            Self::InitApp(typ) => f.debug_tuple("Init").field(typ).finish(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum CallMode {
    #[default]
    Normal,
    ForceStatic,
}

#[cfg(test)]
mod query_tests {
    use super::*;

    fn span(start: u32, end: u32) -> Span {
        let sources = ast::SourceMap::new();
        Span::new(start, end, sources.push_back("query.reds", ""))
    }

    fn value(start: u32) -> Expr<'static> {
        Expr::Const(Const::I32(start as i32), span(start, start + 1))
    }

    fn assert_value(block: &Block<'_>, pos: u32) {
        assert!(
            matches!(block.find_at(pos), Some(Expr::Const(Const::I32(i), _)) if *i == pos as i32)
        );
    }

    #[test]
    fn block_queries_reordered_and_lifted_statements_without_sorting() {
        let block = Block::new([
            Stmt::Break(span(20, 20)),
            value(80).into(),
            value(10).into(),
            value(40).into(),
        ]);
        assert_eq!(block.span(), Some(span(10, 81)));
        for pos in [10, 40, 80] {
            assert_value(&block, pos);
        }
        assert!(block.find_at(30).is_none());
        // Querying must leave execution order unchanged.
        assert_eq!(block.stmts[1].span(), span(80, 81));
    }

    #[test]
    fn overlapping_branch_envelopes_do_not_hide_later_expressions() {
        let empty = || {
            ConditionalBlock::new(
                Expr::Const(Const::Bool(true), span(0, 0)),
                Block::new([Stmt::Break(span(0, 100))]),
            )
        };
        let block = Block::new([
            Stmt::If {
                branches: vec![empty()].into(),
                default: None,
                span: span(0, 100),
            },
            Stmt::If {
                branches: vec![
                    empty(),
                    ConditionalBlock::new(value(80), Block::new([value(10).into()])),
                    ConditionalBlock::new(value(40), Block::default()),
                ]
                .into(),
                default: Some(Block::new([value(60).into()])),
                span: span(0, 100),
            },
        ]);
        for pos in [10, 40, 60, 80] {
            assert_value(&block, pos);
        }
        assert!(block.find_at(30).is_none());
    }
}

#[cfg(test)]
mod prvalue_tests {
    use super::*;
    use crate::{CompileErrorReporter, Evaluator, TypeInterner, parse_file, process_sources};

    #[test]
    fn deref_and_struct_fields_propagate_prvalue_status() {
        let sources = ast::SourceMap::new();
        let intrinsics = sources.push_back(
            "intrinsics.reds",
            "@intrinsic(AsRef) func AsRef<T>(a: T) -> script_ref<T>\n@intrinsic(Deref) func Deref<T>(a: script_ref<T>) -> T",
        );
        let file = sources.push_back(
            "prvalue.reds",
            "
            struct S { public let x: Int32; }
            struct Outer { public let inner: S; }
            class C { public let x: Int32; public let inner: S; }
            native func MakeStruct() -> S
            native func MakeOuter() -> Outer
            native func MakeClass() -> ref<C>
            native func NativeRef() -> script_ref<Int32>
            func TakeRef(v: script_ref<Int32>) {}
            func Out(out v: Int32) {}
            func Test(p: script_ref<Int32>) {
                let l: Int32 = 1;
                Deref(AsRef(42)) = 1;
                Deref(AsRef(l)) = 1;
                Deref(p) = 1;
                Deref(NativeRef()) = 1;
                TakeRef(MakeClass().x);
                TakeRef(MakeClass().inner.x);
                Out(Deref(AsRef(42)));
            }
            ",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = [intrinsics, file]
            .into_iter()
            .map(|id| parse_file(id, sources.get(id).unwrap(), &mut reporter).unwrap())
            .collect::<Vec<_>>();
        let evaluator = Evaluator::from_modules(&modules);
        let (unit, symbols) = process_sources(
            modules,
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
        );
        let codes = reporter
            .reported()
            .iter()
            .map(crate::Diagnostic::code)
            .collect::<Vec<_>>();
        assert!(codes.contains(&"INVALID_PLACE"), "{codes:?}");
        assert!(codes.contains(&"INVALID_TEMP"), "{codes:?}");
        assert!(
            !codes.contains(&"INACCESSIBLE_FIELD"),
            "unexpected field error: {codes:?}"
        );

        let test_fn = symbols
            .free_functions()
            .find(|e| e.name().to_string() == "Test")
            .unwrap();
        let stmts = &unit.functions[test_fn.key()].block.stmts;
        let assign_place = |i: usize| match &stmts[i] {
            Stmt::Expr(expr) => match &**expr {
                Expr::Assign { place, .. } => &**place,
                _ => panic!("expected assign at {i}"),
            },
            _ => panic!("expected expr at {i}"),
        };
        assert!(assign_place(1).is_prvalue(&symbols));
        assert!(!assign_place(2).is_prvalue(&symbols));
        assert!(!assign_place(3).is_prvalue(&symbols));
        assert!(!assign_place(4).is_prvalue(&symbols));

        // Class field (and struct field rooted in a class handle) is a heap place, not a prvalue.
        for idx in [5, 6] {
            let Stmt::Expr(call_expr) = &stmts[idx] else {
                panic!("expected call at {idx}");
            };
            let Expr::Call { call, .. } = &**call_expr else {
                panic!("expected call at {idx}");
            };
            assert!(!call.args()[0].is_prvalue_ref(&symbols));
        }

        // Struct field (and nested struct field) on a temporary struct is a prvalue.
        let make_outer = symbols
            .free_functions()
            .find(|e| e.name().to_string() == "MakeOuter")
            .unwrap();
        let crate::Type::Data(outer_app) = make_outer.func().type_().return_type() else {
            panic!("expected struct return type");
        };
        let outer_id = outer_app.id();
        let (inner_idx, _) = symbols[outer_id]
            .schema()
            .as_aggregate()
            .unwrap()
            .fields()
            .by_name("inner")
            .unwrap();
        let make_struct = symbols
            .free_functions()
            .find(|e| e.name().to_string() == "MakeStruct")
            .unwrap();
        let crate::Type::Data(struct_app) = make_struct.func().type_().return_type() else {
            panic!("expected struct return type");
        };
        let struct_id = struct_app.id();
        let (field_idx, _) = symbols[struct_id]
            .schema()
            .as_aggregate()
            .unwrap()
            .fields()
            .by_name("x")
            .unwrap();
        let span = Span::new(0, 0, file);
        let temp_outer = Expr::Call {
            call: Call::FreeFunction {
                function: *make_outer.key(),
                type_args: Box::default(),
                args: Box::default(),
            }
            .into(),
            span,
        };
        let temp_inner = Expr::Field {
            receiver: temp_outer.into(),
            receiver_type: InferredTypeApp::nullary(outer_id).into(),
            receiver_ref: None,
            field: FieldId::new(outer_id, inner_idx),
            span,
        };
        let temp_field = Expr::Field {
            receiver: temp_inner.into(),
            receiver_type: InferredTypeApp::nullary(struct_id).into(),
            receiver_ref: None,
            field: FieldId::new(struct_id, field_idx),
            span,
        };
        assert!(temp_field.is_prvalue(&symbols));
    }
}

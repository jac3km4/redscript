use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};
use std::ops::{Add, Sub};

use derive_where::derive_where;
use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::{Display, EnumString, IntoStaticStr};

use crate::bytecode::Location;
use crate::Str;

#[derive(EnumAsInner)]
#[derive_where(Debug; N::Class, N::Type, N::Local, N::Member, N::Closure, N::Callable, N::CallMeta, N::Inferred)]
pub enum Expr<N>
where
    N: ExprKind,
{
    Ident(N::Local, Span),
    Constant(Constant, Span),
    ArrayLit(Box<[Self]>, N::Inferred, Span),
    InterpolatedString(Str, Vec<(Self, Str)>, Span),
    Declare(N::Local, Option<Box<N::Type>>, Option<Box<Self>>, Span),
    DynCast(N::Class, Box<Self>, Span),
    Assign(Box<Self>, Box<Self>, N::Inferred, Span),
    Call(Box<Self>, N::Callable, Box<[N::Type]>, Box<[Self]>, N::CallMeta, Span),
    Lambda(N::Closure, Box<Self>, Span),
    Member(Box<Self>, N::Member, Span),
    ArrayElem(Box<Self>, Box<Self>, N::Inferred, Span),
    New(N::Class, Box<[Self]>, Span),
    Return(Option<Box<Self>>, Span),
    Seq(Seq<N>),
    Switch(Box<Self>, Vec<SwitchCase<N>>, Option<Seq<N>>, Span),
    Goto(Target, Span),
    If(Box<Self>, Seq<N>, Option<Seq<N>>, Span),
    Conditional(Box<Self>, Box<Self>, Box<Self>, Span),
    While(Box<Self>, Seq<N>, Span),
    ForIn(N::Local, Box<Self>, Seq<N>, Span),
    BinOp(Box<Self>, Box<Self>, BinOp, Span),
    UnOp(Box<Self>, UnOp, Span),
    This(Span),
    Super(Span),
    Break(Span),
    Null(Span),
}

impl<N> Expr<N>
where
    N: ExprKind,
{
    pub const EMPTY: Self = Expr::Seq(Seq { exprs: vec![] });

    pub fn is_empty(&self) -> bool {
        match self {
            Expr::Seq(seq) => seq.exprs.iter().all(Expr::is_empty),
            _ => false,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(_, span)
            | Expr::Constant(_, span)
            | Expr::ArrayLit(_, _, span)
            | Expr::InterpolatedString(_, _, span)
            | Expr::Declare(_, _, _, span)
            | Expr::DynCast(_, _, span)
            | Expr::Assign(_, _, _, span)
            | Expr::Call(_, _, _, _, _, span)
            | Expr::Lambda(_, _, span)
            | Expr::Member(_, _, span)
            | Expr::ArrayElem(_, _, _, span)
            | Expr::New(_, _, span)
            | Expr::Return(_, span)
            | Expr::Switch(_, _, _, span)
            | Expr::Goto(_, span)
            | Expr::If(_, _, _, span)
            | Expr::Conditional(_, _, _, span)
            | Expr::While(_, _, span)
            | Expr::ForIn(_, _, _, span)
            | Expr::BinOp(_, _, _, span)
            | Expr::UnOp(_, _, span)
            | Expr::This(span)
            | Expr::Super(span)
            | Expr::Break(span)
            | Expr::Null(span) => *span,
            Expr::Seq(seq) => {
                let start = seq.exprs.first().map(Self::span).unwrap_or_default();
                let end = seq.exprs.last().map(Self::span).unwrap_or_default();
                start.merge(end)
            }
        }
    }
}

impl<N: ExprKind> Default for Expr<N> {
    #[inline]
    fn default() -> Self {
        Self::EMPTY
    }
}

pub trait ExprKind {
    type Callable;
    type CallMeta;
    type Inferred;
    type Local;
    type Member;
    type Class;
    type Type;
    type Closure;
}

#[derive(Debug)]
pub struct SourceAst;

impl ExprKind for SourceAst {
    type Local = Ident;
    type Member = Ident;
    type Class = TypeName;
    type Type = TypeName;
    type Closure = Box<[Param]>;
    type Callable = ();
    type CallMeta = ();
    type Inferred = ();
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(Literal, Str),
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    Bool(bool),
}

#[derive(Debug)]
pub struct Param {
    pub name: Ident,
    pub typ: Option<TypeName>,
}

pub type Ident = super::Str;

#[derive(Debug, Clone, Copy, Display, EnumString, IntoStaticStr)]
pub enum BinOp {
    #[strum(serialize = "OperatorAssignAdd")]
    AssignAdd,
    #[strum(serialize = "OperatorAssignSubtract")]
    AssignSubtract,
    #[strum(serialize = "OperatorAssignMultiply")]
    AssignMultiply,
    #[strum(serialize = "OperatorAssignDivide")]
    AssignDivide,
    #[strum(serialize = "OperatorAssignOr")]
    AssignOr,
    #[strum(serialize = "OperatorAssignAnd")]
    AssignAnd,
    #[strum(serialize = "OperatorLogicOr")]
    LogicOr,
    #[strum(serialize = "OperatorLogicAnd")]
    LogicAnd,
    #[strum(serialize = "OperatorOr")]
    Or,
    #[strum(serialize = "OperatorXor")]
    Xor,
    #[strum(serialize = "OperatorAnd")]
    And,
    #[strum(serialize = "OperatorEqual")]
    Equal,
    #[strum(serialize = "OperatorNotEqual")]
    NotEqual,
    #[strum(serialize = "OperatorLess")]
    Less,
    #[strum(serialize = "OperatorLessEqual")]
    LessEqual,
    #[strum(serialize = "OperatorGreater")]
    Greater,
    #[strum(serialize = "OperatorGreaterEqual")]
    GreaterEqual,
    #[strum(serialize = "OperatorAdd")]
    Add,
    #[strum(serialize = "OperatorSubtract")]
    Subtract,
    #[strum(serialize = "OperatorMultiply")]
    Multiply,
    #[strum(serialize = "OperatorDivide")]
    Divide,
    #[strum(serialize = "OperatorModulo")]
    Modulo,
}

impl BinOp {
    pub fn precedence(self) -> usize {
        match self {
            BinOp::AssignAdd
            | BinOp::AssignSubtract
            | BinOp::AssignMultiply
            | BinOp::AssignDivide
            | BinOp::AssignOr
            | BinOp::AssignAnd => 10,
            BinOp::LogicOr => 9,
            BinOp::LogicAnd => 8,
            BinOp::Or => 7,
            BinOp::Xor => 6,
            BinOp::And => 5,
            BinOp::Equal | BinOp::NotEqual => 4,
            BinOp::Less | BinOp::LessEqual | BinOp::Greater | BinOp::GreaterEqual => 3,
            BinOp::Add | BinOp::Subtract => 2,
            BinOp::Multiply | BinOp::Divide | BinOp::Modulo => 1,
        }
    }

    pub fn associative(self) -> bool {
        match self {
            BinOp::AssignAdd
            | BinOp::AssignSubtract
            | BinOp::AssignMultiply
            | BinOp::AssignDivide
            | BinOp::AssignOr
            | BinOp::AssignAnd
            | BinOp::Divide
            | BinOp::Modulo
            | BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Less
            | BinOp::LessEqual
            | BinOp::Greater
            | BinOp::GreaterEqual => false,
            BinOp::LogicOr
            | BinOp::LogicAnd
            | BinOp::Or
            | BinOp::Xor
            | BinOp::And
            | BinOp::Add
            | BinOp::Subtract
            | BinOp::Multiply => true,
        }
    }

    pub fn does_associate(self, parent: BinOp) -> bool {
        parent.precedence() > self.precedence() || (parent.precedence() == self.precedence() && parent.associative())
    }
}

#[derive(Debug, Clone, Copy, Display, EnumString, IntoStaticStr)]
pub enum UnOp {
    #[strum(serialize = "OperatorBitNot")]
    BitNot,
    #[strum(serialize = "OperatorLogicNot")]
    LogicNot,
    #[strum(serialize = "OperatorNeg")]
    Neg,
}

#[derive_where(Debug; N::Class, N::Type, N::Local, N::Member, N::Closure, N::Callable, N::CallMeta, N::Inferred)]
pub struct SwitchCase<N>
where
    N: ExprKind,
{
    pub matcher: Expr<N>,
    pub body: Seq<N>,
}

#[derive_where(Debug; N::Class, N::Type, N::Local, N::Member, N::Closure, N::Callable, N::CallMeta, N::Inferred)]
pub struct Seq<N>
where
    N: ExprKind,
{
    pub exprs: Vec<Expr<N>>,
}

impl<N> Seq<N>
where
    N: ExprKind,
{
    #[inline]
    pub fn new(exprs: Vec<Expr<N>>) -> Seq<N> {
        Seq { exprs }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String,
    Name,
    Resource,
    TweakDbId,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos(pub u32);

impl Pos {
    pub const ZERO: Pos = Pos(0);

    #[inline]
    pub fn new(n: usize) -> Self {
        Pos(n as u32)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<usize> for Pos {
    type Output = Pos;

    #[inline]
    fn add(self, rhs: usize) -> Pos {
        Pos(self.0 + rhs as u32)
    }
}

impl Sub<usize> for Pos {
    type Output = Pos;

    #[inline]
    fn sub(self, rhs: usize) -> Pos {
        Pos(self.0 - rhs as u32)
    }
}

impl From<Pos> for usize {
    #[inline]
    fn from(pos: Pos) -> Self {
        pos.0 as usize
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub low: Pos,
    pub high: Pos,
}

impl Span {
    pub const ZERO: Span = Span::new(Pos::ZERO, Pos::ZERO);

    pub const fn new(low: Pos, high: Pos) -> Self {
        Self { low, high }
    }

    pub fn with_len(low: usize, len: usize) -> Self {
        Self {
            low: Pos(u32::try_from(low).unwrap_or_default()),
            high: Pos(u32::try_from(low + len).unwrap_or_default()),
        }
    }

    pub fn merge(&self, other: Span) -> Span {
        Span::new(self.low.min(other.low), self.high.max(other.high))
    }

    pub fn contains(&self, pos: Pos) -> bool {
        self.low <= pos && self.high > pos
    }

    pub fn compare_pos(&self, pos: Pos) -> Ordering {
        if self.low > pos {
            Ordering::Greater
        } else if self.high < pos {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Target {
    pub position: Location,
}

impl Target {
    pub fn new(position: Location) -> Target {
        Self { position }
    }
}

#[macro_export]
macro_rules! function_type_by_arity {
    ($id:path, $($n:literal),+) => {&[$($id(concat!("Function", $n))),*]};
    ($id:path) => {function_type_by_arity!($id, 0, 1, 2, 3, 4, 5, 6, 7)}
}

#[macro_export]
macro_rules! function_arity_from_str {
    ($e:expr, $($n:literal),+) => {match $e { $(stringify!($n) => Some($n),)* _ => None }};
    ($e:expr) => {function_arity_from_str!($e, 0, 1, 2, 3, 4, 5, 6, 7)}
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeName {
    name: Ident,
    arguments: Box<[TypeName]>,
}

impl TypeName {
    #[inline]
    pub fn new(name: Ident, arguments: Vec<TypeName>) -> Self {
        TypeName {
            name,
            arguments: arguments.into(),
        }
    }

    #[inline]
    pub fn without_args(name: Ident) -> Self {
        TypeName {
            name,
            arguments: [].into(),
        }
    }

    #[inline]
    pub fn name(&self) -> &Ident {
        &self.name
    }

    #[inline]
    pub fn arguments(&self) -> &[TypeName] {
        &self.arguments
    }

    pub fn of_array(elem: TypeName) -> Self {
        Self::new("array".into(), vec![elem])
    }

    pub fn of_function(mut args: Vec<TypeName>, ret: TypeName) -> Self {
        const NAMES: &[Ident] = function_type_by_arity!(Ident::from_static);
        let name = NAMES[args.len()].clone();
        args.push(ret);
        Self::new(name, args)
    }

    pub fn from_repr(str: &str) -> TypeName {
        let mut parts = str.split(':');
        Self::from_parts(parts.next().unwrap(), parts).unwrap()
    }

    fn from_parts<'a>(name: &'a str, mut parts: impl Iterator<Item = &'a str>) -> Option<TypeName> {
        let name = Ident::from_ref(name);
        match parts.next() {
            Some(tail) => {
                let arg = Self::from_parts(tail, parts)?;
                Some(Self::new(name, vec![arg]))
            }
            None => Some(TypeName::without_args(name)),
        }
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.arguments {
            [] => f.write_str(&self.name),
            arguments => write!(f, "{}<{}>", self.name, arguments.iter().format(", ")),
        }
    }
}

#[derive(Debug)]
pub struct TypeParam {
    pub name: Ident,
    pub variance: Variance,
    pub extends: Option<TypeName>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    Co,
    In,
    Contra,
}

impl Variance {
    pub fn combine(self, other: Variance) -> Variance {
        match (self, other) {
            (Self::In, _) | (_, Self::In) => Self::In,
            (Self::Co, Self::Co) | (Self::Contra, Self::Contra) => Self::Co,
            _ => Self::Contra,
        }
    }
}

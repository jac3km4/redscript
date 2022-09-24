use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};
use std::iter;
use std::ops::{Add, Not, Sub};

use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use strum::{Display, EnumString, IntoStaticStr};

use super::str_fmt;
use crate::bytecode::Location;
use crate::Ref;

#[derive(Debug, EnumAsInner)]
pub enum Expr<Name>
where
    Name: NameKind,
    Name::Reference: Debug,
    Name::Callable: Debug,
    Name::Local: Debug,
    Name::Function: Debug,
    Name::Member: Debug,
    Name::Type: Debug,
{
    Ident(Name::Reference, Span),
    Constant(Constant, Span),
    ArrayLit(Box<[Self]>, Option<Box<Name::Type>>, Span),
    InterpolatedString(Ref<str>, Vec<(Self, Ref<str>)>, Span),
    Declare(Name::Local, Option<Box<Name::Type>>, Option<Box<Self>>, Span),
    Cast(Name::Type, Box<Self>, Span),
    Assign(Box<Self>, Box<Self>, Span),
    Call(Name::Callable, Box<[Name::Type]>, Box<[Self]>, Span),
    MethodCall(Box<Self>, Name::Function, Vec<Self>, Span),
    Member(Box<Self>, Name::Member, Span),
    ArrayElem(Box<Self>, Box<Self>, Span),
    New(Name::Type, Box<[Self]>, Span),
    Return(Option<Box<Self>>, Span),
    Seq(Seq<Name>),
    Switch(Box<Self>, Vec<SwitchCase<Name>>, Option<Seq<Name>>, Span),
    Goto(Target, Span),
    If(Box<Self>, Seq<Name>, Option<Seq<Name>>, Span),
    Conditional(Box<Self>, Box<Self>, Box<Self>, Span),
    While(Box<Self>, Seq<Name>, Span),
    ForIn(Name::Local, Box<Self>, Seq<Name>, Span),
    BinOp(Box<Self>, Box<Self>, BinOp, Span),
    UnOp(Box<Self>, UnOp, Span),
    This(Span),
    Super(Span),
    Break(Span),
    Null(Span),
}

pub trait NameKind {
    type Reference;
    type Callable;
    type Local;
    type Function;
    type Member;
    type Type;
}

#[derive(Debug)]
pub struct SourceAst;

impl NameKind for SourceAst {
    type Reference = Ident;
    type Callable = Ident;
    type Local = Ident;
    type Function = Ident;
    type Member = Ident;
    type Type = TypeName;
}

impl<N> Expr<N>
where
    N: NameKind,
    N::Reference: Debug,
    N::Callable: Debug,
    N::Local: Debug,
    N::Function: Debug,
    N::Member: Debug,
    N::Type: Debug,
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
            | Expr::Cast(_, _, span)
            | Expr::Assign(_, _, span)
            | Expr::Call(_, _, _, span)
            | Expr::MethodCall(_, _, _, span)
            | Expr::Member(_, _, span)
            | Expr::ArrayElem(_, _, span)
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

#[derive(Debug, Clone)]
pub enum Constant {
    String(Literal, Ref<str>),
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    Bool(bool),
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

#[derive(Debug)]
pub struct SwitchCase<N>
where
    N: NameKind,
    N::Reference: Debug,
    N::Callable: Debug,
    N::Local: Debug,
    N::Function: Debug,
    N::Member: Debug,
    N::Type: Debug,
{
    pub matcher: Expr<N>,
    pub body: Seq<N>,
}

#[derive(Debug)]
pub struct Seq<N>
where
    N: NameKind,
    N::Reference: Debug,
    N::Callable: Debug,
    N::Local: Debug,
    N::Function: Debug,
    N::Member: Debug,
    N::Type: Debug,
{
    pub exprs: Vec<Expr<N>>,
}

impl<N> Seq<N>
where
    N: NameKind,
    N::Reference: Debug,
    N::Callable: Debug,
    N::Local: Debug,
    N::Function: Debug,
    N::Member: Debug,
    N::Type: Debug,
{
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

#[derive(Debug, PartialEq, Eq)]
pub struct TypeName {
    name: Ident,
    arguments: Option<Box<[TypeName]>>,
}

impl TypeName {
    pub const BOOL: Self = TypeName::basic("Bool");
    pub const INT8: Self = TypeName::basic("Int8");
    pub const INT16: Self = TypeName::basic("Int16");
    pub const INT32: Self = TypeName::basic("Int32");
    pub const INT64: Self = TypeName::basic("Int64");
    pub const UINT8: Self = TypeName::basic("Uint8");
    pub const UINT16: Self = TypeName::basic("Uint16");
    pub const UINT32: Self = TypeName::basic("Uint32");
    pub const UINT64: Self = TypeName::basic("Uint64");
    pub const FLOAT: Self = TypeName::basic("Float");
    pub const DOUBLE: Self = TypeName::basic("Double");
    pub const STRING: Self = TypeName::basic("String");
    pub const VARIANT: Self = TypeName::basic("Variant");
    pub const CNAME: Self = TypeName::basic("CName");
    pub const RESOURCE: Self = TypeName::basic("ResRef");
    pub const TWEAKDB_ID: Self = TypeName::basic("TweakDBID");
    pub const VOID: Self = TypeName::basic("Void");

    #[inline]
    pub fn new(name: Ident, arguments: Vec<TypeName>) -> Self {
        TypeName {
            name,
            arguments: arguments.is_empty().not().then(|| arguments.into_boxed_slice()),
        }
    }

    #[inline]
    pub const fn basic(name: &'static str) -> Self {
        TypeName {
            name: Ident::from_static(name),
            arguments: None,
        }
    }

    #[inline]
    pub fn basic_owned(name: Ref<str>) -> Self {
        TypeName {
            name: Ident::from_heap(name),
            arguments: None,
        }
    }

    #[inline]
    pub fn name(&self) -> Ident {
        self.name.clone()
    }

    #[inline]
    pub fn arguments(&self) -> &[TypeName] {
        self.arguments.as_deref().unwrap_or_default()
    }

    pub fn kind(&self) -> Kind {
        match self.name.as_ref() {
            "ref" => Kind::Ref,
            "wref" => Kind::WRef,
            "script_ref" => Kind::ScriptRef,
            "array" => Kind::Array,
            _ => Kind::Prim,
        }
    }

    fn unwrapped(&self) -> &Self {
        match (self.kind(), self.arguments()) {
            (Kind::Ref | Kind::WRef, [arg]) => arg.unwrapped(),
            _ => self,
        }
    }

    // Used for identifying functions
    pub fn mangled(&self) -> Ident {
        let unwrapped = self.unwrapped();
        match &unwrapped.arguments {
            Some(arguments) => {
                let args = arguments.iter().map(TypeName::mangled).format(",");
                str_fmt!("{}<{args}>", unwrapped.name)
            }
            None => unwrapped.name.clone(),
        }
    }

    pub fn pretty(&self) -> Ident {
        match &self.arguments {
            Some(arguments) => {
                let args = arguments.iter().map(TypeName::pretty).format(", ");
                str_fmt!("{}<{args}>", self.name)
            }
            None => self.name.clone(),
        }
    }

    // Used for storing types in the constant pool
    pub fn repr(&self) -> Ident {
        match &self.arguments {
            Some(arguments) => {
                let str = iter::once(self.name.clone())
                    .chain(arguments.iter().map(TypeName::repr))
                    .join(":");
                Ident::from_heap(str.into())
            }
            None => self.name.clone(),
        }
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
                let type_ = TypeName {
                    name,
                    arguments: Some([arg].into()),
                };
                Some(type_)
            }
            None => Some(TypeName { name, arguments: None }),
        }
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.mangled().as_ref())
    }
}

#[derive(Debug)]
pub enum Kind {
    Prim,
    Ref,
    WRef,
    ScriptRef,
    Array,
}

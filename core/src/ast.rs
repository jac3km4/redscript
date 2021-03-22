use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ops::{Add, Sub};
use std::rc::Rc;

use strum::{Display, IntoStaticStr};

#[derive(Debug)]
pub enum Expr<Name: NameKind>
where
    Name: NameKind,
    Name::Reference: Debug,
    Name::Callable: Debug,
    Name::Local: Debug,
    Name::Function: Debug,
    Name::Member: Debug,
    Name::Type: Debug,
{
    Ident(Name::Reference, Pos),
    Constant(Constant, Pos),
    ArrayLit(Vec<Self>, Option<Name::Type>, Pos),
    Declare(Name::Local, Option<Name::Type>, Option<Box<Self>>, Pos),
    Cast(Name::Type, Box<Self>, Pos),
    Assign(Box<Self>, Box<Self>, Pos),
    Call(Name::Callable, Vec<Self>, Pos),
    MethodCall(Box<Self>, Name::Function, Vec<Self>, Pos),
    Member(Box<Self>, Name::Member, Pos),
    ArrayElem(Box<Self>, Box<Self>, Pos),
    New(Name::Type, Vec<Self>, Pos),
    Return(Option<Box<Self>>, Pos),
    Seq(Seq<Name>),
    Switch(Box<Self>, Vec<SwitchCase<Name>>, Option<Seq<Name>>),
    Goto(Target, Pos),
    If(Box<Self>, Seq<Name>, Option<Seq<Name>>, Pos),
    Conditional(Box<Self>, Box<Self>, Box<Self>, Pos),
    While(Box<Self>, Seq<Name>, Pos),
    ForIn(Name::Local, Box<Self>, Seq<Name>, Pos),
    BinOp(Box<Self>, Box<Self>, BinOp, Pos),
    UnOp(Box<Self>, UnOp, Pos),
    This(Pos),
    Super(Pos),
    Break(Pos),
    Null,
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
pub struct Source;

impl NameKind for Source {
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
            Expr::Seq(seq) => seq.exprs.iter().all(|expr| expr.is_empty()),
            Expr::Goto(target, _) => target.resolved,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(Literal, Rc<String>),
    Float(f64),
    Int(i64),
    Uint(u64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Ident {
    Static(&'static str),
    Owned(Rc<String>),
}

impl Ident {
    pub fn new(str: String) -> Ident {
        Ident::Owned(Rc::new(str))
    }

    pub fn to_owned(&self) -> Rc<String> {
        match self {
            Ident::Static(str) => Rc::new(str.to_string()),
            Ident::Owned(rc) => rc.clone(),
        }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Static(str) => f.write_str(str),
            Ident::Owned(str) => f.write_str(&str),
        }
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        match self {
            Ident::Static(str) => str,
            Ident::Owned(rc) => rc,
        }
    }
}

#[derive(Debug, Clone, Copy, IntoStaticStr)]
pub enum BinOp {
    #[strum(serialize = "OperatorAssignAdd")]
    AssignAdd,
    #[strum(serialize = "OperatorAssignSubtract")]
    AssignSubtract,
    #[strum(serialize = "OperatorAssignMultiply")]
    AssignMultiply,
    #[strum(serialize = "OperatorAssignDivide")]
    AssignDivide,
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

#[derive(Debug, Clone, Copy, Display)]
pub enum UnOp {
    BitNot,
    LogicNot,
    Neg,
}

impl UnOp {
    pub fn name(&self) -> String {
        format!("Operator{}", self)
    }
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    fn add(self, other: usize) -> Pos {
        Pos(self.0 + other as u32)
    }
}

impl Sub<Pos> for Pos {
    type Output = Pos;
    fn sub(self, other: Pos) -> Pos {
        Pos(self.0 - other.0)
    }
}

#[derive(Debug, Clone)]
pub struct Target {
    pub position: u16,
    pub resolved: bool,
}

impl Target {
    pub fn new(position: u16) -> Target {
        Target {
            position,
            resolved: false,
        }
    }
}

#[derive(Debug)]
pub struct TypeName {
    pub name: Ident,
    pub arguments: Vec<TypeName>,
}

impl TypeName {
    pub const BOOL: Self = TypeName::basic("Bool");
    pub const INT32: Self = TypeName::basic("Int32");
    pub const UINT32: Self = TypeName::basic("Uint32");
    pub const FLOAT: Self = TypeName::basic("Float");
    pub const STRING: Self = TypeName::basic("String");
    pub const VARIANT: Self = TypeName::basic("Variant");
    pub const CNAME: Self = TypeName::basic("CName");
    pub const RESOURCE: Self = TypeName::basic("ResRef");
    pub const TWEAKDB_ID: Self = TypeName::basic("TweakDBID");

    fn unwrapped(&self) -> &Self {
        match self.name.as_ref() {
            "ref" => &self.arguments[0].unwrapped(),
            "wref" => &self.arguments[0].unwrapped(),
            _ => self,
        }
    }

    pub const fn basic(name: &'static str) -> Self {
        TypeName {
            name: Ident::Static(name),
            arguments: vec![],
        }
    }

    pub fn basic_owned(name: Rc<String>) -> Self {
        TypeName {
            name: Ident::Owned(name),
            arguments: vec![],
        }
    }

    // Used for identifying functions
    pub fn mangled(&self) -> Ident {
        let unwrapped = self.unwrapped();
        match unwrapped.arguments.first() {
            None => unwrapped.name.clone(),
            Some(head) => {
                let args = unwrapped
                    .arguments
                    .iter()
                    .skip(1)
                    .map(|tp| tp.mangled())
                    .fold(head.mangled(), |acc, el| Ident::new(format!("{},{}", acc, el)));
                Ident::new(format!("{}<{}>", unwrapped.name.as_ref(), args))
            }
        }
    }

    // Used for storing types in the constant pool
    pub fn repr(&self) -> Ident {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            self.arguments.iter().fold(self.name.clone(), |acc, tp| {
                Ident::new(format!("{}:{}", acc, tp.repr()))
            })
        }
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.mangled().as_ref())
    }
}

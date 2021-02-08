use std::fmt::{self, Display};
use std::ops::{Add, Sub};
use std::rc::Rc;

use strum::Display;

#[derive(Debug)]
pub enum Expr {
    Ident(Ident, Pos),
    Constant(Constant, Pos),
    Declare(Ident, Option<TypeName<String>>, Option<Box<Expr>>, Pos),
    Cast(TypeName<String>, Box<Expr>, Pos),
    Assign(Box<Expr>, Box<Expr>, Pos),
    Call(Ident, Vec<Expr>, Pos),
    MethodCall(Box<Expr>, Ident, Vec<Expr>, Pos),
    Member(Box<Expr>, Ident, Pos),
    ArrayElem(Box<Expr>, Box<Expr>, Pos),
    New(Ident, Vec<Expr>, Pos),
    Return(Option<Box<Expr>>, Pos),
    Seq(Seq),
    Switch(Box<Expr>, Vec<SwitchCase>, Option<Seq>),
    Goto(Target, Pos),
    If(Box<Expr>, Seq, Option<Seq>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>, Pos),
    While(Box<Expr>, Seq),
    BinOp(Box<Expr>, Box<Expr>, BinOp, Pos),
    UnOp(Box<Expr>, UnOp, Pos),
    This(Pos),
    Super(Pos),
    Break,
    Null,
}

impl Expr {
    pub const EMPTY: Expr = Expr::Seq(Seq { exprs: vec![] });

    pub fn is_empty(&self) -> bool {
        match self {
            Expr::Seq(seq) => seq.exprs.iter().all(|expr| expr.is_empty()),
            Expr::Goto(target, _) => target.resolved,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Constant {
    String(LiteralType, String),
    Float(f64),
    Int(i64),
    Uint(u64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub Rc<String>);

impl Ident {
    pub fn new(str: String) -> Ident {
        Ident(Rc::new(str))
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub enum BinOp {
    AssignAdd,
    AssignSub,
    AssignMultiply,
    AssignDivide,
    LogicOr,
    LogicAnd,
    Or,
    Xor,
    And,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

impl BinOp {
    pub fn name(&self) -> String {
        format!("Operator{}", self)
    }
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
pub struct SwitchCase(pub Expr, pub Seq);

#[derive(Debug)]
pub struct Seq {
    pub exprs: Vec<Expr>,
}

impl Seq {
    pub fn new(exprs: Vec<Expr>) -> Seq {
        Seq { exprs }
    }
}

#[derive(Debug)]
pub enum LiteralType {
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

#[derive(Debug)]
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
pub struct TypeName<S: AsRef<str>> {
    pub name: S,
    pub arguments: Vec<TypeName<S>>,
}

impl TypeName<&str> {
    pub const BOOL: Self = TypeName::basic("Bool");
    pub const INT32: Self = TypeName::basic("Int32");
    pub const UINT32: Self = TypeName::basic("Uint32");
    pub const FLOAT: Self = TypeName::basic("Float");
    pub const STRING: Self = TypeName::basic("String");
    pub const VARIANT: Self = TypeName::basic("Variant");
    pub const CNAME: Self = TypeName::basic("CName");
    pub const RESOURCE: Self = TypeName::basic("ResRef");
    pub const TWEAKDB_ID: Self = TypeName::basic("TweakDBID");
}

impl<S: AsRef<str>> TypeName<S> {
    fn unwrapped(&self) -> &Self {
        match self.name.as_ref() {
            "ref" => &self.arguments[0].unwrapped(),
            "wref" => &self.arguments[0].unwrapped(),
            _ => self,
        }
    }

    pub const fn basic(name: S) -> Self {
        TypeName {
            name,
            arguments: vec![],
        }
    }

    // Used for identifying functions
    pub fn mangled(&self) -> String {
        let unwrapped = self.unwrapped();
        match unwrapped.arguments.first() {
            None => unwrapped.name.as_ref().to_owned(),
            Some(head) => {
                let args = unwrapped
                    .arguments
                    .iter()
                    .skip(1)
                    .map(|tp| tp.mangled())
                    .fold(head.mangled(), |acc, el| format!("{},{}", acc, el));
                format!("{}<{}>", unwrapped.name.as_ref(), args)
            }
        }
    }

    // Used for storing types in the constant pool
    pub fn repr(&self) -> String {
        if self.arguments.is_empty() {
            self.name.as_ref().to_owned()
        } else {
            self.arguments.iter().fold(self.name.as_ref().to_owned(), |acc, tp| {
                format!("{}:{}", acc, tp.repr())
            })
        }
    }
}

impl<S: AsRef<str>> Display for TypeName<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.mangled())
    }
}

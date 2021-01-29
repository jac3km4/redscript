use std::fmt::Display;
use std::rc::Rc;

use strum::Display;

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    StringLit(LiteralType, String),
    FloatLit(f64),
    IntLit(i64),
    UintLit(u64),
    Declare(Ident, Option<TypeName>, Option<Box<Expr>>),
    Cast(TypeName, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Call(Ident, Vec<Expr>),
    MethodCall(Box<Expr>, Ident, Vec<Expr>),
    Member(Box<Expr>, Ident),
    ArrayElem(Box<Expr>, Box<Expr>),
    New(Ident, Vec<Expr>),
    Return(Option<Box<Expr>>),
    Seq(Seq),
    Switch(Box<Expr>, Vec<SwitchCase>, Option<Seq>),
    Goto(Target),
    If(Box<Expr>, Seq, Option<Seq>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Seq),
    BinOp(Box<Expr>, Box<Expr>, BinOp),
    UnOp(Box<Expr>, UnOp),
    Break,
    True,
    False,
    Null,
    This,
}

impl Expr {
    pub const EMPTY: Expr = Expr::Seq(Seq { exprs: vec![] });

    pub fn is_empty(&self) -> bool {
        match self {
            Expr::Seq(seq) => seq.exprs.iter().all(|expr| expr.is_empty()),
            Expr::Goto(target) => target.resolved,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub Rc<String>);

impl Ident {
    pub fn new(str: String) -> Ident {
        Ident(Rc::new(str.to_owned()))
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
pub struct TypeName {
    pub name: String,
    pub arguments: Vec<TypeName>,
}

impl TypeName {
    fn unwrapped(&self) -> &TypeName {
        match self.name.as_str() {
            "ref" => &self.arguments[0].unwrapped(),
            "wref" => &self.arguments[0].unwrapped(),
            _ => self,
        }
    }

    // Used for identifying functions
    pub fn mangled(&self) -> String {
        let unwrapped = self.unwrapped();
        if unwrapped.arguments.is_empty() {
            unwrapped.name.clone()
        } else {
            let args = unwrapped
                .arguments
                .iter()
                .map(|tp| tp.mangled())
                .fold_first(|acc, el| format!("{},{}", acc, el))
                .unwrap();
            format!("{}<{}>", unwrapped.name, args)
        }
    }

    // Used for storing types in the constant pool
    pub fn repr(&self) -> String {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            self.arguments
                .iter()
                .fold(self.name.to_owned(), |acc, tp| format!("{}:{}", acc, tp.repr()))
        }
    }
}

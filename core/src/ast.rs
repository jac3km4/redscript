use std::rc::Rc;

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    StringLit(String),
    NumLit(String),
    Assign(Box<Expr>, Box<Expr>),
    Call(Ident, Vec<Expr>),
    ArrayElem(Box<Expr>, Box<Expr>),
    New(Ident, Vec<Expr>),
    Return(Option<Box<Expr>>),
    Seq(Seq),
    Switch(Box<Expr>, Vec<SwitchCase>, Option<Seq>),
    Goto(Target),
    If(Box<Expr>, Seq, Option<Seq>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Seq),
    Member(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, Box<Expr>, BinOp),
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

#[derive(Debug)]
pub struct Ident(pub Rc<String>);

impl Ident {
    pub fn new(str: &str) -> Ident {
        Ident(Rc::new(str.to_owned()))
    }
}

#[derive(Debug)]
pub enum BinOp {
    Eq,
    Neq,
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

//! Native REDmod source syntax. No semantic lowering or shared-AST projection lives here.
//! All spans are half-open UTF-8 byte ranges in the original source file.

pub use redscript_community_ast::{BinOp, Constant, FileId, Span, Spanned, UnOp, Visibility};

pub type Name<'src> = Spanned<&'src str>;
pub type Block<'src> = Vec<Spanned<Statement<'src>>>;
pub type SourceExpr<'src> = Expr<'src>;
pub type SourceType<'src> = Type<'src>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'src> {
    pub path: Option<Path<'src>>,
    pub items: Vec<Spanned<Declaration<'src>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<'src> {
    pub segments: Vec<&'src str>,
}

impl<'src> Path<'src> {
    pub fn new(segments: Vec<&'src str>) -> Self {
        Self { segments }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration<'src> {
    pub annotations: Vec<Spanned<Annotation<'src>>>,
    pub visibility: Option<Visibility>,
    pub modifiers: Vec<Spanned<Modifier>>,
    pub metadata: Vec<Spanned<Metadata<'src>>>,
    pub docs: Vec<&'src str>,
    pub item: Item<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Modifier {
    Abstract,
    Const,
    Exec,
    Final,
    ImportOnly,
    Import,
    Persistent,
    Quest,
    Static,
    Editable,
    InstanceEditable,
    Inlined,
    Replicated,
    TestOnly,
    Override,
    Export,
    Virtual,
    Constexpr,
    Mutable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation<'src> {
    pub name: Name<'src>,
    pub args: Vec<Spanned<Expr<'src>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata<'src> {
    pub key: Name<'src>,
    pub value: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item<'src> {
    Import(Path<'src>),
    Field(Field<'src>),
    Aggregate {
        is_struct: bool,
        name: Name<'src>,
        extends: Option<Spanned<Type<'src>>>,
        items: Vec<Spanned<Declaration<'src>>>,
    },
    Function {
        kind: FunctionKind<'src>,
        params: Vec<Spanned<ParamGroup<'src>>>,
        return_type: Option<Spanned<Type<'src>>>,
        const_return: Option<Span>,
        body: Option<Block<'src>>,
    },
    Enum {
        name: Name<'src>,
        variants: Vec<Spanned<EnumVariant<'src>>>,
        storage: Option<Spanned<Type<'src>>>,
    },
    Directive {
        kind: DirectiveKind,
        target: Name<'src>,
        value: Spanned<Expr<'src>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'src> {
    pub names: Vec<Name<'src>>,
    pub typ: Spanned<Type<'src>>,
    pub default: Option<Spanned<Expr<'src>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamGroup<'src> {
    pub modifiers: Vec<Spanned<ParamModifier>>,
    pub names: Vec<Name<'src>>,
    pub typ: Spanned<Type<'src>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamModifier {
    Optional,
    Out,
    Const,
    Skip,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant<'src> {
    pub name: Name<'src>,
    /// Explicit Int32 value; storage syntax does not widen this native subset.
    pub value: Option<Spanned<i32>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind<'src> {
    Function(Name<'src>),
    Event(Name<'src>),
    Operator(Spanned<Operator>),
    Cast { implicit: Option<Span>, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Not,
    BitNot,
    AssignAdd,
    AssignSubtract,
    AssignMultiply,
    AssignDivide,
    AssignBitAnd,
    AssignBitOr,
    Index,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectiveKind {
    Default,
    Hint,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'src> {
    Named {
        name: &'src str,
        args: Vec<Spanned<Type<'src>>>,
    },
    Array(Box<Spanned<Type<'src>>>),
    StaticArray(Box<Spanned<Type<'src>>>, usize),
}

impl<'src> Type<'src> {
    pub fn plain(name: &'src str) -> Self {
        Self::Named { name, args: vec![] }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Ident(&'src str),
    Constant(Constant<'src>),
    Null,
    This,
    Super,
    ArrayLit(Box<[Spanned<Expr<'src>>]>),
    New {
        typ: Box<Spanned<Type<'src>>>,
    },
    Call {
        expr: Box<Spanned<Expr<'src>>>,
        args: Box<[Spanned<Expr<'src>>]>,
    },
    Member {
        expr: Box<Spanned<Expr<'src>>>,
        member: &'src str,
    },
    Index {
        expr: Box<Spanned<Expr<'src>>>,
        index: Box<Spanned<Expr<'src>>>,
    },
    Cast {
        typ: Box<Spanned<Type<'src>>>,
        expr: Box<Spanned<Expr<'src>>>,
        style: CastStyle,
    },
    DynCast {
        expr: Box<Spanned<Expr<'src>>>,
        typ: Box<Spanned<Type<'src>>>,
    },
    UnOp {
        op: UnOp,
        expr: Box<Spanned<Expr<'src>>>,
    },
    BinOp {
        lhs: Box<Spanned<Expr<'src>>>,
        op: BinOp,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    Assign {
        lhs: Box<Spanned<Expr<'src>>>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    Conditional {
        cond: Box<Spanned<Expr<'src>>>,
        then: Box<Spanned<Expr<'src>>>,
        else_: Box<Spanned<Expr<'src>>>,
    },
    Sequence(Box<[Spanned<Expr<'src>>]>),
    /// An explicitly empty argument slot, not an absent argument list.
    Omitted,
    /// A recovery placeholder; its parse result also carries a diagnostic.
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastStyle {
    CStyle,
    Intrinsic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local<'src> {
    pub const_: Option<Span>,
    pub names: Vec<Name<'src>>,
    pub typ: Spanned<Type<'src>>,
    pub value: Option<Spanned<Expr<'src>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'src> {
    Local(Local<'src>),
    Expr(Box<Spanned<Expr<'src>>>),
    Return(Option<Box<Spanned<Expr<'src>>>>),
    Break,
    Continue,
    Empty,
    Block(Block<'src>),
    If {
        blocks: Vec<(Spanned<Expr<'src>>, Block<'src>)>,
        else_: Option<Block<'src>>,
    },
    While {
        condition: Spanned<Expr<'src>>,
        body: Block<'src>,
    },
    DoWhile {
        body: Block<'src>,
        condition: Spanned<Expr<'src>>,
    },
    For {
        init: Option<Box<Spanned<Statement<'src>>>>,
        condition: Option<Spanned<Expr<'src>>>,
        step: Option<Spanned<Expr<'src>>>,
        body: Block<'src>,
    },
    Switch {
        expr: Spanned<Expr<'src>>,
        labels: Vec<Spanned<SwitchLabel<'src>>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchLabel<'src> {
    /// `None` denotes a default label. Labels retain their written order.
    pub value: Option<Spanned<Expr<'src>>>,
    pub body: Block<'src>,
}

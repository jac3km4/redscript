use std::ptr;

use crate::ast::{Condition, ExprT, ItemDeclT, LetCondition, ParamT, PatternT, StmtT, TypeT};
use crate::{
    Aggregate, AstKind, BinOp, Block, Case, ConditionalBlock, Constant, Enum, Expr, Field,
    Function, FunctionBody, Import, Item, ItemDecl, Module, Pattern, Span, Stmt, StrPart, UnOp,
    WithSpan, Wrapper,
};

pub trait AstVisitor<'src, K: AstKind> {
    type Error;

    #[inline]
    fn visit_import(&mut self, _import: &Import<'src>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_class(&mut self, class: &Aggregate<'src, K>) -> Result<(), Self::Error> {
        class
            .items
            .iter()
            .try_for_each(|field| self.visit_item_decl(field))
    }

    fn visit_struct(&mut self, struct_: &Aggregate<'src, K>) -> Result<(), Self::Error> {
        struct_
            .items
            .iter()
            .try_for_each(|field| self.visit_item_decl(field))
    }

    fn visit_function(&mut self, function: &Function<'src, K>) -> Result<(), Self::Error> {
        match &function.body {
            Some(FunctionBody::Inline(expr)) => self.visit_expr(expr),
            Some(FunctionBody::Block(block)) => self.visit_block(block),
            None => Ok(()),
        }
    }

    fn visit_field(&mut self, field: &Field<'src, K>) -> Result<(), Self::Error> {
        field
            .default
            .iter()
            .try_for_each(|value| self.visit_expr(value))
    }

    #[inline]
    fn visit_enum(&mut self, _enum_: &Enum<'src, K>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_block(&mut self, block: &Block<'src, K>) -> Result<(), Self::Error> {
        block
            .stmts
            .iter()
            .try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_let(
        &mut self,
        _name: &K::Inner<&'src str>,
        _type: &Option<Box<TypeT<'src, K>>>,
        value: &Option<Box<ExprT<'src, K>>>,
    ) -> Result<(), Self::Error> {
        value.iter().try_for_each(|value| self.visit_expr(value))
    }

    fn visit_switch(
        &mut self,
        expr: &ExprT<'src, K>,
        cases: &[Case<'src, K>],
        default: &Option<Box<[StmtT<'src, K>]>>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr)?;
        cases.iter().try_for_each(|case| self.visit_case(case))?;
        default
            .iter()
            .try_for_each(|stmts| self.visit_default(stmts))
    }

    fn visit_case(&mut self, case: &Case<'src, K>) -> Result<(), Self::Error> {
        self.visit_condition(&case.condition)?;
        case.body.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_default(&mut self, stmts: &[StmtT<'src, K>]) -> Result<(), Self::Error> {
        stmts.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_if(
        &mut self,
        blocks: &[ConditionalBlock<'src, K>],
        else_: &Option<Block<'src, K>>,
    ) -> Result<(), Self::Error> {
        for block in blocks {
            self.visit_let_condition(&block.condition)?;
            self.visit_block(&block.body)?;
        }
        else_.iter().try_for_each(|block| self.visit_block(block))
    }

    fn visit_while(&mut self, block: &ConditionalBlock<'src, K>) -> Result<(), Self::Error> {
        self.visit_let_condition(&block.condition)?;
        self.visit_block(&block.body)
    }

    fn visit_for_in(
        &mut self,
        _name: &K::Inner<&'src str>,
        iter: &ExprT<'src, K>,
        body: &Block<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(iter)?;
        self.visit_block(body)
    }

    fn visit_return(&mut self, expr: &Option<Box<ExprT<'src, K>>>) -> Result<(), Self::Error> {
        expr.iter().try_for_each(|expr| self.visit_expr(expr))
    }

    #[inline]
    fn visit_break(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn visit_continue(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn visit_ident(&mut self, _ident: &'src str) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn visit_constant(&mut self, _value: &Constant<'src>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_array_lit(&mut self, elements: &[ExprT<'src, K>]) -> Result<(), Self::Error> {
        elements
            .iter()
            .try_for_each(|element| self.visit_expr(element))
    }

    fn visit_interpolated_string(&mut self, parts: &[StrPart<'src, K>]) -> Result<(), Self::Error> {
        for part in parts {
            match part {
                StrPart::Expr(expr) => self.visit_expr(expr)?,
                StrPart::Str(_) => {}
            }
        }
        Ok(())
    }

    fn visit_assign(
        &mut self,
        lhs: &ExprT<'src, K>,
        rhs: &ExprT<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }

    fn visit_bin_op(
        &mut self,
        lhs: &ExprT<'src, K>,
        _op: BinOp,
        rhs: &ExprT<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }

    fn visit_un_op(&mut self, _op: UnOp, expr: &ExprT<'src, K>) -> Result<(), Self::Error> {
        self.visit_expr(expr)
    }

    fn visit_call(
        &mut self,
        expr: &ExprT<'src, K>,
        _type_args: &[TypeT<'src, K>],
        args: &[ExprT<'src, K>],
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr)?;
        args.iter().try_for_each(|arg| self.visit_expr(arg))
    }

    fn visit_member(
        &mut self,
        expr: &ExprT<'src, K>,
        _member: &'src str,
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr)
    }

    fn visit_index(
        &mut self,
        expr: &ExprT<'src, K>,
        index: &ExprT<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr)?;
        self.visit_expr(index)
    }

    fn visit_dyn_cast(
        &mut self,
        expr: &ExprT<'src, K>,
        _type: &TypeT<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr)
    }

    fn visit_new(
        &mut self,
        _type: &TypeT<'src, K>,
        args: &[ExprT<'src, K>],
    ) -> Result<(), Self::Error> {
        args.iter().try_for_each(|arg| self.visit_expr(arg))
    }

    fn visit_conditional(
        &mut self,
        cond: &ExprT<'src, K>,
        then: &ExprT<'src, K>,
        else_: &ExprT<'src, K>,
    ) -> Result<(), Self::Error> {
        self.visit_expr(cond)?;
        self.visit_expr(then)?;
        self.visit_expr(else_)
    }

    fn visit_lambda(
        &mut self,
        _params: &[ParamT<'src, K>],
        body: &FunctionBody<'src, K>,
    ) -> Result<(), Self::Error> {
        match body {
            FunctionBody::Inline(expr) => self.visit_expr(expr),
            FunctionBody::Block(block) => self.visit_block(block),
        }
    }

    #[inline]
    fn visit_this(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn visit_super(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn visit_null(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_module(&mut self, module: &Module<'src, K>) -> Result<(), Self::Error> {
        module
            .items
            .iter()
            .try_for_each(|item| self.visit_item_decl(item))
    }

    fn visit_condition(&mut self, condition: &Condition<'src, K>) -> Result<(), Self::Error> {
        match condition {
            Condition::Expr(expr) => self.visit_expr(expr),
            Condition::Pattern(pat) => self.visit_pattern(pat),
        }
    }

    fn visit_let_condition(
        &mut self,
        condition: &LetCondition<'src, K>,
    ) -> Result<(), Self::Error> {
        match condition {
            LetCondition::Expr(expr) => self.visit_expr(expr),
            LetCondition::LetPattern(pat, val) => {
                self.visit_pattern(pat)?;
                self.visit_expr(val)
            }
        }
    }

    fn visit_pattern(&mut self, condition: &PatternT<'src, K>) -> Result<(), Self::Error> {
        self.visit_node(AstNode::Pattern(condition))?;
        self.post_visit_node(AstNode::Pattern(condition))
    }

    fn visit_item_decl(&mut self, item_decl: &ItemDeclT<'src, K>) -> Result<(), Self::Error> {
        self.visit_node(AstNode::ItemDecl(item_decl))?;
        let inner = item_decl.as_wrapped();
        match &inner.item {
            Item::Import(import) => self.visit_import(import)?,
            Item::Class(class) => self.visit_class(class)?,
            Item::Struct(struct_) => self.visit_struct(struct_)?,
            Item::Function(function) => self.visit_function(function)?,
            Item::Let(field) => self.visit_field(field)?,
            Item::Enum(_enum) => self.visit_enum(_enum)?,
        }
        self.post_visit_node(AstNode::ItemDecl(item_decl))
    }

    fn visit_stmt(&mut self, stmt: &StmtT<'src, K>) -> Result<(), Self::Error> {
        self.visit_node(AstNode::Stmt(stmt))?;
        let inner = stmt.as_wrapped();
        match inner {
            Stmt::Let { name, typ, value } => self.visit_let(name, typ, value)?,
            Stmt::Switch {
                expr,
                cases,
                default,
            } => self.visit_switch(expr, cases, default)?,
            Stmt::If { blocks, else_ } => self.visit_if(blocks, else_)?,
            Stmt::While(block) => self.visit_while(block)?,
            Stmt::ForIn { name, iter, body } => self.visit_for_in(name, iter, body)?,
            Stmt::Return(expr) => self.visit_return(expr)?,
            Stmt::Break => self.visit_break()?,
            Stmt::Continue => self.visit_continue()?,
            Stmt::Expr(expr) => self.visit_expr(expr)?,
        };
        self.post_visit_node(AstNode::Stmt(stmt))
    }

    fn visit_expr(&mut self, expr: &ExprT<'src, K>) -> Result<(), Self::Error> {
        self.visit_node(AstNode::Expr(expr))?;
        let inner = expr.as_wrapped();
        match inner {
            Expr::Ident(ident) => self.visit_ident(ident)?,
            Expr::Constant(value) => self.visit_constant(value)?,
            Expr::ArrayLit(elements) => self.visit_array_lit(elements)?,
            Expr::InterpolatedString(parts) => self.visit_interpolated_string(parts)?,
            Expr::Assign { lhs, rhs } => self.visit_assign(lhs, rhs)?,
            Expr::BinOp { lhs, op, rhs } => self.visit_bin_op(lhs, *op, rhs)?,
            Expr::UnOp { op, expr } => self.visit_un_op(*op, expr)?,
            Expr::Call {
                expr,
                type_args,
                args,
            } => self.visit_call(expr, type_args, args)?,
            Expr::Member { expr, member } => self.visit_member(expr, member)?,
            Expr::Index { expr, index } => self.visit_index(expr, index)?,
            Expr::DynCast { expr, typ } => self.visit_dyn_cast(expr, typ)?,
            Expr::New { typ, args } => self.visit_new(typ, args)?,
            Expr::Conditional { cond, then, else_ } => self.visit_conditional(cond, then, else_)?,
            Expr::Lambda { params, body } => self.visit_lambda(params, body)?,
            Expr::This => self.visit_this()?,
            Expr::Super => self.visit_super()?,
            Expr::Null => self.visit_null()?,
            Expr::Error => {}
        }
        self.post_visit_node(AstNode::Expr(expr))
    }

    #[inline]
    fn visit_node(&mut self, _node: AstNode<'_, 'src, K>) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn post_visit_node(&mut self, _node: AstNode<'_, 'src, K>) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AstNode<'a, 'src, K: AstKind> {
    ItemDecl(&'a ItemDeclT<'src, K>),
    Stmt(&'a StmtT<'src, K>),
    Expr(&'a ExprT<'src, K>),
    Pattern(&'a PatternT<'src, K>),
}

impl<K: AstKind> AstNode<'_, '_, K> {
    pub fn id(self) -> NodeId {
        match self {
            Self::ItemDecl(item_decl) => NodeId::item_decl::<K>(item_decl.as_wrapped()),
            Self::Stmt(stmt) => NodeId::stmt::<K>(stmt.as_wrapped()),
            Self::Expr(stmt) => NodeId::expr::<K>(stmt.as_wrapped()),
            Self::Pattern(pat) => NodeId::condition::<K>(pat.as_wrapped()),
        }
    }
}

impl AstNode<'_, '_, WithSpan> {
    pub fn span(&self) -> Span {
        match self {
            Self::ItemDecl((_, span))
            | Self::Stmt((_, span))
            | Self::Expr((_, span))
            | Self::Pattern((_, span)) => *span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(*const ());

impl NodeId {
    #[inline]
    pub fn item_decl<K: AstKind>(item_decl: &ItemDecl<'_, K>) -> Self {
        Self(ptr::from_ref(item_decl).cast())
    }

    #[inline]
    pub fn stmt<K: AstKind>(stmt: &Stmt<'_, K>) -> Self {
        Self(ptr::from_ref(stmt).cast())
    }

    #[inline]
    pub fn expr<K: AstKind>(expr: &Expr<'_, K>) -> Self {
        Self(ptr::from_ref(expr).cast())
    }

    #[inline]
    pub fn condition<K: AstKind>(condition: &Pattern<'_, K>) -> Self {
        Self(ptr::from_ref(condition).cast())
    }

    #[inline]
    pub fn block<K: AstKind>(block: &Block<'_, K>) -> Self {
        Self(ptr::from_ref(block).cast())
    }
}

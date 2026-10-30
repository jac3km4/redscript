use std::cell::Cell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use hashbrown::{HashMap, HashSet};
use redscript_ast as ast;
use redscript_io::{CodeIter, Function, Instr, LocalIndex, Offset, ScriptBundle};

use crate::control_flow::{BlockType, ControlFlowBlock};
use crate::error::{Error, Result};
use crate::location::{Bounds, Location};
use crate::{BundleOps, expect_borrowed, extract_mangled_name, extract_type};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Verbosity {
    #[default]
    Quiet,
    Verbose,
}

pub fn decompile_block<'ctx: 'i, 'i>(
    function: &'ctx Function<'i>,
    bundle: &'ctx ScriptBundle<'i>,
    code: &[Instr],
    block: &ControlFlowBlock,
    verbosity: Verbosity,
) -> Result<ast::Block<'i>> {
    let mut decomp = Decompiler::new(function, code, block, bundle, verbosity);
    let block = decomp.consume_block(block, Location::MAX)?;
    let mut stmts = decomp.into_prelude()?;
    stmts.extend(block.stmts);
    Ok(ast::Block::new(stmts))
}

struct Decompiler<'scope, 'ctx, 'i> {
    function: &'ctx Function<'i>,
    bundle: &'ctx ScriptBundle<'i>,
    code: CodeIter<'scope, Offset>,
    control_flow: &'scope ControlFlowBlock,
    liveness: HashMap<LocalIndex, Bounds>,
    declared: HashSet<Declaration>,
    verbosity: Verbosity,
}

impl<'scope, 'ctx: 'i, 'i> Decompiler<'scope, 'ctx, 'i> {
    fn new(
        function: &'ctx Function<'i>,
        code: &'scope [Instr],
        control_flow: &'scope ControlFlowBlock,
        bundle: &'ctx ScriptBundle<'i>,
        verbosity: Verbosity,
    ) -> Self {
        Self {
            function,
            code: CodeIter::new(code),
            control_flow,
            bundle,
            liveness: calculate_liveness(code),
            declared: HashSet::new(),
            verbosity,
        }
    }

    fn position(&self) -> Location {
        self.code.virtual_offset().into()
    }

    fn consume_instr(&mut self) -> Result<&Instr> {
        self.code.next().ok_or(Error::UnexpectedEndOfCode)
    }

    fn consume_block(
        &mut self,
        cf_block: &ControlFlowBlock,
        end: Location,
    ) -> Result<ast::Block<'i>> {
        let mut stmts = vec![];
        while self.position() < end {
            match self.consume_stmt(cf_block, end)? {
                Some(stmt @ ast::Stmt::Return(_)) => {
                    stmts.push(stmt);
                    // The rest of this region is unreachable, but may include a
                    // structural jump before the next branch or switch label.
                    if end != Location::MAX {
                        while self.position() < end && self.code.next().is_some() {}
                    }
                    break;
                }
                Some(stmt) => {
                    stmts.push(stmt);
                }
                None => break,
            }
        }
        Ok(ast::Block::new(stmts))
    }

    fn consume_stmt(
        &mut self,
        cf_block: &ControlFlowBlock,
        end: Location,
    ) -> Result<Option<ast::Stmt<'i>>> {
        if self.position() >= end {
            return Ok(None);
        }
        let Some(instr) = self.code.clone().next() else {
            return Ok(None);
        };

        let stmt = match instr {
            Instr::Switch(_) => {
                let mut cases = Vec::new();
                let mut default = None;

                let mut switch_exit = None;
                let mut case_exit: Option<(Location, Location)> = None;

                self.consume_instr()?;
                let scrutinee = self.consume_expr()?.into_expr().into();

                while self.position() < end {
                    match (self.code.clone().next(), switch_exit) {
                        (Some(Instr::SwitchLabel(label)), _) => {
                            let pos = self.position();
                            let body = pos + label.body();
                            let next_case = pos + label.next_case();

                            let exit = case_exit
                                .as_ref()
                                .and_then(|&(b, e)| (b == body).then_some(e))
                                .unwrap_or_default()
                                .max(next_case);
                            case_exit = Some((body, exit));

                            self.consume_instr()?;
                            let label = self.consume_expr()?.into_expr();

                            let nested = cf_block.get_block(Bounds::new(body, next_case));
                            // keep track of where the switch breaks out to in order to be able
                            // to determine where the final case ends
                            if let Some(e) = nested.exit()
                                && e > next_case
                            {
                                switch_exit = Some(e);
                            }

                            let block = self.consume_block(nested, exit)?;
                            cases.push(ast::Case::new(ast::Condition::Expr(label), block.stmts));
                        }
                        (Some(Instr::SwitchDefault), Some(exit)) => {
                            self.consume_instr()?;
                            let block = self.consume_block(cf_block, exit)?;
                            if !block.stmts.is_empty() {
                                default = Some(block.stmts);
                            }
                            break;
                        }
                        (Some(Instr::SwitchDefault), None) => {
                            self.consume_instr()?;
                            break;
                        }
                        _ => break,
                    }
                }

                ast::Stmt::Switch {
                    expr: scrutinee,
                    cases: cases.into(),
                    default,
                }
            }
            Instr::SwitchLabel(_) | Instr::SwitchDefault => return Ok(None),
            Instr::Jump(jump) => {
                let offset = self.position();
                let target = offset + jump.target();

                self.consume_instr()?;
                if i32::from(jump.target()) == instr.virtual_size() as i32 {
                    // no-op jump
                    return self.consume_stmt(cf_block, end);
                }

                match cf_block.type_() {
                    // end of loop
                    BlockType::While if cf_block.exit() == Some(offset) => return Ok(None),
                    // break
                    BlockType::While | BlockType::Case if cf_block.exit() == Some(target) => {
                        ast::Stmt::Break
                    }
                    // continue
                    BlockType::While if cf_block.entry() == Some(target) => ast::Stmt::Continue,
                    // exit if
                    BlockType::Conditional => return Ok(None),
                    _ => ast::Stmt::Break,
                }
            }
            Instr::JumpIfFalse(jump) => {
                let start = self.position();
                let end = start + jump.target();
                let nested = cf_block.get_block(Bounds::new(start, end));

                if matches!(nested.type_(), BlockType::While) {
                    self.consume_instr()?;
                    let cond = self.consume_expr()?.into_expr();
                    let body = self.consume_block(nested, end)?;
                    return Ok(Some(ast::Stmt::While(
                        ast::ConditionalBlock::new(ast::LetCondition::Expr(cond), body).into(),
                    )));
                }

                let mut blocks = vec![];
                let mut else_ = None;
                loop {
                    let if_start = self.position();
                    match (self.code.clone().next(), nested.exit()) {
                        (Some(Instr::JumpIfFalse(jump)), exit) => {
                            let if_end = if_start + jump.target();
                            let nested = nested.get_block(Bounds::new(if_start, if_end));
                            if nested.type_() != BlockType::Conditional || nested.exit() != exit {
                                if blocks.is_empty() {
                                    return Ok(None);
                                }
                                break;
                            }
                            self.consume_instr()?;
                            let cond = self.consume_expr()?.into_expr();
                            let then = self.consume_block(nested, if_end)?;
                            blocks.push(ast::ConditionalBlock::new(
                                ast::LetCondition::Expr(cond),
                                then,
                            ));
                        }
                        (_, Some(exit)) => {
                            let nested = nested.get_block(Bounds::new(if_start, exit));
                            if nested.type_() != BlockType::Conditional
                                || nested.exit() != Some(exit)
                            {
                                break;
                            }

                            let block = self.consume_block(nested, exit)?;
                            if !block.stmts.is_empty() {
                                else_ = Some(block);
                            }
                            break;
                        }
                        _ => break,
                    }
                    if nested.exit().is_none() || nested.exit() == Some(self.position()) {
                        break;
                    }
                }

                ast::Stmt::If {
                    blocks: blocks.into(),
                    else_,
                }
            }
            Instr::Return => {
                self.consume_instr()?;
                if matches!(self.code.clone().next(), Some(Instr::Nop)) {
                    self.consume_instr()?;
                    ast::Stmt::Return(None)
                } else {
                    ast::Stmt::Return(Some(self.consume_expr()?.into_expr().into()))
                }
            }
            Instr::Nop => {
                self.consume_instr()?;
                return Ok(None);
            }
            _ => self.consume_expr()?.into_stmt(),
        };
        Ok(Some(stmt))
    }

    #[inline]
    fn consume_expr(&mut self) -> Result<Node<'i>> {
        self.consume_expr_with(None)
    }

    fn consume_expr_with(&mut self, ctx: Option<ast::Expr<'i>>) -> Result<Node<'i>> {
        let verbose = self.verbosity == Verbosity::Verbose;
        let expr = match self.consume_instr()? {
            Instr::Null | Instr::WeakRefNull => ast::Expr::Null,
            Instr::I32One => ast::Expr::Constant(ast::Constant::I32(1)),
            Instr::I32Zero => ast::Expr::Constant(ast::Constant::I32(0)),
            &Instr::I8Const(val) => ast::Expr::Constant(ast::Constant::I32(val.into())),
            &Instr::I16Const(val) => ast::Expr::Constant(ast::Constant::I32(val.into())),
            &Instr::I32Const(val) => ast::Expr::Constant(ast::Constant::I32(val)),
            &Instr::I64Const(val) => ast::Expr::Constant(ast::Constant::I64(val)),
            &Instr::U8Const(val) => ast::Expr::Constant(ast::Constant::U32(val.into())),
            &Instr::U16Const(val) => ast::Expr::Constant(ast::Constant::U32(val.into())),
            &Instr::U32Const(val) => ast::Expr::Constant(ast::Constant::U32(val)),
            &Instr::U64Const(val) => ast::Expr::Constant(ast::Constant::U64(val)),
            &Instr::F32Const(val) => ast::Expr::Constant(ast::Constant::F32(val)),
            &Instr::F64Const(val) => ast::Expr::Constant(ast::Constant::F64(val)),
            &Instr::CNameConst(idx) => {
                let str = expect_borrowed(self.bundle.try_get_item(idx)?);
                ast::Expr::Constant(ast::Constant::CName(str.into()))
            }
            &Instr::StringConst(idx) => {
                let str = expect_borrowed(self.bundle.try_get_item(idx)?);
                ast::Expr::Constant(ast::Constant::String(str.into()))
            }
            &Instr::TweakDbIdConst(idx) => {
                let str = expect_borrowed(self.bundle.try_get_item(idx)?);
                ast::Expr::Constant(ast::Constant::TweakDbId(str.into()))
            }
            &Instr::ResourceConst(idx) => {
                let str = expect_borrowed(self.bundle.try_get_item(idx)?);
                ast::Expr::Constant(ast::Constant::Resource(str.into()))
            }
            Instr::TrueConst => ast::Expr::Constant(ast::Constant::Bool(true)),
            Instr::FalseConst => ast::Expr::Constant(ast::Constant::Bool(false)),
            Instr::Assign => {
                if let Some(&Instr::Local(local_idx)) = self.code.clone().next() {
                    let pos = self.position();
                    let (block_bounds, _) = self.control_flow.get_containing(pos);
                    let local_bounds = &self.liveness[&local_idx];

                    if local_bounds.start() == pos && local_bounds.end() < block_bounds.end() {
                        self.consume_instr()?;

                        let id = Declaration::new(local_idx);
                        self.declared.insert(id.clone());

                        let local = self.bundle.try_get_item(local_idx)?;
                        let name = self.bundle.try_get_item_hint(local.name(), "local name")?;
                        let typ = extract_type(local.type_(), self.bundle)?;
                        let value = self.consume_expr()?.into_expr();

                        return Ok(Node::Decl {
                            id,
                            name,
                            typ,
                            value,
                        });
                    }
                    ast::Expr::Assign {
                        lhs: self.consume_expr()?.into_expr().into(),
                        rhs: self.consume_expr()?.into_expr().into(),
                    }
                } else {
                    ast::Expr::Assign {
                        lhs: self.consume_expr()?.into_expr().into(),
                        rhs: self.consume_expr()?.into_expr().into(),
                    }
                }
            }
            &Instr::Local(idx) => {
                let local = self.bundle.try_get_item(idx)?;
                ast::Expr::Ident(self.bundle.try_get_item_hint(local.name(), "local name")?)
            }
            &Instr::Param(idx) => {
                let param = self.bundle.try_get_item(idx)?;
                ast::Expr::Ident(self.bundle.try_get_item_hint(param.name(), "param name")?)
            }
            &Instr::EnumConst { enum_, value } => {
                let enum_ = self.bundle.try_get_item(enum_)?;
                let member = self.bundle.try_get_item(value)?;
                ast::Expr::Member {
                    expr: ast::Expr::Ident(
                        self.bundle.try_get_item_hint(enum_.name(), "enum name")?,
                    )
                    .into(),
                    member: self
                        .bundle
                        .try_get_item_hint(member.name(), "enum member name")?,
                }
            }
            &Instr::ObjectField(field) => {
                let field = self.bundle.try_get_item(field)?;
                ast::Expr::Member {
                    expr: ctx.unwrap_or(ast::Expr::This).into(),
                    member: self.bundle.try_get_item_hint(field.name(), "field name")?,
                }
            }
            &Instr::StructField(field) => {
                let field = self.bundle.try_get_item(field)?;
                ast::Expr::Member {
                    expr: self.consume_expr()?.into_expr().into(),
                    member: self.bundle.try_get_item_hint(field.name(), "field name")?,
                }
            }
            Instr::Conditional(_) => ast::Expr::Conditional {
                cond: self.consume_expr()?.into_expr().into(),
                then: self.consume_expr()?.into_expr().into(),
                else_: self.consume_expr()?.into_expr().into(),
            },
            &Instr::Construct { arg_count, class } => {
                let class = self.bundle.try_get_item(class)?;
                let args = (0..arg_count)
                    .map(|_| Ok(self.consume_expr()?.into_expr()))
                    .collect::<Result<_>>()?;
                ast::Expr::New {
                    typ: ast::Type::plain(
                        self.bundle.try_get_item_hint(class.name(), "class name")?,
                    )
                    .into(),
                    args,
                }
            }
            &Instr::New(class) => {
                let class = self.bundle.try_get_item(class)?;
                ast::Expr::New {
                    typ: ast::Type::plain(
                        self.bundle.try_get_item_hint(class.name(), "class name")?,
                    )
                    .into(),
                    args: [].into(),
                }
            }
            &Instr::InvokeStatic { function, .. } => {
                let function = self.bundle.try_get_item(function)?;
                let mangled_name = self
                    .bundle
                    .try_get_item_hint(function.name(), "function name")?;
                let name = extract_mangled_name(mangled_name);
                let receiver = if matches!(ctx, None | Some(ast::Expr::This))
                    && self.is_base_method(function)?
                {
                    ast::Expr::Member {
                        expr: ast::Expr::Super.into(),
                        member: name,
                    }
                } else if let Some(ctx) = ctx {
                    ast::Expr::Member {
                        expr: ctx.into(),
                        member: name,
                    }
                } else if function.flags().is_static() {
                    if let Some(class) = function.class() {
                        let class = self.bundle.try_get_item(class)?;
                        let cls_name = self.bundle.try_get_item_hint(class.name(), "class name")?;
                        ast::Expr::Member {
                            expr: ast::Expr::Ident(cls_name).into(),
                            member: name,
                        }
                    } else if mangled_name.starts_with("Cast;") {
                        let typ = function.return_type().ok_or(Error::VoidCast)?;
                        return Ok(Node::Expr(ast::Expr::Call {
                            expr: ast::Expr::Ident("Cast").into(),
                            type_args: [extract_type(typ, self.bundle)?].into(),
                            args: self.consume_args()?.into(),
                        }));
                    } else if let Some(op) = ast::BinOp::from_name(name) {
                        let mut args = self.consume_args()?;
                        let rhs = args.pop().ok_or(Error::InvalidOperatorArgs)?.into();
                        let lhs = args.pop().ok_or(Error::InvalidOperatorArgs)?.into();
                        if !args.is_empty() {
                            return Err(Error::InvalidOperatorArgs);
                        }
                        return Ok(Node::Expr(ast::Expr::BinOp { lhs, op, rhs }));
                    } else if let Some(op) = ast::UnOp::from_name(name) {
                        let mut args = self.consume_args()?;
                        let expr = args.pop().ok_or(Error::InvalidOperatorArgs)?.into();
                        if !args.is_empty() {
                            return Err(Error::InvalidOperatorArgs);
                        }
                        return Ok(Node::Expr(ast::Expr::UnOp { op, expr }));
                    } else {
                        ast::Expr::Ident(name)
                    }
                } else {
                    ast::Expr::Member {
                        expr: ast::Expr::This.into(),
                        member: name,
                    }
                };
                ast::Expr::Call {
                    expr: receiver.into(),
                    type_args: [].into(),
                    args: self.consume_args()?.into(),
                }
            }
            &Instr::InvokeVirtual { function, .. } => {
                let mangled_name = self.bundle.try_get_item_hint(function, "function name")?;
                let name = extract_mangled_name(mangled_name);
                let receiver = if let Some(ctx) = ctx {
                    ctx
                } else {
                    ast::Expr::This
                };
                ast::Expr::Call {
                    expr: ast::Expr::Member {
                        expr: receiver.into(),
                        member: name,
                    }
                    .into(),
                    type_args: [].into(),
                    args: self.consume_args()?.into(),
                }
            }
            Instr::Context(_) => {
                let ctx = self.consume_expr()?.into_expr();
                return self.consume_expr_with(Some(ctx));
            }
            Instr::Equals(_)
            | Instr::RefStringEqualsString(_)
            | Instr::StringEqualsRefString(_) => self.consume_intrinsic("Equals", 2)?,
            Instr::NotEquals(_)
            | Instr::RefStringNotEqualsString(_)
            | Instr::StringNotEqualsRefString(_) => self.consume_intrinsic("NotEquals", 2)?,
            Instr::Delete => self.consume_intrinsic("Delete", 1)?,
            Instr::This => ast::Expr::This,
            Instr::ArrayClear(_) => self.consume_intrinsic("ArrayClear", 1)?,
            Instr::ArraySize(_) | Instr::StaticArraySize(_) => {
                self.consume_intrinsic("ArraySize", 1)?
            }
            Instr::ArrayResize(_) => self.consume_intrinsic("ArrayResize", 2)?,
            Instr::ArrayFindFirst(_)
            | Instr::ArrayFindFirstFast(_)
            | Instr::StaticArrayFindFirst(_)
            | Instr::StaticArrayFindFirstFast(_) => self.consume_intrinsic("ArrayFindFirst", 2)?,
            Instr::ArrayFindLast(_)
            | Instr::ArrayFindLastFast(_)
            | Instr::StaticArrayFindLast(_)
            | Instr::StaticArrayFindLastFast(_) => self.consume_intrinsic("ArrayFindLast", 2)?,
            Instr::ArrayContains(_)
            | Instr::ArrayContainsFast(_)
            | Instr::StaticArrayContains(_)
            | Instr::StaticArrayContainsFast(_) => self.consume_intrinsic("ArrayContains", 2)?,
            Instr::ArrayCount(_)
            | Instr::ArrayCountFast(_)
            | Instr::StaticArrayCount(_)
            | Instr::StaticArrayCountFast(_) => self.consume_intrinsic("ArrayCount", 2)?,
            Instr::ArrayPush(_) => self.consume_intrinsic("ArrayPush", 2)?,
            Instr::ArrayPop(_) => self.consume_intrinsic("ArrayPop", 1)?,
            Instr::ArrayInsert(_) => self.consume_intrinsic("ArrayInsert", 3)?,
            Instr::ArrayRemove(_) | Instr::ArrayRemoveFast(_) => {
                self.consume_intrinsic("ArrayRemove", 2)?
            }
            Instr::ArrayGrow(_) => self.consume_intrinsic("ArrayGrow", 2)?,
            Instr::ArrayErase(_) | Instr::ArrayEraseFast(_) => {
                self.consume_intrinsic("ArrayErase", 2)?
            }
            Instr::ArrayLast(_) | Instr::StaticArrayLast(_) => {
                self.consume_intrinsic("ArrayLast", 1)?
            }
            Instr::ArrayElement(_) | Instr::StaticArrayElement(_) => ast::Expr::Index {
                expr: self.consume_expr()?.into_expr().into(),
                index: self.consume_expr()?.into_expr().into(),
            },
            Instr::ArraySort(_) => self.consume_intrinsic("ArraySort", 1)?,
            Instr::ArraySortByPredicate(_) => self.consume_intrinsic("ArraySortByPredicate", 2)?,
            Instr::RefToBool | Instr::WeakRefToBool | Instr::VariantIsDefined => {
                self.consume_intrinsic("IsDefined", 1)?
            }
            Instr::EnumToI32 { .. } => self.consume_intrinsic("EnumInt", 1)?,
            &Instr::I32ToEnum { enum_type, .. } => {
                let enum_ = self.bundle.try_get_item(enum_type)?;
                let type_args = vec![ast::Type::plain(
                    self.bundle.try_get_item_hint(enum_.name(), "enum name")?,
                )];
                self.consume_intrinsic_with_type_args("IntEnum", 1, type_args)?
            }
            &Instr::DynamicCast { class, .. } => {
                let class = self.bundle.try_get_item(class)?;
                ast::Expr::DynCast {
                    expr: self.consume_expr()?.into_expr().into(),
                    typ: ast::Type::plain(
                        self.bundle.try_get_item_hint(class.name(), "class name")?,
                    )
                    .into(),
                }
            }
            Instr::ToString(_) | Instr::VariantToString => self.consume_intrinsic("ToString", 1)?,
            Instr::ToVariant(_) => self.consume_intrinsic("ToVariant", 1)?,
            &Instr::FromVariant(typ) => {
                let type_args = vec![extract_type(typ, self.bundle)?];
                self.consume_intrinsic_with_type_args("FromVariant", 1, type_args)?
            }
            Instr::VariantIsRef => self.consume_intrinsic("VariantIsRef", 1)?,
            Instr::VariantIsArray => self.consume_intrinsic("VariantIsArray", 1)?,
            Instr::VariantTypeName => self.consume_intrinsic("VariantTypeName", 1)?,
            Instr::WeakRefToRef if verbose => self.consume_intrinsic("WeakRefToRef", 1)?,
            Instr::RefToWeakRef if verbose => self.consume_intrinsic("RefToWeakRef", 1)?,
            Instr::AsRef(_) if verbose => self.consume_intrinsic("AsRef", 1)?,
            Instr::Deref(_) if verbose => self.consume_intrinsic("Deref", 1)?,
            Instr::WeakRefToRef | Instr::RefToWeakRef | Instr::AsRef(_) | Instr::Deref(_) => {
                return self.consume_expr();
            }
            instr => todo!("{instr:?}"),
        };
        Ok(Node::Expr(expr))
    }

    fn is_base_method(&self, function: &Function<'_>) -> Result<bool> {
        if function.flags().is_static() || function.class().is_none() {
            return Ok(false);
        }
        let Some(class) = self.function.class() else {
            return Ok(false);
        };
        let mut base = self.bundle.try_get_item(class)?.base();
        let mut visited = HashSet::new();
        while let Some(class) = base {
            if !visited.insert(class) {
                break;
            }
            if Some(class) == function.class() {
                return Ok(true);
            }
            base = self.bundle.try_get_item(class)?.base();
        }
        Ok(false)
    }

    fn consume_args(&mut self) -> Result<Vec<ast::Expr<'i>>> {
        let mut args = vec![];
        loop {
            while matches!(self.code.clone().next(), Some(Instr::Skip(_) | Instr::Nop)) {
                self.code.next();
            }
            if matches!(self.code.clone().next(), Some(Instr::ParamEnd)) {
                break;
            }
            args.push(self.consume_expr()?.into_expr());
        }
        if self.code.clone().next() == Some(&Instr::ParamEnd) {
            self.code.next();
        } else {
            return Err(Error::MissingParamEnd);
        }
        Ok(args)
    }

    #[inline]
    fn consume_intrinsic(&mut self, name: &'static str, arg_count: usize) -> Result<ast::Expr<'i>> {
        self.consume_intrinsic_with_type_args(name, arg_count, vec![])
    }

    fn consume_intrinsic_with_type_args(
        &mut self,
        name: &'static str,
        arg_count: usize,
        type_args: Vec<ast::Type<'i>>,
    ) -> Result<ast::Expr<'i>> {
        let args = (0..arg_count)
            .map(|_| Ok(self.consume_expr()?.into_expr()))
            .collect::<Result<_>>()?;
        Ok(ast::Expr::Call {
            expr: ast::Expr::Ident(name).into(),
            type_args: type_args.into(),
            args,
        })
    }

    /// Consumes the decompiler and generates a prelude with declarations that need to be hoisted
    /// to the top of the function.
    fn into_prelude(mut self) -> Result<Vec<ast::Stmt<'i>>> {
        self.declared.retain(|decl| decl.statement.get());

        let mut stmts = vec![];
        for &local in self.function.locals() {
            if !self.declared.contains(&Declaration::new(local)) {
                let local = self.bundle.try_get_item(local)?;
                let name = self.bundle.try_get_item_hint(local.name(), "local name")?;

                stmts.push(ast::Stmt::Let {
                    name: expect_borrowed(name),
                    typ: Some(extract_type(local.type_(), self.bundle)?.into()),
                    value: None,
                });
            }
        }

        Ok(stmts)
    }
}

#[derive(Debug)]
enum Node<'i> {
    Expr(ast::Expr<'i>),
    Decl {
        name: &'i str,
        typ: ast::Type<'i>,
        value: ast::Expr<'i>,
        id: Declaration,
    },
}

impl<'i> Node<'i> {
    fn into_expr(self) -> ast::Expr<'i> {
        match self {
            Node::Expr(expr) => expr,
            Node::Decl { name, value, .. } => ast::Expr::Assign {
                lhs: ast::Expr::Ident(name).into(),
                rhs: value.into(),
            },
        }
    }

    fn into_stmt(self) -> ast::Stmt<'i> {
        match self {
            Node::Expr(expr) => ast::Stmt::Expr(expr.into()),
            Node::Decl {
                name,
                typ,
                value,
                id,
            } => {
                id.statement.replace(true);
                ast::Stmt::Let {
                    name,
                    typ: Some(typ.into()),
                    value: Some(value.into()),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Declaration {
    local: LocalIndex,
    statement: Rc<Cell<bool>>,
}

impl Declaration {
    fn new(local: LocalIndex) -> Self {
        Self {
            local,
            statement: Rc::new(Cell::new(false)),
        }
    }
}

impl PartialEq for Declaration {
    fn eq(&self, other: &Self) -> bool {
        self.local == other.local
    }
}

impl Eq for Declaration {}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.local.hash(state);
    }
}

fn calculate_liveness(code: &[Instr]) -> HashMap<LocalIndex, Bounds> {
    let mut liveness = HashMap::new();
    for (offset, instr) in CodeIter::new(code).with_offsets() {
        if let &Instr::Local(local) = instr {
            let loc = Location::from(offset);
            let bounds = liveness.entry(local).or_insert(Bounds::new(loc, loc));
            *bounds = Bounds::new(bounds.start().min(loc), bounds.end().max(loc));
        }
    }

    liveness
}

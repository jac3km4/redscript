use bon::{bon, builder};
use closure::{CALL_METHOD, emit_closure};
use redscript_compiler_frontend::ast::Span;
use redscript_compiler_frontend::utils::ScopedMap;
use redscript_compiler_frontend::{
    CoalesceError, MonoType, RefType, Symbols, TypeSchema, ir, predef,
};
use redscript_io::{
    Conditional, FieldIndex as PoolFieldIndex, FunctionIndex as PoolFunctionIndex, Instr,
    InvokeFlags, Jump, Local as PoolLocal, LocalFlags as PoolLocalFlags,
    LocalIndex as PoolLocalIndex, Offset, ParameterIndex as PoolParameterIndex, ScriptBundle,
    Switch, SwitchLabel, TypeIndex as PoolTypeIndex, byte,
};
use slab::Slab;
use thiserror::Error;

use crate::inputs::Signature;
use crate::monomorph::{MethodWithReceiver, Monomorphizer};

mod closure;

#[builder(finish_fn = build)]
pub fn block_builder<'ctx>(
    function_index: PoolFunctionIndex,
    locals: impl IntoIterator<Item = ir::LocalInfo<'ctx>>,
    #[builder(default = vec![])] captures: Vec<(ir::Local, PoolFieldIndex)>,
    block: &ir::Block<'ctx>,
    symbols: &Symbols<'ctx>,
    type_env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
    bundle: &mut ScriptBundle<'ctx>,
    monomorphizer: &mut Monomorphizer<'ctx>,
) -> Result<Vec<Instr>, AssembleError<'ctx>> {
    let mut assembler = Assembler::builder()
        .function_index(function_index)
        .locals(locals)
        .captures(captures)
        .symbols(symbols)
        .type_env(type_env)
        .bundle(bundle)
        .monomorphizer(monomorphizer)
        .build()?;
    assembler.assemble_block(block, None)?;
    assembler.into_code()
}

struct Assembler<'scope, 'ctx> {
    code: Vec<Instr<Label>>,
    locals: Vec<(ir::Local, PoolLocalIndex)>,
    params: Vec<(ir::Local, PoolParameterIndex)>,
    captures: Vec<(ir::Local, PoolFieldIndex)>,
    has_return_value: bool,

    labels: Slab<u32>,
    virtual_offset: u32,

    symbols: &'scope Symbols<'ctx>,
    type_env: &'scope ScopedMap<'scope, &'ctx str, MonoType<'ctx>>,
    bundle: &'scope mut ScriptBundle<'ctx>,
    monomorph: &'scope mut Monomorphizer<'ctx>,
}

#[bon]
impl<'scope, 'ctx> Assembler<'scope, 'ctx> {
    #[builder]
    fn new(
        function_index: PoolFunctionIndex,
        locals: impl IntoIterator<Item = ir::LocalInfo<'ctx>>,
        mut captures: Vec<(ir::Local, PoolFieldIndex)>,
        symbols: &'scope Symbols<'ctx>,
        type_env: &'scope ScopedMap<'scope, &'ctx str, MonoType<'ctx>>,
        bundle: &'scope mut ScriptBundle<'ctx>,
        monomorphizer: &'scope mut Monomorphizer<'ctx>,
    ) -> Result<Self, AssembleError<'ctx>> {
        let mut local_indices = vec![];
        let mut param_indices = vec![];

        for local in locals {
            match local.id {
                ir::Local::Var(i) => {
                    let name = bundle
                        .cnames_mut()
                        .add(format!("{}${i}", local.name.unwrap_or("var")));
                    let typ = local
                        .typ
                        .coalesce(symbols)
                        .map_err(|e| {
                            AssembleError::Coalesce(
                                e.into(),
                                local.span.expect("non-spanned local should coalesce"),
                            )
                        })?
                        .assume_mono(type_env);
                    let typ = monomorphizer.type_(&typ, symbols, bundle);
                    let flags = PoolLocalFlags::default();
                    let idx = bundle.define(PoolLocal::new(name, function_index, typ, flags));
                    local_indices.push((local.id, idx));
                }
                ir::Local::Param(_) => {
                    let idx = bundle[function_index].parameters()[param_indices.len()];
                    param_indices.push((local.id, idx));
                }
                ir::Local::This => {}
            }
        }

        local_indices.sort_by_key(|&(l, _)| l);

        let locals = local_indices
            .iter()
            .map(|&(_, idx)| idx)
            .collect::<Vec<_>>();
        bundle[function_index].set_locals(locals);

        captures.sort_by_key(|&(l, _)| l);

        Ok(Self {
            code: Vec::new(),
            locals: local_indices,
            params: param_indices,
            captures,
            has_return_value: bundle[function_index].return_type().is_some(),

            labels: Slab::new(),
            virtual_offset: 0,

            symbols,
            bundle,
            monomorph: monomorphizer,
            type_env,
        })
    }

    #[inline]
    fn emit(&mut self, instr: Instr<Label>) {
        self.virtual_offset += u32::from(instr.virtual_size());
        self.code.push(instr);
    }

    fn new_label(&mut self) -> Label {
        let idx = self
            .labels
            .insert(u32::MAX)
            .try_into()
            .expect("label count should not exceed u16::MAX");
        Label(idx)
    }

    #[inline]
    fn mark_label(&mut self, label: Label) {
        self.labels[usize::from(label)] = self.virtual_offset;
    }

    fn assemble_block(
        &mut self,
        block: &ir::Block<'ctx>,
        span: Option<BlockSpan>,
    ) -> Result<(), AssembleError<'ctx>> {
        block
            .stmts
            .iter()
            .try_for_each(|stmt| self.assemble(stmt, span))
    }

    fn assemble(
        &mut self,
        stmt: &ir::Stmt<'ctx>,
        block_span: Option<BlockSpan>,
    ) -> Result<(), AssembleError<'ctx>> {
        match stmt {
            ir::Stmt::Expr(expr) => {
                self.assemble_expr(expr)?;
            }
            ir::Stmt::Block(block, _) => {
                self.assemble_block(block, block_span)?;
            }
            ir::Stmt::While(block, _) => {
                let start = self.new_label();
                let end = self.new_label();

                self.mark_label(start);
                self.emit(Instr::JumpIfFalse(Jump::new(end)));
                self.assemble_expr(&block.condition)?;
                self.assemble_block(
                    &block.block,
                    Some(BlockSpan {
                        continue_target: Some(start),
                        break_target: end,
                    }),
                )?;
                self.emit(Instr::Jump(Jump::new(start)));
                self.mark_label(end);
            }
            ir::Stmt::Branches {
                branches, default, ..
            } => {
                let mut next_branch = self.new_label();
                let exit = self.new_label();

                for branch in &**branches {
                    self.emit(Instr::JumpIfFalse(Jump::new(next_branch)));
                    self.assemble_expr(&branch.condition)?;
                    self.assemble_block(&branch.block, block_span)?;
                    self.emit(Instr::Jump(Jump::new(exit)));
                    self.mark_label(next_branch);
                    next_branch = self.new_label();
                }
                if let Some(default) = default {
                    self.assemble_block(default, block_span)?;
                }
                self.mark_label(exit);
            }
            ir::Stmt::Switch {
                scrutinee,
                scrutinee_type,
                branches,
                default,
                span,
            } => {
                let end = self.new_label();
                let switch_span = Some(BlockSpan {
                    continue_target: block_span.and_then(|span| span.continue_target),
                    break_target: end,
                });
                let mut next_label = self.new_label();

                let typ = self.mono_type(scrutinee_type, *span)?;
                let typ = self.monomorph.type_(&typ, self.symbols, self.bundle);

                self.emit(Instr::Switch(Switch::new(typ, next_label)));
                self.assemble_expr(scrutinee)?;

                let mut case_iter = branches.iter().peekable();
                while case_iter.peek().is_some() {
                    let body_label = self.new_label();

                    for case in case_iter.by_ref() {
                        self.mark_label(next_label);
                        next_label = self.new_label();

                        let switch_label = SwitchLabel::new(next_label, body_label);
                        self.emit(Instr::SwitchLabel(switch_label));
                        self.assemble_expr(&case.matcher)?;

                        self.mark_label(body_label);
                        if !case.block.stmts.is_empty() {
                            self.assemble_block(&case.block, switch_span)?;
                            break;
                        }
                    }
                }
                self.mark_label(next_label);

                if let Some(default) = default {
                    self.emit(Instr::SwitchDefault);
                    self.assemble_block(default, switch_span)?;
                }
                self.mark_label(end);
            }
            ir::Stmt::InitArray {
                local,
                elements,
                element_type,
                span,
            } => {
                if elements.is_empty() {
                    return Ok(());
                }

                let local = self
                    .resolve_local(*local)
                    .expect("every local should be defined");
                let element_t = self.mono_type(element_type, *span)?;
                let array_t = MonoType::new(predef::ARRAY, [element_t]);
                let array_t = self.monomorph.type_(&array_t, self.symbols, self.bundle);

                self.emit(Instr::ArrayResize(array_t));
                self.emit(Instr::Local(local));
                self.emit(Instr::U64Const(elements.len() as u64));

                for (i, element) in elements.iter().enumerate() {
                    self.emit(Instr::Assign);
                    self.emit(Instr::ArrayElement(array_t));
                    self.emit(Instr::Local(local));
                    self.emit(Instr::U64Const(i as u64));

                    self.assemble_expr(element)?;
                }
            }
            ir::Stmt::InitDefault { local, typ, span } => {
                let local = self
                    .resolve_local(*local)
                    .expect("every local should be defined");
                let typ = self.mono_type(typ, *span)?;

                if typ.id() == predef::ARRAY {
                    let array_t = self.monomorph.type_(&typ, self.symbols, self.bundle);
                    self.emit(Instr::ArrayClear(array_t));
                    self.emit(Instr::Local(local));
                } else {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(local));
                    self.emit_default_instr(typ)?;
                }
            }
            ir::Stmt::Break(span) => match block_span {
                Some(BlockSpan { break_target, .. }) => {
                    self.emit(Instr::Jump(Jump::new(break_target)));
                }
                _ => {
                    return Err(AssembleError::InvalidControlFlow(
                        "break",
                        "a loop or switch is required",
                        *span,
                    ));
                }
            },
            ir::Stmt::Continue(span) => {
                if let Some(BlockSpan {
                    continue_target: Some(start),
                    ..
                }) = block_span
                {
                    self.emit(Instr::Jump(Jump::new(start)));
                } else {
                    return Err(AssembleError::InvalidControlFlow(
                        "continue",
                        "a loop is required",
                        *span,
                    ));
                }
            }
            ir::Stmt::Return(expr, _) if self.has_return_value => {
                self.emit(Instr::Return);
                if let Some(expr) = expr {
                    self.assemble_expr(expr)?;
                } else {
                    self.emit(Instr::Nop);
                }
            }
            ir::Stmt::Return(expr, _) => {
                if let Some(expr) = expr {
                    self.assemble_expr(expr)?;
                }
                self.emit(Instr::Return);
                self.emit(Instr::Nop);
            }
        };
        Ok(())
    }

    fn assemble_expr(&mut self, expr: &ir::Expr<'ctx>) -> Result<(), AssembleError<'ctx>> {
        match expr {
            ir::Expr::Call { call, span } => self.assemble_call(call, *span)?,
            ir::Expr::Const(const_, _) => self.assemble_const(const_)?,
            ir::Expr::Null { is_weak: false, .. } => self.emit(Instr::Null),
            ir::Expr::Null { is_weak: true, .. } => self.emit(Instr::WeakRefNull),
            ir::Expr::NewInstance { class_type, span } => {
                let class_t = self.mono_type_app(class_type, *span)?;
                let class_t = self.monomorph.class(&class_t, self.symbols, self.bundle);
                self.emit(Instr::New(class_t));
            }
            ir::Expr::Construct {
                args,
                struct_type,
                span,
            } => {
                let struct_t = self.mono_type_app(struct_type, *span)?;
                let struct_t = self.monomorph.class(&struct_t, self.symbols, self.bundle);
                self.emit(Instr::Construct {
                    arg_count: u8::try_from(args.len())
                        .map_err(|_| AssembleError::TooManyArguments(args.len(), *span))?,
                    class: struct_t,
                });
                for arg in args {
                    self.assemble_expr(arg)?;
                }
            }
            ir::Expr::NewClosure { closure, span } => {
                emit_closure(self, closure, *span)?;
            }
            ir::Expr::Assign { place, expr, .. } => {
                self.emit(Instr::Assign);
                self.assemble_expr(place)?;
                self.assemble_expr(expr)?;
            }
            ir::Expr::Field {
                receiver,
                receiver_type,
                receiver_ref,
                field,
                span,
            } => {
                let receiver_t = self.mono_type_app(receiver_type, *span)?;
                let cls_id = receiver_t.id();
                let class = self.monomorph.class(&receiver_t, self.symbols, self.bundle);
                let field = self.bundle[class].fields()[usize::from(field.index())];
                match self.symbols[cls_id].schema() {
                    TypeSchema::Aggregate(agg) if agg.flags().is_struct() => {
                        self.emit(Instr::StructField(field));
                        self.assemble_expr(receiver)?;
                    }
                    _ => {
                        let exit = self.new_label();
                        self.emit(Instr::Context(Jump::new(exit)));
                        if matches!(receiver_ref, Some(RefType::Weak)) {
                            self.emit(Instr::WeakRefToRef);
                        }
                        self.assemble_expr(receiver)?;
                        self.emit(Instr::ObjectField(field));
                        self.mark_label(exit);
                    }
                }
            }
            ir::Expr::Index {
                array,
                array_type,
                index,
                span,
            } => {
                let array_t = self.mono_type(array_type, *span)?;
                let is_dynarray = array_t.id() == predef::ARRAY;
                let array_t = self.monomorph.type_(&array_t, self.symbols, self.bundle);
                if is_dynarray {
                    self.emit(Instr::ArrayElement(array_t));
                } else {
                    self.emit(Instr::StaticArrayElement(array_t));
                }
                self.assemble_expr(array)?;
                self.assemble_expr(index)?;
            }
            ir::Expr::Conditional {
                condition,
                then,
                else_,
                ..
            } => {
                let else_label = self.new_label();
                let exit = self.new_label();
                self.emit(Instr::Conditional(Conditional::new(else_label, exit)));
                self.assemble_expr(condition)?;
                self.assemble_expr(then)?;
                self.mark_label(else_label);
                self.assemble_expr(else_)?;
                self.mark_label(exit);
            }
            ir::Expr::DynCast {
                expr,
                expr_type,
                target_type,
                span,
            } => {
                let expr_t = self.mono_type(expr_type, *span)?;
                let target_t = self.mono_type_app(target_type, *span)?;
                let class = self.monomorph.class(&target_t, self.symbols, self.bundle);
                self.emit(Instr::DynamicCast {
                    class,
                    is_weak: matches!(expr_t.ref_type(), Some(RefType::Weak)),
                });
                self.assemble_expr(expr)?;
            }
            ir::Expr::Local(local, _) => {
                let instr = self
                    .create_local_instr(*local)
                    .expect("every local should be defined");
                self.emit(instr);
            }
            ir::Expr::Capture(local, _) => {
                let idx = self
                    .resolve_capture(*local)
                    .expect("every capture should be defined");
                self.emit(Instr::ObjectField(idx));
            }
        }
        Ok(())
    }

    fn assemble_call(
        &mut self,
        call: &ir::Call<'ctx>,
        span: Span,
    ) -> Result<(), AssembleError<'ctx>> {
        match call {
            ir::Call::FreeFunction {
                function,
                type_args,
                args,
            } => {
                let free_function = &self.symbols[*function];
                let type_args = type_args
                    .iter()
                    .map(|typ| self.mono_type(typ, span))
                    .collect::<Result<Box<_>, AssembleError<'ctx>>>()?;

                if let Some(intrinsic) = free_function.intrinsic() {
                    self.emit_intrinsic(intrinsic, &type_args, span)?;
                    args.iter().try_for_each(|arg| self.assemble_expr(arg))?;
                    return Ok(());
                }

                let (idx, mode) = if let Some(method) = free_function.method_alias() {
                    let idx = self
                        .monomorph
                        .existing_alias(method)
                        .expect("method alias should be defined");
                    (idx, ir::CallMode::ForceStatic)
                } else {
                    let sig = Signature::new(*function, type_args);
                    let idx = self.monomorph.function(sig, self.symbols, self.bundle);
                    (idx, ir::CallMode::default())
                };

                self.emit_call(idx, args, mode, span)?;
            }
            ir::Call::Static {
                parent_id,
                parent_type_args,
                method,
                type_args,
                args,
            } => {
                let parent_t_args = parent_type_args
                    .iter()
                    .map(|typ| self.mono_type(typ, span))
                    .collect::<Result<Box<_>, AssembleError<'ctx>>>()?;
                let parent_t = MonoType::new(*parent_id, parent_t_args);
                let parent_t = parent_t
                    .instantiate_as(method.parent(), self.symbols)
                    .expect("should instantiate as parent");

                let function = if parent_t.args().is_empty() && type_args.is_empty() {
                    let index = method.index();
                    self.monomorph
                        .mono_method(&parent_t, index, self.symbols, self.bundle)
                } else {
                    let method_id = MethodWithReceiver::new(parent_t.into_owned(), method.index());
                    let types = type_args
                        .iter()
                        .map(|typ| self.mono_type(typ, span))
                        .collect::<Result<Box<_>, AssembleError<'ctx>>>()?;
                    let sig = Signature::new(method_id, types);

                    self.monomorph.method(sig, self.symbols, self.bundle)
                };

                let exit = self.new_label();
                self.emit_call(function, args, ir::CallMode::default(), span)?;
                self.mark_label(exit);
            }
            ir::Call::Instance {
                receiver,
                receiver_type,
                receiver_ref,
                method,
                type_args,
                args,
                mode,
            } => {
                let exit = self.new_label();
                let receiver_t = self.mono_type_app(receiver_type, span)?;
                let parent_t = receiver_t
                    .instantiate_as(method.parent(), self.symbols)
                    .expect("should instantiate as parent");

                self.emit(Instr::Context(Jump::new(exit)));

                if matches!(receiver_ref, Some(RefType::Weak)) {
                    self.emit(Instr::WeakRefToRef);
                }
                self.assemble_expr(receiver)?;

                let function = if type_args.is_empty() {
                    let idx = method.index();
                    self.monomorph
                        .mono_method(&parent_t, idx, self.symbols, self.bundle)
                } else {
                    let method_id = MethodWithReceiver::new(parent_t.into_owned(), method.index());
                    let types = type_args
                        .iter()
                        .map(|typ| self.mono_type(typ, span))
                        .collect::<Result<Box<_>, AssembleError<'ctx>>>()?;
                    let sig = Signature::new(method_id, types);
                    self.monomorph.method(sig, self.symbols, self.bundle)
                };

                self.emit_call(function, args, *mode, span)?;
                self.mark_label(exit);
            }
            ir::Call::Closure {
                closure,
                closure_type,
                args,
            } => {
                let closure_t = self.mono_type_app(closure_type, span)?;
                let call = self.symbols[closure_t.id()]
                    .schema()
                    .as_aggregate()
                    .ok_or(AssembleError::InvalidFunctionClass(span))?
                    .methods()
                    .by_name(CALL_METHOD)
                    .next()
                    .ok_or(AssembleError::InvalidFunctionClass(span))?;
                let call = call.key();
                let call = self
                    .monomorph
                    .mono_method(&closure_t, *call, self.symbols, self.bundle);

                let context = self.new_label();
                let function = self.bundle[call].name();

                self.emit(Instr::Context(Jump::new(context)));
                self.assemble_expr(closure)?;
                self.mark_label(context);

                let exit = self.new_label();
                self.emit(Instr::InvokeVirtual {
                    exit: Jump::new(exit),
                    line: self.monomorph.source_line(closure.span()),
                    function,
                    flags: InvokeFlags::default(),
                });
                args.iter().try_for_each(|arg| self.assemble_expr(arg))?;
                self.emit(Instr::ParamEnd);
                self.mark_label(exit);
            }
        };
        Ok(())
    }

    fn assemble_const(&mut self, const_: &ir::Const<'ctx>) -> Result<(), AssembleError<'ctx>> {
        match const_ {
            ir::Const::Str(str) => {
                let str = self.bundle.strings_mut().add(str.clone());
                self.emit(Instr::StringConst(str));
            }
            ir::Const::CName(name) => {
                let name = self.bundle.cnames_mut().add(name.clone());
                self.emit(Instr::CNameConst(name));
            }
            ir::Const::ResRef(res) => {
                let res = self.bundle.resources_mut().add(res.clone());
                self.emit(Instr::ResourceConst(res));
            }
            ir::Const::TweakDbId(id) => {
                let id = self.bundle.tdb_ids_mut().add(id.clone());
                self.emit(Instr::TweakDbIdConst(id));
            }
            &ir::Const::EnumVariant(variant) => {
                let enum_ = self
                    .monomorph
                    .existing_enum(variant.parent())
                    .expect("every enum should be defined");
                let value = self.bundle[enum_].values()[usize::from(variant.index())];
                self.emit(Instr::EnumConst { enum_, value });
            }
            &ir::Const::F32(val) => self.emit(Instr::F32Const(val)),
            &ir::Const::F64(val) => self.emit(Instr::F64Const(val)),
            &ir::Const::I8(val) => self.emit(Instr::I8Const(val)),
            &ir::Const::I16(val) => self.emit(Instr::I16Const(val)),
            &ir::Const::I32(0) => self.emit(Instr::I32Zero),
            &ir::Const::I32(1) => self.emit(Instr::I32One),
            &ir::Const::I32(val) => self.emit(Instr::I32Const(val)),
            &ir::Const::I64(val) => self.emit(Instr::I64Const(val)),
            &ir::Const::U8(val) => self.emit(Instr::U8Const(val)),
            &ir::Const::U16(val) => self.emit(Instr::U16Const(val)),
            &ir::Const::U32(val) => self.emit(Instr::U32Const(val)),
            &ir::Const::U64(val) => self.emit(Instr::U64Const(val)),
            ir::Const::Bool(true) => self.emit(Instr::TrueConst),
            ir::Const::Bool(false) => self.emit(Instr::FalseConst),
        };
        Ok(())
    }

    fn emit_call(
        &mut self,
        index: PoolFunctionIndex,
        args: &[ir::Expr<'ctx>],
        mode: ir::CallMode,
        span: Span,
    ) -> Result<(), AssembleError<'ctx>> {
        let exit = self.new_label();
        let func = &self.bundle[index];

        let jump = Jump::new(exit);
        let line = self.monomorph.source_line(span);

        let mut flags = InvokeFlags::default();
        for (arg, i) in args.iter().zip(0u8..) {
            if arg.is_prvalue_ref(self.symbols) {
                flags.set_is_rvalue_ref(i);
            }
        }

        if func.flags().is_final()
            || func.flags().is_static()
            || func.flags().is_native()
            || matches!(mode, ir::CallMode::ForceStatic)
        {
            self.emit(Instr::InvokeStatic {
                exit: jump,
                line,
                function: index,
                flags,
            });
        } else {
            self.emit(Instr::InvokeVirtual {
                exit: jump,
                line,
                function: func.name(),
                flags,
            });
        }
        for (i, arg) in args.iter().enumerate() {
            if self.bundle[self.bundle[index].parameters()[i]]
                .flags()
                .is_short_circuit()
            {
                let skip = self.new_label();
                self.emit(Instr::Skip(Jump::new(skip)));
                self.assemble_expr(arg)?;
                self.mark_label(skip);
            } else {
                self.assemble_expr(arg)?;
            }
        }
        for _ in 0..self.bundle[index].parameters().len() - args.len() {
            self.emit(Instr::Nop);
        }
        self.emit(Instr::ParamEnd);

        self.mark_label(exit);
        Ok(())
    }

    fn emit_intrinsic(
        &mut self,
        intrinsic: ir::Intrinsic,
        type_args: &[MonoType<'ctx>],
        span: Span,
    ) -> Result<(), AssembleError<'ctx>> {
        fn emit_typed<'ctx>(
            assembler: &mut Assembler<'_, 'ctx>,
            typ: &MonoType<'ctx>,
            instr: fn(PoolTypeIndex) -> Instr<Label>,
        ) {
            let typ = assembler
                .monomorph
                .type_(typ, assembler.symbols, assembler.bundle);
            assembler.emit(instr(typ));
        }

        match (intrinsic, type_args) {
            (ir::Intrinsic::Equals, [arg]) => emit_typed(self, arg, Instr::Equals),
            (ir::Intrinsic::NotEquals, [arg]) => emit_typed(self, arg, Instr::NotEquals),
            (ir::Intrinsic::ToString, [arg]) => emit_typed(self, arg, Instr::ToString),
            (ir::Intrinsic::EnumInt, [arg]) => {
                emit_typed(self, arg, |typ| Instr::EnumToI32 {
                    enum_type: typ,
                    size: 4,
                });
            }
            (ir::Intrinsic::IntEnum, [arg]) => {
                emit_typed(self, arg, |typ| Instr::I32ToEnum {
                    enum_type: typ,
                    size: 4,
                });
            }
            (ir::Intrinsic::LongIntEnum, [arg]) => {
                emit_typed(self, arg, |typ| Instr::I32ToEnum {
                    enum_type: typ,
                    size: 8,
                });
            }
            (ir::Intrinsic::ToVariant, [arg]) => emit_typed(self, arg, Instr::ToVariant),
            (ir::Intrinsic::FromVariant, [arg]) => emit_typed(self, arg, Instr::FromVariant),
            (ir::Intrinsic::VariantIsDefined, []) => self.emit(Instr::VariantIsDefined),
            (ir::Intrinsic::VariantIsRef, []) => self.emit(Instr::VariantIsRef),
            (ir::Intrinsic::VariantIsArray, []) => self.emit(Instr::VariantIsArray),
            (ir::Intrinsic::VariantTypeName, []) => self.emit(Instr::VariantTypeName),
            (ir::Intrinsic::AsRef, [arg]) => emit_typed(self, arg, Instr::AsRef),
            (ir::Intrinsic::Deref, [arg]) => emit_typed(self, arg, Instr::Deref),
            (ir::Intrinsic::RefToWeakRef, [_]) => self.emit(Instr::RefToWeakRef),
            (ir::Intrinsic::WeakRefToRef, [_]) => self.emit(Instr::WeakRefToRef),
            (ir::Intrinsic::IsDefined, []) => self.emit(Instr::RefToBool),
            (ir::Intrinsic::NameOf, [arg]) => {
                let name = self.bundle.cnames_mut().add(arg.to_string());
                self.emit(Instr::CNameConst(name));
            }
            (
                ir::Intrinsic::ArrayClear
                | ir::Intrinsic::ArraySize
                | ir::Intrinsic::ArrayResize
                | ir::Intrinsic::ArrayFindFirst
                | ir::Intrinsic::ArrayFindLast
                | ir::Intrinsic::ArrayContains
                | ir::Intrinsic::ArrayCount
                | ir::Intrinsic::ArrayPush
                | ir::Intrinsic::ArrayPop
                | ir::Intrinsic::ArrayInsert
                | ir::Intrinsic::ArrayRemove
                | ir::Intrinsic::ArrayGrow
                | ir::Intrinsic::ArrayErase
                | ir::Intrinsic::ArrayLast
                | ir::Intrinsic::ArraySort
                | ir::Intrinsic::ArraySortByPredicate,
                [arg],
            ) => {
                let array_t = MonoType::new(predef::ARRAY, [arg.clone()]);
                let instr = match intrinsic {
                    ir::Intrinsic::ArrayClear => Instr::ArrayClear,
                    ir::Intrinsic::ArraySize => Instr::ArraySize,
                    ir::Intrinsic::ArrayResize => Instr::ArrayResize,
                    ir::Intrinsic::ArrayFindFirst => Instr::ArrayFindFirst,
                    ir::Intrinsic::ArrayFindLast => Instr::ArrayFindLast,
                    ir::Intrinsic::ArrayContains => Instr::ArrayContains,
                    ir::Intrinsic::ArrayCount => Instr::ArrayCount,
                    ir::Intrinsic::ArrayPush => Instr::ArrayPush,
                    ir::Intrinsic::ArrayPop => Instr::ArrayPop,
                    ir::Intrinsic::ArrayInsert => Instr::ArrayInsert,
                    ir::Intrinsic::ArrayRemove => Instr::ArrayRemove,
                    ir::Intrinsic::ArrayGrow => Instr::ArrayGrow,
                    ir::Intrinsic::ArrayErase => Instr::ArrayErase,
                    ir::Intrinsic::ArrayLast => Instr::ArrayLast,
                    ir::Intrinsic::ArraySort => Instr::ArraySort,
                    ir::Intrinsic::ArraySortByPredicate => Instr::ArraySortByPredicate,
                    _ => unreachable!(),
                };
                emit_typed(self, &array_t, instr);
            }
            (i, _) => return Err(AssembleError::InvalidIntrinsic(i, span)),
        };
        Ok(())
    }

    fn into_code(self) -> Result<Vec<Instr>, AssembleError<'ctx>> {
        let mut virtual_offset = 0;
        self.code
            .into_iter()
            .map(|instr| {
                let instr = instr
                    .map_labels(|label| {
                        let loc: i64 = self.labels[usize::from(label)].into();
                        Some(Offset::from(i16::try_from(loc - virtual_offset).ok()?))
                    })
                    .ok_or(AssembleError::LabelTooDistant)?;
                virtual_offset += i64::from(instr.virtual_size());
                Ok(instr)
            })
            .collect()
    }

    fn create_local_instr(&self, local: ir::Local) -> Option<Instr<Label>> {
        if let Some(idx) = self.resolve_capture(local) {
            return Some(Instr::ObjectField(idx));
        }
        if matches!(local, ir::Local::This) {
            return Some(Instr::This);
        };
        if let Some(idx) = self.resolve_local(local) {
            return Some(Instr::Local(idx));
        }
        if let Some(idx) = self.resolve_param(local) {
            return Some(Instr::Param(idx));
        }
        None
    }

    fn emit_default_instr(&mut self, typ: MonoType<'ctx>) -> Result<(), AssembleError<'ctx>> {
        match self.symbols[typ.id()].schema() {
            TypeSchema::Aggregate(aggregate) if aggregate.flags().is_struct() => {
                let class = self.monomorph.class(&typ, self.symbols, self.bundle);
                self.emit(Instr::Construct {
                    arg_count: 0,
                    class,
                });
            }
            TypeSchema::Aggregate(_) => {
                self.emit(Instr::Null);
            }
            TypeSchema::Enum(_) => {
                let enum_type = self.monomorph.type_(&typ, self.symbols, self.bundle);
                self.emit(Instr::I32ToEnum { enum_type, size: 1 });
                self.emit(Instr::I8Const(0));
            }
            TypeSchema::Primitive => {
                let instr = match typ.id() {
                    id if id == predef::STRING => {
                        let str = self.bundle.strings_mut().add("");
                        Some(Instr::StringConst(str))
                    }
                    id if id == predef::CNAME => {
                        let cname = self.bundle.cnames_mut().add("");
                        Some(Instr::CNameConst(cname))
                    }
                    id if id == predef::RES_REF => {
                        let res = self.bundle.resources_mut().add("");
                        Some(Instr::ResourceConst(res))
                    }
                    id if id == predef::TWEAK_DB_ID => {
                        let tdbid = self.bundle.tdb_ids_mut().add("");
                        Some(Instr::TweakDbIdConst(tdbid))
                    }
                    id if id == predef::FLOAT => Some(Instr::F32Const(0.0)),
                    id if id == predef::DOUBLE => Some(Instr::F64Const(0.0)),
                    id if id == predef::INT8 => Some(Instr::I8Const(0)),
                    id if id == predef::INT16 => Some(Instr::I16Const(0)),
                    id if id == predef::INT32 => Some(Instr::I32Zero),
                    id if id == predef::INT64 => Some(Instr::I64Const(0)),
                    id if id == predef::UINT8 => Some(Instr::U8Const(0)),
                    id if id == predef::UINT16 => Some(Instr::U16Const(0)),
                    id if id == predef::UINT32 => Some(Instr::U32Const(0)),
                    id if id == predef::UINT64 => Some(Instr::U64Const(0)),
                    id if id == predef::BOOL => Some(Instr::FalseConst),
                    id if id == predef::REF => Some(Instr::Null),
                    id if id == predef::WREF => Some(Instr::WeakRefNull),
                    _ => None,
                };
                if let Some(instr) = instr {
                    self.emit(instr);
                }
            }
        };
        Ok(())
    }

    fn resolve_local(&self, loc: ir::Local) -> Option<PoolLocalIndex> {
        let i = self.locals.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.locals.get(i)?.1)
    }

    fn resolve_param(&self, loc: ir::Local) -> Option<PoolParameterIndex> {
        let i = self.params.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.params.get(i)?.1)
    }

    fn resolve_capture(&self, loc: ir::Local) -> Option<PoolFieldIndex> {
        let i = self.captures.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.captures.get(i)?.1)
    }

    fn mono_type(
        &self,
        typ: &ir::Type<'ctx>,
        span: Span,
    ) -> Result<MonoType<'ctx>, AssembleError<'ctx>> {
        Ok(typ
            .coalesced(self.symbols)
            .map_err(|e| AssembleError::Coalesce(e.into(), span))?
            .assume_mono(self.type_env))
    }

    fn mono_type_app(
        &self,
        typ: &ir::TypeApp<'ctx>,
        span: Span,
    ) -> Result<MonoType<'ctx>, AssembleError<'ctx>> {
        Ok(typ
            .coalesced(self.symbols)
            .map_err(|e| AssembleError::Coalesce(e.into(), span))?
            .assume_mono(self.type_env))
    }
}

#[derive(Debug, Error)]
pub enum AssembleError<'ctx> {
    #[error(
        "jump offset exceeds the signed 16-bit bytecode limit; reduce the size of this function"
    )]
    LabelTooDistant,
    #[error("`{0}` cannot be used here; {1}")]
    InvalidControlFlow(&'static str, &'static str, Span),
    #[error("struct construction has {0} arguments; the bytecode limit is 255")]
    TooManyArguments(usize, Span),
    #[error("cannot generate code for intrinsic `{0:?}` with these arguments")]
    InvalidIntrinsic(ir::Intrinsic, Span),
    #[error(
        "function type must be a class with a `Call` method; check the function type definitions"
    )]
    InvalidFunctionClass(Span),
    #[error("{0}")]
    Coalesce(Box<CoalesceError<'ctx>>, Span),
    #[error("decoding error: {0}")]
    Decoding(#[from] byte::Error),
}

impl AssembleError<'_> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::InvalidControlFlow(_, _, span)
            | Self::TooManyArguments(_, span)
            | Self::InvalidIntrinsic(_, span)
            | Self::InvalidFunctionClass(span)
            | Self::Coalesce(_, span) => Some(*span),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Label(u16);

impl From<Label> for usize {
    fn from(label: Label) -> usize {
        usize::from(label.0)
    }
}

#[derive(Debug, Clone, Copy)]
struct BlockSpan {
    continue_target: Option<Label>,
    break_target: Label,
}

#[cfg(test)]
mod diagnostic_tests {
    use redscript_compiler_frontend::ast::SourceMap;

    use super::*;

    #[test]
    fn backend_errors_explain_source_restrictions() {
        let sources = SourceMap::new();
        let file = sources.push_back("test.reds", "break;");
        let span = Span::new(0, 6, file);
        let error =
            AssembleError::InvalidControlFlow("break", "a loop or switch is required", span);
        assert_eq!(
            error.to_string(),
            "`break` cannot be used here; a loop or switch is required"
        );
        assert_eq!(error.span(), Some(span));
        let error = AssembleError::TooManyArguments(256, span);
        assert_eq!(
            error.to_string(),
            "struct construction has 256 arguments; the bytecode limit is 255"
        );
        assert_eq!(error.span(), Some(span));
        assert_eq!(
            AssembleError::LabelTooDistant.to_string(),
            "jump offset exceeds the signed 16-bit bytecode limit; reduce the size of this function"
        );
    }
}

use redscript_compiler_frontend::ast::Span;
use redscript_compiler_frontend::{MonoType, RefType, TypeSchema, ir, predef};
use redscript_io::{
    Conditional, FunctionIndex as PoolFunctionIndex, Instr, InvokeFlags, Jump,
    TypeIndex as PoolTypeIndex,
};

use super::closure::{CALL_METHOD, emit_closure};
use super::{AssembleError, Assembler, Label};
use crate::inputs::Signature;
use crate::monomorph::MethodWithReceiver;

impl<'scope, 'ctx> Assembler<'scope, 'ctx> {
    pub(super) fn assemble_expr(
        &mut self,
        expr: &ir::Expr<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        match expr {
            ir::Expr::Nop(_) => self.emit(Instr::Nop),
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
                self.touched_locals.extend(closure.captures.iter().copied());
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
                let target_t = self.mono_type(target_type, *span)?;
                let class = self.monomorph.class(&target_t, self.symbols, self.bundle);
                self.emit(Instr::DynamicCast {
                    class,
                    is_weak: matches!(expr_t.ref_type(), Some(RefType::Weak)),
                });
                self.assemble_expr(expr)?;
            }
            ir::Expr::Local(local, _) => {
                self.touched_locals.insert(*local);
                let instr = self
                    .create_local_instr(*local)
                    .expect("every local should be defined");
                self.emit(instr);
            }
            ir::Expr::Capture(local, _) => {
                self.touched_locals.insert(*local);
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
                    if free_function
                        .type_()
                        .params()
                        .iter()
                        .any(|param| param.flags().is_short_circuit())
                    {
                        return Err(AssembleError::InvalidIntrinsic(intrinsic, span));
                    }
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
        for (i, arg) in args.iter().enumerate() {
            if arg.is_prvalue_ref(self.symbols) {
                if i >= u16::BITS as usize {
                    return Err(AssembleError::TemporaryReferenceArgument(i + 1, arg.span()));
                }
                flags.set_is_rvalue_ref(i as u8);
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
}

use redscript_compiler_frontend::{MonoType, TypeSchema, ir, predef};
use redscript_io::{Instr, Jump, Switch, SwitchLabel};

use super::{AssembleError, Assembler, BlockSpan};

impl<'scope, 'ctx> Assembler<'scope, 'ctx> {
    pub(super) fn assemble_block(
        &mut self,
        block: &ir::Block<'ctx>,
        span: Option<BlockSpan>,
    ) -> Result<(), AssembleError<'ctx>> {
        block
            .stmts
            .iter()
            .try_for_each(|stmt| self.assemble(stmt, span))
    }

    pub(super) fn assemble(
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
                let was_in_loop = std::mem::replace(&mut self.in_loop, true);
                let result = self.assemble_block(
                    &block.block,
                    Some(BlockSpan {
                        continue_target: Some(start),
                        break_target: end,
                    }),
                );
                self.in_loop = was_in_loop;
                result?;
                self.emit(Instr::Jump(Jump::new(start)));
                self.mark_label(end);
            }
            ir::Stmt::If {
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
                default_index,
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

                if let Some(default) = default
                    && *default_index < branches.len()
                {
                    // Dispatch independently of body order: unmatched values must
                    // search labels after default before entering its body.
                    let bodies = branches
                        .iter()
                        .map(|_| self.new_label())
                        .collect::<Vec<_>>();
                    let default_body = self.new_label();
                    for (case, body) in branches.iter().zip(&bodies) {
                        self.mark_label(next_label);
                        next_label = self.new_label();
                        self.emit(Instr::SwitchLabel(SwitchLabel::new(next_label, *body)));
                        self.assemble_expr(&case.matcher)?;
                    }
                    self.mark_label(next_label);
                    self.emit(Instr::SwitchDefault);
                    self.emit(Instr::Jump(Jump::new(default_body)));
                    for (index, (case, body)) in branches.iter().zip(&bodies).enumerate() {
                        if index == *default_index {
                            self.mark_label(default_body);
                            self.assemble_block(default, switch_span)?;
                        }
                        self.mark_label(*body);
                        self.assemble_block(&case.block, switch_span)?;
                    }
                    self.mark_label(end);
                    return Ok(());
                }

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
                ..
            } => {
                let ir_local = *local;
                // Empty literals are only inert while the storage is freshly
                // default-constructed. If this statement can execute more than once
                // or the local already holds a value, its previous contents must be
                // cleared.
                let needs_reset = self.in_loop || self.touched_locals.contains(&ir_local);
                self.touched_locals.insert(ir_local);

                if elements.is_empty() && !needs_reset {
                    return Ok(());
                }

                let local = self
                    .resolve_local(ir_local)
                    .expect("every local should be defined");

                if elements.is_empty() {
                    // Reuse the local's array type: it was already resolved when the
                    // local was defined, unlike a possibly-fresh literal element type.
                    let array_t = self.bundle[local].type_();
                    self.emit(Instr::ArrayClear(array_t));
                    self.emit(Instr::Local(local));
                    return Ok(());
                }

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
            ir::Stmt::InitDefault {
                local, typ, span, ..
            } => {
                let ir_local = *local;
                let typ = self.mono_type(typ, *span)?;

                // Variant locals are default-constructed by the VM, not by bytecode.
                if typ.id() == predef::VARIANT {
                    return Ok(());
                }

                // An array local is only inert while its storage is freshly
                // default-constructed. Skip the redundant reset at function entry,
                // and clear it whenever the declaration can run again or the local
                // already holds a value.
                let needs_reset = self.in_loop || self.touched_locals.contains(&ir_local);
                self.touched_locals.insert(ir_local);

                if typ.id() == predef::ARRAY && !needs_reset {
                    return Ok(());
                }

                let local = self
                    .resolve_local(ir_local)
                    .expect("every local should be defined");

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
}

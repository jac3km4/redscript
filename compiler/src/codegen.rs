use std::rc::Rc;

use itertools::Itertools;
use redscript::ast::{Constant, Expr, Literal, Seq, Span};
use redscript::bundle::{CName, ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr, Intrinsic, Label, Location, Offset};
use redscript::definition::{
    Class as PoolClass, Definition, Field as PoolField, Function, FunctionFlags, Local as PoolLocal, LocalFlags,
    Parameter as PoolParam, Type as PoolType,
};
use redscript::Str;
use smallvec::SmallVec;

use crate::codegen::builders::{ClassBuilder, FieldBuilder, FunctionBuilder, LocalBuilder, ParamBuilder, TypeCache};
use crate::compiler::CompilationDb;
use crate::type_repo::{predef, GlobalId, Parameterized, Prim, RefType, ScopedName, Type, TypeId, TypeRepo};
use crate::typer::{CallMetadata, Callable, CheckedAst, Data, InferType, Local, Member};
use crate::IndexMap;

pub mod builders;
pub(crate) mod names;

pub type IndexVec<A> = SmallVec<[PoolIndex<A>; 8]>;

#[derive(Debug)]
pub struct CodeGen<'ctx, 'id> {
    repo: &'ctx TypeRepo<'id>,
    db: &'ctx CompilationDb<'id>,
    captures: LocalIndices<'id, PoolField>,
    params: LocalIndices<'id, PoolParam>,
    locals: LocalIndices<'id, PoolLocal>,
    wrapped: Option<PoolIndex<Function>>,
    instructions: Vec<Instr<Label>>,
    labels: usize,
}

impl<'ctx, 'id> CodeGen<'ctx, 'id> {
    fn new(
        params: LocalIndices<'id, PoolParam>,
        captures: LocalIndices<'id, PoolField>,
        type_repo: &'ctx TypeRepo<'id>,
        db: &'ctx CompilationDb<'id>,
        wrapped: Option<PoolIndex<Function>>,
    ) -> Self {
        Self {
            repo: type_repo,
            db,
            params,
            captures,
            wrapped,
            locals: LocalIndices::default(),
            instructions: vec![],
            labels: 0,
        }
    }

    #[inline]
    fn emit(&mut self, instr: Instr<Label>) {
        self.instructions.push(instr);
    }

    #[inline]
    fn emit_label(&mut self, label: Label) {
        self.instructions.push(Instr::Target(label));
    }

    #[inline]
    fn new_label(&mut self) -> Label {
        let label = Label { index: self.labels };
        self.labels += 1;
        label
    }

    fn assemble_seq(
        &mut self,
        seq: Seq<CheckedAst<'id>>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
        exit: Option<Label>,
    ) {
        for expr in seq.exprs {
            self.assemble_with(expr, pool, cache, exit);
        }
    }

    #[inline]
    fn assemble(&mut self, expr: Expr<CheckedAst<'id>>, pool: &mut ConstantPool, cache: &mut TypeCache) {
        self.assemble_with(expr, pool, cache, None);
    }

    fn assemble_with(
        &mut self,
        expr: Expr<CheckedAst<'id>>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
        exit: Option<Label>,
    ) {
        match expr {
            Expr::Ident(id, _) => match id {
                Local::Var(_) => {
                    let idx = self.locals.get_index(id).expect("local should be defined");
                    self.emit(Instr::Local(idx));
                }
                Local::Param(_) => {
                    let idx = self.params.get_index(id).expect("param should be defined");
                    self.emit(Instr::Param(idx));
                }
                Local::Capture(_) => {
                    let idx = self.captures.get_index(id).expect("capture should be defined");
                    self.emit(Instr::ObjectField(idx));
                }
                Local::This => self.emit(Instr::This),
            },
            Expr::Constant(cons, _) => match cons {
                Constant::String(lit, str) => match lit {
                    Literal::String => self.emit(Instr::StringConst(pool.strings_mut().add(str))),
                    Literal::Name => self.emit(Instr::NameConst(pool.names_mut().add(str))),
                    Literal::Resource => self.emit(Instr::ResourceConst(pool.resources_mut().add(str))),
                    Literal::TweakDbId => self.emit(Instr::TweakDbIdConst(pool.tweakdb_ids_mut().add(str))),
                },
                Constant::F32(i) => self.emit(Instr::F32Const(i)),
                Constant::F64(i) => self.emit(Instr::F64Const(i)),
                Constant::I32(i) => self.emit(Instr::I32Const(i)),
                Constant::I64(i) => self.emit(Instr::I64Const(i)),
                Constant::U32(i) => self.emit(Instr::U32Const(i)),
                Constant::U64(i) => self.emit(Instr::U64Const(i)),
                Constant::Bool(true) => self.emit(Instr::TrueConst),
                Constant::Bool(false) => self.emit(Instr::FalseConst),
            },
            Expr::Declare(local, typ, init, _) => {
                let typ = typ.expect("type should be populated in codegen").simplify(self.repo);
                let idx = LocalBuilder::builder()
                    .name(names::local(local))
                    .typ(typ.clone())
                    .build()
                    .commit(self.repo, pool, cache);
                self.locals.add(local, typ, idx);
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(idx));
                    self.assemble(*val, pool, cache);
                } else {
                    // TODO: emit initializers
                }
            }
            Expr::DynCast(target, expr, _) => {
                let &tt = self.db.classes.get(&target.id).unwrap();
                self.emit(Instr::DynamicCast(tt, 0));
                self.assemble(*expr, pool, cache);
            }
            Expr::Assign(lhs, rhs, _, _) => {
                self.emit(Instr::Assign);
                self.assemble(*lhs, pool, cache);
                self.assemble(*rhs, pool, cache);
            }
            Expr::Call(expr, cb, targs, args, meta, _) => match &*cb {
                Callable::Static(mid) => {
                    let &idx = self.db.statics.get(mid).unwrap();
                    self.emit_static_call(idx, args.into_vec(), &meta.arg_types, pool, cache);
                }
                Callable::WrappedStatic(_) => {
                    let wrapped = self.wrapped.expect("wrapped should be defined for WrappedStatic");
                    self.emit_static_call(wrapped, args.into_vec(), &meta.arg_types, pool, cache);
                }
                Callable::Instance(mid) => {
                    let &idx = self.db.methods.get(mid).unwrap();
                    self.emit_instance_call(*expr, args, &meta, idx, pool, cache, false);
                }
                Callable::WrappedMethod(_) => {
                    let wrapped = self.wrapped.expect("wrapped should be defined for WrappedMethod");
                    self.emit_instance_call(*expr, args, &meta, wrapped, pool, cache, true);
                }
                Callable::Lambda => {
                    let exit_label = self.new_label();
                    self.emit(Instr::Context(exit_label));
                    self.assemble(*expr, pool, cache);
                    self.emit_virtual_call(pool.names_mut().add("Apply"), args.into_vec(), pool, cache);
                    self.emit_label(exit_label);
                }
                Callable::Global(gid) => {
                    let &idx = self.db.globals.get(gid).unwrap();
                    self.emit_static_call(idx, args.into_vec(), &meta.arg_types, pool, cache);
                }
                &Callable::Intrinsic(op) => {
                    self.emit_intrinsic(op, &targs, pool, cache);
                    for arg in args.into_vec() {
                        self.assemble(arg, pool, cache);
                    }
                }
                Callable::Cast => {
                    let from = meta
                        .arg_types
                        .first()
                        .expect("cast should define one type argument")
                        .simplify(self.repo);
                    let to = meta.ret_type.simplify(self.repo);
                    let entry = self
                        .repo
                        .globals()
                        .by_name(&ScopedName::top_level(Str::from_static("Cast")))
                        .find(|entry| {
                            entry.function.typ.params.first().map(|param| &param.typ) == Some(&from)
                                && entry.function.typ.ret == to
                        })
                        .expect("cast not found");
                    let &idx = self.db.globals.get(&GlobalId::new(entry.index)).unwrap();
                    self.emit_static_call(idx, args.into_vec(), &meta.arg_types, pool, cache);
                }
            },
            Expr::Lambda(env, body, _) => {
                let args = env
                    .captures
                    .values()
                    .map(|&(_, captured)| match captured {
                        Local::Var(_) => {
                            let idx = self
                                .locals
                                .get_index(captured)
                                .expect("captured local should be defined");
                            Instr::Local(idx)
                        }
                        Local::Param(_) => {
                            let idx = self
                                .params
                                .get_index(captured)
                                .expect("captured param should be defined");
                            Instr::Param(idx)
                        }
                        Local::Capture(_) => {
                            let idx = self.captures.get_index(captured).expect("capture should be defined");
                            Instr::ObjectField(idx)
                        }
                        Local::This => Instr::This,
                    })
                    .collect_vec();
                let params: IndexMap<_, _> = env
                    .params
                    .into_iter()
                    .map(|(l, t)| (l, t.simplify(self.repo)))
                    .collect();
                let captures = env
                    .captures
                    .into_iter()
                    .map(|(l, (t, _))| (l, t.simplify(self.repo)))
                    .collect();
                let &base = self
                    .db
                    .classes
                    .get(&TypeId::get_fn_by_arity(params.len()).expect("lambda should have valid arity"))
                    .unwrap();
                let idx = pool.definitions().len();
                let parent_class =
                    Self::closure(&params, &captures, idx).commit_with_base(base, self.repo, pool, cache);

                let &[summoner, apply, ..] = &pool[parent_class].methods[..] else {
                    panic!("lambda should have a summoner and an apply method")
                };

                Self::impl_summoner(parent_class, summoner, pool);
                self.emit_static_call(summoner, args, &[], pool, cache);

                let param_indices = LocalIndices::new(params, pool[apply].parameters.iter().copied().collect());
                let capture_indices = LocalIndices::new(captures, pool[parent_class].fields.iter().copied().collect());
                let body = if env.ret_type.simplify(self.repo) == Type::Prim(Prim::Void) {
                    *body
                } else {
                    Expr::Return(Some(body), Span::ZERO)
                };
                let (locals, code) = Self::build_expr(
                    body,
                    param_indices,
                    capture_indices,
                    self.repo,
                    self.db,
                    None,
                    pool,
                    cache,
                );
                pool.complete_function(apply, locals.into_vec(), code);
            }
            Expr::Member(expr, member, _) => match member {
                Member::Field(field, _) => {
                    let dt = self.repo[field.owner()]
                        .as_class()
                        .expect("field should only be used on classes");
                    if dt.flags.is_struct() {
                        self.emit(Instr::StructField(*self.db.fields.get(&field).unwrap()));
                        self.assemble(*expr, pool, cache);
                    } else {
                        let exit_label = self.new_label();
                        self.emit(Instr::Context(exit_label));
                        self.assemble(*expr, pool, cache);
                        self.emit(Instr::ObjectField(*self.db.fields.get(&field).unwrap()));
                        self.emit_label(exit_label);
                    }
                }
                Member::EnumMember(member) => {
                    let enum_ = self.db.enums.get(&member.owner()).unwrap();
                    let item = self.db.enum_members.get(&member).unwrap();
                    self.emit(Instr::EnumConst(*enum_, *item));
                }
            },
            Expr::ArrayElem(arr, idx, typ, _) => {
                let arr_type = Type::Data(Parameterized::new(predef::ARRAY, Rc::new([typ.simplify(self.repo)])));
                self.emit(Instr::ArrayElement(cache.alloc_type(&arr_type, self.repo, pool)));
                self.assemble(*arr, pool, cache);
                self.assemble(*idx, pool, cache);
            }
            Expr::New(typ, args, _) => {
                let class = self.repo[typ.id]
                    .as_class()
                    .expect("new should only be used on classes");
                let &tt = self.db.classes.get(&typ.id).unwrap();
                if class.flags.is_struct() {
                    self.emit(Instr::Construct(args.len() as u8, tt));
                    for arg in args.into_vec() {
                        self.assemble(arg, pool, cache);
                    }
                } else {
                    self.emit(Instr::New(tt));
                }
            }
            Expr::Return(Some(expr), _) => {
                self.emit(Instr::Return);
                self.assemble(*expr, pool, cache);
            }
            Expr::Return(None, _) => {
                self.emit(Instr::Return);
                self.emit(Instr::Nop);
            }
            Expr::Seq(seq) => {
                self.assemble_seq(seq, pool, cache, exit);
            }
            Expr::Switch(scrutinee, cases, default, scrutinee_type, _) => {
                let first_case_label = self.new_label();
                let mut next_case_label = self.new_label();
                let exit_label = self.new_label();

                let scrutinee_type = cache.alloc_type(&scrutinee_type.simplify(self.repo), self.repo, pool);
                self.emit(Instr::Switch(scrutinee_type, first_case_label));
                self.assemble(*scrutinee, pool, cache);
                self.emit_label(first_case_label);

                let mut case_iter = cases.into_iter().peekable();
                while case_iter.peek().is_some() {
                    let body_label = self.new_label();

                    for case in &mut case_iter {
                        self.emit_label(next_case_label);
                        next_case_label = self.new_label();
                        self.emit(Instr::SwitchLabel(next_case_label, body_label));
                        self.assemble(case.matcher, pool, cache);

                        if !case.body.exprs.iter().all(Expr::is_empty) {
                            self.emit_label(body_label);
                            self.assemble_seq(case.body, pool, cache, Some(exit_label));
                            break;
                        }
                    }
                }
                self.emit_label(next_case_label);

                if let Some(body) = default {
                    self.emit(Instr::SwitchDefault);
                    self.assemble_seq(body, pool, cache, Some(exit_label));
                }
                self.emit_label(exit_label);
            }
            Expr::If(condition, if_, else_, _) => {
                let else_label = self.new_label();
                self.emit(Instr::JumpIfFalse(else_label));
                self.assemble(*condition, pool, cache);
                self.assemble_seq(if_, pool, cache, exit);
                if let Some(else_code) = else_ {
                    let exit_label = self.new_label();
                    self.emit(Instr::Jump(exit_label));
                    self.emit_label(else_label);
                    self.assemble_seq(else_code, pool, cache, Some(exit_label));
                    self.emit_label(exit_label);
                } else {
                    self.emit_label(else_label);
                }
            }
            Expr::Conditional(cond, true_, false_, _) => {
                let false_label = self.new_label();
                let exit_label = self.new_label();
                self.emit(Instr::Conditional(false_label, exit_label));
                self.assemble(*cond, pool, cache);
                self.assemble(*true_, pool, cache);
                self.emit_label(false_label);
                self.assemble(*false_, pool, cache);
                self.emit_label(exit_label);
            }
            Expr::While(cond, body, _) => {
                let exit_label = self.new_label();
                let loop_label = self.new_label();
                self.emit_label(loop_label);
                self.emit(Instr::JumpIfFalse(exit_label));
                self.assemble(*cond, pool, cache);
                self.assemble_seq(body, pool, cache, Some(exit_label));
                self.emit(Instr::Jump(loop_label));
                self.emit_label(exit_label);
            }
            Expr::Null(_) => {
                self.emit(Instr::Null);
            }
            Expr::This(_) | Expr::Super(_) => {
                self.emit(Instr::This);
            }
            Expr::Break(_) => {
                self.emit(Instr::Jump(exit.expect("break should have exit label")));
            }
            Expr::BinOp(_, _, _, _)
            | Expr::UnOp(_, _, _)
            | Expr::Goto(_, _)
            | Expr::ArrayLit(_, _, _)
            | Expr::InterpolatedString(_, _, _)
            | Expr::ForIn(_, _, _, _) => {
                unreachable!("this expression should be removed before codegen")
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_instance_call(
        &mut self,
        expr: Expr<CheckedAst<'id>>,
        args: Box<[Expr<CheckedAst<'id>>]>,
        meta: &CallMetadata<'id>,
        idx: PoolIndex<Function>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
        force_static: bool,
    ) {
        let flags = pool[idx].flags;
        let exit_label = self.new_label();
        self.emit(Instr::Context(exit_label));

        if matches!(
            meta.this_type.as_ref().and_then(InferType::ref_type),
            Some((RefType::Weak, _))
        ) {
            self.emit(Instr::WeakRefToRef);
        }
        self.assemble(expr, pool, cache);
        if force_static || flags.is_final() {
            self.emit_static_call(idx, args.into_vec(), &meta.arg_types, pool, cache);
        } else {
            let name = pool.def_name_idx(idx).unwrap();
            self.emit_virtual_call(name, args.into_vec(), pool, cache);
        }
        self.emit_label(exit_label);
    }

    fn emit_static_call<I, A>(
        &mut self,
        idx: PoolIndex<Function>,
        args: I,
        arg_types: &[InferType<'id>],
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
    ) where
        I: IntoIterator<Item = A>,
        for<'a> &'a I: IntoIterator<Item = &'a A>,
        A: Assemble<'id>,
    {
        let exit_label = self.new_label();
        let flags = pool[idx]
            .parameters
            .iter()
            .map(|&param| pool[param].flags)
            .collect_vec();

        let mut invoke_flags = 0u16;
        for (n, (arg, typ)) in (&args).into_iter().zip(arg_types).enumerate() {
            let is_rvalue_ref = A::is_rvalue_ref(arg, typ);
            if is_rvalue_ref {
                invoke_flags |= 1 << n;
            }
        }

        self.emit(Instr::InvokeStatic(exit_label, 0, idx, invoke_flags));
        for (arg, flags) in args.into_iter().zip(flags) {
            if flags.is_short_circuit() {
                let skip_label = self.new_label();
                self.emit(Instr::Skip(skip_label));
                arg.assemble(self, pool, cache);
                self.emit_label(skip_label);
            } else {
                arg.assemble(self, pool, cache);
            }
        }
        self.emit(Instr::ParamEnd);
        self.emit_label(exit_label);
    }

    fn emit_virtual_call<Arg: Assemble<'id>>(
        &mut self,
        idx: PoolIndex<CName>,
        args: impl IntoIterator<Item = Arg>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
    ) {
        let exit_label = self.new_label();
        let invoke_flags = 0u16;

        self.emit(Instr::InvokeVirtual(exit_label, 0, idx, invoke_flags));
        for arg in args {
            arg.assemble(self, pool, cache);
        }
        self.emit(Instr::ParamEnd);
        self.emit_label(exit_label);
    }

    fn emit_intrinsic(
        &mut self,
        op: Intrinsic,
        type_args: &[InferType<'id>],
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
    ) {
        let array_of = |elem: &InferType<'id>| InferType::data(Data::array(elem.clone()));
        let mut alloc_type = |typ: &InferType<'id>| cache.alloc_type(&typ.simplify(self.repo), self.repo, pool);

        let instr = match op {
            Intrinsic::Equals => Instr::Equals(alloc_type(&type_args[0])),
            Intrinsic::NotEquals => Instr::NotEquals(alloc_type(&type_args[0])),
            Intrinsic::ArrayClear => Instr::ArrayClear(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArraySize => Instr::ArraySize(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayResize => Instr::ArrayResize(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayFindFirst => Instr::ArrayFindFirst(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayFindLast => Instr::ArrayFindLast(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayContains => Instr::ArrayContains(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayCount => Instr::ArrayCount(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayPush => Instr::ArrayPush(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayPop => Instr::ArrayPop(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayInsert => Instr::ArrayInsert(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayRemove => Instr::ArrayRemove(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayGrow => Instr::ArrayGrow(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayErase => Instr::ArrayErase(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArrayLast => Instr::ArrayLast(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArraySort => Instr::ArraySort(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ArraySortByPredicate => Instr::ArraySortByPredicate(alloc_type(&array_of(&type_args[0]))),
            Intrinsic::ToString => Instr::ToString(alloc_type(&type_args[0])),
            Intrinsic::EnumInt => Instr::EnumToI32(alloc_type(&type_args[0]), 4),
            Intrinsic::IntEnum => Instr::I32ToEnum(alloc_type(&type_args[0]), 4),
            Intrinsic::ToVariant => Instr::ToVariant(alloc_type(&type_args[0])),
            Intrinsic::FromVariant => Instr::FromVariant(alloc_type(&type_args[0])),
            Intrinsic::VariantIsRef => Instr::VariantIsRef,
            Intrinsic::VariantIsArray => Instr::VariantIsArray,
            Intrinsic::VariantTypeName => Instr::VariantTypeName,
            Intrinsic::AsRef => Instr::AsRef(alloc_type(&type_args[0])),
            Intrinsic::Deref => Instr::Deref(alloc_type(&type_args[0])),
            Intrinsic::RefToWeakRef => Instr::RefToWeakRef,
            Intrinsic::WeakRefToRef => Instr::WeakRefToRef,
            Intrinsic::IsDefined => Instr::RefToBool,
        };
        self.emit(instr);
    }

    fn closure(
        params: &IndexMap<Local, Type<'id>>,
        captures: &IndexMap<Local, Type<'id>>,
        id: usize,
    ) -> ClassBuilder<'id> {
        let summon_params = captures
            .iter()
            .map(|(loc, typ)| ParamBuilder::builder().name(names::param(loc)).typ(typ.clone()).build());
        let summoner = FunctionBuilder::builder()
            .name("Summon")
            .flags(FunctionFlags::new().with_is_static(true).with_is_final(true))
            .params(summon_params)
            .build();
        let apply_params = params
            .iter()
            .map(|(loc, _)| ParamBuilder::builder().name(names::param(loc)).typ(Type::Top).build());
        let apply = FunctionBuilder::builder()
            .name("Apply")
            .params(apply_params)
            .return_type(Type::Top)
            .build();
        let fields = captures
            .iter()
            .map(|(loc, typ)| FieldBuilder::builder().name(names::field(loc)).typ(typ.clone()).build());
        ClassBuilder::builder()
            .name(names::lambda(id))
            .methods(vec![summoner, apply])
            .fields(fields)
            .build()
    }

    fn impl_summoner(class: PoolIndex<PoolClass>, summoner: PoolIndex<Function>, pool: &mut ConstantPool) {
        let name = pool.def_name_idx(class).unwrap();
        let class_idx = pool.add_definition(Definition::type_(name, PoolType::Class));
        let ref_ = pool.add_definition(Definition::type_(name, PoolType::Ref(class_idx)));
        let this = Definition::local(
            pool.names_mut().add("self"),
            summoner,
            PoolLocal::new(ref_, LocalFlags::new()),
        );
        let this = pool.add_definition(this);
        let fields = &pool[class].fields;
        let params = &pool[summoner].parameters;

        let mut code = vec![];
        code.push(Instr::Assign);
        code.push(Instr::Local(this));
        code.push(Instr::New(class));
        for (field, param) in fields.iter().zip(params) {
            code.push(Instr::Assign);
            code.push(Instr::Context(Offset::new(0)));
            code.push(Instr::Local(this));
            code.push(Instr::ObjectField(*field));
            code.push(Instr::Param(*param));
        }
        code.push(Instr::Return);
        code.push(Instr::Local(this));

        let summoner = &mut pool[summoner];
        summoner.return_type = Some(ref_);
        summoner.code = Code(code);
        summoner.locals = vec![this];
    }

    fn into_code(self) -> (IndexVec<PoolLocal>, Code<Offset>) {
        let mut locations = Vec::with_capacity(self.labels);
        locations.resize(self.labels, Location::new(0));

        let code = Code(self.instructions);
        for (loc, instr) in code.iter() {
            if let Instr::Target(label) = instr {
                locations[label.index] = loc;
            }
        }

        let mut resolved = Vec::with_capacity(code.0.len());
        for (loc, instr) in code.iter().filter(|(_, instr)| !matches!(instr, Instr::Target(_))) {
            resolved.push(instr.resolve_labels(loc, &locations));
        }
        (self.locals.indices, Code(resolved))
    }

    #[allow(clippy::too_many_arguments)]
    pub fn build_expr(
        expr: Expr<CheckedAst<'id>>,
        params: LocalIndices<'id, PoolParam>,
        captures: LocalIndices<'id, PoolField>,
        type_repo: &'ctx TypeRepo<'id>,
        db: &'ctx CompilationDb<'id>,
        wrapped: Option<PoolIndex<Function>>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
    ) -> (IndexVec<PoolLocal>, Code<Offset>) {
        let mut assembler = Self::new(params, captures, type_repo, db, wrapped);
        assembler.assemble(expr, pool, cache);
        assembler.emit(Instr::Nop);
        assembler.into_code()
    }

    #[inline]
    pub fn build_function(
        seq: Seq<CheckedAst<'id>>,
        params: LocalIndices<'id, PoolParam>,
        type_repo: &'ctx TypeRepo<'id>,
        db: &'ctx CompilationDb<'id>,
        wrapped: Option<PoolIndex<Function>>,
        pool: &mut ConstantPool,
        cache: &mut TypeCache,
    ) -> (IndexVec<PoolLocal>, Code<Offset>) {
        Self::build_expr(
            Expr::Seq(seq),
            params,
            LocalIndices::default(),
            type_repo,
            db,
            wrapped,
            pool,
            cache,
        )
    }
}

#[derive(Debug)]
pub struct LocalIndices<'id, A> {
    types: IndexMap<Local, Type<'id>>,
    indices: IndexVec<A>,
}

impl<'id, A> LocalIndices<'id, A> {
    #[inline]
    pub fn new(types: IndexMap<Local, Type<'id>>, indices: IndexVec<A>) -> Self {
        Self { types, indices }
    }

    fn add(&mut self, loc: Local, typ: Type<'id>, index: PoolIndex<A>) {
        self.types.insert(loc, typ);
        self.indices.push(index);
    }

    #[inline]
    fn get_index(&self, local: Local) -> Option<PoolIndex<A>> {
        self.indices.get(self.types.get_index_of(&local)?).copied()
    }
}

impl<'id, A> Default for LocalIndices<'id, A> {
    #[inline]
    fn default() -> Self {
        Self::new(IndexMap::default(), IndexVec::default())
    }
}

trait Assemble<'id> {
    fn assemble(self, gen: &mut CodeGen<'_, 'id>, pool: &mut ConstantPool, cache: &mut TypeCache);
    fn is_rvalue_ref(&self, typ: &InferType<'id>) -> bool;
}

impl<'id> Assemble<'id> for Instr<Label> {
    #[inline]
    fn assemble(self, gen: &mut CodeGen<'_, 'id>, _pool: &mut ConstantPool, _cache: &mut TypeCache) {
        gen.emit(self);
    }

    fn is_rvalue_ref(&self, _typ: &InferType<'id>) -> bool {
        false
    }
}

impl<'id> Assemble<'id> for Expr<CheckedAst<'id>> {
    #[inline]
    fn assemble(self, gen: &mut CodeGen<'_, 'id>, pool: &mut ConstantPool, cache: &mut TypeCache) {
        gen.assemble(self, pool, cache);
    }

    fn is_rvalue_ref(&self, typ: &InferType<'id>) -> bool {
        match (self, typ) {
            (Expr::Call(_, intrinsic, _, args, _, _), _)
                if matches!(**intrinsic, Callable::Intrinsic(Intrinsic::AsRef)) =>
            {
                is_rvalue(&args[0])
            }
            (_, typ) if matches!(typ.ref_type(), Some((RefType::Script, _))) => true,
            _ => false,
        }
    }
}

fn is_rvalue(expr: &Expr<CheckedAst<'_>>) -> bool {
    match expr {
        Expr::Constant(_, _) | Expr::Ident(_, _) | Expr::This(_) | Expr::Super(_) => false,
        Expr::Member(inner, _, _) | Expr::ArrayElem(inner, _, _, _) => is_rvalue(inner),
        _ => true,
    }
}

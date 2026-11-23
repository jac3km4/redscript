use std::mem;

use bon::builder;
use redscript_compiler_frontend::ast::Span;
use redscript_compiler_frontend::{MonoType, Symbols, ir, predef};
use redscript_io::{
    Class as PoolClass, ClassFlags as PoolClassFlags, ClassIndex as PoolClassIndex,
    Field as PoolField, FieldFlags as PoolFieldFlags, FieldIndex as PoolFieldIndex,
    Function as PoolFunction, FunctionFlags as PoolFunctionFlags,
    FunctionIndex as PoolFunctionIndex, Instr, InvokeFlags, Jump, Local as PoolLocal,
    LocalFlags as PoolLocalFlags, Offset, Parameter as PoolParameter,
    ParameterFlags as PoolParameterFlags, ScriptBundle, TypeIndex as PoolTypeIndex, Visibility,
};

use super::{AssembleError, Assembler, block_builder};
use crate::monomorph::Monomorphizer;

pub(super) const INSTANTIATE_METHOD: &str = "Instantiate";
pub(super) const CALL_METHOD: &str = "Call";

pub fn emit_closure<'ctx>(
    assembler: &mut Assembler<'_, 'ctx>,
    closure: &ir::Closure<'ctx>,
    span: Span,
) -> Result<(), AssembleError<'ctx>> {
    let symbols = assembler.symbols;
    let type_env = assembler.type_env;

    let cname_idx = assembler.bundle.cnames_mut().len();
    let class = assembler
        .bundle
        .cnames_mut()
        .add(format!("lambda${cname_idx}"));
    let func_t = assembler.mono_type_app(&closure.typ, span)?;
    let func_t_idx = assembler
        .monomorph
        .type_(&func_t, symbols, assembler.bundle);
    let func_class = assembler
        .monomorph
        .class(&func_t, symbols, assembler.bundle);
    let flags = PoolClassFlags::default().with_is_final(true);
    let class = PoolClass::new(class, Visibility::Public, flags).with_base(Some(func_class));
    let class = assembler.bundle.define(class);

    let fields = closure
        .captures
        .iter()
        .map(|local| {
            let typ =
                resolve_local_type(assembler, *local).expect("capture local should be defined");
            let name = assembler
                .bundle
                .cnames_mut()
                .add(format!("capture${}", local.to_index()));
            let field_flags = PoolFieldFlags::default();
            let field = PoolField::new(name, class, Visibility::Public, typ, field_flags);
            assembler.bundle.define(field)
        })
        .collect::<Vec<_>>();

    let instantiate_name = assembler.bundle.cnames_mut().add(INSTANTIATE_METHOD);
    let instantiate_flags = PoolFunctionFlags::default()
        .with_is_final(true)
        .with_is_static(true);
    let instantiate = PoolFunction::new(instantiate_name, Visibility::Public, instantiate_flags);
    let instantiate = assembler
        .bundle
        .define_and_init(instantiate, |bundle, idx, func| {
            create_instantiate_method(idx, func, class, func_t_idx, &fields, bundle)
        });

    let call = symbols[func_t.id()]
        .schema()
        .as_aggregate()
        .ok_or(AssembleError::InvalidFunctionClass(span))?
        .methods()
        .by_name(CALL_METHOD)
        .next()
        .ok_or(AssembleError::InvalidFunctionClass(span))?;
    let call = assembler
        .monomorph
        .mono_method(&func_t, *call.key(), symbols, assembler.bundle);
    let call_name = assembler.bundle[call].name();
    let call_flags = PoolFunctionFlags::default().with_is_final(true);
    let source_ref = assembler.monomorph.source_ref(span, assembler.bundle);
    let call =
        PoolFunction::new(call_name, Visibility::Public, call_flags).with_source(Some(source_ref));
    let call = assembler.bundle.define_and_init(call, |bundle, idx, func| {
        call_method_builder()
            .function_index(idx)
            .function(func)
            .class_index(class)
            .function_type(&func_t)
            .symbols(assembler.symbols)
            .bundle(bundle)
            .monomorphizer(assembler.monomorph)
            .build()
    });

    let captures = closure
        .captures
        .iter()
        .copied()
        .zip(fields.iter().copied())
        .collect();
    let code = block_builder()
        .function_index(call)
        .locals(closure.locals.iter().cloned())
        .captures(captures)
        .block(&closure.block)
        .symbols(symbols)
        .type_env(type_env)
        .bundle(assembler.bundle)
        .monomorphizer(assembler.monomorph)
        .build()?;

    assembler.bundle[call].set_code(code);

    assembler.bundle[class] = mem::take(&mut assembler.bundle[class])
        .with_fields(fields)
        .with_methods(vec![instantiate, call]);

    let exit = assembler.new_label();
    assembler.emit(Instr::InvokeStatic {
        exit: Jump::new(exit),
        line: 0,
        function: instantiate,
        flags: InvokeFlags::default(),
    });
    for &capture in &closure.captures {
        let instr = assembler
            .create_local_instr(capture)
            .expect("capture local should be defined");
        assembler.emit(instr);
    }
    assembler.emit(Instr::ParamEnd);
    assembler.mark_label(exit);

    Ok(())
}

#[builder(finish_fn = build)]
fn call_method_builder<'ctx>(
    function_index: PoolFunctionIndex,
    function: PoolFunction<'ctx>,
    class_index: PoolClassIndex,
    function_type: &MonoType<'ctx>,
    symbols: &Symbols<'ctx>,
    bundle: &mut ScriptBundle<'ctx>,
    monomorphizer: &mut Monomorphizer<'ctx>,
) -> PoolFunction<'ctx> {
    let (return_t, param_types) = function_type
        .args()
        .split_last()
        .expect("lambda should have at least one type argument");

    let call_params = param_types
        .iter()
        .enumerate()
        .map(|(i, typ)| {
            let typ = monomorphizer.type_(typ, symbols, bundle);
            let name = bundle.cnames_mut().add(format!("arg${i}"));
            let flags = PoolParameterFlags::default();
            let param = PoolParameter::new(name, function_index, typ, flags);
            bundle.define(param)
        })
        .collect::<Vec<_>>();

    let return_t_idx = match return_t.id() {
        id if id == predef::VOID || id == predef::NOTHING => None,
        _ => Some(monomorphizer.type_(return_t, symbols, bundle)),
    };

    function
        .with_class(Some(class_index))
        .with_parameters(call_params)
        .with_return_type(return_t_idx)
}

fn create_instantiate_method<'ctx>(
    index: PoolFunctionIndex,
    func: PoolFunction<'ctx>,
    class_index: PoolClassIndex,
    type_index: PoolTypeIndex,
    capture_fields: &[PoolFieldIndex],
    bundle: &mut ScriptBundle<'ctx>,
) -> PoolFunction<'ctx> {
    let params = capture_fields
        .iter()
        .map(|&field| {
            let field = &bundle[field];
            let param_flags = PoolParameterFlags::default();
            let param = PoolParameter::new(field.name(), index, field.type_(), param_flags);
            bundle.define(param)
        })
        .collect::<Vec<_>>();

    let self_name = bundle.cnames_mut().add("self");

    let self_flags = PoolLocalFlags::default();
    let self_local = PoolLocal::new(self_name, index, type_index, self_flags);
    let self_local = bundle.define(self_local);

    let mut body: Vec<Instr<Offset>> = capture_fields.iter().zip(&params).fold(
        vec![
            Instr::Assign,
            Instr::Local(self_local),
            Instr::New(class_index),
        ],
        |mut acc, (&field, &param)| {
            acc.push(Instr::Assign);
            acc.push(Instr::Context(Jump::new(Offset::from(15))));
            acc.push(Instr::Local(self_local));
            acc.push(Instr::ObjectField(field));
            acc.push(Instr::Param(param));
            acc
        },
    );
    body.push(Instr::Return);
    body.push(Instr::Local(self_local));

    func.with_class(Some(class_index))
        .with_locals(vec![self_local])
        .with_parameters(params)
        .with_return_type(Some(type_index))
        .with_code(body)
}

fn resolve_local_type(assembler: &Assembler<'_, '_>, local: ir::Local) -> Option<PoolTypeIndex> {
    if let Some(idx) = assembler.resolve_capture(local) {
        return Some(assembler.bundle[idx].type_());
    };
    if matches!(local, ir::Local::This) {
        return assembler
            .monomorph
            .existing_type(&MonoType::nullary(predef::ISCRIPTABLE));
    }
    if let Some(idx) = assembler.resolve_local(local) {
        return Some(assembler.bundle[idx].type_());
    };
    if let Some(idx) = assembler.resolve_param(local) {
        return Some(assembler.bundle[idx].type_());
    };
    None
}

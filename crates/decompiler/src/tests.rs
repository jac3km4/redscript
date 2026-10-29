use hashbrown::HashMap;
use redscript_ast as ast;
use redscript_formatter::{FormatCtx, FormatSettings, SyntaxOps};
use redscript_io::{
    CodeIter, Function, FunctionFlags, Instr, Jump, Local, LocalFlags, ScriptBundle, Switch,
    SwitchLabel, Type, TypeKind, Visibility,
};

use crate::control_flow::{BlockType, build_control_flow};
use crate::decompiler::{Verbosity, decompile_block};
use crate::location::{Bounds, Location};

// Labels are instruction indices, including one-past-the-end. map_labels expects
// offsets relative to the instruction; opcode-specific accessors account for
// the positions of serialized offset fields.
fn resolve_labels(code: Vec<Instr<usize>>) -> Vec<Instr> {
    let mut offsets = vec![0i16];
    for instr in &code {
        offsets.push(offsets.last().unwrap() + instr.virtual_size() as i16);
    }
    code.into_iter()
        .enumerate()
        .map(|(i, instr)| {
            instr
                .map_labels(|label| Some((offsets[label] - offsets[i]).into()))
                .unwrap()
        })
        .collect()
}

fn function(bundle: &mut ScriptBundle<'_>) -> Function<'static> {
    let name = bundle.cnames_mut().add("Test");
    Function::new(name, Visibility::Public, FunctionFlags::default())
}

fn decompile<'ctx: 'i, 'i>(
    bundle: &'ctx ScriptBundle<'i>,
    function: &'ctx Function<'i>,
    code: &[Instr],
) -> ast::Block<'i> {
    let cf = build_control_flow(CodeIter::new(code)).unwrap();
    decompile_block(function, bundle, code, &cf, Verbosity::Quiet).unwrap()
}

fn format_block(block: &ast::Block<'_>) -> String {
    block
        .as_fmt(FormatCtx::new(
            &FormatSettings::default(),
            &HashMap::default(),
        ))
        .to_string()
}

#[test]
fn initialized_locals_preserve_declared_types() {
    for (type_name, value) in [
        ("Int8", Instr::I8Const(1)),
        ("Int16", Instr::I16Const(1)),
        ("Uint8", Instr::U8Const(1)),
        ("Uint16", Instr::U16Const(1)),
        ("Foo", Instr::Null),
    ] {
        let mut bundle = ScriptBundle::default();
        let mut function = function(&mut bundle);
        let function_idx = bundle.define(function.clone());
        let name = bundle.cnames_mut().add(type_name);
        let inner = bundle.define(Type::new(name, TypeKind::Primitive));
        let typ = if type_name == "Foo" {
            let name = bundle.cnames_mut().add("ref:Foo");
            bundle.define(Type::new(name, TypeKind::Ref(inner)))
        } else {
            inner
        };
        let name = bundle.cnames_mut().add("x");
        let local = bundle.define(Local::new(name, function_idx, typ, LocalFlags::default()));
        function.set_locals([local]);
        let code = vec![
            Instr::Assign,
            Instr::Local(local),
            value,
            Instr::Return,
            Instr::Nop,
        ];
        let block = decompile(&bundle, &function, &code);
        assert_eq!(block.stmts.len(), 2);
        let ast::Stmt::Let {
            typ: Some(typ),
            value: Some(_),
            ..
        } = &block.stmts[0]
        else {
            panic!("expected a typed initialized local: {block:?}");
        };
        assert_eq!(typ.as_ref(), &ast::Type::plain(type_name));
    }
}

#[test]
fn assignment_expression_keeps_hoisted_local_type() {
    let mut bundle = ScriptBundle::default();
    let mut function = function(&mut bundle);
    let function_idx = bundle.define(function.clone());
    let name = bundle.cnames_mut().add("Int8");
    let typ = bundle.define(Type::new(name, TypeKind::Primitive));
    let name = bundle.cnames_mut().add("x");
    let local = bundle.define(Local::new(name, function_idx, typ, LocalFlags::default()));
    function.set_locals([local]);
    let code = vec![
        Instr::Return,
        Instr::Assign,
        Instr::Local(local),
        Instr::I8Const(1),
    ];
    let block = decompile(&bundle, &function, &code);
    assert_eq!(block.stmts.len(), 2);
    assert!(
        matches!(&block.stmts[0], ast::Stmt::Let { typ: Some(typ), value: None, .. }
        if typ.as_ref() == &ast::Type::plain("Int8"))
    );
    assert!(matches!(&block.stmts[1], ast::Stmt::Return(Some(expr))
        if matches!(expr.as_ref(), ast::Expr::Assign { .. })));
}

#[test]
fn switch_fallthrough_preserves_default_body() {
    let mut bundle = ScriptBundle::default();
    let function = function(&mut bundle);
    let name = bundle.cnames_mut().add("Int32");
    let typ = bundle.define(Type::new(name, TypeKind::Primitive));
    let code = resolve_labels(vec![
        Instr::Switch(Switch::new(typ, 2)),
        Instr::I32Zero,
        Instr::SwitchLabel(SwitchLabel::new(5, 4)),
        Instr::I32Zero,
        Instr::Jump(Jump::new(11)),
        Instr::SwitchLabel(SwitchLabel::new(8, 7)),
        Instr::I32One,
        Instr::I32One,
        Instr::SwitchDefault,
        Instr::I32Const(2),
        Instr::I32Const(3),
        Instr::I32Const(4),
        Instr::Return,
        Instr::Nop,
    ]);
    let block = decompile(&bundle, &function, &code);
    insta::assert_snapshot!(format_block(&block));
}

#[test]
fn no_op_jump_preserves_conditional_boundary() {
    let code = resolve_labels(vec![
        Instr::JumpIfFalse(Jump::new(4)),
        Instr::TrueConst,
        Instr::I32One,
        Instr::Jump(Jump::new(4)),
        Instr::I32Zero,
        Instr::Return,
        Instr::Nop,
    ]);
    let cf = build_control_flow(CodeIter::new(&code)).unwrap();
    let end = CodeIter::new(&code).with_offsets().nth(4).unwrap().0;
    let conditional = cf.get_block(Bounds::new(Location::ZERO, end.into()));
    assert_eq!(conditional.type_(), BlockType::Conditional);
    assert_eq!(conditional.exit(), None);

    let mut bundle = ScriptBundle::default();
    let function = function(&mut bundle);
    let block = decompile(&bundle, &function, &code);
    insta::assert_snapshot!(format_block(&block));
}

#[test]
fn no_op_jump_preserves_empty_conditional_boundary() {
    let code = resolve_labels(vec![
        Instr::JumpIfFalse(Jump::new(3)),
        Instr::TrueConst,
        Instr::Jump(Jump::new(3)),
        Instr::I32One,
        Instr::Return,
        Instr::Nop,
    ]);
    let mut bundle = ScriptBundle::default();
    let function = function(&mut bundle);
    let block = decompile(&bundle, &function, &code);
    insta::assert_snapshot!(format_block(&block));
}

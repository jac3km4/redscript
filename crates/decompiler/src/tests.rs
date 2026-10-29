use hashbrown::HashMap;
use redscript_ast as ast;
use redscript_formatter::{FormatCtx, FormatSettings, SyntaxOps};
use redscript_io::{CodeIter, Function, FunctionFlags, Instr, Jump, ScriptBundle, Visibility};

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

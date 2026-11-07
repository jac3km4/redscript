mod support;

use redscript_io::{Instr, ScriptBundle, TypeKind};

fn compile(source: &str) -> Vec<u8> {
    support::compile(source, "arrays.script", &[], support::empty_bundle())
        .into_writeable()
        .to_bytes()
        .unwrap()
}

#[test]
fn grow_and_count_roundtrip_as_typed_array_opcodes() {
    let bytes = compile(
        "function GrowValues(out values: array<Int32>, amount: Int32) { values.Grow(amount); }
         function CountValues(const values: array<Int32>, needle: Int32): Int32 { return values.Count(needle); }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    for (prefix, grow) in [("GrowValues;", true), ("CountValues;", false)] {
        let function = support::function_with_prefix(&bundle, prefix);
        let code = function
            .body()
            .code_iter()
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        let (opcode, operands) = if grow {
            assert!(bundle[function.parameters()[0]].flags().is_out());
            assert!(function.return_type().is_none());
            assert!(matches!(code[0], Instr::ArrayGrow(_)), "{code:?}");
            (&code[0], &code[1..3])
        } else {
            assert!(bundle[function.parameters()[0]].flags().is_const());
            assert_eq!(
                &*bundle[bundle[function.return_type().unwrap()].name()],
                "Int32"
            );
            assert!(
                matches!(code[..2], [Instr::Return, Instr::ArrayCount(_)]),
                "{code:?}"
            );
            (&code[1], &code[2..4])
        };
        assert_eq!(
            operands,
            [
                Instr::Param(function.parameters()[0]),
                Instr::Param(function.parameters()[1])
            ]
        );
        let array_type = match opcode {
            Instr::ArrayGrow(typ) | Instr::ArrayCount(typ) => *typ,
            _ => unreachable!(),
        };
        let TypeKind::Array(element) = bundle[array_type].kind() else {
            panic!("expected dynamic-array opcode type")
        };
        assert_eq!(&*bundle[bundle[element].name()], "Int32");
        assert!(!code.iter().any(|instr| matches!(
            instr,
            Instr::InvokeStatic { .. } | Instr::InvokeVirtual { .. } | Instr::Assign
        )));
    }
}

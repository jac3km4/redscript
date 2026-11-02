use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, IndexedDefinition, Instr, ScriptBundle, TypeKind, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> Vec<u8> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("arrays.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) =
        process_sources(parsed, dependencies, symbols, &mut reporter, &interner);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_writeable().to_bytes().unwrap()
}

#[test]
fn grow_and_count_roundtrip_as_typed_array_opcodes() {
    let bytes = compile(
        "function GrowValues(out values: array<Int32>, amount: Int32) { values.Grow(amount); }
         function CountValues(const values: array<Int32>, needle: Int32): Int32 { return values.Count(needle); }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    for (prefix, grow) in [("GrowValues;", true), ("CountValues;", false)] {
        let function = bundle
            .indexed_definitions()
            .find_map(|def| match def {
                IndexedDefinition::Function(_, function)
                    if bundle[function.name()].starts_with(prefix) =>
                {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
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

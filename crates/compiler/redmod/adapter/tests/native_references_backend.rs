use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, Function, IndexedDefinition, Instr, ScriptBundle, TypeKind, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> Vec<u8> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("references.script", source);
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

fn function<'a>(bundle: &'a ScriptBundle<'_>, prefix: &str) -> &'a Function<'a> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with(prefix) =>
            {
                Some(function)
            }
            _ => None,
        })
        .unwrap()
}

fn code(bundle: &ScriptBundle<'_>, prefix: &str) -> Vec<Instr> {
    function(bundle, prefix)
        .body()
        .code_iter()
        .map(Result::unwrap)
        .collect()
}

#[test]
fn native_reference_types_flags_and_dereferenced_writes_roundtrip() {
    let bytes = compile(
        r#"
        struct Value { var number: Int32; }
        class Object extends IScriptable { public var number: Int32; }
        function Write(value: ref<Int32>) { value = 7; }
        function Read(const value: ref<String>) -> String { return value; }
        function Out(out value: ref<String>) { value = "changed"; }
        function Forward(value: ref<String>) { Out(value); }
        function Structure(value: ref<Value>) { value.number = 7; }
        function Array(value: ref<array<Int32>>) -> Int32 { value.PushBack(7); value[0] = 8; return value[0]; }
        function Weak(value: ref<weak<Object>>) -> weak<Object> { return value; }
        function TakeWeak(value: weak<Object>) -> weak<Object> { return Weak(value); }
        function Class(value: ref<Object>) -> Int32 { return value.number; }
        function Calls(object: Object) { var value: Int32; Write(value); Read("temporary"); Class(object); Class(NULL); }
    "#,
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let write = code(&bundle, "Write;");
    assert!(matches!(
        &write[..3],
        [Instr::Assign, Instr::Deref(_), Instr::Param(_)]
    ));
    let read = function(&bundle, "Read;");
    let param = &bundle[read.parameters()[0]];
    assert!(param.flags().is_const());
    assert!(matches!(
        bundle[param.type_()].kind(),
        TypeKind::ScriptRef(_)
    ));
    let out = function(&bundle, "Out;");
    assert!(bundle[out.parameters()[0]].flags().is_out());
    assert!(matches!(
        bundle[bundle[out.parameters()[0]].type_()].kind(),
        TypeKind::ScriptRef(_)
    ));
    let weak = function(&bundle, "Weak;");
    let TypeKind::ScriptRef(inner) = bundle[bundle[weak.parameters()[0]].type_()].kind() else {
        panic!("expected script ref")
    };
    assert!(matches!(bundle[inner].kind(), TypeKind::WeakRef(_)));
    let array = code(&bundle, "Array;");
    assert!(array.iter().any(|instr| matches!(instr, Instr::Deref(_))));
    let calls = code(&bundle, "Calls;");
    assert!(calls.iter().any(|instr| matches!(instr, Instr::AsRef(_))));
    // Loading the serialized bundle must retain native value-reference signatures.
    let interner = TypeInterner::default();
    let (symbols, _) = CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
        .unwrap()
        .into_inner();
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Write")
        .unwrap();
    assert_eq!(
        entry.func().type_().params()[0].type_().to_string(),
        "script_ref<Int32>"
    );
}

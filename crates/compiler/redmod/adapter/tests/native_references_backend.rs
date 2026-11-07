mod support;

use redscript_compiler_api::TypeFlagRegistry;
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::TypeInterner;
use redscript_io::{Instr, ScriptBundle, TypeKind};
use support::{code_with_prefix as code, function_with_prefix as function};

fn compile(source: &str) -> Vec<u8> {
    support::compile(source, "references.script", &[], support::empty_bundle())
        .into_writeable()
        .to_bytes()
        .unwrap()
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

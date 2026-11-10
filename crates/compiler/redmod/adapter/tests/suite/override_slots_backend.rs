use redscript_io::{
    Class, ClassFlags, Function, IndexedDefinition, Instr, ScriptBundle, Visibility,
};
use support::function;

use crate::support;

fn compile(source: &str) -> ScriptBundle<'static> {
    compile_with_native_root(source, false)
}

fn compile_with_native_root(source: &str, native_root: bool) -> ScriptBundle<'static> {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    let root = bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    if native_root {
        let name = bundle.cnames_mut().add("ProbeNativeBase");
        let class = bundle.define(
            Class::new(
                name,
                Visibility::Public,
                ClassFlags::default().with_is_native(true),
            )
            .with_base(Some(root)),
        );
        let name = bundle.cnames_mut().add("Get");
        let method = bundle.define(
            Function::new(
                name,
                Visibility::Public,
                redscript_io::FunctionFlags::default().with_is_native(true),
            )
            .with_class(Some(class))
            .with_source(None),
        );
        bundle[class].add_method(method);
    }
    support::compile(source, "slots.script", &[], bundle)
}

#[test]
fn deep_overrides_of_native_slots_keep_the_unmangled_root_name() {
    let bundle = compile_with_native_root(
        "
        class Middle extends ProbeNativeBase { public override function Get() {} }
        class Child extends Middle { public override function Get() {} }
        function Run(x: Child) { x.Get(); }
    ",
        true,
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let names = bundle
        .indexed_definitions()
        .filter_map(|def| match def {
            IndexedDefinition::Function(_, method)
                if method.class().is_some_and(|id| {
                    matches!(&*bundle[bundle[id].name()], "Middle" | "Child")
                }) =>
            {
                Some(bundle[method.name()].to_string())
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(names, ["Get", "Get"]);
    let calls = function(&bundle, "Run;Child")
        .body()
        .code_iter()
        .filter_map(|instr| match instr.unwrap() {
            Instr::InvokeVirtual { function, .. } => Some(bundle[function].to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(calls, ["Get"]);
}

#[test]
fn covariance_keeps_virtual_names_and_written_return_metadata_after_roundtrip() {
    let bundle = compile(
        "
        class Value {} class Sub extends Value {} class Sibling extends Value {}
        class Base { public function Get() -> Value { return NULL; } }
        class Middle extends Base { public override function Get() -> Sub { return NULL; } }
        class Child extends Middle { public override function Get() -> Sibling { return NULL; } }
        class Leaf extends Child { public override function Get() -> Value { return NULL; } }
        function BaseCall(x: Base) -> Value { return x.Get(); }
        function MiddleCall(x: Middle) -> Sub { return x.Get(); }
        function ChildCall(x: Child) -> Sibling { return x.Get(); }
    ",
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let mut found = 0;
    for def in bundle.indexed_definitions() {
        if let IndexedDefinition::Function(_, method) = def
            && &*bundle[method.name()] == "Get;"
        {
            let owner = &*bundle[bundle[method.class().unwrap()].name()];
            let expected = match owner {
                "Base" | "Leaf" => "ref:Value",
                "Middle" => "ref:Sub",
                "Child" => "ref:Sibling",
                _ => continue,
            };
            assert_eq!(
                &*bundle[bundle[method.return_type().unwrap()].name()],
                expected
            );
            found += 1;
        }
    }
    assert_eq!(found, 4);
    for name in ["BaseCall;Base", "MiddleCall;Middle", "ChildCall;Child"] {
        let calls = function(&bundle, name)
            .body()
            .code_iter()
            .filter_map(|instr| match instr.unwrap() {
                Instr::InvokeVirtual { function, .. } => Some(bundle[function].to_string()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(calls, ["Get;"]);
    }
}

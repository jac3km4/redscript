use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, Function, IndexedDefinition, Instr, ScriptBundle, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> ScriptBundle<'static> {
    compile_with_native_root(source, false)
}

fn compile_with_native_root(source: &str, native_root: bool) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("slots.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
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
    bundle.into_owned()
}

fn function<'a>(bundle: &'a ScriptBundle<'_>, name: &str) -> &'a Function<'a> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function) if &*bundle[function.name()] == name => {
                Some(function)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function {name}"))
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

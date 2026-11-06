use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostic, Diagnostics, SourceMapExt, TypeFlagRegistry, TypeInterner,
};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::infer_from_sources;
use redscript_io::{Class, ClassFlags, Definition, ScriptBundle, Visibility};

#[test]
fn concrete_field_default_owners_survive_serialization() {
    let mut metadata = Vec::new();
    for (prefix, value) in [
        ("", 23),
        (
            "module Defaults\n@nameImplementation(IntHolder as Holder<Int32>)",
            37,
        ),
    ] {
        let bundle = compile(&format!(
            "{prefix}
             struct Holder<T> {{ let value: T; let count: Int32 = {value}; }}
             func Use(first: Holder<Int32>, second: Holder<Bool>) {{}}"
        ));
        for definition in bundle.definitions() {
            if let Definition::Class(class) = definition
                && !matches!(&*bundle[class.name()], "IScriptable" | "Void" | "Nothing")
            {
                let fields = class
                    .fields()
                    .iter()
                    .map(|&index| {
                        let field = &bundle[index];
                        format!(
                            "{} (owner: {}): {:?}",
                            bundle[field.name()],
                            bundle[bundle[field.class()].name()],
                            field.defaults()
                        )
                    })
                    .collect::<Vec<_>>();
                metadata.push(format!("{}: {}", bundle[class.name()], fields.join(", ")));
            }
        }
    }
    metadata.sort();
    insta::assert_snapshot!(metadata.join("\n"), @r#"
    Defaults.Holder<Bool>: value (owner: Defaults.Holder<Bool>): [], count (owner: Defaults.Holder<Bool>): [Property { name: "Defaults.Holder<Bool>", value: "37" }]
    Defaults.IntHolder: value (owner: Defaults.IntHolder): [], count (owner: Defaults.IntHolder): [Property { name: "Defaults.IntHolder", value: "37" }]
    Holder<Bool>: value (owner: Holder<Bool>): [], count (owner: Holder<Bool>): [Property { name: "Holder<Bool>", value: "23" }]
    Holder<Int32>: value (owner: Holder<Int32>): [], count (owner: Holder<Int32>): [Property { name: "Holder<Int32>", value: "23" }]
    "#);
}

fn compile(source: &str) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("default_owners.reds", source);
    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    let diagnostics = reporter.into_reported();
    assert!(
        !diagnostics.iter().any(Diagnostic::is_error),
        "{}",
        Diagnostics::from(diagnostics)
    );
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    // Reloading nonempty defaults also checks that the serialized has_default flag is set.
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    ScriptBundle::from_bytes(&bytes).unwrap().into_owned()
}

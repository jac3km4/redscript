use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, infer_from_sources, predef,
};
use redscript_io::{Class, ClassFlags, ScriptBundle, Visibility};

fn assert_root(symbols: &Symbols<'_>) {
    assert!(
        symbols[predef::ISERIALIZABLE]
            .schema()
            .base_type()
            .is_none()
    );
    assert_eq!(
        symbols[predef::ISCRIPTABLE]
            .schema()
            .base_type()
            .unwrap()
            .id(),
        predef::ISERIALIZABLE,
    );
}

#[test]
fn binary_serializable_becomes_the_root_in_either_definition_order() {
    for names in [
        ["ISerializable", "IScriptable"],
        ["IScriptable", "ISerializable"],
    ] {
        let mut bundle = ScriptBundle::default();
        for name in names {
            let name = bundle.cnames_mut().add(name);
            bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
        }
        let interner = TypeInterner::default();
        let flags = TypeFlagRegistry::default();
        let (symbols, _) = CompilationInputs::load(&bundle, &interner, &flags)
            .unwrap()
            .into_inner();
        assert_root(&symbols);
        let symbols = CompilationInputs::load_without_mapping(&bundle, &interner, &flags).unwrap();
        assert_root(&symbols);
    }
}

#[test]
fn existing_binary_ancestry_is_preserved() {
    let mut bundle = ScriptBundle::default();
    let mut base = None;
    for name in ["ISerializable", "Intermediate", "IScriptable"] {
        let name = bundle.cnames_mut().add(name);
        base =
            Some(bundle.define(
                Class::new(name, Visibility::Public, ClassFlags::default()).with_base(base),
            ));
    }
    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    assert_eq!(
        symbols[predef::ISCRIPTABLE]
            .schema()
            .base_type()
            .unwrap()
            .id(),
        interner.intern("Intermediate"),
    );
}

#[test]
fn roots_can_be_split_between_source_and_binary() {
    for (binary, source) in [
        ("IScriptable", "class ISerializable {}"),
        ("ISerializable", "class IScriptable {}"),
    ] {
        let sources = SourceMap::new();
        let mut bundle = ScriptBundle::default();
        let name = bundle.cnames_mut().add(binary);
        bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
        let interner = TypeInterner::default();
        let symbols = CompilationInputs::load_without_mapping(
            &bundle,
            &interner,
            &TypeFlagRegistry::default(),
        )
        .unwrap();
        sources.populate_boot_lib();
        sources.push_back("root.reds", source);
        sources.push_back(
            "child.reds",
            "class Child {} func Upcast(value: Child) -> ISerializable { return value; }",
        );
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        assert_root(&symbols);
    }
}

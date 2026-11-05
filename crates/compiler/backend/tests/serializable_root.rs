use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, Type, TypeInterner, infer_from_sources, ir, predef,
};
use redscript_io::{
    Class, ClassFlags, Function, FunctionFlags, Parameter, ParameterFlags, ScriptBundle,
    Type as PoolType, TypeKind, Visibility,
};

fn assert_is_defined_parameter<'ctx>(
    symbols: &Symbols<'ctx>,
    root: redscript_compiler_frontend::TypeId<'ctx>,
) {
    let mut found = false;
    for entry in symbols.free_functions() {
        match entry.func().intrinsic() {
            Some(ir::Intrinsic::IsDefined) => {
                assert_eq!(
                    entry.func().type_().params()[0].type_(),
                    &Type::nullary(root),
                );
                found = true;
            }
            Some(ir::Intrinsic::VariantIsDefined) => {
                assert_eq!(
                    entry.func().type_().params()[0].type_(),
                    &Type::nullary(predef::VARIANT)
                );
            }
            _ => {}
        }
    }
    assert!(found);
}

#[test]
fn is_defined_keeps_scriptable_without_serializable() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "check.reds",
        "func Check(value: IScriptable) -> Bool = IsDefined(value);",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_is_defined_parameter(&symbols, predef::ISCRIPTABLE);
}

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
fn binary_root_methods_are_linked_after_connecting_the_hierarchy() {
    for names in [
        ["ISerializable", "IScriptable", "BinaryChild"],
        ["BinaryChild", "IScriptable", "ISerializable"],
    ] {
        for explicit_base in [false, true] {
            let mut bundle = ScriptBundle::default();
            let int_name = bundle.cnames_mut().add("Int32");
            let int_type = bundle.define(PoolType::new(int_name, TypeKind::Primitive));
            let mut classes = vec![];
            for name in names {
                let pool_name = bundle.cnames_mut().add(name);
                let class = bundle.define(Class::new(
                    pool_name,
                    Visibility::Public,
                    ClassFlags::default(),
                ));
                for (name, parameter) in [("Check;", false), ("Check;Int32", true)] {
                    let name = bundle.cnames_mut().add(name);
                    let function = Function::new(
                        name,
                        Visibility::Public,
                        FunctionFlags::default().with_is_native(true),
                    )
                    .with_class(Some(class))
                    .with_source(None);
                    let function = bundle.define_and_init(function, |bundle, index, function| {
                        if parameter {
                            let name = bundle.cnames_mut().add("value");
                            let param = bundle.define(Parameter::new(
                                name,
                                index,
                                int_type,
                                ParameterFlags::default(),
                            ));
                            function.with_parameters([param])
                        } else {
                            function
                        }
                    });
                    bundle[class].add_method(function);
                }
                classes.push((name, class));
            }
            let class = |name| classes.iter().find(|(n, _)| *n == name).unwrap().1;
            bundle[class("BinaryChild")] = bundle[class("BinaryChild")]
                .clone()
                .with_base(Some(class("IScriptable")));
            if explicit_base {
                bundle[class("IScriptable")] = bundle[class("IScriptable")]
                    .clone()
                    .with_base(Some(class("ISerializable")));
            }

            let sources = SourceMap::new();
            sources.populate_boot_lib();
            sources.push_back(
                "calls.reds",
                "class Child extends BinaryChild {
                     public func Check() {}
                 }
                 func Calls(root: ISerializable, script: IScriptable, binary: BinaryChild, child: Child) {
                     root.Check(); root.Check(1);
                     script.Check(); script.Check(1);
                     binary.Check(); binary.Check(1);
                     child.Check(); child.Check(1);
                 }",
            );
            let interner = TypeInterner::default();
            let flags = TypeFlagRegistry::default();
            for symbols in [
                CompilationInputs::load(&bundle, &interner, &flags)
                    .unwrap()
                    .into_inner()
                    .0,
                CompilationInputs::load_without_mapping(&bundle, &interner, &flags).unwrap(),
            ] {
                for (name, parent) in [
                    ("IScriptable", "ISerializable"),
                    ("BinaryChild", "IScriptable"),
                ] {
                    let methods = symbols
                        .query_methods_by_name(interner.intern(name), "Check")
                        .collect::<Vec<_>>();
                    assert_eq!(methods.len(), 2);
                    for method in methods {
                        assert_eq!(method.key().parent(), interner.intern(name));
                        let base = method.func().overriden().unwrap();
                        assert_eq!(base.parent(), interner.intern(parent));
                        assert_eq!(
                            symbols[base].type_().params().len(),
                            method.func().type_().params().len(),
                        );
                    }
                }

                let mut reporter = CompileErrorReporter::default();
                infer_from_sources(&sources, symbols, &mut reporter, &interner);
                assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
            }
        }
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
            "class Child {} func Upcast(value: Child) -> ISerializable { return value; }
             func Check(value: ISerializable, weak: wref<ISerializable>, child: Child, variant: Variant) {
                 let _: Bool = IsDefined(value);
                 let _: Bool = IsDefined(weak);
                 let _: Bool = IsDefined(child);
                 let empty: ISerializable = null;
                 let _: Bool = IsDefined(empty);
                 let _: Bool = IsDefined(variant);
             }",
        );
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        assert_root(&symbols);
        assert_is_defined_parameter(&symbols, predef::ISERIALIZABLE);
    }
}

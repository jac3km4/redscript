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
fn identical_final_native_is_a_methods_on_both_roots_are_unambiguous() {
    for roots in [
        ["ISerializable", "IScriptable"],
        ["IScriptable", "ISerializable"],
    ] {
        for (method_name, added_from_source) in
            [("IsA", true), ("IsA", false), ("IsA;CName", false)]
        {
            let mut bundle = ScriptBundle::default();
            let name = bundle.cnames_mut().add("CName");
            let cname_type = bundle.define(PoolType::new(name, TypeKind::Primitive));
            let name = bundle.cnames_mut().add("Bool");
            let bool_type = bundle.define(PoolType::new(name, TypeKind::Primitive));
            for root in roots {
                let name = bundle.cnames_mut().add(root);
                let class = bundle.define(Class::new(
                    name,
                    Visibility::Public,
                    ClassFlags::default().with_is_native(true),
                ));
                if root == "ISerializable" && added_from_source {
                    continue;
                }
                let name = bundle.cnames_mut().add(method_name);
                let function = Function::new(
                    name,
                    Visibility::Public,
                    FunctionFlags::default()
                        .with_is_native(true)
                        .with_is_final(true),
                )
                .with_class(Some(class))
                .with_return_type(Some(bool_type))
                .with_source(None);
                let function = bundle.define_and_init(function, |bundle, index, function| {
                    let name = bundle.cnames_mut().add("className");
                    let parameter = bundle.define(Parameter::new(
                        name,
                        index,
                        cname_type,
                        ParameterFlags::default(),
                    ));
                    function.with_parameters([parameter])
                });
                bundle[class].add_method(function);
            }

            let sources = SourceMap::new();
            sources.populate_boot_lib();
            if added_from_source {
                sources.push_back(
                    "codeware.reds",
                    "@addMethod(ISerializable)
                     public final native func IsA(className: CName) -> Bool;",
                );
            }
            sources.push_back(
                "is_a.reds",
                "class Child {
                     public func Test(className: CName) -> Bool = this.IsA(className);
                 }
                 func FromRoot(value: ISerializable, className: CName) -> Bool = value.IsA(className);
                 func FromScriptable(value: IScriptable, className: CName) -> Bool = value.IsA(className);
                 func FromChild(value: Child, className: CName) -> Bool = value.IsA(className);",
            );
            let invalid_sources = SourceMap::new();
            invalid_sources.populate_boot_lib();
            if added_from_source {
                invalid_sources.push_back(
                    "codeware.reds",
                    "@addMethod(ISerializable)
                     public final native func IsA(className: CName) -> Bool;",
                );
            }
            invalid_sources.push_back(
                "invalid.reds",
                "class InvalidChild { public func IsA(className: CName) -> Bool = false; }",
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
                let mut reporter = CompileErrorReporter::default();
                infer_from_sources(&invalid_sources, symbols.clone(), &mut reporter, &interner);
                assert!(
                    matches!(
                        reporter.reported(),
                        [redscript_compiler_frontend::Diagnostic::FinalMethodOverride("IsA", _)]
                    ),
                    "{:?}",
                    reporter.reported()
                );
                let mut reporter = CompileErrorReporter::default();
                let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
                assert!(
                    reporter.reported().is_empty(),
                    "{roots:?}, {method_name}, added={added_from_source}: {:?}",
                    reporter.reported()
                );
                let methods = symbols
                    .query_methods_by_name(predef::ISCRIPTABLE, "IsA")
                    .collect::<Vec<_>>();
                assert_eq!(methods.len(), 1, "{roots:?}, {method_name}: {methods:?}");
                let method = &methods[0];
                assert_eq!(method.key().parent(), predef::ISCRIPTABLE);
                assert!(method.func().flags().is_final());
                let base = method.func().overriden().unwrap();
                assert_eq!(base.parent(), predef::ISERIALIZABLE);
                assert!(symbols[base].flags().is_final());
                assert_eq!(method.func().type_().params()[0].name(), "className");
                assert_eq!(
                    method.func().type_().params()[0].type_(),
                    &Type::nullary(predef::CNAME)
                );
                assert_eq!(
                    method.func().type_().return_type(),
                    &Type::nullary(predef::BOOL)
                );

                assert_eq!(
                    symbols
                        .query_methods_by_name(interner.intern("Child"), "IsA")
                        .count(),
                    1
                );
            }
        }
    }
}

#[test]
fn root_reconciliation_does_not_hide_distinct_overloads_or_static_methods() {
    let mut bundle = ScriptBundle::default();
    for root in ["IScriptable", "ISerializable"] {
        let name = bundle.cnames_mut().add(root);
        bundle.define(Class::new(
            name,
            Visibility::Public,
            ClassFlags::default().with_is_native(true),
        ));
    }
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "overloads.reds",
        "@addMethod(IScriptable)
         public final native func IsA(className: CName) -> Bool;
         @addMethod(ISerializable)
         public final native func IsA(className: Int32) -> Bool;
         @addMethod(IScriptable)
         public final native static func Check() -> Bool;
         @addMethod(ISerializable)
         public final native static func Check() -> Bool;
         func CheckName(value: IScriptable, name: CName) -> Bool = value.IsA(name);
         func CheckIndex(value: IScriptable, index: Int32) -> Bool = value.IsA(index);",
    );
    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    for name in ["IsA", "Check"] {
        let methods = symbols
            .query_methods_by_name(predef::ISCRIPTABLE, name)
            .collect::<Vec<_>>();
        assert_eq!(methods.len(), 2);
        assert!(
            methods
                .iter()
                .all(|entry| entry.func().overriden().is_none())
        );
    }
}

#[test]
fn source_added_root_methods_keep_covariant_clone_returns() {
    let mut bundle = ScriptBundle::default();
    for root in ["IScriptable", "ISerializable"] {
        let name = bundle.cnames_mut().add(root);
        bundle.define(Class::new(
            name,
            Visibility::Public,
            ClassFlags::default().with_is_native(true),
        ));
    }
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "clone.reds",
        "@addMethod(IScriptable)
         public final native func Clone() -> ref<IScriptable>;
         @addMethod(ISerializable)
         public final native func Clone() -> ref<ISerializable>;
         func FromRoot(value: ISerializable) -> ISerializable = value.Clone();
         func FromScriptable(value: IScriptable) -> IScriptable = value.Clone();",
    );
    let interner = TypeInterner::default();
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    for root in [predef::ISCRIPTABLE, predef::ISERIALIZABLE] {
        let methods = symbols
            .query_methods_by_name(root, "Clone")
            .collect::<Vec<_>>();
        assert_eq!(methods.len(), 1);
        assert!(methods[0].func().flags().is_final());
        assert_eq!(
            methods[0].func().type_().return_type(),
            &Type::nullary(root)
        );
    }
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
}

#[test]
fn scriptable_methods_stay_final_without_serializable() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    let class = bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let name = bundle.cnames_mut().add("IsA");
    let function = bundle.define(
        Function::new(
            name,
            Visibility::Public,
            FunctionFlags::default()
                .with_is_native(true)
                .with_is_final(true),
        )
        .with_class(Some(class))
        .with_source(None),
    );
    bundle[class].add_method(function);
    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let method = symbols
        .query_methods_by_name(predef::ISCRIPTABLE, "IsA")
        .next()
        .unwrap();
    assert!(method.func().flags().is_final());
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

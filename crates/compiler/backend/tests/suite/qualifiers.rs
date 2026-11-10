use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, TypeFlagRegistry, TypeInterner};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{Field, Symbols, infer_from_sources};
use redscript_io::{Class, ClassFlags, IndexedDefinition, ScriptBundle, Visibility};

const PARAMETER_FLAGS: [(&str, bool, bool, bool); 8] = [
    ("plain", false, false, false),
    ("output", false, true, false),
    ("constant", false, false, true),
    ("constantOutput", false, true, true),
    ("optionalValue", true, false, false),
    ("optionalOutput", true, true, false),
    ("optionalConstant", true, false, true),
    ("optionalConstantOutput", true, true, true),
];

fn assert_parameter_symbol_flags<'ctx>(symbols: &Symbols<'ctx>, interner: &'ctx TypeInterner) {
    let aggregate = symbols[interner.intern("QualifiedParameters")]
        .schema()
        .as_aggregate()
        .unwrap();
    let methods = aggregate.methods().by_name("Check").collect::<Vec<_>>();
    assert_eq!(methods.len(), 1);
    let actual = methods[0]
        .func()
        .type_()
        .params()
        .iter()
        .map(|param| {
            let flags = param.flags();
            (
                param.name(),
                flags.is_optional(),
                flags.is_out(),
                flags.is_const(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(actual, PARAMETER_FLAGS);
}

fn assert_parameter_pool_flags(bundle: &ScriptBundle<'_>) {
    let functions = bundle
        .indexed_definitions()
        .filter_map(|def| match def {
            IndexedDefinition::Function(_, func)
                if func.class().is_some_and(|class| {
                    &*bundle[bundle[class].name()] == "QualifiedParameters"
                }) && bundle[func.name()].split(';').next() == Some("Check") =>
            {
                Some(func)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(functions.len(), 1);
    let actual = functions[0]
        .parameters()
        .iter()
        .map(|&index| {
            let param = &bundle[index];
            let flags = param.flags();
            (
                &*bundle[param.name()],
                flags.is_optional(),
                flags.is_out(),
                flags.is_const(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(actual, PARAMETER_FLAGS);
}

#[test]
fn parameter_qualifiers_survive_emission_bundle_io_and_loading() {
    let sources = SourceMap::new();
    sources.push_back(
        "parameter_qualifiers.reds",
        r#"
        class QualifiedParameters {
            public func Check(
                plain: Int32,
                out output: Int32,
                const constant: Int32,
                const out constantOutput: Int32,
                opt optionalValue: Int32,
                opt out optionalOutput: Int32,
                opt const optionalConstant: Int32,
                opt const out optionalConstantOutput: Int32
            ) {}
        }
        "#,
    );
    sources.populate_boot_lib();
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
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_parameter_symbol_flags(&symbols, &interner);

    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    assert_parameter_pool_flags(&bundle);

    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    assert_parameter_pool_flags(&bundle);

    let reloaded =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    assert_parameter_symbol_flags(&reloaded, &interner);
}

fn assert_field_flags(field: &Field<'_>) {
    let flags = field.flags();
    assert!(flags.is_editable());
    assert!(flags.is_instance_editable());
    assert!(flags.is_inline());
    assert!(flags.is_const());
    assert!(flags.is_replicated());
    assert!(flags.is_test_only());
    assert!(flags.is_persistent());
    assert_eq!(field.hint().map(AsRef::as_ref), Some("Editor hint"));
}

#[test]
fn qualifiers_and_hints_survive_emission_and_loading() {
    let sources = SourceMap::new();
    sources.push_back(
        "qualifiers.reds",
        r#"
        testonly class Qualified {
            @hint("Editor hint")
            @runtimeProperty("category", "Testing")
            editable instanceeditable inlined const replicated testonly persistent let value: Int32;
            let plain: Int32;
        }
        testonly struct QualifiedStruct {
            testonly let value: Int32;
        }
        @addField(IScriptable)
        @hint("Editor hint")
        editable instanceeditable inlined const replicated testonly persistent let added: Int32;
        "#,
    );
    sources.populate_boot_lib();
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
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let qualified = symbols[interner.intern("Qualified")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert!(qualified.flags().is_test_only());
    assert_field_flags(qualified.fields().iter().next().unwrap().field());
    let base = symbols[interner.intern("IScriptable")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert_field_flags(base.fields().iter().next().unwrap().field());

    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();

    let mut qualified_classes = 0;
    let mut qualified_fields = 0;
    for def in bundle.indexed_definitions() {
        match def {
            IndexedDefinition::Class(_, class)
                if matches!(&*bundle[class.name()], "Qualified" | "QualifiedStruct") =>
            {
                assert!(class.flags().is_test_only());
                qualified_classes += 1;
            }
            IndexedDefinition::Field(_, field) => match &*bundle[field.name()] {
                "value" | "added" if field.hint().is_some() => {
                    let flags = field.flags();
                    assert!(flags.is_editable());
                    assert!(flags.is_instance_editable());
                    assert!(flags.is_inline());
                    assert!(flags.is_const());
                    assert!(flags.is_replicated());
                    assert!(flags.is_test_only());
                    assert!(flags.is_persistent());
                    assert_eq!(field.hint(), Some("Editor hint"));
                    qualified_fields += 1;
                }
                "plain" => {
                    assert_eq!(field.flags(), redscript_io::FieldFlags::default());
                    assert_eq!(field.hint(), None);
                }
                _ => {}
            },
            _ => {}
        }
    }
    assert_eq!(qualified_classes, 2);
    assert_eq!(qualified_fields, 2);

    let reloaded =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    for name in ["Qualified", "IScriptable"] {
        let aggregate = reloaded[interner.intern(name)]
            .schema()
            .as_aggregate()
            .unwrap();
        assert_eq!(aggregate.flags().is_test_only(), name == "Qualified");
        let field = aggregate
            .fields()
            .iter()
            .find(|entry| entry.field().hint().is_some())
            .unwrap();
        assert_field_flags(field.field());
    }
}

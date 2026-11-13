use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{Class, ClassFlags, IndexedDefinition, Property, ScriptBundle, Visibility};
use redscript_redmod_adapter::{
    CapabilityError, Error, parse_file, parse_file_with_metadata, process_sources,
};

fn compile(native: &[&str], dependency: &str, bundle: ScriptBundle<'_>) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("dependency.reds", dependency);
    let mut modules = Vec::new();
    let mut native_files = Vec::new();
    for (i, source) in native.iter().enumerate() {
        let file = sources.push_back(format!("defaults{i}.script"), *source);
        native_files.push(file);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        modules.push(parsed.unwrap());
    }
    let dependencies = sources
        .files()
        .filter(|(id, _)| !native_files.contains(id))
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut bundle = bundle;
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) =
        process_sources(modules, dependencies, symbols, &mut reporter, &interner);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    // Defaults are metadata, not synthesized initialization functions/statements.
    for class in unit
        .classes
        .values()
        .filter(|class| native_files.contains(&class.span.file))
    {
        assert!(class.methods.is_empty());
    }
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_owned()
}

fn empty_bundle() -> ScriptBundle<'static> {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    bundle
}

fn properties(bundle: &ScriptBundle<'_>, owner: &str, name: &str) -> Vec<(String, String)> {
    let field = bundle
        .indexed_definitions()
        .find_map(|definition| match definition {
            IndexedDefinition::Field(_, field)
                if &*bundle[field.name()] == name
                    && &*bundle[bundle[field.class()].name()] == owner =>
            {
                Some(field)
            }
            _ => None,
        })
        .unwrap_or_else(|| {
            let available = bundle
                .indexed_definitions()
                .filter_map(|definition| match definition {
                    IndexedDefinition::Field(_, field) => Some(format!(
                        "{}.{}",
                        &*bundle[bundle[field.class()].name()],
                        &*bundle[field.name()]
                    )),
                    _ => None,
                })
                .collect::<Vec<_>>();
            panic!("missing {owner}.{name}; available: {available:?}")
        });
    assert_ne!(u16::from(field.flags()) & (1 << 7), 0);
    field
        .defaults()
        .iter()
        .map(|p| (p.name().to_owned(), p.value().to_owned()))
        .collect()
}

fn assert_props(bundle: &ScriptBundle<'_>, owner: &str, name: &str, expected: &[(&str, &str)]) {
    assert_eq!(
        properties(bundle, owner, name),
        expected
            .iter()
            .map(|&(k, v)| (k.to_owned(), v.to_owned()))
            .collect::<Vec<_>>()
    );
}

#[test]
fn community_defaults_use_shared_aggregate_lowering() {
    let bundle = compile(
        &[],
        r#"
        public class Base {
            default value = -128;
            public let value: Int8 = 2;
            default value = "ignored";
            public let real: Double;
            default real = -1.5;
        }
        public class Derived extends Base { default value = 3; default value = 4; }
        "#,
        empty_bundle(),
    );
    let props = properties(&bundle, "Base", "value");
    assert_eq!(props.len(), 2);
    assert!(props.contains(&("Base".into(), "-128".into())));
    assert!(props.contains(&("Derived".into(), "4".into())));
    assert_props(&bundle, "Base", "real", &[("Base", "-1.5")]);
}

#[test]
fn inherited_defaults_bind_after_all_files_and_preserve_declaring_identity() {
    let bundle = compile(
        &[
            "class Derived extends Middle { default value = 2; default value = 3; } class Sibling extends Base { default value = 4; }",
            "class Middle extends Base {} class Base extends IScriptable { private var value: Int32; default value = 1; }",
        ],
        "",
        empty_bundle(),
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let props = properties(&bundle, "Base", "value");
    // Class collection order is not a semantic ordering between different keys.
    assert_eq!(props.len(), 3);
    for (key, value) in [("Base", "1"), ("Derived", "3"), ("Sibling", "4")] {
        assert!(
            props.contains(&(key.to_owned(), value.to_owned())),
            "{props:?}"
        );
    }
    let fields = bundle
        .indexed_definitions()
        .filter(|d| matches!(d, IndexedDefinition::Field(_, f) if &*bundle[f.name()] == "value"))
        .count();
    assert_eq!(fields, 1);
}

#[test]
fn own_defaults_are_first_wins_including_inline_initializers() {
    let bundle = compile(
        &[r#"
        class First { default value = 1; var value: Int32 = 2; default value = "ignored"; }
        class Inline { var value: Int32 = 4; default value = 5; }
        struct Structure { default value = -1.5; var value: Float; }
    "#],
        "",
        empty_bundle(),
    );
    assert_props(&bundle, "First", "value", &[("First", "1")]);
    assert_props(&bundle, "Inline", "value", &[("Inline", "4")]);
    assert_props(&bundle, "Structure", "value", &[("Structure", "-1.5")]);
}

#[test]
fn literals_are_typed_using_the_field_and_enum_members_use_module_scope() {
    let bundle = compile(
        &[r#"
        enum Choice { A = 0, B = 1 }
        class Values {
          var small: Int8; default small = -128;
          var wide: Int64; default wide = 12;
          var minimum: Int32; default minimum = -2147483648;
          var longMinimum: Int64; default longMinimum = -9223372036854775808l;
          var real: Double; default real = -1.5;
          var zero: Float; default zero = -0.0;
          var trailing: Float; default trailing = 1.000;
          var precise: Double; default precise = 1.23456789;
          var flag: Bool; default flag = true;
          var text: String; default text = "text";
          var name: CName; default name = n"name";
          var record: TweakDBID; default record = T"record";
          var resource: ResRef; default resource = R"path";
          var choice: Choice; default choice = Choice.B;
        }
    "#],
        "",
        empty_bundle(),
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    for (field, value) in [
        ("small", "-128"),
        ("wide", "12"),
        ("minimum", "-2147483648"),
        ("longMinimum", "-9223372036854775808"),
        ("real", "-1.5"),
        ("zero", "-0.0"),
        ("trailing", "1.000"),
        ("precise", "1.23456789"),
        ("flag", "true"),
        ("text", "text"),
        ("name", "name"),
        ("record", "record"),
        ("resource", "path"),
        ("choice", "Choice.B"),
    ] {
        assert_props(&bundle, "Values", field, &[("Values", value)]);
    }
}

#[test]
fn defaults_on_imported_fields_merge_without_erasing_bundle_properties() {
    let base = compile(
        &["class Base extends IScriptable { var value: Int32; default value = 1; }"],
        "",
        empty_bundle(),
    );
    let mut base = base;
    let field = base
        .indexed_definitions()
        .find_map(|d| match d {
            IndexedDefinition::Field(i, f) if &*base[f.name()] == "value" => Some(i),
            _ => None,
        })
        .unwrap();
    base[field].set_defaults([Property::new("Base", "1"), Property::new("Unrelated", "9")]);
    let bytes = base.into_writeable().to_bytes().unwrap();
    let base = ScriptBundle::from_bytes(&bytes).unwrap();
    let result = compile(
        &["class Derived extends Base { default value = 2; }"],
        "@addField(Base) public let extra: Int32 = 9;",
        base,
    );
    assert_props(&result, "Base", "extra", &[("Base", "9")]);
    let bytes = result.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    assert_props(
        &bundle,
        "Base",
        "value",
        &[("Base", "1"), ("Unrelated", "9"), ("Derived", "2")],
    );
}

#[test]
fn generic_dependency_fields_use_the_concrete_base_instantiation() {
    let bundle = compile(
        &["class Derived extends ConcreteBase { default value = 7; }"],
        "public class GenericBase<T> { public let value: T; } public class ConcreteBase extends GenericBase<Int32> {}",
        empty_bundle(),
    );
    assert_props(&bundle, "GenericBase<Int32>", "value", &[("Derived", "7")]);
}

#[test]
fn ast_defaults_clone_with_their_original_expression_and_source_spans() {
    let source = "class Host { var value: Float; default value = -1.500; }";
    let sources = SourceMap::new();
    let file = sources.push_back("spans.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap().clone();
    // Processing a cloned ParsedModule is the same path used by IDE snapshots.
    // The directive and its retained spelling live in the shared tree.
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        [parsed],
        [],
        redscript_compiler_frontend::Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let default = &unit.classes[&interner.intern("Host")].defaults[0];
    assert_eq!(default.literal_text, Some("-1.500"));
    assert_eq!(
        &source[default.span.start as usize..default.span.end as usize],
        "default value = -1.500;"
    );
    let (native, errors) = redscript_redmod_parser::parse_module(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let lowered = redscript_redmod_adapter::bridge::lower(native.unwrap()).unwrap();
    // The declaration remains a field without an injected initializer.
    let redscript_community_ast::Item::Class(class) = &lowered.module.items[0].0.item else {
        panic!()
    };
    let redscript_community_ast::Item::Let(field) = &class.items[0].0.item else {
        panic!()
    };
    assert!(field.default.is_none());
    let redscript_community_ast::Item::Default(default) = &class.items[1].0.item else {
        panic!()
    };
    assert_eq!(default.target.0, "value");
    assert_eq!(
        &source[default.value.1.start as usize..default.value.1.end as usize],
        "-1.500"
    );
}

#[test]
fn defaults_stay_with_the_accepted_aggregate_declaration() {
    use redscript_compiler_frontend::{Diagnostic, Symbols};
    let source =
        "class Host { var value: Int32; default value = 1; } class Host { default value = 2; }";
    let sources = SourceMap::new();
    let file = sources.push_back("duplicate.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        parsed,
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(matches!(
        reporter.reported(),
        [Diagnostic::NameRedefinition(..)]
    ));
    let defaults = &unit.classes[&interner.intern("Host")].defaults;
    assert_eq!(defaults.len(), 1);
    assert_eq!(defaults[0].literal_text, Some("1"));
}

#[test]
fn target_and_value_diagnostics_keep_written_spans() {
    use redscript_compiler_frontend::{Diagnostic, LowerError, Symbols};
    let source = r#"class Bad { var value: Int8; default missing = 1; default value = 128; }
        class Wrong { var value: Int32; default value = "bad"; }"#;
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("diagnostics.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, f)| {
            redscript_community_parser::parse_module(f.source(), id)
                .0
                .unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert_eq!(reporter.reported().len(), 3, "{:?}", reporter.reported());
    for error in reporter.reported() {
        let Diagnostic::LoweringError(error) = error else {
            panic!("{error:?}")
        };
        let span = error.span();
        let written = &source[span.start as usize..span.end as usize];
        match error {
            LowerError::UnresolvedMember(_, "missing", _) => assert_eq!(written, "missing"),
            LowerError::LiteralOutOfRange(_, _) => assert_eq!(written, "128"),
            LowerError::Type(_, _) => assert_eq!(written, "\"bad\""),
            _ => panic!("{error:?}"),
        }
    }
}

#[test]
fn enum_defaults_resolve_in_the_supplying_module_import_scope() {
    let bundle = compile(
        &[
            "@module \"Models\"\npublic enum Choice { A = 0, B = 1 } public class Base { var value: Int32; default value = 1; }",
            "@module \"Consumers\"\n@using \"Models\"; class Derived extends Base { default value = 2; var choice: Choice; default choice = Choice.B; }",
        ],
        "",
        empty_bundle(),
    );

    let props = properties(&bundle, "Models.Base", "value");
    assert!(props.contains(&("Models.Base".into(), "1".into())));
    assert!(props.contains(&("Consumers.Derived".into(), "2".into())));
    assert_props(
        &bundle,
        "Consumers.Derived",
        "choice",
        &[("Consumers.Derived", "Choice.B")],
    );
}

#[test]
fn bare_ast_api_preserves_defaults_and_richer_native_expressions_are_rejected() {
    let sources = SourceMap::new();
    let source = "class Host { var value: Int32; default value = 1; }";
    let file = sources.push_back("valid.script", source);
    let (parsed, errors) = parse_file(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let redscript_community_ast::Item::Class(class) = &parsed.unwrap().items[0].0.item else {
        panic!()
    };
    assert!(matches!(
        class.items[1].0.item,
        redscript_community_ast::Item::Default(_)
    ));
    for source in [
        "class Host { var value: Int32; default value = 1 + 2; }",
        "class Host { var value: Int32; default value = MakeValue(); }",
    ] {
        let file = sources.push_back("invalid.script", source);
        let (parsed, errors) = parse_file(source, file);
        assert!(parsed.is_none());
        assert!(
            errors.iter().any(|e| matches!(
                e,
                Error::Capability {
                    kind: CapabilityError::DefaultDirective,
                    ..
                }
            )),
            "{errors:?}"
        );
    }
}

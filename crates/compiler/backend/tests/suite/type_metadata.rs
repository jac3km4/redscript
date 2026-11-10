use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostics, SourceMapExt, TypeFlagRegistry, TypeInterner,
};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::infer_from_sources;
use redscript_io::{Class, ClassFlags, Definition, ScriptBundle, TypeIndex, TypeKind, Visibility};

#[test]
fn const_local_and_method_flags_survive_emission_and_loading() {
    let bundle = compile("class V { public const func Read() {} } func F() { const x: V; }");
    let local = bundle
        .definitions()
        .find_map(|def| match def {
            Definition::Local(local) => Some(local),
            _ => None,
        })
        .unwrap();
    assert!(local.flags().is_const());
    let method = bundle
        .definitions()
        .find_map(|def| match def {
            Definition::Function(method) if bundle[method.name()].starts_with("Read;") => {
                Some(method)
            }
            _ => None,
        })
        .unwrap();
    assert!(method.flags().is_const());
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let interner = TypeInterner::default();
    let (symbols, _) = CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
        .unwrap()
        .into_inner();
    let class = symbols
        .get_type(interner.intern("V"))
        .unwrap()
        .schema()
        .as_aggregate()
        .unwrap();
    assert!(
        class
            .methods()
            .iter()
            .any(|entry| entry.func().flags().is_const())
    );
}

#[test]
fn enum_widths() {
    // Unsuffixed enum initializers are Int32 tokens, so the negative limit's
    // magnitude (2147483648) and the 8-byte width require a wider literal.
    let cases = [
        (0, 1),
        (127, 1),
        (-128, 1),
        (128, 2),
        (-129, 2),
        (32_767, 2),
        (-32_768, 2),
        (32_768, 4),
        (-32_769, 4),
        (i32::MAX, 4),
        (-i32::MAX, 4),
    ];
    let source = cases
        .iter()
        .enumerate()
        .map(|(index, (value, _))| format!("enum Width{index} {{ Value = {value}, }}\n"))
        .collect::<String>();
    let bundle = compile(&source);

    for (index, (value, expected_size)) in cases.into_iter().enumerate() {
        let name = format!("Width{index}");
        let enum_ = bundle
            .definitions()
            .find_map(|def| match def {
                Definition::Enum(enum_) if bundle[enum_.name()] == name => Some(enum_),
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing enum {name}"));
        assert_eq!(enum_.size(), expected_size, "{name}: value {value}");
        assert_eq!(enum_.values().len(), 1, "{name}");
        assert_eq!(
            bundle[enum_.values()[0]].value(),
            i64::from(value),
            "{name}"
        );
    }
}

#[test]
fn explicit_enum_storage_survives_bundle_roundtrip_and_loading() {
    let bundle = compile(
        "enum Signed: Int8 { A = -128, B = 127 }
         enum Unsigned: Uint8 { A = 0, B = 255 }
         enum Wide: Int64 { A = 0 }
         enum Half: Uint16 { A = 65535 }
         enum Word: Uint32 { A = 4294967295l }",
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let sources = SourceMap::new();
    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let (_, mappings) = CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
        .unwrap()
        .into_inner();
    let mut monomorphizer = mappings.into_monomorphizer(&sources);
    let mut reemitted = ScriptBundle::default();
    for (name, size, values) in [
        ("Signed", 1, vec![-128, 127]),
        ("Unsigned", 1, vec![0, 255]),
        ("Wide", 8, vec![0]),
        ("Half", 2, vec![65_535]),
        ("Word", 4, vec![4_294_967_295]),
    ] {
        let enum_ = symbols[interner.intern(name)].schema().as_enum().unwrap();
        monomorphizer.add_enum_to_pool(interner.intern(name), &symbols, &mut reemitted);
        let emitted = reemitted
            .definitions()
            .find_map(|def| match def {
                Definition::Enum(enm) if reemitted[enm.name()] == name => Some(enm),
                _ => None,
            })
            .unwrap();
        assert_eq!(emitted.size(), size);
        assert_eq!(enum_.storage_size(), Some(size));
        assert_eq!(enum_.storage(), None, "bundle erases signedness");
        assert_eq!(
            enum_.variants().map(|(_, value)| value).collect::<Vec<_>>(),
            values
        );
    }
}

#[test]
fn static_array_lengths_and_element_types() {
    let bundle = compile(
        r#"
        func Arrays() {
          let empty: [Int32; 0];
          let single: [Int32; 1];
          let numbers: [Int32; 10];
          let flags: [Bool; 10];
          let regular: [Int32; 16];
          let large: [Int32; 4096];
          let nested: [[Bool; 2]; 3];
        }
        "#,
    );

    for (name, expected_size, expected_element) in [
        ("empty", 0, "Int32"),
        ("single", 1, "Int32"),
        ("numbers", 10, "Int32"),
        ("flags", 10, "Bool"),
        ("regular", 16, "Int32"),
        ("large", 4096, "Int32"),
    ] {
        let element = assert_static_array(&bundle, local_type(&bundle, name), expected_size);
        assert_eq!(&bundle[bundle[element].name()], expected_element, "{name}");
    }

    let inner = assert_static_array(&bundle, local_type(&bundle, "nested"), 3);
    let element = assert_static_array(&bundle, inner, 2);
    assert_eq!(&bundle[bundle[element].name()], "Bool");
}

#[test]
fn generic_structs_used_only_in_types_have_definitions() {
    let bundle = compile(
        r#"
        struct Box<T> { let value: T; }
        struct Outer { let inner: Box<Bool>; }
        func Accept(value: Box<Int32>) {}
        "#,
    );

    for (name, field_type) in [("Box<Int32>", "Int32"), ("Box<Bool>", "Bool")] {
        let classes = bundle
            .definitions()
            .filter_map(|def| match def {
                Definition::Class(class) if bundle[class.name()] == name => Some(class),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(classes.len(), 1, "{name}");
        let fields = classes[0].fields();
        assert_eq!(fields.len(), 1, "{name}");
        assert_eq!(
            &bundle[bundle[bundle[fields[0]].type_()].name()],
            field_type
        );
    }
}

#[test]
fn recursive_generic_fields_share_one_type_definition() {
    let bundle = compile(
        r#"
        struct Node<T> { let next: script_ref<Node<T>>; }
        func Accept(value: Node<Int32>) {}
        "#,
    );

    for name in ["Node<Int32>", "script_ref:Node<Int32>"] {
        assert_eq!(
            bundle
                .definitions()
                .filter(|def| matches!(def, Definition::Type(typ) if bundle[typ.name()] == name))
                .count(),
            1,
            "{name}"
        );
    }
}

fn local_type(bundle: &ScriptBundle<'_>, name: &str) -> TypeIndex {
    bundle
        .definitions()
        .find_map(|def| match def {
            Definition::Local(local) if bundle[local.name()].split('$').next() == Some(name) => {
                Some(local.type_())
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing local {name}"))
}

fn assert_static_array(
    bundle: &ScriptBundle<'_>,
    type_: TypeIndex,
    expected_size: u32,
) -> TypeIndex {
    let TypeKind::StaticArray { element_type, size } = bundle[type_].kind() else {
        panic!("expected static array, got {:?}", bundle[type_]);
    };
    assert_eq!(size, expected_size, "{}", bundle[bundle[type_].name()]);
    element_type
}

fn compile(source: &str) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.push_back("type_metadata.reds", source);
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
    let diagnostics = reporter.into_reported();
    assert_eq!(
        diagnostics.iter().filter(|d| d.is_error()).count(),
        0,
        "{}",
        Diagnostics::from(diagnostics)
    );

    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_owned()
}

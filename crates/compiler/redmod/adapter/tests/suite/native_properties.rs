use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::TypeFlagRegistry;
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{IndexedDefinition, ScriptBundle};
use redscript_redmod_adapter::{CapabilityError, Error, parse_file_with_metadata, process_sources};

use crate::support;

fn compile(source: &str) -> ScriptBundle<'static> {
    support::compile(source, "properties.script", &[], support::empty_bundle())
}

#[test]
fn mutable_array_fields_preserve_types_metadata_and_independent_flags() {
    let bundle = compile(
        r#"
        struct Data { var number: Int32; }
        struct Values { mutable editable var values: array<Bool>; }
        class Host {
            [category = "Authored"]
            mutable editable inlined persistent var first, second: array<Data>;
            instanceeditable mutable var names: array<CName>;
            mutable var objects: array<weak<Host>>;
            mutable editable var fixed: Int32[2];
            mutable const editable var explicitConst: array<Int32>;
            public function Mutate() { names.PushBack(n"Authored"); names.Clear(); }
        }
        "#,
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let mut seen = Vec::new();
    for definition in bundle.indexed_definitions() {
        let IndexedDefinition::Field(_, field) = definition else {
            continue;
        };
        let name = &*bundle[field.name()];
        if name == "number" {
            continue;
        }
        seen.push(name.to_owned());
        let flags = field.flags();
        assert_eq!(flags.is_const(), name == "explicitConst", "{name}");
        assert_eq!(flags.is_instance_editable(), name == "names", "{name}");
        assert_eq!(
            flags.is_editable(),
            matches!(
                name,
                "values" | "first" | "second" | "fixed" | "explicitConst"
            ),
            "{name}"
        );
        assert_eq!(flags.is_inline(), matches!(name, "first" | "second"));
        assert_eq!(flags.is_persistent(), matches!(name, "first" | "second"));
        if matches!(name, "first" | "second") {
            assert_eq!(field.attributes().len(), 1);
            assert_eq!(field.attributes()[0].name(), "category");
            assert_eq!(field.attributes()[0].value(), "Authored");
        }
        assert_eq!(
            &*bundle[bundle[field.type_()].name()],
            match name {
                "values" => "array:Bool",
                "first" | "second" => "array:Data",
                "names" => "array:CName",
                "objects" => "array:wref:Host",
                "fixed" => "Int32[2]",
                "explicitConst" => "array:Int32",
                _ => unreachable!(),
            }
        );
    }
    seen.sort();
    assert_eq!(
        seen,
        [
            "explicitConst",
            "first",
            "fixed",
            "names",
            "objects",
            "second",
            "values"
        ]
    );
}

#[test]
fn observed_mutable_float_preserves_explicit_const_and_flags() {
    let bundle = compile(
        "class Host { protected mutable instanceeditable inlined var value: Float = 1.0f; instanceeditable const mutable inlined var frozen: Float = 1.0f; public function Mutate() { value = 2.0f; } }",
    );
    let mut count = 0;
    for definition in bundle.indexed_definitions() {
        if let IndexedDefinition::Field(_, field) = definition {
            count += 1;
            assert!(field.flags().is_instance_editable());
            assert!(field.flags().is_inline());
            assert_eq!(field.flags().is_const(), &*bundle[field.name()] == "frozen");
        }
    }
    assert_eq!(count, 2);

    for modifiers in [
        "const mutable instanceeditable inlined",
        "mutable instanceeditable inlined const",
    ] {
        let source = format!(
            "class Host {{ {modifiers} var value: Float = 1.0f; public function Mutate() {{ value = 2.0f; }} }}"
        );
        let sources = SourceMap::new();
        let file = sources.push_back("const-float.script", &source);
        let (parsed, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols, errors) = process_sources(
            parsed,
            [],
            redscript_compiler_frontend::Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        let host = symbols[interner.intern("Host")]
            .schema()
            .as_aggregate()
            .unwrap();
        assert!(
            host.fields()
                .iter()
                .all(|field| field.field().flags().is_const()),
            "explicit const must survive either modifier order even in a writing body"
        );
    }
}

#[test]
fn mutable_does_not_enable_unmapped_types_or_declaration_targets() {
    for source in [
        "class Host { mutable var value: Int32; }",
        "class Host { mutable var value: Float; }",
        "class Host { mutable editable inlined var value: Float; }",
        "class Host { mutable instanceeditable var value: Float; }",
        "struct Host { mutable instanceeditable inlined var value: Float; }",
        "import class Host { mutable instanceeditable inlined var value: Float; }",
        "class Host { mutable instanceeditable inlined var value: Double; }",
        "class Host { mutable var value: String; }",
        "struct Data {} class Host { mutable var value: Data; }",
        "class Host { mutable var value: Host; }",
        "mutable var value: array<Int32>;",
        "mutable class Host {}",
        "mutable struct Host {}",
        "mutable function Run() {}",
        "class Host { mutable function Run() {} }",
    ] {
        rejected(
            source,
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Mutable),
            "mutable",
        );
    }
}

fn rejected(source: &str, expected: CapabilityError, text: &str) {
    let sources = SourceMap::new();
    let file = sources.push_back("properties.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(parsed.is_none(), "{source}");
    assert_eq!(errors.len(), 1, "{source}: {errors:?}");
    assert!(
        matches!(errors[0], Error::Capability { kind, .. } if kind == expected),
        "{source}: {errors:?}"
    );
    let span = errors[0].span();
    assert_eq!(span.file, file);
    assert_eq!(&source[span.start as usize..span.end as usize], text);
}

#[test]
fn profile_and_other_nonfield_metadata_are_not_dropped() {
    rejected(
        r#"[profile = ""] class Host {}"#,
        CapabilityError::Metadata,
        "profile = \"\"",
    );
    rejected(
        r#"class Host { [profile = true] public function Run() {} }"#,
        CapabilityError::Metadata,
        "profile = true",
    );
    rejected(
        "[runtimeProperty = true] class Host {}",
        CapabilityError::Metadata,
        "runtimeProperty = true",
    );
}

#[test]
fn typed_metadata_is_preserved_by_native_ast_and_not_arbitrarily_stringified() {
    for (literal, check) in [
        ("true", "bool"),
        ("7", "integer"),
        ("1.5", "float"),
        ("n\"Authored\"", "name"),
    ] {
        let source = format!("class Host {{ [runtimeProperty = {literal}] var value: Int32; }}");
        let sources = SourceMap::new();
        let file = sources.push_back("properties.script", &source);
        let (module, errors) = redscript_redmod_parser::parse_module(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let module = module.unwrap();
        let redscript_redmod_ast::Item::Aggregate { items, .. } = &module.items[0].0.item else {
            panic!("expected aggregate")
        };
        let value = &items[0].0.metadata[0].0.value.0;
        assert!(
            matches!(value, redscript_redmod_ast::Expr::Constant(constant) if match check {
                "bool" => matches!(constant, ast::Constant::Bool(true)),
                "integer" => matches!(constant, ast::Constant::I32(7)),
                "float" => matches!(constant, ast::Constant::F32(_)),
                "name" => matches!(constant, ast::Constant::CName(_)),
                _ => unreachable!(),
            }),
            "{value:?}"
        );
        rejected(&source, CapabilityError::Metadata, literal);
    }
}

#[test]
fn const_returns_keep_the_native_property() {
    for source in [
        "function Run() -> const Int32 { return 7; }",
        "class Host { public function Run() -> const Host { return this; } }",
        "class Host { public function Run() -> const array<Int32> { var value: array<Int32>; return value; } }",
        "import class Host { public import const function Run() -> const Int32; }",
    ] {
        // The imported example contains a separate method-const modifier.
        let sources = SourceMap::new();
        let file = sources.push_back("properties.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let parsed = parsed.unwrap();
        let decl = &parsed.module.items[0].0;
        let function = match &decl.item {
            ast::Item::Function(function) => function,
            ast::Item::Class(class) => match &class.items[0].0.item {
                ast::Item::Function(function) => function,
                _ => panic!("expected method"),
            },
            _ => panic!("expected function"),
        };
        assert!(function.properties.is_const_return);
    }
}

#[test]
fn function_properties_survive_symbols_emission_and_bundle_reload() {
    let source = r#"
        [profile = "Authored label"]
        function Scalar() -> const Int32 { return 7; }
        [profile = ""]
        function Array() -> const array<Int32> { var values: array<Int32>; return values; }
        function Plain() -> Int32 { return 8; }
        import function Imported() -> const Int32;
        function UseImported() -> Int32 { return Imported(); }
        class Host {
            [profile = "Method label"]
            public function Object() -> const Host { return this; }
            public function PlainMethod() -> Int32 { return 1; }
        }
        struct Value {
            [profile = "Struct label"]
            public static function Static() -> const Int32 { return 2; }
        }
    "#;
    let bundle = compile(source);
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let mut count = 0;
    for definition in bundle.indexed_definitions() {
        let IndexedDefinition::Function(_, function) = definition else {
            continue;
        };
        let name = bundle[function.name()].split(';').next().unwrap();
        let profiled = matches!(name, "Scalar" | "Array" | "Object" | "Static");
        if !profiled && !matches!(name, "Plain" | "PlainMethod" | "Imported") {
            continue;
        }
        count += 1;
        assert_eq!(
            function.is_const_return(),
            profiled || name == "Imported",
            "{name}"
        );
        assert!(
            !function.flags().is_const(),
            "return const must not become method const"
        );
        let code = function.body().code_owned().unwrap();
        if profiled {
            let redscript_io::Instr::Profile(profile) = &code[0] else {
                panic!("missing profile for {name}")
            };
            assert!(profile.is_enabled());
            assert!(profile.function().is_empty());
            assert_eq!(
                code.iter()
                    .filter(|instr| matches!(instr, redscript_io::Instr::Profile(_)))
                    .count(),
                1
            );
        } else {
            assert!(
                !code
                    .iter()
                    .any(|instr| matches!(instr, redscript_io::Instr::Profile(_)))
            );
        }
    }
    assert_eq!(count, 7);
    let interner = TypeInterner::default();
    let (symbols, _) = CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
        .unwrap()
        .into_inner();
    let scalar = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Scalar")
        .unwrap();
    assert!(scalar.func().type_().is_const_return());
    let imported = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Imported")
        .unwrap();
    assert!(imported.func().type_().is_const_return());
    assert_eq!(
        scalar.func().type_().properties().profile.as_deref(),
        Some("")
    );
    let host = symbols[interner.intern("Host")]
        .schema()
        .as_aggregate()
        .unwrap();
    let method = host.methods().by_name("Object").next().unwrap();
    assert!(method.func().type_().is_const_return());
    assert_eq!(
        method.func().type_().properties().profile.as_deref(),
        Some("")
    );
}

#[test]
fn profile_io_preserves_unknown_payload_bytes_and_virtual_size() {
    use redscript_io::byte::{Measure, TryRead, TryWrite};
    // Official layout: opcode, u32-length byte vector, eight runtime bytes,
    // enabled byte. Do not consume the runtime bytes as Nops or the enable flag.
    let raw = [0x33, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 1];
    let (instruction, consumed) =
        redscript_io::Instr::<redscript_io::Offset>::try_read(&raw, redscript_io::byte::LE)
            .unwrap();
    assert_eq!(consumed, raw.len());
    assert_eq!(usize::from(instruction.virtual_size()), raw.len());
    assert!(matches!(&instruction, redscript_io::Instr::Profile(profile) if profile.is_enabled()));
    let mut output = vec![0; instruction.measure(redscript_io::byte::LE)];
    instruction
        .try_write(&mut output, redscript_io::byte::LE)
        .unwrap();
    assert_eq!(output, raw);
}

#[test]
fn profile_label_and_field_profile_have_distinct_paths() {
    let source = r#"[profile = "Authored label"] function Run() -> const Int32 { return 7; }
        class Host { [profile = "Field label"] var value: Int32; }"#;
    let sources = SourceMap::new();
    let file = sources.push_back("properties.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols, errors) = process_sources(
        [parsed],
        [],
        redscript_compiler_frontend::Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let function = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Run")
        .unwrap();
    assert_eq!(
        function.func().type_().properties().profile.as_deref(),
        Some("Authored label")
    );
    assert!(function.func().type_().is_const_return());
    let host = symbols[interner.intern("Host")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert_eq!(
        host.fields().iter().next().unwrap().field().properties(),
        &[("profile".into(), "Field label".into())]
    );
}

// Keep the official checks opt-in: ordinary tests never execute installed tools.
#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn official_properties_flags_and_unrepresented_metadata() {
    let compiler = redscript_test_support::OfficialCompiler::from_env();
    for (name, source, diagnostic) in [
        (
            "plain",
            "class ProbeHost extends IScriptable { editable var value: array<Int32>; public function Run() -> Int32 { return 7; } }",
            None,
        ),
        (
            "mutable",
            "class ProbeHost extends IScriptable { mutable editable var value: array<Int32>; public function Run() -> Int32 { return 7; } }",
            None,
        ),
        (
            "instance",
            "class ProbeHost extends IScriptable { instanceeditable var value: array<Int32>; }",
            None,
        ),
        (
            "mutable-instance",
            "class ProbeHost extends IScriptable { mutable instanceeditable var value: array<Int32>; }",
            None,
        ),
        (
            "explicit-const",
            "class ProbeHost extends IScriptable { mutable editable const var value: array<Int32>; }",
            None,
        ),
        (
            "mutable-write",
            "class ProbeHost extends IScriptable { mutable editable var value: array<Int32>; public function Run() { value.PushBack(7); } }",
            None,
        ),
        (
            "plain-write",
            "class ProbeHost extends IScriptable { editable var value: array<Int32>; public function Run() { value.PushBack(7); } }",
            Some("Only mutable arrays can be modified"),
        ),
        (
            "profile",
            "class ProbeHost extends IScriptable { [profile = \"Authored\"] public function Run() -> Int32 { return 7; } }",
            None,
        ),
        (
            "const-return",
            "class ProbeHost extends IScriptable { public function Run() -> const Int32 { return 7; } }",
            None,
        ),
        (
            "typed-field",
            "class ProbeHost extends IScriptable { [runtimeProperty = true] var value: Int32; }",
            None,
        ),
        (
            "class-metadata",
            "[runtimeProperty = true] class ProbeHost extends IScriptable {}",
            None,
        ),
    ] {
        let probe = format!("native-properties-{name}");
        if let Some(diagnostic) = diagnostic {
            compiler.reject(&probe, source, diagnostic);
            continue;
        }
        let output = compiler.compile(&probe, source);
        output.assert_version("v1.2.2");
        let bytes = output.bundle_bytes();
        let bundle = redscript_test_support::OfficialBundle::new(&bytes);
        match name {
            "plain" | "mutable" | "instance" | "mutable-instance" | "explicit-const" => {
                let field = bundle.definition("value", 8);
                let flags = u16::from_le_bytes(field[5..7].try_into().unwrap());
                assert_eq!(
                    flags & 0x4a,
                    match name {
                        "plain" | "explicit-const" => 0x0a,
                        "mutable" => 0x02,
                        "instance" => 0x48,
                        "mutable-instance" => 0x40,

                        _ => unreachable!(),
                    }
                );
                if name == "plain" {
                    let function = bundle.definition("Run;", 5);
                    assert_eq!(function[17], 0); // Return const, separate from method flags.
                    assert_eq!(function[22], 0x33); // Profile instruction.
                    assert_eq!(function[35], 0); // Profiling disabled by default.
                    use redscript_io::byte::TryRead;
                    let (marker, size) = redscript_io::Instr::<redscript_io::Offset>::try_read(
                        &function[22..],
                        redscript_io::byte::LE,
                    )
                    .unwrap();
                    assert_eq!(size, 14);
                    assert!(
                        matches!(marker, redscript_io::Instr::Profile(profile) if !profile.is_enabled())
                    );
                }
            }
            "profile" => {
                let function = bundle.definition("Run;", 5);
                assert_eq!(function[22], 0x33);
                assert_eq!(function[35], 1); // [profile] changes executable metadata.
                use redscript_io::byte::TryRead;
                let (official_marker, size) =
                    redscript_io::Instr::<redscript_io::Offset>::try_read(
                        &function[22..],
                        redscript_io::byte::LE,
                    )
                    .unwrap();
                assert_eq!(size, 14);
                let emitted = compile(
                    r#"class ProbeHost { [profile = "Authored"] public function Run() -> Int32 { return 7; } }"#,
                );

                let marker = emitted.indexed_definitions().find_map(|def| match def {
                    IndexedDefinition::Function(_, f) if emitted[f.name()].starts_with("Run;") => {
                        f.body().code_owned().unwrap().into_iter().next()
                    }
                    _ => None,
                });
                assert_eq!(marker, Some(official_marker));
            }
            "const-return" => assert_eq!(bundle.definition("Run;", 5)[17], 1),
            "typed-field" => {
                let field = bundle.definition("value", 8);
                assert!(
                    field
                        .windows(b"runtimeProperty".len())
                        .any(|w| w == b"runtimeProperty")
                );
                assert!(field.windows(4).any(|w| w == b"true"));
            }
            "class-metadata" => {
                // Acceptance alone does not establish preserved class metadata.
                assert_eq!(bundle.definition("ProbeHost", 1).len(), 7);
            }
            _ => {}
        }
    }
}

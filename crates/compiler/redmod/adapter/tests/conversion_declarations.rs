use redscript_community_ast::{ConversionKind, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

/// Optional installed-source inventory; no engine sources are redistributed.
#[test]
#[ignore = "requires REDMOD_SCRIPTS_DIR pointing to installed REDmod scripts"]
fn installed_foundational_conversion_files_are_retained() {
    let root = std::env::var_os("REDMOD_SCRIPTS_DIR").expect("REDMOD_SCRIPTS_DIR");
    let root = std::path::Path::new(&root);
    let sources = SourceMap::new();
    let mut implicit = 0;
    let mut explicit = 0;
    for path in [
        "core/math/casting.script",
        "core/blackboard/blackboardDefinitions.script",
        "core/entity/entityID.script",
        "core/world/globalNodeID.script",
        "core/systems/vehicleSystem.script",
        "core/ai/aiScripting.script",
        "cyberpunk/NPC/components/eventHandlerComponent.script",
        "cyberpunk/ai/Tasks/PatrolControllerTask.script",
        "cyberpunk/systems/equipmentSystem.script",
    ] {
        let source = std::fs::read_to_string(root.join(path)).unwrap();
        let file = sources.push_back(path, source.clone());
        let (parsed, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{path}: {errors:?}");
        for (decl, _) in parsed.unwrap().module.items {
            if let redscript_community_ast::Item::Function(function) = decl.item {
                match function.properties.conversion {
                    Some(ConversionKind::Implicit) => implicit += 1,
                    Some(ConversionKind::Explicit) => explicit += 1,
                    None => {}
                }
            }
        }
    }
    assert_eq!((implicit, explicit), (52, 75));
}

#[test]
fn providers_retain_conversion_intent_signatures_and_source_identity() {
    for reverse in [false, true] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let providers = [
            (
                "ids.script",
                "struct EntityID { var value: Uint64; } import implicit cast(value: EntityID): Uint64; function Provider(value: EntityID): EntityID { return value; }",
            ),
            (
                "math.script",
                "import implicit cast(value: Uint8): Float; import cast(value: Int8): Float;",
            ),
            (
                "scripted.script",
                "struct Data { var value: Int32; } implicit cast(value: Data): Int32 { return value.value; }",
            ),
            (
                "use.script",
                "function Use(value: EntityID): EntityID { return Provider(value); }",
            ),
        ];
        let mut modules = Vec::new();
        let mut files = Vec::new();
        for (name, source) in providers {
            let file = sources.push_back(name, source);
            let (module, errors) = parse_file_with_metadata(source, file);
            assert!(errors.is_empty(), "{errors:?}");
            modules.push(module.unwrap());
            files.push(file);
        }
        if reverse {
            modules.reverse();
        }
        let dependencies = sources
            .files()
            .filter(|(file, _)| !files.contains(file))
            .map(|(file, source)| {
                let (module, errors) =
                    redscript_community_parser::parse_module(source.source(), file);
                assert!(errors.is_empty(), "{errors:?}");
                module.unwrap()
            })
            .collect::<Vec<_>>();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols, errors) = process_sources(
            modules,
            dependencies,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        let mut conversions = symbols
            .free_functions()
            .filter(|entry| entry.func().type_().conversion().is_some())
            .map(|entry| {
                let function = entry.func();
                assert_eq!(entry.name().to_string(), "Cast");
                let typ = function.type_();
                assert!(typ.is_valid());
                assert_eq!(typ.params().len(), 1);
                (
                    typ.params()[0].type_().to_string(),
                    typ.return_type().to_string(),
                    typ.conversion().unwrap(),
                    function.flags().is_native(),
                    function.span().unwrap().file,
                )
            })
            .collect::<Vec<_>>();
        conversions.sort_by(|a, b| a.0.cmp(&b.0));
        assert_eq!(
            conversions,
            [
                (
                    "Data".into(),
                    "Int32".into(),
                    ConversionKind::Implicit,
                    false,
                    files[2]
                ),
                (
                    "EntityID".into(),
                    "Uint64".into(),
                    ConversionKind::Implicit,
                    true,
                    files[0]
                ),
                (
                    "Int8".into(),
                    "Float".into(),
                    ConversionKind::Explicit,
                    true,
                    files[1]
                ),
                (
                    "Uint8".into(),
                    "Float".into(),
                    ConversionKind::Implicit,
                    true,
                    files[1]
                ),
            ]
        );
    }
}

#[test]
fn mixed_source_member_and_operator_conversions_are_not_dispatchable() {
    for source in [
        "class Host { public implicit func Convert(value: Int32) -> Int32 = value; } func Use(host: Host) -> Int32 { return host.Convert(1); }",
        "class Host { public static explicit func Convert(value: Int32) -> Int32 = value; } func Use() -> Int32 { return Host.Convert(1); }",
        "struct Data {} implicit func OperatorAdd(lhs: Data, rhs: Data) -> Data = lhs; func Use(value: Data) -> Data { return value + value; }",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let shared = sources.push_back("shared.reds", source);
        let file = sources.push_back("use.script", "function Native() {}");
        let (native, errors) = parse_file_with_metadata("function Native() {}", file);
        assert!(errors.is_empty(), "{errors:?}");
        let dependencies = sources
            .files()
            .filter(|(id, _)| *id != file)
            .map(|(id, source)| {
                let (module, errors) =
                    redscript_community_parser::parse_module(source.source(), id);
                assert!(errors.is_empty(), "{errors:?}");
                module.unwrap()
            })
            .collect::<Vec<_>>();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            native,
            dependencies,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(!reporter.reported().is_empty(), "{source}");
        assert!(
            reporter
                .reported()
                .iter()
                .all(|error| error.span().file == shared)
        );
    }
}

#[test]
fn retained_implicit_declarations_never_become_ordinary_cast_overloads() {
    for declaration in [
        "implicit cast(value: Data): Int32 { return value.value; }",
        "import implicit cast(value: Data): Int32;",
        "import cast(value: Data): Int32;",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let provider = sources.push_back("provider.script", declaration);
        let (provider, errors) = parse_file_with_metadata(declaration, provider);
        assert!(errors.is_empty(), "{errors:?}");
        let source = "struct Data { var value: Int32; } function Use(value: Data): Int32 { return (Int32)value; }";
        let file = sources.push_back("consumer.script", source);
        let (consumer, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let dependencies = sources
            .files()
            .filter(|(_, source)| source.path().extension().is_some_and(|ext| ext == "reds"))
            .map(|(file, source)| {
                redscript_community_parser::parse_module(source.source(), file)
                    .0
                    .unwrap()
            })
            .collect::<Vec<_>>();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            [consumer.unwrap(), provider.unwrap()],
            dependencies,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        let codes = reporter
            .reported()
            .iter()
            .map(redscript_compiler_frontend::Diagnostic::code)
            .collect::<Vec<_>>();
        assert_eq!(
            codes,
            ["UNSUPPORTED_NATIVE_CONVERSION"],
            "{declaration}: {:?}",
            reporter.reported()
        );
        assert_eq!(reporter.reported()[0].span().file, file);
    }
}

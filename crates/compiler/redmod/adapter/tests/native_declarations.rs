use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(
    source: &str,
    dependency: &str,
    inspect: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &'ctx TypeInterner),
) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("dependency.reds", dependency);
    let file = sources.push_back("declarations.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    inspect(&unit, &symbols, &interner);
    errors
        .into_iter()
        .map(|error| error.to_string())
        .chain(
            reporter
                .into_reported()
                .into_iter()
                .map(|error| error.to_string()),
        )
        .collect()
}

#[test]
fn imported_scriptable_root_with_boot_dependencies_terminates() {
    let (send, recv) = std::sync::mpsc::channel();
    let worker = std::thread::spawn(move || {
        let errors = analyze(
            "import class IScriptable {}",
            "class Community {}",
            |_, symbols, interner| {
                let root = interner.intern("IScriptable");
                assert!(symbols[root].schema().base_type().is_none());
                assert!(
                    symbols[root]
                        .schema()
                        .as_aggregate()
                        .unwrap()
                        .flags()
                        .is_native()
                );
                for name in ["Community", "Function0", "Function8"] {
                    assert_eq!(
                        symbols[interner.intern(name)]
                            .schema()
                            .base_type()
                            .unwrap()
                            .id(),
                        root,
                        "{name}"
                    );
                }
            },
        );
        assert!(errors.is_empty(), "{errors:?}");
        send.send(()).unwrap();
    });
    recv.recv_timeout(std::time::Duration::from_secs(10))
        .expect("imported root resolution must terminate");
    worker.join().unwrap();
}

#[test]
fn omitted_fields_are_public_but_explicit_visibility_and_method_defaults_survive() {
    for kind in ["class", "import class", "importonly class", "struct"] {
        let imported = if kind.starts_with("import") {
            "import "
        } else {
            ""
        };
        let errors = analyze(
            &format!(
                "{kind} Host {{
                   {imported}var omitted: Int32;
                   public {imported}var visible: Int32;
                   protected {imported}var inherited: Int32;
                   private {imported}var hidden: Int32;
                 }}
                 function Read(value: Host): Int32 {{ return value.omitted; }}"
            ),
            "class Community { let hidden: Int32; func Hidden() {} }
             struct CommunityStruct { let visible: Int32; }",
            |_, symbols, interner| {
                let host = symbols[interner.intern("Host")]
                    .schema()
                    .as_aggregate()
                    .unwrap();
                let fields = host
                    .fields()
                    .iter()
                    .map(|entry| (entry.name(), entry.field().flags().visibility()))
                    .collect::<Vec<_>>();
                assert_eq!(
                    fields,
                    [
                        ("omitted", Visibility::Public),
                        ("visible", Visibility::Public),
                        ("inherited", Visibility::Protected),
                        ("hidden", Visibility::Private),
                    ]
                );
                let community = symbols[interner.intern("Community")]
                    .schema()
                    .as_aggregate()
                    .unwrap();
                assert_eq!(
                    community
                        .fields()
                        .iter()
                        .next()
                        .unwrap()
                        .field()
                        .flags()
                        .visibility(),
                    Visibility::Private
                );
                assert_eq!(
                    community
                        .methods()
                        .iter()
                        .next()
                        .unwrap()
                        .func()
                        .flags()
                        .visibility(),
                    Visibility::Private
                );
                let structure = symbols[interner.intern("CommunityStruct")]
                    .schema()
                    .as_aggregate()
                    .unwrap();
                assert_eq!(
                    structure
                        .fields()
                        .iter()
                        .next()
                        .unwrap()
                        .field()
                        .flags()
                        .visibility(),
                    Visibility::Public
                );
            },
        );
        assert!(errors.is_empty(), "{kind}: {errors:?}");
    }
    let errors = analyze(
        "class Host { function Hidden() {} }",
        "",
        |_, symbols, interner| {
            let host = symbols[interner.intern("Host")]
                .schema()
                .as_aggregate()
                .unwrap();
            assert_eq!(
                host.methods()
                    .iter()
                    .next()
                    .unwrap()
                    .func()
                    .flags()
                    .visibility(),
                Visibility::Private
            );
        },
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn explicit_field_visibility_still_controls_access() {
    for visibility in ["private", "protected"] {
        let errors = analyze(
            &format!(
                "class Host {{ {visibility} var value: Int32; }}
                      function Read(value: Host): Int32 {{ return value.value; }}"
            ),
            "",
            |_, _, _| {},
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("access"), "{errors:?}");
    }
}

#[test]
fn imported_event_prototypes_are_non_native_callbacks_without_lowered_bodies() {
    // The official compiler emits these as callbacks with the native flag unset.
    // Unlike a body-bearing event, a prototype must not invent an implementation.
    let errors = analyze(
        "import class Listener {
           protected event OnReady(value: Int32);
           event OnDone();
           protected event OnBody() {}
         }",
        "",
        |unit, symbols, interner| {
            let listener = symbols[interner.intern("Listener")]
                .schema()
                .as_aggregate()
                .unwrap();
            let methods = listener.methods().iter().collect::<Vec<_>>();
            assert_eq!(methods.len(), 3);
            for entry in &methods {
                let flags = entry.func().flags();
                assert!(flags.is_callback());
                assert!(!flags.is_native());
                assert_eq!(flags.is_unimplemented(), **entry.name() != "OnBody");
            }
            let class = &unit.classes[&interner.intern("Listener")];
            assert_eq!(class.methods.len(), 1);
        },
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn free_scripted_and_importonly_prototypes_still_require_bodies() {
    for (source, dependency) in [
        ("function OnReady();", ""),
        ("class Listener { protected event OnReady(); }", ""),
        (
            "importonly class Listener { protected event OnReady(); }",
            "",
        ),
    ] {
        let errors = analyze(source, dependency, |_, _, _| {});
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("body"), "{errors:?}");
    }
}

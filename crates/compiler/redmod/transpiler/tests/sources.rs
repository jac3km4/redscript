use std::path::Path;
use std::time::Duration;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::package::{ScriptArtifact, publish_mod, source_at};
use redscript_redmod_transpiler::validate::OfficialValidator;
use redscript_redmod_transpiler::{analyze_sources, transpile_sources};

#[test]
fn selected_sources_need_no_manual_bindings() {
    let sources = SourceMap::new();
    sources.push_back(
        "dependency.reds",
        "module Dependency\npublic func Value() -> Int32 = 42;",
    );
    let selected = sources.push_back("authored.reds", "module Authored\nimport Dependency.*;\npublic func Echo(value: Int32) -> Int32 = value;\npublic func Echo(value: Bool) -> Bool = value;\npublic func Run() -> Int32 = Value();\nclass Widget { let value: Int32; public func Read() -> Int32 = this.value; }\n");
    let empty = sources.push_back("empty.reds", "");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected, empty],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    assert_eq!(output.len(), 2);
    assert_eq!(output[0].file, selected);
    assert_eq!(output[1].module.text, "");
    insta::assert_snapshot!("automatic.script", output[0].module.text);
    assert!(
        output[0]
            .module
            .mappings
            .iter()
            .all(|m| m.source.file == selected)
    );
    assert!(
        source_at(
            &output[0].module.mappings,
            output[0].module.text.find("return").unwrap()
        )
        .is_some()
    );
}

#[test]
fn conditional_association_fails_with_source_provenance() {
    let sources = SourceMap::new();
    let selected = sources.push_back(
        "conditional.reds",
        "@if(false)\npublic func Missing() -> Int32 = 1;\n",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let errors = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap_err();
    insta::assert_snapshot!(
        "automatic-condition-error",
        errors
            .iter()
            .map(|e| e.display(&sources).to_string())
            .collect::<String>()
    );
}

#[test]
fn analysis_and_selection_errors_do_not_emit() {
    let sources = SourceMap::new();
    let selected = sources.push_back("invalid.reds", "func Broken() -> Int32 = Unknown();");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let errors = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("community analysis"))
    );
    assert_eq!(errors[0].span.unwrap().file, selected);

    let sources = SourceMap::new();
    let selected = sources.push_back("empty.reds", "");
    let other = sources.push_back("other.reds", "");
    let analysis = analyze_sources(&sources, Symbols::with_default_types(), &interner).unwrap();
    assert!(analysis.transpile_selected(&[selected, selected]).is_err());
    assert!(analysis.transpile_selected(&[other]).is_ok());
}

#[test]
fn automatic_patch_association_uses_resolved_signature() {
    use redscript_compiler_frontend::{
        Aggregate, FunctionType, Method, MethodFlags, Param, ParamFlags, Type, TypeDef, TypeSchema,
        Visibility, predef,
    };
    let sources = SourceMap::new();
    let selected = sources.push_back("wrapper.reds", "@wrapMethod(EngineWidget) public func Value(input: Int32) -> Int32 = wrappedMethod(input);");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut aggregate = Aggregate::default();
    aggregate.methods_mut().add(
        "Value",
        Method::new(
            MethodFlags::default().with_visibility(Visibility::Public),
            FunctionType::new(
                [],
                [Param::new(
                    "input",
                    ParamFlags::default(),
                    Type::nullary(predef::INT32),
                    None,
                )],
                Type::nullary(predef::INT32),
            ),
            None,
            [],
            None,
        ),
    );
    let mut symbols = Symbols::with_default_types();
    symbols.add_type(
        interner.intern("EngineWidget"),
        TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
    );
    let output = transpile_sources(&sources, &[selected], symbols, &interner).unwrap();
    insta::assert_snapshot!("automatic-wrapper.script", output[0].module.text);
}

#[test]
fn competing_dependency_replacements_cannot_supply_selected_body() {
    use redscript_compiler_frontend::{
        Aggregate, FunctionType, Method, MethodFlags, Type, TypeDef, TypeSchema, predef,
    };
    use redscript_redmod_transpiler::{
        DeclarationPath, ResolvedDeclaration, TranspileModule, transpile,
    };
    let sources = SourceMap::new();
    let selected = sources.push_back(
        "a.reds",
        "@replaceMethod(EngineWidget) public func Value() -> Int32 = 11;",
    );
    let dependency = sources.push_back(
        "b.reds",
        "@replaceMethod(EngineWidget) public func Value() -> Int32 = 22;",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut aggregate = Aggregate::default();
    let index = aggregate.methods_mut().add(
        "Value",
        Method::new(
            MethodFlags::default(),
            FunctionType::new([], [], Type::nullary(predef::INT32)),
            None,
            [],
            None,
        ),
    );
    let owner = interner.intern("EngineWidget");
    let mut symbols = Symbols::with_default_types();
    symbols.add_type(
        owner,
        TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
    );
    let analysis = analyze_sources(&sources, symbols, &interner).unwrap();
    let errors = analysis.transpile_selected(&[selected]).unwrap_err();

    insta::assert_snapshot!(
        "competing-replacements",
        errors
            .iter()
            .map(|e| e.display(&sources).to_string())
            .collect::<String>()
    );
    // Even the explicit API must not emit the same resolved identity from two
    // different source modules. The body table has already lost ownership here.
    let identity =
        ResolvedDeclaration::Replacement(redscript_compiler_frontend::MethodId::new(owner, index));
    let requests: Vec<_> = [selected, dependency]
        .iter()
        .map(|file| TranspileModule {
            source: &analysis
                .modules()
                .iter()
                .find(|(id, _)| id == file)
                .unwrap()
                .1,
            bindings: [(DeclarationPath(vec![0]), identity)].into(),
        })
        .collect();
    let errors = transpile(&requests, analysis.unit(), analysis.symbols()).unwrap_err();

    insta::assert_snapshot!(
        "duplicate-transpile-identities",
        errors
            .iter()
            .map(|e| e.display(&sources).to_string())
            .collect::<String>()
    );
}

#[test]
fn eager_interpolation_and_switch_prefixes_are_rejected() {
    for (name, body, expected) in [
        (
            "interpolation-prefix",
            r#"func Test(out x: Int32) -> String = s"\(Read(x)) \(Check([Set(x)]))";"#,
            "earlier interpolation",
        ),
        (
            "switch-prefix",
            "func Test(out x: Int32) { switch Read(x) { case 0: break; case Check([Set(x)]): break; } }",
            "switch matcher",
        ),
    ] {
        let sources = SourceMap::new();
        sources.push_back("dependency.reds", "native func OperatorAdd(a: String, b: String) -> String; func Read(out x: Int32) -> Int32 = x; func Set(out x: Int32) -> Int32 { x = 1; return x; } func Check(values: [Int32]) -> Int32 = 0;");
        let selected = sources.push_back("rejected.reds", body);
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let errors = transpile_sources(
            &sources,
            &[selected],
            Symbols::with_default_types(),
            &interner,
        )
        .unwrap_err();
        assert!(errors[0].message.contains(expected), "{errors:?}");
        insta::assert_snapshot!(
            name,
            errors
                .iter()
                .map(|e| e.display(&sources).to_string())
                .collect::<String>()
        );
    }
}

struct Temp(std::path::PathBuf);
impl Temp {
    fn new() -> Self {
        static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let p = std::env::temp_dir().join(format!(
            "redmod-transpile-test-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        ));
        std::fs::create_dir(&p).unwrap();
        Self(p)
    }
}
impl Drop for Temp {
    fn drop(&mut self) {
        drop(std::fs::remove_dir_all(&self.0));
    }
}

#[test]
fn package_publishes_new_mod_and_preserves_existing_destination() {
    let temp = Temp::new();
    let sources = SourceMap::new();
    let selected = sources.push_back("simple.reds", "public func Value() -> Int32 = 1;");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    let scripts = [ScriptArtifact {
        path: Path::new("nested/simple.script"),
        source: &output[0],
    }];
    let published = publish_mod(&temp.0, "Example", &scripts, None).unwrap();
    assert_eq!(
        std::fs::read_to_string(&published.scripts[0].path).unwrap(),
        output[0].module.text
    );
    assert_eq!(published.scripts[0].mappings, output[0].module.mappings);
    assert_eq!(
        publish_mod(&temp.0, "Example", &scripts, None)
            .unwrap_err()
            .kind(),
        std::io::ErrorKind::AlreadyExists
    );
    assert_eq!(
        publish_mod(&temp.0, "bad.name", &scripts, None)
            .unwrap_err()
            .kind(),
        std::io::ErrorKind::InvalidInput
    );
    for path in [
        "../escape.script",
        "/absolute.script",
        "CON.script",
        "dir\\file.script",
        "file.reds",
    ] {
        assert_eq!(
            publish_mod(
                &temp.0,
                "Unsafe",
                &[ScriptArtifact {
                    path: Path::new(path),
                    source: &output[0]
                }],
                None
            )
            .unwrap_err()
            .kind(),
            std::io::ErrorKind::InvalidInput
        );
    }
    let duplicate = [
        ScriptArtifact {
            path: Path::new("X.script"),
            source: &output[0],
        },
        ScriptArtifact {
            path: Path::new("x.script"),
            source: &output[0],
        },
    ];
    assert_eq!(
        publish_mod(&temp.0, "Duplicate", &duplicate, None)
            .unwrap_err()
            .kind(),
        std::io::ErrorKind::InvalidInput
    );
    let validator = OfficialValidator {
        executable: temp.0.join("missing-redMod.exe"),
        timeout: Duration::from_millis(100),
    };
    assert!(publish_mod(&temp.0, "Failed", &scripts, Some(&validator)).is_err());
    assert!(!temp.0.join("Failed").exists());
    assert_eq!(std::fs::read_dir(&temp.0).unwrap().count(), 1);
}

// An authored process stub exercises invocation/timeout/output contracts; it is
// deliberately not an official compiler conformance test.
#[test]
fn validator_requires_final_stage_and_bounds_execution() {
    let temp = Temp::new();
    let stub = temp.0.join("validator.rs");
    std::fs::write(
        &stub,
        r#"
fn main() {
    let exe = std::env::current_exe().unwrap();
    let mode = exe.file_stem().unwrap().to_str().unwrap();
    assert_eq!(std::env::args().nth(1).unwrap(), "validate-mod-scripts");
    assert_eq!(std::env::args().nth(2).unwrap(), "-compile");
    assert_eq!(std::env::current_dir().unwrap(), exe.parent().unwrap());
    if mode == "timeout" { std::thread::sleep(std::time::Duration::from_secs(30)); }
    let arg = std::env::args().nth(3).unwrap();
    assert_eq!(arg, "-directory");
    let path = std::env::args().nth(4).unwrap();
    let directory = std::path::Path::new(&path);
    assert!(!directory.file_name().unwrap().to_str().unwrap().contains('.'));
    std::fs::write(directory.join("observed-mod-name"), directory.file_name().unwrap().to_str().unwrap()).unwrap();
    std::fs::create_dir_all(directory.parent().unwrap().join("Dependencies")).unwrap();
    for name in ["final.redscripts", "final.redscripts.h64"] {
        std::fs::write(directory.join("r6/cache").join(name), "authored mock output").unwrap();
    }
    if mode == "preliminary" { println!("Validate mod scripts success!"); }
    else { println!("[INFO] Commandlet validate-mod-scripts has succeeded"); }
    if mode == "failure" { std::process::exit(1); }
}
"#,
    )
    .unwrap();
    let executable = temp
        .0
        .join(format!("success{}", std::env::consts::EXE_SUFFIX));
    assert!(
        std::process::Command::new("rustc")
            .arg(&stub)
            .arg("-o")
            .arg(&executable)
            .status()
            .unwrap()
            .success()
    );
    let sources = SourceMap::new();
    let selected = sources.push_back("simple.reds", "public func Value() -> Int32 = 1;");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    let published = publish_mod(
        &temp.0,
        "FinalName",
        &[ScriptArtifact {
            path: Path::new("simple.script"),
            source: &output[0],
        }],
        Some(&OfficialValidator {
            executable: executable.clone(),
            timeout: Duration::from_secs(2),
        }),
    )
    .unwrap();
    assert_eq!(
        std::fs::read_to_string(published.directory.join("observed-mod-name")).unwrap(),
        "FinalName"
    );
    assert!(!temp.0.join("Dependencies").exists());
    assert!(
        !std::fs::read_dir(&temp.0).unwrap().any(|e| e
            .unwrap()
            .file_name()
            .to_string_lossy()
            .contains("stage-"))
    );
    for mode in ["success", "preliminary", "failure", "timeout"] {
        let path = temp
            .0
            .join(format!("{mode}{}", std::env::consts::EXE_SUFFIX));
        if mode != "success" {
            std::fs::copy(&executable, &path).unwrap();
        }
        let directory = temp.0.join(format!("mod-{mode}"));
        std::fs::create_dir(&directory).unwrap();
        let validator = OfficialValidator {
            executable: path,
            timeout: Duration::from_secs(2),
        };
        let result = validator.validate(&directory);
        if mode == "success" {
            assert!(result.is_ok(), "{result:?}");
            assert_eq!(
                validator.validate(&directory).unwrap_err().kind(),
                std::io::ErrorKind::AlreadyExists
            );
        } else if mode == "timeout" {
            assert_eq!(result.unwrap_err().kind(), std::io::ErrorKind::TimedOut);
        } else {
            assert!(result.is_err());
        }
    }
}

#[test]
fn validator_rejects_stale_outputs_before_invocation() {
    let temp = Temp::new();
    std::fs::create_dir_all(temp.0.join("r6/cache")).unwrap();
    std::fs::write(temp.0.join("r6/cache/final.redscripts"), "stale").unwrap();
    let validator = OfficialValidator {
        executable: temp.0.join("missing-redMod.exe"),
        timeout: Duration::from_millis(100),
    };
    assert_eq!(
        validator.validate(&temp.0).unwrap_err().kind(),
        std::io::ErrorKind::AlreadyExists
    );
}

use hashbrown::HashMap;
use redscript_community_ast::SourceMap;
use redscript_community_formatter::{FormatCtx, FormatSettings, SyntaxOps};
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner, infer_from_sources};
use redscript_decompiler::{Settings, decompile_function};
use redscript_io::{Class, ClassFlags, IndexedDefinition, ScriptBundle, Visibility};

thread_local! {
    static WARNINGS: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
}

struct WarningLogger;

impl log::Log for WarningLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        metadata.level() == log::Level::Warn
    }

    fn log(&self, record: &log::Record<'_>) {
        if self.enabled(record.metadata()) {
            WARNINGS.with_borrow_mut(|warnings| warnings.push(record.args().to_string()));
        }
    }

    fn flush(&self) {}
}

fn with_warnings<T>(f: impl FnOnce() -> T) -> (T, Vec<String>) {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        log::set_logger(&WarningLogger).unwrap();
        log::set_max_level(log::LevelFilter::Warn);
    });
    WARNINGS.with_borrow_mut(Vec::clear);
    let result = f();
    let warnings = WARNINGS.with_borrow_mut(std::mem::take);
    (result, warnings)
}

fn compile(source: &str) -> Vec<u8> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("const.reds", source.to_owned());
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
    assert!(
        reporter.reported().is_empty(),
        "{source}\n{:?}",
        reporter.reported()
    );
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_writeable().to_bytes().unwrap()
}

#[test]
fn ordinary_const_declarations_roundtrip() {
    for (declaration, expected) in [
        ("const x: Int32 = 1;", "const x: Int32 = 1;"),
        ("const x: Int32;", "const x: Int32 = 0;"),
    ] {
        let bytes = compile(&format!("func Probe() {{ {declaration} }}"));
        let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
        let function = bundle
            .indexed_definitions()
            .find_map(|def| match def {
                IndexedDefinition::Function(_, function)
                    if bundle[function.name()].starts_with("Probe;") =>
                {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
        let (item, warnings) =
            with_warnings(|| decompile_function(function, &bundle, &Settings::default()).unwrap());
        assert!(warnings.is_empty(), "{warnings:?}");
        let output = item
            .as_fmt(FormatCtx::new(
                &FormatSettings::default(),
                &HashMap::default(),
            ))
            .to_string();
        let local_name = &bundle[bundle[function.locals()[0]].name()];
        let expected = expected.replace(" x:", &format!(" {local_name}:"));
        assert!(output.contains(&expected), "{output}");
        // Backend local names contain '$', which is not a source identifier.
        // Normalize only the name to isolate const preservation on recompilation.
        compile(&output.replace(local_name.as_ref(), "x"));
    }
}

#[test]
fn read_only_default_const_variant_roundtrips() {
    let bytes = compile("func Probe() -> Variant { const value: Variant; return value; }");
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let function = bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with("Probe;") =>
            {
                Some(function)
            }
            _ => None,
        })
        .unwrap();
    let code = function
        .body()
        .code_iter()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    assert!(
        code.iter()
            .all(|instr| !matches!(instr, redscript_io::Instr::Assign))
    );
    let local_name = &bundle[bundle[function.locals()[0]].name()];
    for settings in [Settings::default(), Settings::default().verbose()] {
        let (item, warnings) =
            with_warnings(|| decompile_function(function, &bundle, &settings).unwrap());
        assert!(warnings.is_empty(), "{warnings:?}");
        let output = item
            .as_fmt(FormatCtx::new(
                &FormatSettings::default(),
                &HashMap::default(),
            ))
            .to_string();
        assert!(
            output.contains(&format!("const {local_name}: Variant;")),
            "{output}"
        );
        assert!(
            output.contains(&format!("return {local_name};")),
            "{output}"
        );
        compile(&output.replace(local_name.as_ref(), "value"));
    }
}

#[test]
fn expanded_const_array_initializers_warn_and_only_downgrade_affected_local() {
    for value in ["[]", "[1, 2]"] {
        let bytes = compile(&format!(
            "func Probe() -> Int32 {{ const kept: Int32 = 7; const defaulted: Variant; const values: [Int32] = {value}; let mutable: Int32 = 1; ArraySize(values); mutable; return kept; }}"
        ));
        let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
        let function = bundle
            .indexed_definitions()
            .find_map(|def| match def {
                IndexedDefinition::Function(_, function)
                    if bundle[function.name()].starts_with("Probe;") =>
                {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
        let local_name = |prefix: &str| {
            function
                .locals()
                .iter()
                .map(|&local| bundle[bundle[local].name()].as_ref())
                .find(|name| name.starts_with(prefix))
                .unwrap()
        };
        for settings in [Settings::default(), Settings::default().verbose()] {
            let (item, warnings) =
                with_warnings(|| decompile_function(function, &bundle, &settings).unwrap());
            assert_eq!(warnings.len(), 1, "{warnings:?}");
            assert!(warnings[0].contains(local_name("values$")), "{warnings:?}");
            assert!(
                warnings[0].contains(bundle[function.name()].as_ref()),
                "{warnings:?}"
            );
            assert!(warnings[0].contains("emitting mutable let"), "{warnings:?}");
            let output = item
                .as_fmt(FormatCtx::new(
                    &FormatSettings::default(),
                    &HashMap::default(),
                ))
                .to_string();
            for expected in [
                format!("let {}: [Int32];", local_name("values$")),
                format!("const {}: Int32 = 7;", local_name("kept$")),
                format!("const {}: Variant;", local_name("defaulted$")),
                format!("let {}: Int32 = 1;", local_name("mutable$")),
            ] {
                assert!(output.contains(&expected), "{output}");
            }
            let mut normalized = output;
            for prefix in ["values", "kept", "defaulted", "mutable"] {
                normalized = normalized.replace(local_name(&format!("{prefix}$")), prefix);
            }
            // Nonempty arrays currently emit Uint64 resize/index operands, which
            // the compiler cannot recompile as Int32. Keep that separate from
            // const fallback coverage.
            if value == "[]" {
                compile(&normalized);
            }
        }
        assert!(
            decompile_function(
                function,
                &bundle,
                &Settings::default().omit_function_bodies()
            )
            .is_ok()
        );
    }
}

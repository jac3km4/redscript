use hashbrown::HashMap;
use redscript_community_ast::SourceMap;
use redscript_community_formatter::{FormatCtx, FormatSettings, SyntaxOps};
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner, infer_from_sources};
use redscript_decompiler::{Settings, decompile_function};
use redscript_io::{Class, ClassFlags, IndexedDefinition, ScriptBundle, Visibility};

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
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
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
        let item = decompile_function(function, &bundle, &Settings::default()).unwrap();
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
fn expanded_const_array_initializers_fail_closed() {
    for value in ["[]", "[1, 2]"] {
        let bytes = compile(&format!(
            "func Probe() -> Int32 {{ const values: [Int32] = {value}; return ArraySize(values); }}"
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
        for settings in [Settings::default(), Settings::default().verbose()] {
            let error = decompile_function(function, &bundle, &settings).unwrap_err();
            assert_eq!(
                error.to_string(),
                format!(
                    "Cannot preserve initializer for const local '{}': initialization requiring hoisting or expanded writes is unsupported; omit function bodies to decompile declarations only",
                    bundle[bundle[function.locals()[0]].name()]
                )
            );
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

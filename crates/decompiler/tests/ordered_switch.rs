use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner, infer_from_sources};
use redscript_decompiler::{Settings, decompile_function};
use redscript_io::{Class, ClassFlags, IndexedDefinition, ScriptBundle, Visibility};

#[test]
fn detached_switch_bodies_are_rejected_instead_of_reassigned_to_labels() {
    for (labels, detached) in [
        (
            "default: result = 30; case 1: result = 10; break; case 2: result = 20; break;",
            true,
        ),
        (
            "case 1: result = 10; break; default: result = 30; case 2: result = 20; break;",
            true,
        ),
        (
            "case 1: result = 10; break; case 2: result = 20; break; default: result = 30;",
            false,
        ),
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "switch.reds",
            format!("func Probe(value: Int32) -> Int32 {{ let result: Int32; switch value {{ {labels} }} return result; }}"),
        );
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
        let bytes = bundle.into_writeable().to_bytes().unwrap();
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
        let result = decompile_function(function, &bundle, &Settings::default());
        if !detached {
            assert!(result.is_ok(), "{result:?}");
            continue;
        }
        let error = result.unwrap_err();
        assert_eq!(
            error.to_string(),
            "Detached switch bodies are unsupported",
            "{labels}"
        );
        // Declaration-only decompilation remains available without misrepresenting ownership.
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

#![allow(dead_code)]

use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner,
};
use redscript_io::{
    Class, ClassFlags, Function, IndexedDefinition, Instr, ScriptBundle, Visibility,
};
use redscript_redmod_adapter::{Error, parse_file_with_metadata, process_sources};

fn sources(dependencies: &[(&str, &str)]) -> SourceMap {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    for &(path, source) in dependencies {
        sources.push_back(path, source);
    }
    sources
}

pub fn analyze<R>(
    source: &str,
    path: &str,
    dependencies: &[(&str, &str)],
    inspect: impl for<'ctx> FnOnce(
        &LoweredCompilationUnit<'ctx>,
        &Symbols<'ctx>,
        &'ctx TypeInterner,
        &[Error],
        &CompileErrorReporter<'ctx>,
    ) -> R,
) -> R {
    let sources = sources(dependencies);
    let file = sources.push_back(path, source);
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
    inspect(&unit, &symbols, &interner, &errors, &reporter)
}

pub fn empty_bundle() -> ScriptBundle<'static> {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    bundle
}

pub fn compile(
    source: &str,
    path: &str,
    dependencies: &[(&str, &str)],
    bundle: ScriptBundle<'_>,
) -> ScriptBundle<'static> {
    let mut bundle = bundle;
    let sources = sources(dependencies);
    let file = sources.push_back(path, source);
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
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) =
        process_sources(parsed, dependencies, symbols, &mut reporter, &interner);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_owned()
}

pub fn function<'a>(bundle: &'a ScriptBundle<'_>, name: &str) -> &'a Function<'a> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function) if &*bundle[function.name()] == name => {
                Some(function)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function {name}"))
}

pub fn function_with_prefix<'a>(bundle: &'a ScriptBundle<'_>, prefix: &str) -> &'a Function<'a> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with(prefix) =>
            {
                Some(function)
            }
            _ => None,
        })
        .unwrap()
}

pub fn code_with_prefix(bundle: &ScriptBundle<'_>, prefix: &str) -> Vec<Instr> {
    function_with_prefix(bundle, prefix)
        .body()
        .code_iter()
        .map(Result::unwrap)
        .collect()
}

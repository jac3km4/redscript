use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, Function, IndexedDefinition, Instr, ScriptBundle, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("selection.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
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

fn function<'a>(bundle: &'a ScriptBundle<'_>, name: &str) -> &'a Function<'a> {
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

fn calls(bundle: &ScriptBundle<'_>, name: &str) -> Vec<String> {
    function(bundle, name)
        .body()
        .code_iter()
        .filter_map(|instr| match instr.unwrap() {
            Instr::InvokeVirtual { function, .. } => Some(bundle[function].to_string()),
            Instr::InvokeStatic { function, .. } => {
                Some(bundle[bundle[function].name()].to_string())
            }
            _ => None,
        })
        .collect()
}

#[test]
fn overload_and_forwarder_calls_emit_the_selected_targets() {
    let bundle = compile(
        "class ProbeValueBase {} class ProbeValueChild extends ProbeValueBase {}
         class ProbeHost {
           public static function Pick(self: ProbeHost, value: Int32) -> Int32 { return 1; }
           public function Pick(value: Int32) -> Int32 { return 2; }
           public function Specific(value: ProbeValueBase) -> Int32 { return 1; }
           public function Specific(value: ProbeValueChild) -> Int32 { return 2; }
           public static function Get() -> ProbeHost { return NULL; }
           public static function Arg() -> Int32 { return 1; }
           public static function Alias(self: ProbeHost, value: Int32) -> Int32 { return value; }
         }
         function RunReal(host: ProbeHost) -> Int32 { return host.Pick(1); }
         function RunSpecific(host: ProbeHost, value: ProbeValueChild) -> Int32 { return host.Specific(value); }
         function RunAlias() -> Int32 { return ProbeHost.Get().Alias(ProbeHost.Arg()); }",
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    assert_eq!(calls(&bundle, "RunReal;ProbeHost"), ["Pick;Int32"]);
    assert_eq!(
        calls(&bundle, "RunSpecific;ProbeHostProbeValueChild"),
        ["Specific;ProbeValueChild"]
    );
    // Prefix bytecode: alias call, then exactly one receiver evaluation, then
    // exactly one argument evaluation. No synthetic instance target is emitted.
    assert_eq!(
        calls(&bundle, "RunAlias;"),
        ["Alias;ProbeHostInt32", "Get;", "Arg;"]
    );
}

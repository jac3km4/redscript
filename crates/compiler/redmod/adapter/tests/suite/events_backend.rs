use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, Function, FunctionFlags, IndexedDefinition, Instr, ScriptBundle, Type,
    TypeKind, Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> Vec<u8> {
    try_compile(source).unwrap()
}

fn try_compile(source: &str) -> Result<Vec<u8>, Vec<String>> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("casts.reds", "native func Cast(value: Int32) -> Bool;");
    sources.push_back(
        "community.reds",
        "class Community { protected cb func OnCommunity() {} }",
    );
    let file = sources.push_back("events.script", source);
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
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let name = bundle.cnames_mut().add("Bool");
    let bool_t = bundle.define(Type::new(name, TypeKind::Primitive));
    let name = bundle.cnames_mut().add("Native");
    let parent = bundle.define(Class::new(
        name,
        Visibility::Public,
        ClassFlags::default().with_is_native(true),
    ));
    let mut methods = Vec::new();
    for name in ["OnReplace", "OnWrap", "OnBase"] {
        let name = bundle.cnames_mut().add(name);
        methods.push(
            bundle.define(
                Function::new(
                    name,
                    Visibility::Protected,
                    FunctionFlags::default().with_is_callback(true),
                )
                .with_class(Some(parent))
                .with_return_type(Some(bool_t)),
            ),
        );
    }
    bundle[parent] = bundle[parent].clone().with_methods(methods);
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) =
        process_sources(parsed, dependencies, symbols, &mut reporter, &interner);
    let errors: Vec<_> = errors
        .iter()
        .map(ToString::to_string)
        .chain(reporter.reported().iter().map(ToString::to_string))
        .collect();
    if !errors.is_empty() {
        return Err(errors);
    }
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    Ok(bundle.into_writeable().to_bytes().unwrap())
}
fn functions<'a>(bundle: &'a ScriptBundle<'_>, name: &str) -> Vec<&'a Function<'a>> {
    bundle
        .indexed_definitions()
        .filter_map(|d| match d {
            IndexedDefinition::Function(_, f) if &*bundle[f.name()] == name => Some(f),
            _ => None,
        })
        .collect()
}
fn code(f: &Function<'_>) -> Vec<Instr> {
    f.body().code_iter().map(Result::unwrap).collect()
}

#[test]
fn event_fallthrough_and_bool_returns_roundtrip_without_a_default_value() {
    let bytes = compile(
        "class Listener extends Native {
        event OnEmpty() {}
        event OnPrototype();
        event OnTrue() { return true; }
        event OnPartial(value: Bool) { if(value) { return false; } }
        event OnBase() { return true; }
        public function Run(): Bool { return OnEmpty(); }
    }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    for name in ["OnEmpty", "OnPrototype", "OnTrue", "OnPartial"] {
        let f = functions(&bundle, name)[0];
        assert!(f.flags().is_callback());
        assert!(!f.body().is_empty());
        assert_eq!(&*bundle[bundle[f.return_type().unwrap()].name()], "Bool");
    }
    assert_eq!(code(functions(&bundle, "OnEmpty")[0]), [Instr::Nop]);
    assert_eq!(code(functions(&bundle, "OnPrototype")[0]), [Instr::Nop]);
    assert_eq!(
        code(functions(&bundle, "OnTrue")[0]),
        [Instr::Return, Instr::TrueConst, Instr::Nop]
    );
    assert!(
        code(functions(&bundle, "OnCommunity")[0]).is_empty(),
        "community fallthrough is unchanged"
    );
    let partial = code(functions(&bundle, "OnPartial")[0]);
    assert_eq!(partial.last(), Some(&Instr::Nop));
    assert_eq!(
        partial
            .iter()
            .filter(|i| matches!(i, Instr::Return))
            .count(),
        1
    );
    let base = functions(&bundle, "OnBase");
    assert_eq!(base.len(), 2);
    assert!(base.iter().any(|f| f.base_method().is_some()));
    let run = code(functions(&bundle, "Run;")[0]);
    assert_eq!(run.first(), Some(&Instr::Return));
    assert!(
        run.iter()
            .any(|i| matches!(i, Instr::InvokeVirtual { .. } | Instr::InvokeStatic { .. }))
    );
}

#[test]
fn callback_patches_preserve_bool_metadata_and_wrapper_result() {
    let bytes = compile(
        "@replaceMethod(Native) event OnReplace() { return true; }
        @wrapMethod(Native) event OnWrap() { return OnWrap(); }
        @addMethod(Native) event OnAdded() {}",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let replacement = functions(&bundle, "OnReplace")[0];
    assert!(replacement.flags().is_callback());
    assert_eq!(
        code(replacement),
        [Instr::Return, Instr::TrueConst, Instr::Nop]
    );
    for name in ["OnReplace", "OnWrap", "OnAdded"] {
        let f = functions(&bundle, name)[0];
        assert!(f.flags().is_callback());
        assert_eq!(&*bundle[bundle[f.return_type().unwrap()].name()], "Bool");
        assert_eq!(code(f).last(), Some(&Instr::Nop));
    }
    let wrapper = code(functions(&bundle, "OnWrap")[0]);
    assert_eq!(wrapper.first(), Some(&Instr::Return));
    assert!(
        wrapper
            .iter()
            .any(|i| matches!(i, Instr::InvokeStatic { .. }))
    );
}

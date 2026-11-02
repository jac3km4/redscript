use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, MonoType, Symbols, Type, TypeInterner, TypeSchema, infer_from_sources,
    predef,
};
use redscript_io::{Class, ClassFlags, Definition, ScriptBundle, TypeKind, Visibility};

#[test]
fn primitives_emit_and_reload_without_changing_identity_or_duplicating_types() {
    let ids = [
        predef::NODE_REF,
        predef::CRUID,
        predef::LOCALIZATION_STRING,
        predef::RED_RESOURCE_REFERENCE_SCRIPT_TOKEN,
    ];
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let mut source = "struct Box<T> { let value: T; }\n".to_owned();
    for (i, id) in ids.iter().enumerate() {
        source.push_str(&format!(
            "class Holder{i} {{ let value: {id}; let values: [{id}]; let boxed: Box<{id}>; }}
             func Pass{i}(value: {id}) -> {id} {{ let local: {id} = value; return local; }}
             func ArrayPass{i}(value: [{id}]) -> [{id}] {{ return value; }}"
        ));
    }
    sources.push_back("primitives.reds", source.as_str());
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
    let mut bundle = ScriptBundle::from_bytes(&bytes).unwrap();

    // Use a fresh interner to ensure loading canonicalizes the serialized names.
    let interner = TypeInterner::default();
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let unmapped =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let defaults = Symbols::with_default_types();
    for loaded in [&symbols, &unmapped] {
        for id in ids {
            assert_eq!(interner.get(id.as_str()), Some(id));
            assert_eq!(
                loaded
                    .types()
                    .filter(|(other, _)| other.as_str() == id.as_str())
                    .count(),
                1
            );
            assert!(matches!(loaded[id].schema(), TypeSchema::Primitive));
            assert_eq!(format!("{:?}", loaded[id]), format!("{:?}", defaults[id]));
        }
    }
    for (i, id) in ids.iter().enumerate() {
        let pass = symbols
            .free_functions()
            .find(|e| e.name().to_string() == format!("Pass{i}"))
            .unwrap();
        assert_eq!(pass.func().type_().return_type(), &Type::nullary(*id));
        assert_eq!(pass.func().type_().params()[0].type_(), &Type::nullary(*id));
    }

    // Reusing the loaded mappings must reference the existing primitive records.
    let mut monomorphizer = mappings.into_monomorphizer(&sources);
    for id in ids {
        monomorphizer.type_(&MonoType::nullary(id), &symbols, &mut bundle);
    }
    for id in ids {
        let types = bundle
            .definitions()
            .filter_map(|def| match def {
                Definition::Type(typ) if bundle[typ.name()] == id.as_str() => Some(typ),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(types.len(), 1, "{id}");
        assert_eq!(types[0].kind(), TypeKind::Primitive, "{id}");
        assert!(
            !bundle.definitions().any(|def| matches!(def,
                Definition::Class(class) if bundle[class.name()] == id.as_str()
            )),
            "{id} must not become a class"
        );
    }
}

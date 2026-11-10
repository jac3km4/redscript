use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, Type, TypeInterner, TypeSchema,
    infer_from_sources, ir, predef,
};

fn primitives() -> [redscript_compiler_frontend::TypeId<'static>; 4] {
    [
        predef::NODE_REF,
        predef::CRUID,
        predef::LOCALIZATION_STRING,
        predef::RED_RESOURCE_REFERENCE_SCRIPT_TOKEN,
    ]
}

fn check(source: &str, inspect: impl FnOnce(&LoweredCompilationUnit<'_>, &Symbols<'_>, &[String])) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("primitives.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let messages = reporter
        .reported()
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    inspect(&unit, &symbols, &messages);
}

#[test]
fn builtins_have_canonical_distinct_opaque_identities() {
    let interner = TypeInterner::default();
    let symbols = Symbols::with_default_types();
    for (id, name) in primitives().into_iter().zip([
        "NodeRef",
        "CRUID",
        "LocalizationString",
        "redResourceReferenceScriptToken",
    ]) {
        assert_eq!(id.as_str(), name);
        assert_eq!(interner.get(name), Some(id));
        assert_eq!(interner.intern(name.to_owned()), id);
        let def = symbols.get_type(id).unwrap();
        assert!(matches!(def.schema(), TypeSchema::Primitive));
        assert_eq!(
            symbols
                .types()
                .filter(|(other, _)| other.as_str() == name)
                .count(),
            1
        );
        assert_ne!(id, predef::UINT64);
        assert_ne!(id, predef::STRING);
        assert_ne!(id, predef::RES_REF);
        for other in primitives() {
            assert_eq!(id == other, id.as_str() == other.as_str());
        }
    }
    assert!(interner.get("DefinitelyMissingParityType").is_none());
}

#[test]
fn primitives_resolve_in_signatures_arrays_fields_locals_and_generics() {
    for id in primitives() {
        check(&format!(
            "struct Box<T> {{ let value: T; }}
             class Holder {{ let value: {id}; let values: [{id}]; let boxed: Box<{id}>; }}
             func Identity<T>(value: T) -> T {{ return value; }}
             func Pass(value: {id}) -> {id} {{ let local: {id} = Identity(value); return local; }}
             func ArrayPass(values: [{id}]) -> [{id}] {{ let local: [{id}] = values; return local; }}
             func BoxPass(value: Box<{id}>) -> Box<{id}> {{ return Identity(value); }}"
        ), |unit, symbols, messages| {
            assert!(messages.is_empty(), "{id}: {messages:?}");
            let pass = symbols.free_functions().find(|e| e.name().to_string() == "Pass").unwrap();
            assert!(pass.func().type_().is_valid());
            assert_eq!(pass.func().type_().params()[0].type_(), &Type::nullary(id));
            assert_eq!(pass.func().type_().return_type(), &Type::nullary(id));
            assert!(unit.functions.contains_key(pass.key()));
        });
    }
}

#[test]
fn unknown_names_and_integer_aliases_are_not_introduced() {
    check(
        "native func Missing(value: DefinitelyMissingParityType);",
        |_, _, messages| {
            assert_eq!(messages.len(), 1, "{messages:?}");
            assert!(messages[0].contains("`DefinitelyMissingParityType` is not a known type"));
        },
    );
    for id in primitives() {
        for source in [
            format!("func Bad() -> {id} {{ return 0ul; }}"),
            format!("func Bad(value: {id}) -> Uint64 {{ return value; }}"),
        ] {
            check(&source, |_, _, messages| {
                assert_eq!(messages.len(), 1, "{id}: {messages:?}");
                assert!(!messages[0].contains("not a known type"));
                assert!(!messages[0].contains("invalid signature"));
            });
        }
    }
}

#[test]
fn known_token_operators_remain_valid_and_unmatched_equality_keeps_fallback() {
    let mut source = String::new();
    for id in primitives() {
        source.push_str(&format!(
            "native func OperatorEqual(lhs: {id}, rhs: {id}) -> Bool;
             native func OperatorNotEqual(lhs: {id}, rhs: {id}) -> Bool;
             func Equal{id}(lhs: {id}, rhs: {id}) -> Bool {{ return lhs == rhs; }}
             func NotEqual{id}(lhs: {id}, rhs: {id}) -> Bool {{ return lhs != rhs; }}"
        ));
    }
    source.push_str(
        "func Fallback(lhs: Int32, rhs: Int32) -> Bool { return lhs == rhs; }
                     func NotFallback(lhs: Int32, rhs: Int32) -> Bool { return lhs != rhs; }",
    );
    check(&source, |unit, symbols, messages| {
        assert!(messages.is_empty(), "{messages:?}");
        let mut calls = 0;
        for entry in symbols.free_functions() {
            let name = entry.name().to_string();
            if name.starts_with("Operator") {
                assert!(entry.func().type_().is_valid());
                continue;
            }
            if !(name.starts_with("Equal")
                || name.starts_with("NotEqual")
                || name.ends_with("Fallback"))
            {
                continue;
            }
            // Boot's generic Equals/NotEquals declarations have no body.
            let Some(function) = unit.functions.get(entry.key()) else {
                continue;
            };
            let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
                panic!("return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("call")
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                panic!("free call")
            };
            let selected = symbols
                .free_functions()
                .find(|e| e.key() == function)
                .unwrap();
            let expected = match name.as_str() {
                "Fallback" => "Equals",
                "NotFallback" => "NotEquals",
                name if name.starts_with("NotEqual") => "OperatorNotEqual",
                _ => "OperatorEqual",
            };
            assert_eq!(selected.name().to_string(), expected, "{name}");
            calls += 1;
        }
        assert_eq!(calls, 10);
    });
}

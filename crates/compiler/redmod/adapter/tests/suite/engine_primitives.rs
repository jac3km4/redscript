use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, Type, TypeInterner, ir, predef,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

const NAMES: [&str; 4] = [
    "NodeRef",
    "CRUID",
    "LocalizationString",
    "redResourceReferenceScriptToken",
];

fn check(source: &str, inspect: impl FnOnce(&LoweredCompilationUnit<'_>, &Symbols<'_>, &[String])) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "generics.reds",
        "func Identity<T>(value: T) -> T { return value; }",
    );
    let file = sources.push_back("primitives.script", source);
    let (module, errors) = parse_file_with_metadata(source, file);
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
        module,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let messages = reporter
        .reported()
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    inspect(&unit, &symbols, &messages);
}

#[test]
fn native_signatures_fields_locals_and_arrays_use_default_primitives() {
    for (name, id) in NAMES.into_iter().zip([
        predef::NODE_REF,
        predef::CRUID,
        predef::LOCALIZATION_STRING,
        predef::RED_RESOURCE_REFERENCE_SCRIPT_TOKEN,
    ]) {
        check(
            &format!(
                "class Holder {{ var value: {name}; var values: array<{name}>; }}
             function Pass(value: {name}): {name} {{ var local: {name} = Identity(value); return local; }}
             function ArrayPass(values: array<{name}>): array<{name}> {{
                 var local: array<{name}> = values; return local;
             }}"
            ),
            |unit, symbols, messages| {
                assert!(messages.is_empty(), "{name}: {messages:?}");
                let pass = symbols
                    .free_functions()
                    .find(|e| e.name().to_string() == "Pass")
                    .unwrap();
                assert!(pass.func().type_().is_valid());
                assert_eq!(pass.func().type_().params()[0].type_(), &Type::nullary(id));
                assert_eq!(pass.func().type_().return_type(), &Type::nullary(id));
                assert!(unit.functions.contains_key(pass.key()));
            },
        );
    }
}

#[test]
fn native_unknown_types_and_integer_conversions_still_fail() {
    check(
        "import function Missing(value: DefinitelyMissingParityType);",
        |_, _, messages| {
            assert_eq!(messages.len(), 1, "{messages:?}");
            assert!(messages[0].contains("`DefinitelyMissingParityType` is not a known type"));
        },
    );
    for name in NAMES {
        for source in [
            format!("function Bad(value: Uint64): {name} {{ return value; }}"),
            format!("function Bad(value: {name}): Uint64 {{ return value; }}"),
        ] {
            check(&source, |_, _, messages| {
                assert_eq!(messages.len(), 1, "{name}: {messages:?}");
                assert!(!messages[0].contains("not a known type"));
                assert!(!messages[0].contains("invalid signature"));
            });
        }
    }
}

#[test]
fn native_token_operators_are_valid_without_poisoning_equality_fallback() {
    let mut source = String::new();
    for name in NAMES {
        source.push_str(&format!(
            "import operator ==(lhs, rhs: {name}): Bool;
             import operator !=(lhs, rhs: {name}): Bool;
             function Equal{name}(lhs, rhs: {name}): Bool {{ return lhs == rhs; }}
             function NotEqual{name}(lhs, rhs: {name}): Bool {{ return lhs != rhs; }}"
        ));
    }
    source.push_str(
        "function Fallback(lhs, rhs: Int32): Bool { return lhs == rhs; }
                     function NotFallback(lhs, rhs: Int32): Bool { return lhs != rhs; }",
    );
    check(&source, |unit, symbols, messages| {
        assert!(messages.is_empty(), "{messages:?}");
        let mut calls = 0;
        for entry in symbols.free_functions() {
            if entry.name().to_string().starts_with("Operator") {
                assert!(entry.func().type_().is_valid());
            }
            let Some(function) = unit.functions.get(entry.key()) else {
                continue;
            };
            let name = entry.name().to_string();
            if name == "Identity" {
                continue;
            }
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

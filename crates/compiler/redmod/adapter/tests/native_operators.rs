use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, ir,
};
use redscript_redmod_adapter::{CapabilityError, Error, parse_file_with_metadata, process_sources};

fn analyze(
    source: &str,
    community: &str,
    inspect: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("community.reds", community);
    let file = sources.push_back("operators.script", source);
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
    assert!(errors.is_empty(), "{errors:?}");

    let codes = reporter
        .reported()
        .iter()
        .map(|error| error.code().to_string())
        .collect::<Vec<_>>();
    inspect(&unit, &symbols, &codes);
}

#[test]
fn lazy_calls_share_prefix_array_rejection_with_community_sources() {
    for (source, community) in [
        (
            "import function Lazy(skip value: array<Int32>); function Run() { Lazy([1]); }",
            "",
        ),
        (
            "function Run() { Lazy([1]); }",
            "native func Lazy(skip value: [Int32]);",
        ),
    ] {
        analyze(source, community, |_, _, codes| {
            assert!(
                codes.iter().any(|code| code == "UNSAFE_ARRAY_EVALUATION"),
                "{codes:?}"
            );
        });
    }
}

fn returned_call<'ctx>(
    unit: &LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> String {
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    let ir::Stmt::Return(Some(expr), _) = &unit.functions[entry.key()].block.stmts[0] else {
        panic!("expected return in {name}")
    };
    let ir::Expr::Call { call, .. } = &**expr else {
        panic!("expected return call in {name}")
    };
    let ir::Call::FreeFunction { function, .. } = &**call else {
        panic!("expected free function in {name}")
    };
    let selected = symbols
        .free_functions()
        .find(|entry| entry.key() == function)
        .unwrap();
    format!(
        "{} -> {}",
        selected.name(),
        selected.func().type_().return_type()
    )
}

#[test]
fn operator_tokens_map_to_shared_names_without_changing_signatures() {
    for (token, name) in [
        ("+", "OperatorAdd"),
        ("-", "OperatorSubtract"),
        ("*", "OperatorMultiply"),
        ("/", "OperatorDivide"),
        ("%", "OperatorModulo"),
        ("==", "OperatorEqual"),
        ("!=", "OperatorNotEqual"),
        ("<", "OperatorLess"),
        ("<=", "OperatorLessEqual"),
        (">", "OperatorGreater"),
        (">=", "OperatorGreaterEqual"),
        ("&&", "OperatorLogicAnd"),
        ("||", "OperatorLogicOr"),
        ("&", "OperatorAnd"),
        ("|", "OperatorOr"),
        ("^", "OperatorXor"),
        ("!", "OperatorLogicNot"),
        ("~", "OperatorBitNot"),
        ("+=", "OperatorAssignAdd"),
        ("-=", "OperatorAssignSubtract"),
        ("*=", "OperatorAssignMultiply"),
        ("/=", "OperatorAssignDivide"),
        ("&=", "OperatorAssignAnd"),
        ("|=", "OperatorAssignOr"),
    ] {
        let source = format!("import operator {token}(out a, b: Int32): Float;");
        let sources = SourceMap::new();
        let file = sources.push_back("operators.script", &source);
        let (parsed, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{token}: {errors:?}");
        let module = parsed.unwrap().module;
        let ast::Item::Function(function) = &module.items[0].0.item else {
            panic!("expected function")
        };
        assert_eq!(function.name.0, name);
        assert_eq!(
            &source[function.name.1.start as usize..function.name.1.end as usize],
            token
        );
        assert_eq!(function.params.len(), 2);
        assert!(
            function
                .params
                .iter()
                .all(|(param, _)| { param.qualifiers.contains(ast::ParamQualifiers::OUT) })
        );
        assert!(
            module.items[0]
                .0
                .qualifiers
                .contains(ast::ItemQualifiers::NATIVE)
        );
        assert!(function.body.is_none());
        assert!(matches!(
            &function.return_type.as_ref().unwrap().0,
            ast::Type::Named { name: "Float", .. }
        ));
    }
}

#[test]
fn operators_resolve_and_conversion_declarations_remain_queryable() {
    analyze(
        r#"
        struct Value {}
        operator +(a, b: Value): Value { return a; }
        operator -(a: Value): Value { return a; }
        operator -(a, b: Value): Value { return b; }
        operator !(a: Value): Value { return a; }
        operator ==(a, b: Value): Value { return a; }
        cast(a: Value): Int32 { return 1; }
        cast(a: Value): Float { return 1.0; }
        function Add(a, b: Value): Value { return a + b; }
        function Neg(a: Value): Value { return -a; }
        function Sub(a, b: Value): Value { return a - b; }
        function Not(a: Value): Value { return !a; }
        function Equal(a, b: Value): Value { return a == b; }

        "#,
        "func CommunityAdd(a: Value, b: Value) -> Value = a + b;",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for name in ["Add", "Neg", "Sub", "Not", "Equal", "CommunityAdd"] {
                let expected = match name {
                    "Add" | "CommunityAdd" => "OperatorAdd",
                    "Neg" => "OperatorNeg",
                    "Sub" => "OperatorSubtract",
                    "Not" => "OperatorLogicNot",
                    "Equal" => "OperatorEqual",
                    _ => unreachable!(),
                };
                assert_eq!(
                    returned_call(unit, symbols, name),
                    format!("{expected} -> Value")
                );
            }

            for entry in symbols.free_functions() {
                let name = entry.name().to_string();
                if name.starts_with("Operator") {
                    assert!(entry.func().flags().is_operator());
                } else if name == "Cast" {
                    assert!(entry.func().flags().is_cast());
                }
            }
        },
    );
}

#[test]
fn logical_overloads_keep_original_operands_and_non_bool_results() {
    analyze(
        "struct Value {}
         class Object {}
         operator !(a: Value): Value { return a; }
         operator !(a: Object): Int32 { return 7; }
         operator &&(a, b: Value): Value { return a; }
         operator ||(a, b: Object): Object { return a; }
         function NotValue(a: Value): Value { return !a; }
         function NotObject(a: Object): Int32 { return !a; }
         function And(a, b: Value): Value { return a && b; }
         function Or(a, b: Object): Object { return a || b; }",
        "native func OperatorLogicNot(a: Bool) -> Bool;
         native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool;
         native func OperatorLogicOr(a: Bool, b: Bool) -> Bool;",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, expected) in [
                ("NotValue", "OperatorLogicNot -> Value"),
                ("NotObject", "OperatorLogicNot -> Int32"),
                ("And", "OperatorLogicAnd -> Value"),
                ("Or", "OperatorLogicOr -> Object"),
            ] {
                assert_eq!(returned_call(unit, symbols, name), expected);
                let entry = symbols
                    .free_functions()
                    .find(|entry| entry.name().to_string() == name)
                    .unwrap();
                let ir::Stmt::Return(Some(expr), _) = &unit.functions[entry.key()].block.stmts[0]
                else {
                    panic!()
                };
                let ir::Expr::Call { call, .. } = &**expr else {
                    panic!()
                };
                assert!(
                    call.args()
                        .iter()
                        .all(|arg| matches!(arg, ir::Expr::Local(..)))
                );
            }
        },
    );
}

#[test]
fn typed_operator_resolution_rejects_wrong_counts_and_preserves_ambiguity() {
    analyze(
        "struct Value {}
         operator +(a: Value): Value { return a; }
         operator ~(a, b: Value): Value { return a; }
         function Binary(a: Value): Value { return a + a; }
         function Unary(a: Value): Value { return ~a; }",
        "",
        |_, _, codes| assert_eq!(codes, ["INVALID_ARG_COUNT", "INVALID_ARG_COUNT"]),
    );
    analyze(
        "class Value {}
         operator !(a: Value): Value { return a; }
         operator !(b: Value): Int32 { return 1; }
         function Ambiguous(a: Value): Value { return !a; }",
        "native func OperatorLogicNot(a: Bool) -> Bool;",
        |_, _, codes| assert_eq!(codes, ["MULTIPLE_MATCHING_OVERLOADS"]),
    );
}

#[test]
fn native_conversions_fail_closed_without_disabling_community_casts() {
    analyze(
        "struct Value {}
         cast(a: Value): Int32 { return 1; }
         cast(a: ref<Value>): Float { return 1.0; }
         function Native(a: Value): Int32 { return (Int32)a; }
         function Reference(a: Value): Float { return Cast<Float>(a); }
         function Community(a: Bool): Int64 { return (Int64)a; }",
        "native func Cast(a: Bool) -> Int64;
         func Mixed(a: Value) -> Int32 = Cast<Int32>(a);",
        |unit, symbols, codes| {
            assert_eq!(
                codes,
                [
                    "UNSUPPORTED_NATIVE_CONVERSION",
                    "UNSUPPORTED_NATIVE_CONVERSION",
                    "UNSUPPORTED_NATIVE_CONVERSION"
                ]
            );
            assert_eq!(returned_call(unit, symbols, "Community"), "Cast -> Int64");
            assert_eq!(
                symbols
                    .free_functions()
                    .filter(|entry| entry.name().to_string() == "Cast")
                    .count(),
                3
            );
        },
    );
}

#[test]
fn malformed_signatures_and_unrepresented_dispatch_have_targeted_errors() {
    for (source, expected, message) in [
        (
            "operator +(): Int32 {}",
            CapabilityError::InvalidOperatorSignature("operators require one or two parameters"),
            "one or two",
        ),
        (
            "operator +(a, b, c: Int32): Int32 {}",
            CapabilityError::InvalidOperatorSignature("operators require one or two parameters"),
            "one or two",
        ),
        (
            "cast(a, b: Int32): Float {}",
            CapabilityError::InvalidConversionSignature(
                "conversions require exactly one parameter",
            ),
            "exactly one",
        ),
        (
            "cast(a: Int32) {}",
            CapabilityError::InvalidConversionSignature(
                "conversions require an explicit non-Void return type",
            ),
            "non-Void",
        ),
        (
            "cast(a: Int32): Void {}",
            CapabilityError::InvalidConversionSignature(
                "conversions require an explicit non-Void return type",
            ),
            "non-Void",
        ),
        (
            "struct Value { operator +(a, b: Value): Value {} }",
            CapabilityError::InvalidOperatorSignature("operators must be declared in global scope"),
            "global scope",
        ),
        (
            "struct Value { cast(a: Int32): Value {} }",
            CapabilityError::InvalidConversionSignature(
                "conversions must be declared in global scope",
            ),
            "global scope",
        ),
        (
            "operator [](a: Int32, b: Int32): Int32 {}",
            CapabilityError::OperatorDeclaration,
            "dispatch",
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("operators.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_none(), "{source}");
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        assert!(matches!(errors[0], Error::Capability { kind, .. } if kind == expected));
        assert!(errors[0].to_string().contains(message), "{errors:?}");
        assert_eq!(errors[0].span().file, file);
    }
}

#[test]
fn declaration_and_use_types_are_checked_by_shared_resolution() {
    analyze(
        "operator +(a, b: Int32): Int32 { return true; }
         cast(a: Int32): Float { return false; }",
        "",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR", "TYPE_ERR"]),
    );
    analyze(
        "struct Value {}
         operator +(a, b: Value): Value { return a; }
         cast(a: Value): Float { return 1.0; }
         function BadOperator(a: Value): Value { return a + true; }
         function BadCast(a: Bool): Float { return (Float)a; }",
        "",
        |_, _, codes| {
            assert_eq!(codes, ["UNRESOLVED_FN", "UNSUPPORTED_NATIVE_CONVERSION"]);
        },
    );
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn authored_official_operator_and_conversion_characterization() {
    use std::path::PathBuf;
    use std::process::Command;

    let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let parent = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "native-operators-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    for (name, source, diagnostic) in [
        (
            "operators",
            "struct OpProbe {} operator +(a, b: OpProbe): OpProbe { return a; } operator -(a: OpProbe): OpProbe { return a; } function Probe(a, b: OpProbe): OpProbe { return -(a + b); }",
            None,
        ),
        (
            "unusual-types",
            "struct OpProbe {} operator ==(a, b: OpProbe): OpProbe { return a; } operator !(a: OpProbe): OpProbe { return a; } function Probe(a: OpProbe): OpProbe { return !a; }",
            None,
        ),
        (
            "cast-declaration",
            "struct OpProbe {} cast(a: OpProbe): Int32 { return 1; }",
            None,
        ),
        (
            "cast-selection",
            "struct OpProbe {} cast(a: OpProbe): Int32 { return 1; } function Probe(a: OpProbe): Int32 { return (Int32)a; }",
            Some("Unable to convert"),
        ),
        (
            "cast-ref-selection",
            "struct OpProbe {} cast(a: ref<OpProbe>): Int32 { return 1; } function Probe(a: OpProbe): Int32 { return (Int32)a; }",
            Some("Unable to convert"),
        ),
        (
            "cast-explicit-ref-selection",
            "struct OpProbe {} cast(a: ref<OpProbe>): Int32 { return 1; } function Probe(a: ref<OpProbe>): Int32 { return (Int32)a; }",
            Some("Unable to convert"),
        ),
        (
            "not-object",
            "class OpProbe {} operator !(a: OpProbe): Int32 { return 7; } function Probe(a: OpProbe): Int32 { return !a; }",
            None,
        ),
        (
            "logical-struct",
            "struct OpProbe {} operator &&(a, b: OpProbe): OpProbe { return a; } function Probe(a, b: OpProbe): OpProbe { return a && b; }",
            None,
        ),
        (
            "arity",
            "operator +(a, b, c: Int32): Int32 { return a; }",
            Some("should have one or two parameters"),
        ),
        (
            "cast-arity",
            "cast(a, b: Int32): Float { return 1.0; }",
            Some("should have exactly one parameter"),
        ),
        (
            "cast-return",
            "cast(a: Int32) {}",
            Some("must have a return value"),
        ),
    ] {
        let directory = parent.join(name);
        std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
        let output = Command::new(&executable)
            .args(["validate-mod-scripts", "-compile", "-root"])
            .arg(&root)
            .arg("-directory")
            .arg(&directory)
            .output()
            .unwrap();
        std::fs::write(directory.join("stdout.log"), &output.stdout).unwrap();
        std::fs::write(directory.join("stderr.log"), &output.stderr).unwrap();
        let log = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        if let Some(diagnostic) = diagnostic {
            assert!(!output.status.success(), "{name}: {log}");
            assert!(log.contains(diagnostic), "{name}: {log}");
        } else {
            assert!(output.status.success(), "{name}: {log}");
        }
        println!("{name}: {}", directory.display());
    }
}

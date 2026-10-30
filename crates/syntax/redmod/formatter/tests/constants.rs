use redscript_redmod_ast::{Constant, Expr, FileId, Item, UnOp};
use redscript_redmod_formatter::{PrintErrorKind, print_module};

macro_rules! float_roundtrip {
    ($test:ident, $ty:ident, $variant:ident, $suffix:literal) => {
        #[test]
        fn $test() {
            let file = FileId::from_i32(0);
            let (module, errors) =
                redscript_redmod_parser::parse_module("var value: Float = 0.0;", file);
            assert!(errors.is_empty());
            let mut module = module.unwrap();
            for value in [
                0.0,
                -0.0,
                1.0,
                -42.0,
                1.25,
                1e-20,
                1e20,
                $ty::MIN_POSITIVE,
                $ty::from_bits(1),
                $ty::MAX,
                $ty::MIN,
            ] {
                let Item::Field(field) = &mut module.items[0].0.item else {
                    panic!()
                };
                field.default.as_mut().unwrap().0 = Expr::Constant(Constant::$variant(value));
                let output = print_module(&module).unwrap();
                let literal = output.split(" = ").nth(1).unwrap().trim_end_matches(";\n");
                assert!(!literal.contains(['e', 'E']), "{literal}");
                assert!(literal.contains('.'), "{literal}");
                assert!(literal.ends_with($suffix), "{literal}");
                assert_eq!(
                    literal
                        .trim_end_matches($suffix)
                        .parse::<$ty>()
                        .unwrap()
                        .to_bits(),
                    value.to_bits()
                );
                let (parsed, errors) = redscript_redmod_parser::parse_module(&output, file);
                assert!(errors.is_empty(), "{output}: {errors:?}");
                let parsed = parsed.unwrap();
                let Item::Field(field) = &parsed.items[0].0.item else {
                    panic!()
                };
                let expr = &field.default.as_ref().unwrap().0;
                // Negative literals parse as unary negation, not signed constants.
                let actual = match expr {
                    Expr::Constant(Constant::$variant(value)) => *value,
                    Expr::UnOp {
                        op: UnOp::Neg,
                        expr,
                    } => {
                        let Expr::Constant(Constant::$variant(value)) = &expr.0 else {
                            panic!()
                        };
                        -*value
                    }
                    _ => panic!("{expr:?}"),
                };
                assert_eq!(actual.to_bits(), value.to_bits(), "{output}");
            }
        }
    };
}

#[test]
fn raw_literal_values_and_errors() {
    let file = FileId::from_i32(0);
    let (module, errors) = redscript_redmod_parser::parse_module("var value: String = \"\";", file);
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    let mut snapshots = String::new();
    for value in [
        r"assets\widgets\button",
        r"assets\",
        r"\n\t\u{22}\\",
        "first\n  second",
        "café",
    ] {
        for constant in [
            Constant::String(value.into()),
            Constant::CName(value.into()),
        ] {
            let Item::Field(field) = &mut module.items[0].0.item else {
                panic!()
            };
            field.default.as_mut().unwrap().0 = Expr::Constant(constant.clone());
            let output = print_module(&module).unwrap();
            assert!(output.contains(value), "{output}");
            snapshots.push_str(&output);
            let (parsed, errors) = redscript_redmod_parser::parse_module(&output, file);
            assert!(errors.is_empty(), "{errors:?}");
            let parsed = parsed.unwrap();
            let Item::Field(field) = &parsed.items[0].0.item else {
                panic!()
            };
            assert_eq!(field.default.as_ref().unwrap().0, Expr::Constant(constant));
        }
    }
    insta::assert_snapshot!("raw_literal_values", snapshots);
    let mut failures = String::new();
    for (constant, kind) in [
        (
            Constant::String("say\"hello".into()),
            PrintErrorKind::LiteralDelimiter,
        ),
        (
            Constant::CName("can't \"quote\"".into()),
            PrintErrorKind::LiteralDelimiter,
        ),
        (Constant::String("\0".into()), PrintErrorKind::LiteralNul),
        (
            Constant::CName("can't\0".into()),
            PrintErrorKind::LiteralNul,
        ),
        (
            Constant::CName("can't\t".into()),
            PrintErrorKind::LiteralControl,
        ),
        (
            Constant::String("\t".into()),
            PrintErrorKind::LiteralControl,
        ),
        (
            Constant::String("\r".into()),
            PrintErrorKind::LiteralControl,
        ),
        (
            Constant::String("\u{85}".into()),
            PrintErrorKind::LiteralControl,
        ),
        (
            Constant::Resource(r"raw\path".into()),
            PrintErrorKind::UnverifiedLiteral,
        ),
        (
            Constant::TweakDbId("first\nsecond".into()),
            PrintErrorKind::UnverifiedLiteral,
        ),
    ] {
        let Item::Field(field) = &mut module.items[0].0.item else {
            panic!()
        };
        let span = field.default.as_ref().unwrap().1;
        field.default.as_mut().unwrap().0 = Expr::Constant(constant);
        let error = print_module(&module).unwrap_err();
        assert_eq!(error.kind, kind);
        assert_eq!(error.span, Some(span));
        failures.push_str(&format!("{:?}: {error}\n", error.kind));
    }
    insta::assert_snapshot!("literal_errors", failures);

    // This value uses the verified single-quote CName form, not n"...".
    let Item::Field(field) = &mut module.items[0].0.item else {
        panic!()
    };
    field.default.as_mut().unwrap().0 =
        Expr::Constant(Constant::CName("name with \"quotes\"".into()));
    let output = print_module(&module).unwrap();
    assert!(output.contains("'name with \"quotes\"'"));
    let (parsed, errors) = redscript_redmod_parser::parse_module(&output, file);
    assert!(errors.is_empty());
    let Item::Field(field) = &parsed.as_ref().unwrap().items[0].0.item else {
        panic!()
    };
    assert_eq!(
        field.default.as_ref().unwrap().0,
        Expr::Constant(Constant::CName("name with \"quotes\"".into()))
    );
}

float_roundtrip!(float_plain_decimal_roundtrip, f32, F32, "");
float_roundtrip!(double_plain_decimal_roundtrip, f64, F64, "d");

#[test]
fn signed_integer_minima_roundtrip_as_constants() {
    let file = FileId::from_i32(0);
    let (module, errors) = redscript_redmod_parser::parse_module("var value: Int64 = 0;", file);
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    for (value, spelling) in [
        (Constant::I32(i32::MIN), "-2147483648"),
        (Constant::I64(i64::MIN), "-9223372036854775808l"),
    ] {
        let Item::Field(field) = &mut module.items[0].0.item else {
            panic!()
        };
        field.default.as_mut().unwrap().0 = Expr::Constant(value.clone());
        let output = print_module(&module).unwrap();
        assert_eq!(output, format!("var value: Int64 = {spelling};\n"));
        let (reparsed, errors) = redscript_redmod_parser::parse_module(&output, file);
        assert!(errors.is_empty(), "{output}: {errors:?}");
        let reparsed = reparsed.unwrap();
        let Item::Field(field) = &reparsed.items[0].0.item else {
            panic!()
        };
        assert_eq!(field.default.as_ref().unwrap().0, Expr::Constant(value));
        assert_eq!(print_module(&reparsed).unwrap(), output);
    }
}

#[test]
fn float_output() {
    let source = "function Run() { Use(1e-20, 1e20, -0.0, 1e-20d, 1e20d, -0.0d); }";
    let (module, errors) = redscript_redmod_parser::parse_module(source, FileId::from_i32(0));
    assert!(errors.is_empty());
    insta::assert_snapshot!(print_module(&module.unwrap()).unwrap());
}

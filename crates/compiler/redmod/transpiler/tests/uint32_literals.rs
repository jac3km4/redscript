//! Exact Uint32 expression constants, with opt-in official bundle evidence.
//! REDmod field defaults use unsigned decimal spelling instead of signed-bit casts.
mod support;

const VALUES: [u32; 6] = [
    0,
    1,
    2_147_483_647,
    2_147_483_648,
    3_670_998_070,
    4_294_967_295,
];

use support::try_emit_source as emit;

fn fields() -> String {
    let mut source = String::from("class Uint32LiteralFields {\n");
    for (i, value) in VALUES.iter().enumerate() {
        source.push_str(&format!(" public let field{i}: Uint32 = {value}u;\n"));
    }
    source.push_str("}\n");
    source
}

fn expressions() -> String {
    VALUES
        .iter()
        .enumerate()
        .map(|(i, value)| format!("func Uint32Literal{i}() -> Uint32 {{ return {value}u; }}\n"))
        .collect()
}

#[test]
fn unsigned_boundaries_use_exact_signed_bits_without_widening() {
    let text = emit(&expressions()).unwrap();
    for value in [
        "Cast<Uint32>(0)",
        "Cast<Uint32>(1)",
        "Cast<Uint32>(2147483647)",
        "Cast<Uint32>(-2147483648)",
        "Cast<Uint32>(-623969226)",
        "Cast<Uint32>(-1)",
    ] {
        assert!(text.contains(&format!("return {value};")), "{text}");
    }
    assert!(!text.contains("Int64"), "{text}");
    assert!(!text.contains("Uint64"), "{text}");
    for (ty, literal) in [("Int64", "2147483648l"), ("Uint64", "2147483648ul")] {
        let errors = emit(&format!("func Wide() -> {ty} {{ return {literal}; }}")).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("unverified target range")),
            "{errors:?}"
        );
    }
}

#[test]
fn field_defaults_use_literal_only_unsigned_decimals() {
    let text = emit(&fields()).unwrap();
    for (i, value) in VALUES.iter().enumerate() {
        assert!(
            text.contains(&format!("var m_field{i}: Uint32 = {value};")),
            "{text}"
        );
    }
    assert!(!text.contains("Cast<"), "{text}");
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn exact_values_with_official_compiler() {
    use redscript_io::Instr;
    use redscript_test_support::{OfficialBundle, OfficialCompiler};

    let source = format!(
        "{}{}",
        emit(&expressions()).unwrap(),
        emit(&fields()).unwrap()
    );
    let output = OfficialCompiler::from_env().compile("uint32-literals", &source);
    output.assert_version("v1.2.2");
    let directory = &output.directory;
    assert!(
        std::fs::metadata(directory.join("r6/cache/final.redscripts.h64"))
            .unwrap()
            .len()
            > 0
    );
    let bytes = output.bundle_bytes();
    let bundle = OfficialBundle::new(&bytes);
    let reader = redscript_io::BundleReader::new(&bytes).unwrap();
    let mut listing = String::new();
    for (i, expected) in VALUES.iter().enumerate() {
        let code = bundle.function_code_prefix(&format!("Uint32Literal{i};"));
        listing.push_str(&format!("Uint32Literal{i} ({expected}): {code:?}\n"));
        let ops = code
            .iter()
            .filter(|op| !matches!(op, Instr::Breakpoint(_)))
            .collect::<Vec<_>>();
        assert!(
            matches!(ops.as_slice(), [Instr::Return, Instr::U32Const(actual), Instr::Nop] if actual == expected),
            "{code:?}"
        );
        let redscript_io::Definition::Field(field) = reader
            .definitions()
            .get(bundle.definition_index(&format!("field{i}"), 8))
            .unwrap()
        else {
            panic!("expected field")
        };
        assert_eq!(field.defaults().len(), 1, "{field:?}");
        assert_eq!(
            field.defaults()[0].value().parse::<u32>().unwrap(),
            *expected
        );
        listing.push_str(&format!("field{i}: {:?}\n", field.defaults()));
    }
    std::fs::write(directory.join("decoded.txt"), listing).unwrap();
}

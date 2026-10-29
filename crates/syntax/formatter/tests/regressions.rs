mod common;

use common::{assert_preserved, format};
use redscript_formatter::FormatSettings;

#[test]
fn zero_sized_array_parameters_do_not_panic() {
    assert_preserved(
        "func F(value: [Int32; 0]) {}",
        &FormatSettings::default(),
        "test.reds",
    );
}

#[test]
fn large_expression_widths_do_not_overflow_or_wrap() {
    for length in [65_533, 65_536] {
        let value = "x".repeat(length);
        for expression in [
            format!("[\"{value}\"]"),
            format!("Call(\"{value}\")"),
            format!("value.Method(\"{value}\")"),
        ] {
            let source = format!("func F() {{ return {expression}; }}");
            let formatted = assert_preserved(&source, &FormatSettings::default(), "test.reds");
            assert!(
                formatted.contains("\n      \"") || formatted.contains("\n    \""),
                "large expression should wrap"
            );
        }
    }
}

#[test]
fn overflowing_float_chain_bases_do_not_panic() {
    for trunc_sig_digits in [None, Some(3)] {
        let settings = FormatSettings {
            trunc_sig_digits,
            ..FormatSettings::default()
        };
        for (literal, rendered) in [("1.0e100", "inf"), ("1.0e400d", "infd")] {
            for suffix in [".Member", ".Method()", "[0].Member"] {
                let source = format!("func F() {{ return {literal}{suffix}; }}");
                let formatted = format(&source, &settings, "test.reds");
                assert!(
                    formatted.contains(&format!("return {rendered}{suffix};")),
                    "{formatted}"
                );
            }
        }
    }
}

#[test]
fn overflowing_floats_in_lists_do_not_panic() {
    for trunc_sig_digits in [None, Some(3)] {
        let settings = FormatSettings {
            trunc_sig_digits,
            ..FormatSettings::default()
        };
        for (expression, expected) in [
            ("[1.0e100, 1.0e400d]", "[inf, infd]"),
            ("Call(1.0e100, 1.0e400d)", "Call(inf, infd)"),
            ("value.Method(1.0e100, 1.0e400d)", "value.Method(inf, infd)"),
        ] {
            let source = format!("func F() {{ return {expression}; }}");
            let formatted = format(&source, &settings, "test.reds");
            assert!(
                formatted.contains(&format!("return {expected};")),
                "{formatted}"
            );
        }
    }
}

#[test]
fn large_indentation_does_not_overflow() {
    let settings = FormatSettings {
        indent: u16::MAX,
        ..FormatSettings::default()
    };
    let formatted = assert_preserved("func F() { if true { return 1; } }", &settings, "test.reds");
    assert!(formatted.contains(&format!(
        "{}return 1;",
        " ".repeat(2 * usize::from(u16::MAX))
    )));
}

#[test]
fn lambda_chain_bases_keep_parentheses() {
    for expression in [
        "((x) -> x).Member",
        "((x) -> x).Method()",
        "((x) -> x).Member.Other",
        "((x) -> x)[index].Member",
        "((x) -> x)(value)",
        "((x) -> x) + other",
        "((x) -> { return x; }).Member",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        assert_preserved(&source, &FormatSettings::default(), "test.reds");
    }
}

#[test]
fn nested_assignments_keep_parentheses() {
    for expression in [
        "(a = b) + c",
        "a + (b = c)",
        "(a = b).Member",
        "(a = b).Method()",
        "(a = b)[c]",
        "(a = b)(c)",
        "!(a = b)",
        "(a = b) as Bool",
        "(a = b) ? c : d",
        "(a = b) = c",
        "a = (b = c)",
        "a = b ? c : d",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        assert_preserved(&source, &FormatSettings::default(), "test.reds");
    }
}

#[test]
fn conditional_conditions_keep_parentheses() {
    for expression in [
        "(a ? b : c) ? d : e",
        "((a ? b : c) ? d : e) ? f : g",
        "a ? (b ? c : d) : e",
        "a ? b : (c ? d : e)",
        "a + b ? c : d",
        "a as Bool ? b : c",
        "!a ? b : c",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        assert_preserved(&source, &FormatSettings::default(), "test.reds");
    }
}

#[test]
fn chain_wrapping_counts_the_base_expression() {
    for (max_width, breaks) in [(24, false), (23, true)] {
        let settings = FormatSettings {
            max_width,
            ..FormatSettings::default()
        };
        let formatted = assert_preserved(
            "func F() { abcdefghijklmnop.Value; }",
            &settings,
            "test.reds",
        );
        assert_eq!(
            formatted.contains("abcdefghijklmnop\n    .Value"),
            breaks,
            "{formatted}"
        );
    }
}

#[test]
fn parenthesized_chain_prefixes_are_emitted_once() {
    for expression in [
        "a && /* keep */ (b || c)",
        "a * /* keep */ (b + c)",
        "/* keep */ (a + b).Method()",
        "/* keep */ a.Member.Method()",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        let formatted = assert_preserved(&source, &FormatSettings::default(), "test.reds");
        assert_eq!(formatted.matches("/* keep */").count(), 1, "{formatted}");
    }
}

#[test]
fn pattern_prefixes_are_preserved() {
    let formatted = assert_preserved(
        "func F() { if let /* binding explanation */ bound = value {} }",
        &FormatSettings::default(),
        "test.reds",
    );
    assert_eq!(formatted.matches("/* binding explanation */").count(), 1);
}

#[test]
fn inline_line_comments_keep_their_delimiters() {
    for comment in [
        "// literal */ marker",
        "// unmatched /* marker",
        "// ordinary comment",
    ] {
        let source = format!("func F() {{ return {comment}\n true; }}");
        let formatted = assert_preserved(&source, &FormatSettings::default(), "test.reds");
        assert!(formatted.contains(&format!("{comment}\n")), "{formatted}");
    }
}

#[test]
fn aggregate_patterns_keep_explicit_fields() {
    for pattern in [
        "Outer { inner: Inner { value } }",
        "Outer { inner: value? }",
        "Outer { inner: [first, second] }",
        "Outer { inner: value as Int32 }",
        "Outer { inner: renamed }",
        "Outer { inner }",
    ] {
        let source = format!("func F() {{ if let {pattern} = value {{}} }}");
        let formatted = assert_preserved(&source, &FormatSettings::default(), "test.reds");
        assert!(formatted.contains(pattern), "{formatted}");
    }
}

#[test]
fn integer_chain_bases_keep_member_access_unambiguous() {
    for literal in ["0", "1", "2147483647", "1l", "1u", "1ul"] {
        for suffix in [
            ".Member",
            ".Method()",
            ".Member.Other",
            "[0].Member",
            "(value).Member",
        ] {
            let source = format!("func F() {{ return ({literal}){suffix}; }}");
            assert_preserved(&source, &FormatSettings::default(), "test.reds");
        }
    }
}

#[test]
fn doubles_keep_their_suffix() {
    for trunc_sig_digits in [None, Some(3)] {
        let settings = FormatSettings {
            trunc_sig_digits,
            ..FormatSettings::default()
        };
        let formatted = assert_preserved("func F() { let value = 1.25d; }", &settings, "test.reds");
        assert!(formatted.contains("1.25d"), "{formatted}");
    }
}

use std::io::Cursor;

use redscript::bundle::ScriptBundle;
use redscript_compiler::parser;
use redscript_compiler::source_map::Files;
use redscript_compiler::unit::CompilationUnit;

const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

/// Pins the diagnostics of permissive type-checking (used by tooling such as the language
/// server) to their 0.5.32 behaviour.
///
/// The diagnostics of a permissive type-check of `source`, as `message @ span`.
fn permissive_diagnostics(source: &str) -> Vec<String> {
    let module = parser::parse_str(source).unwrap();
    let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF)).unwrap();
    let output = CompilationUnit::new_with_defaults(&mut scripts.pool)
        .unwrap()
        .typecheck(vec![module], &Files::default(), false, true)
        .unwrap();
    output
        .diagnostics()
        .iter()
        .map(|diagnostic| {
            let span = diagnostic.span();
            format!("{diagnostic} @ {}..{}", span.low, span.high)
        })
        .collect()
}

#[test]
fn permissive_reports_failed_overload_before_its_arguments() {
    let source = "
        func Takes(a: Int32) {}

        func Test() {
            Takes(Missing);
        }";

    assert_eq!(
        permissive_diagnostics(source),
        [
            "unresolved reference 'Missing' @ 74..81",
            "arguments passed to 'Takes' do not match any of the overloads:\n1st argument: expected 'Int32', given 'Null' \
             @ 68..82",
            "unresolved reference 'Missing' @ 74..81",
        ]
    );
}

#[test]
fn permissive_drops_diagnostics_before_a_fatal_error() {
    let source = "
        func Test() {
            Missing;
            let s = \"a\";
            s.NoSuchMethod();
        }";

    // the diagnostics reported before the error that stops the checking of a body are dropped
    assert_eq!(permissive_diagnostics(source), ["'String' has no members @ 81..97"]);
}

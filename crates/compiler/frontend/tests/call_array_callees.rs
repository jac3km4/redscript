use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, infer_from_sources,
};

fn codes(source: &str) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("callees.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    reporter
        .reported()
        .iter()
        .map(|diagnostic| diagnostic.code().to_owned())
        .collect()
}

#[test]
fn closure_callee_cannot_be_changed_by_array_prefix() {
    assert_eq!(
        codes(
            "native func Change(out f: ([Int32]) -> Int32) -> Int32;
             func Run(f: ([Int32]) -> Int32) -> Int32 { return f([Change(f)]); }"
        ),
        ["UNSAFE_ARRAY_EVALUATION"]
    );
}

#[test]
fn type_qualified_static_callee_allows_array_arguments() {
    assert!(
        codes(
            "class Host { public static func Take(values: [Int32]) {} }
             func Run() { Host.Take([1]); Host.Take([]); }"
        )
        .is_empty()
    );
}

#[test]
fn runtime_member_callees_still_reject_array_prefixes() {
    for declaration in [
        "public func Take(values: [Int32]) {}",
        "public let Take: ([Int32]) -> Int32;",
    ] {
        assert_eq!(
            codes(&format!(
                "class Host {{ {declaration} }} func Run(host: Host) {{ host.Take([1]); }}"
            )),
            ["UNSAFE_ARRAY_EVALUATION"],
            "{declaration}"
        );
    }
}

#[test]
fn free_function_and_closure_without_array_prefix_remain_valid() {
    assert!(
        codes(
            "func Take(values: [Int32]) {} func Run(f: ([Int32]) -> Int32, values: [Int32]) {
                 Take([1]); f(values);
             }"
        )
        .is_empty()
    );
}

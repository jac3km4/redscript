use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn check(body: &str, expected: &[&str]) {
    let source = format!(
        "class V {{ public var field: Int32; public const function Read() -> Int32 {{ return this.field; }} public function Change() {{}} }} struct S {{ var field: Int32; }} function Take(v: V) {{}} function Out(out v: V) {{}} function F(v: V) {{ {body} }}"
    );
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("const.script", &source);
    let (parsed, errors) = parse_file_with_metadata(&source, file);
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
    let (unit, _, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(
        unit.functions
            .values()
            .flat_map(|f| f.locals.iter())
            .any(|l| l.is_const)
    );
    let spans = reporter
        .reported()
        .iter()
        .map(|d| {
            assert!(
                d.to_string().contains("cannot write through a const local"),
                "{d:?}"
            );
            let span = d.span();
            assert_eq!(span.file, file);
            &source[span.start as usize..span.end as usize]
        })
        .collect::<Vec<_>>();
    assert_eq!(spans, expected, "{body}");
}

#[test]
fn handles_rebind_and_forward_but_referents_remain_const() {
    check(
        "const var x: V; x = v; x = v; x.Read(); Take(x); Out(x);",
        &[],
    );
    check(
        "const var x: weak<V>; x = v; x = v; x.Read(); Take(x);",
        &[],
    );
    check("const var x: V = v; x.Read();", &[]);
    check(
        "const var x: V; x.field = 1; x.Change();",
        &["x.field", "x"],
    );
    check(
        "const var x: weak<V>; x.field = 1; x.Change();",
        &["x.field", "x"],
    );
    check("const var x: V; ((V)x).field = 1;", &["((V)x).field"]);
    check("const var x, y: V; x = v; y = v; x = v; y.Read();", &[]);
    check("x = v; const var x: V; x = v; x.Read();", &[]);
}

#[test]
fn scalar_struct_and_array_writes_are_not_initialization_exceptions() {
    check("const var x: Int32; x = 1; x = 2;", &["x", "x"]);
    // Official const-local probes reject scalar inline initialization too.
    check("const var x: Int32 = 1; x = 2;", &["x", "x"]);
    check("const var x: S; var y: S; x = y;", &["x"]);
    check(
        "const var x: array<Int32>; x[0] = 1; x.Resize(2);",
        &["x[0]", "x"],
    );
}

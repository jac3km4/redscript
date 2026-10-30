use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn check(source: &str, dependency: &str) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("dependency.reds", dependency);
    let file = sources.push_back("test.script", source);
    let interner = TypeInterner::default();
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
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    errors
        .into_iter()
        .map(|e| e.to_string())
        .chain(reporter.into_reported().into_iter().map(|e| e.to_string()))
        .collect()
}

#[test]
fn implicit_fields_support_arrays_truthiness_and_assignment() {
    let errors = check(
        "class Object {} class Host { private var a: array<Int32>; private var o: Object; public function Test(): Int32 { o = new Object; if (o) { a.PushBack(1); } return a.Size(); } }",
        "",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn implicit_methods_include_inherited_and_static_members() {
    let errors = check(
        "class Base { protected function Foo(): Int32 { return 1; } } class Host extends Base { public static function Bar(): Int32 { return 2; } public function Test(): Int32 { Bar(); return Foo(); } public static function TestStatic(): Int32 { return Bar(); } }",
        "",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn parameters_and_hoisted_locals_shadow_fields_and_methods() {
    let errors = check(
        "class Host { private var a: array<Int32>; public function Foo() {} public function Test(a: Int32): Int32 { return a; } public function TestLocal(): Int32 { return a; var a: Int32; } public function Shadow(Foo: Int32) { Foo(); } }",
        "",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
}

#[test]
fn implicit_method_precedes_free_function_but_free_fallback_remains() {
    let errors = check(
        "class Host { public function Foo(): Int32 { return 1; } public function Test(): Int32 { Other(); return Foo(); } }",
        "func Foo() -> String = \"wrong\"; func Other() {} ",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn static_bodies_cannot_read_instance_fields_and_visibility_is_checked() {
    let errors = check(
        "class Base { private var a: Int32; } class Host extends Base { public function Test(): Int32 { return a; } public static function TestStatic(): Int32 { return a; } }",
        "",
    );
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(
        errors.iter().any(|e| e.contains("not defined")),
        "{errors:?}"
    );
}

#[test]
fn community_dependencies_do_not_gain_implicit_members() {
    let errors = check(
        "class Native {}",
        "class Community { private let value: Int32; public func Test() -> Int32 { return value; } }",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("not defined"), "{errors:?}");
}

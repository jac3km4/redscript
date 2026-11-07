mod support;

fn check(source: &str, dependency: &str) -> Vec<String> {
    support::analyze(
        source,
        "test.script",
        &[("dependency.reds", dependency)],
        |_, _, _, errors, reporter| {
            errors
                .iter()
                .map(ToString::to_string)
                .chain(reporter.reported().iter().map(ToString::to_string))
                .collect()
        },
    )
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
fn implicit_methods_precede_same_named_types_in_instance_inherited_and_static_calls() {
    let errors = check(
        "struct Choose { var value: Int32; }
         class Base { protected function Choose(): Int32 { return 1; } }
         class Host extends Base { public function Run(): Int32 { return Choose(); } }
         struct StaticChoice { var value: Int32; }
         class StaticHost {
           public static function StaticChoice(): Int32 { return 2; }
           public function Run(): Int32 { return StaticChoice(); }
           public static function RunStatic(): Int32 { return StaticChoice(); }
         }",
        "",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn parameters_and_hoisted_locals_shadow_type_constructors_too() {
    for body in [
        "public function Run(Choice: Int32) { Choice(); }",
        "public function Run() { Choice(); var Choice: Int32; }",
    ] {
        let errors = check(
            &format!(
                "struct Choice {{ var value: Int32; }}
                 class Host {{ public function Choice() {{}} {body} }}"
            ),
            "",
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("Int32"), "{errors:?}");
    }
}

#[test]
fn selected_method_does_not_fall_back_to_constructor_or_free_function() {
    for method in [
        "public function Choice(value: String) {}",
        "private function Choice() {}",
    ] {
        let errors = check(
            &format!(
                "struct Choice {{}}
                 class Base {{ {method} }}
                 class Host extends Base {{ public function Run() {{ Choice(); }} }}"
            ),
            "",
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        let errors = check(
            &format!(
                "class Base {{ {method} }}
                 class Host extends Base {{ public function Run() {{ Choice(); }} }}"
            ),
            "func Choice() {}",
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
    }
}

#[test]
fn constructors_and_free_functions_remain_available_without_an_implicit_method() {
    let errors = check(
        "struct Choice { var value: Int32; }
         class Host { public function Run(): Choice { Other(); return Choice(1); } }",
        "func Other() {}",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn community_calls_keep_constructor_precedence_over_local_names() {
    let errors = check(
        "class Native {}",
        "struct Choice { let value: Int32; }
         func Run(Choice: Int32) -> Choice { return Choice(1); }",
    );
    assert!(errors.is_empty(), "{errors:?}");
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

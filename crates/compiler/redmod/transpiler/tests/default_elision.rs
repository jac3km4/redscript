//! Whole-local overwrite proofs must not mistake partial writes or borrows for initialization.
mod support;

fn emit(body: &str) -> Result<String, Vec<redscript_redmod_transpiler::TranspileDiagnostic>> {
    support::try_emit(
        &format!("func Test(enabled: Bool, input: Value) -> Value {{ {body} }}"),
        "native struct Value { public let count: Int32; } native func Make() -> Value native func Read(value: Value) native func Borrow(value: script_ref<Value>) native func Write(out value: Value) native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool",
    )
}

#[test]
fn whole_local_assignments_make_native_defaults_unnecessary() {
    let text = emit("let value: Value; while enabled { if enabled { value = Make(); Read(value); } } value = input; return value;").unwrap();
    insta::assert_snapshot!(text);
    for body in [
        "let value: Value; value = input; return value;",
        "let value: Value; if enabled { value = input; } else { value = Make(); } return value;",
        "let value: Value; if enabled { return input; } value = input; return value;",
        "let value: Value; while enabled { value = input; Read(value); continue; } return input;",
        "let value: Value; while enabled { if enabled { break; } value = input; Read(value); } return input;",
        "let value: Value; while enabled { if enabled { continue; } value = input; Read(value); } return input;",
    ] {
        let result = emit(body);
        let text = result.unwrap();
        assert!(!text.contains("Value()"), "{body}: {text}");
    }
}

#[test]
fn reads_partial_writes_and_repeated_declarations_keep_defaults() {
    for body in [
        "let value: Value; return value;",
        "let value: Value; value = value; return value;",
        "let value: Value; Read(value); value = input; return value;",
        "let value: Value; Borrow(value); value = input; return value;",
        "let value: Value; Write(value); return value;",
        "let value: Value; value.count = 1; return value;",
        "let value: Value; if enabled { value = input; } return value;",
        "let value: Value; if enabled { return value; } value = input; return value;",
        "let value: Value; while enabled { while enabled { value = input; break; } Read(value); } return input;",
        "let value: Value; while enabled { value = input; } return value;",
        "let value: Value; while enabled { Read(value); value = input; } return input;",
        "let value: Value; while enabled { if enabled { continue; } value = input; } return value;",
        "let value: Value; while enabled { if enabled { break; } value = input; } return value;",
        "let value: Value; while enabled { if enabled { value = input; } Read(value); } return input;",
        "while enabled { let value: Value; value = input; Read(value); } return input;",
        "if enabled { let value: Value; value = input; Read(value); } return input;",
        "let value: Value; switch enabled { case true: value = input; break; default: value = input; } return value;",
    ] {
        let text = emit(body).unwrap();
        assert!(text.contains("= Value();"), "{body}: {text}");
    }
}

#[test]
fn repeated_handle_defaults_still_reset_before_borrowing() {
    let text = support::emit(
        "class Object {} native func Borrow(value: script_ref<ref<Object>>) func Test(again: Bool, input: ref<Object>) { while again { let value: ref<Object>; Borrow(value); value = input; } }",
        "",
    );
    assert!(
        text.contains(
            "while (again) {\n        value = NULL;\n        Borrow(value);\n        value = input;"
        ),
        "{text}"
    );
}

#[test]
fn unused_native_defaults_are_not_silently_discarded() {
    // This pass proves overwritten defaults, not general unused-local removal.
    for body in [
        "let value: Value; return input;",
        "let value: Value; return input; value = input;",
    ] {
        let text = emit(body).unwrap();
        assert!(text.contains("= Value();"), "{body}: {text}");
    }
}

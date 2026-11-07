//! Compound reads and array fields retain their place and evaluation guards.
mod support;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use support::try_emit_source as emit;

const SOURCE: &str = r#"
struct FieldVisual { public let m_isIconic: Bool; }
struct FieldData {
    public let visualStateData: FieldVisual;
    public let owner: ref<FieldOwner>;
    public static func Bump(self: script_ref<FieldData>) {}
}
class FieldOwner {
    public let child: ref<FieldOwner>;
    public let data: FieldData;
    public let flag: Bool;
}
func FieldSelect(out steps: Int32, owner: ref<FieldOwner>) -> ref<FieldOwner> {
    steps = steps + 1;
    return owner;
}

func FieldNull() -> ref<FieldOwner> { return null; }
func FieldStructRead(owner: ref<FieldOwner>) -> Bool { return owner.child.data.visualStateData.m_isIconic; }
func FieldLocalRead(data: FieldData) -> Bool { return data.visualStateData.m_isIconic; }
func FieldHandleRead(data: FieldData) -> Bool { return data.owner.child.flag; }
func FieldNullRead() -> Bool { return FieldNull().child.flag; }
func FieldMutate(out owner: ref<FieldOwner>) -> Bool {
    owner = new FieldOwner();
    return true;
}
func FieldPair(first: Bool, second: Bool) -> Bool { return first; }
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func OperatorAdd(a: Int32, b: Int32) -> Int32
func FieldDeref(data: script_ref<FieldData>) -> Bool {
    return Deref(data).visualStateData.m_isIconic;
}
func FieldProbe(enabled: Bool, owner: ref<FieldOwner>, other: ref<FieldOwner>, steps: Int32, data: FieldData) -> Bool {
    FieldPair(FieldSelect(steps, owner).child.flag, FieldMutate(owner));
    FieldPair(data.owner.child.flag, FieldMutate(owner));
    (enabled ? FieldSelect(steps, owner) : FieldSelect(steps, other)).child.flag;

    if enabled { FieldSelect(steps, owner).child.flag; }
    while enabled && FieldSelect(steps, owner).child.flag { continue; }
    let result: Bool = enabled && FieldSelect(steps, owner).child.flag;
    return FieldNull().child.flag;
}
"#;

#[test]
fn compound_reads_stay_inline_and_round_trip() {
    let text = emit(SOURCE).unwrap();
    insta::assert_snapshot!(text);
    assert!(text.contains("return data.visualStateData.m_isIconic;"));
    assert!(text.contains("FieldPair(FieldSelect(steps, owner).child.flag, FieldMutate(owner))"));
    assert!(text.contains("data.owner.child.flag"));
    assert!(
        text.contains(
            "(enabled ? FieldSelect(steps, owner) : FieldSelect(steps, other)).child.flag"
        )
    );

    assert_eq!(
        text.matches("enabled && FieldSelect(steps, owner).child.flag")
            .count(),
        2
    );
    assert!(text.contains("return FieldNull().child.flag;"));

    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("field_reads.script", text.clone());
    let (parsed, errors) = redscript_redmod_adapter::parse_file_with_metadata(&text, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = redscript_compiler_frontend::CompileErrorReporter::default();
    let (_, _, errors) = redscript_redmod_adapter::process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
}

#[test]
fn temporary_structs_and_nonlocal_reference_escapes_remain_rejected() {
    let definitions = r#"
        struct Value { public let flag: Bool; public static func Bump(self: script_ref<Value>) {} }
        class Owner { public let child: ref<Owner>; public let value: Value; public let flag: Bool; public let values: [Bool]; }
        func Make() -> ref<Owner> { return new Owner(); }
        func MakeValue() -> Value { return Value(true); }
        func Take(out flag: Bool) {}
    "#;
    for (body, diagnostic) in [
        (
            "Make().value.flag = true;",
            "assignment requires a writable variable-rooted place",
        ),
        ("MakeValue().flag = true;", "INVALID_TEMP"),
        (
            "Take(Make().value.flag);",
            "out arguments require a local variable or a writable field",
        ),
        (
            "ArrayPush(Make().values, true);",
            "this array operation requires a local array",
        ),
        (
            "AsRef(owner.value);",
            "nonlocal AsRef requires an immediate exact non-out script_ref<T> call parameter",
        ),
    ] {
        let source = format!(
            "{definitions} func Test(owner: ref<Owner>, value: script_ref<Value>) {{ {body} }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors.iter().all(|e| e.span.is_some()),
            "{body}: {errors:?}"
        );
        assert!(
            errors.iter().any(|e| e.message.contains(diagnostic)),
            "{body}: {errors:?}"
        );
    }
    // Existing single local-rooted class-field writes remain legal.
    assert!(
        emit(&format!(
            "{definitions} func Test(owner: ref<Owner>) {{ owner.flag = true; }}"
        ))
        .is_ok()
    );
}

#[test]
fn field_array_arguments_require_non_mutating_expressions() {
    let definitions = "struct Data { public let count: Int32; } class Owner { public let child: ref<Owner>; public let data: Data; public let values: [Int32]; } func Make() -> ref<Owner> { return new Owner(); } func Mutate(out owner: ref<Owner>) -> Int32 { owner = Make(); return 1; }";
    for body in [
        "ArrayPush(owner.values, value);",
        "ArrayInsert(owner.values, 0, value);",
        "ArrayResize(owner.values, 3);",
        "ArrayPush(owner.values, owner.data.count);",
        "ArrayPush(owner.values, owner.child.data.count);",
        "ArrayPush(owner.values, owner.values[0]);",
        "ArrayPush(owner.values, true ? owner.data.count : value);",
    ] {
        let text = emit(&format!(
            "{definitions} func Test(owner: ref<Owner>, value: Int32) {{ {body} }}"
        ))
        .unwrap();
        assert!(!text.contains("    var "), "{text}");
    }
    let references = "class Listener {} class Owner { public let listeners: [wref<Listener>]; public let strong: [ref<Listener>]; } func Test(owner: ref<Owner>, listener: ref<Listener>, weak: wref<Listener>) { ArrayPush(owner.listeners, listener); ArrayContains(owner.listeners, listener); ArrayRemove(owner.listeners, listener); ArrayPush(owner.strong, weak); }";
    let text = emit(references).unwrap();
    assert!(
        text.contains("owner.listeners.PushBack(listener);"),
        "{text}"
    );
    assert!(text.contains("owner.strong.PushBack(weak);"), "{text}");
    for body in [
        "ArrayPush(owner.values, Mutate(owner));",
        "ArrayInsert(owner.values, Mutate(owner), value);",
        "ArrayPush(Make().values, value);",
        "ArrayPush(owner.values, owner.values[Mutate(owner)]);",
        "ArrayPush(owner.values, Make().data.count);",
        "ArrayPush(owner.values, true ? value : Mutate(owner));",
        "ArrayPush(owner.values, value = 1);",
    ] {
        let errors = emit(&format!(
            "{definitions} func Test(owner: ref<Owner>, value: Int32) {{ {body} }}"
        ))
        .unwrap_err();
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("this array operation requires a local array")),
            "{body}: {errors:?}"
        );
    }
}

#[test]
fn call_rooted_struct_fields_remain_rejected() {
    let definitions = "struct Value { public let flag: Bool; } class Owner { public let child: ref<Owner>; public let value: Value; } func Make() -> ref<Owner> { return new Owner(); }";
    for expr in [
        "Make().value.flag",
        "Make().child.value.flag",
        "(enabled ? owner : Make()).value.flag",
    ] {
        let errors = emit(&format!(
            "{definitions} func Test(enabled: Bool, owner: ref<Owner>) -> Bool {{ return {expr}; }}"
        ))
        .unwrap_err();
        assert!(errors.iter().all(|e| e.span.is_some()));
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("this field read requires a local receiver")),
            "{errors:?}"
        );
    }
}

#[test]
fn temporary_struct_reads_remain_rejected() {
    for body in [
        "return Make().flag;",
        "return (enabled ? Make() : Make()).flag;",
    ] {
        let source = format!(
            "struct Value {{ public let flag: Bool; }} func Make() -> Value {{ return Value(true); }} func Test(enabled: Bool) -> Bool {{ {body} }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors.iter().any(|e| e.message.contains("INVALID_TEMP")),
            "{errors:?}"
        );
        assert!(errors.iter().all(|e| e.span.is_some()));
    }
}

#[test]
fn unsupported_receiver_forms_still_fail_closed() {
    let definitions = "class Owner { public let child: ref<Owner>; public let flag: Bool; }";
    let errors = emit(&format!(
        "{definitions} func Test() -> Bool {{ return (new Owner()).flag; }}"
    ))
    .unwrap_err();
    assert!(errors.iter().all(|e| e.span.is_some()), "{errors:?}");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("this field read requires a local receiver")),
        "{errors:?}"
    );
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn field_reads_with_official_compiler() {
    use redscript_io::Instr;
    use redscript_test_support::{OfficialBundle, OfficialCompiler};

    let generated = emit(SOURCE).unwrap();
    let body = generated
        .lines()
        .filter(|line| !line.starts_with("import function"))
        .collect::<Vec<_>>()
        .join("\n");
    let output = OfficialCompiler::from_env().compile("field-reads", &body);
    output.assert_version("v1.2.2");
    let directory = &output.directory;
    let bytes = output.bundle_bytes();
    let bundle = OfficialBundle::new(&bytes);
    let mut listing = String::new();
    for name in [
        "FieldDeref",
        "FieldStructRead",
        "FieldLocalRead",
        "FieldHandleRead",
        "FieldNullRead",
        "FieldProbe",
    ] {
        let code = bundle.function_code_prefix(&format!("{name};"));
        let mut offsets = Vec::new();
        let mut offset = 0i32;
        let mut calls = Vec::new();
        listing.push_str(&format!("\n{name}:\n"));
        for instr in &code {
            offsets.push(offset);
            let call = match instr {
                Instr::InvokeStatic {
                    function, flags, ..
                } => {
                    assert_eq!(*flags, redscript_io::InvokeFlags::default());
                    let index: u32 = (*function).into();
                    Some(
                        bundle
                            .definition_name(index)
                            .split(';')
                            .next()
                            .unwrap()
                            .to_owned(),
                    )
                }
                _ => None,
            };
            listing.push_str(&format!(
                "{offset}: {instr:?} {}\n",
                call.as_deref().unwrap_or("")
            ));
            calls.push(call);
            offset += i32::from(instr.virtual_size());
        }
        offsets.push(offset);
        let target = |i: usize, jump: redscript_io::Offset| {
            offsets
                .iter()
                .position(|o| *o == offsets[i] + i32::from(jump))
                .expect("jump to instruction boundary")
        };
        for (i, instr) in code.iter().enumerate() {
            if let Instr::Context(exit) = instr {
                let end = target(i, exit.target());
                assert!(end > i + 1, "{name}: {code:?}");
                assert!(
                    matches!(code[end - 1], Instr::ObjectField(_)),
                    "{name}: {code:?}"
                );
            }
        }
        let ops = code
            .iter()
            .filter(|i| !matches!(i, Instr::Breakpoint(_)))
            .cloned()
            .collect::<Vec<_>>();
        match name {
            "FieldDeref" => {
                assert!(
                    matches!(
                        &ops[..4],
                        [
                            Instr::Return,
                            Instr::StructField(_),
                            Instr::StructField(_),
                            Instr::Deref(_)
                        ]
                    ),
                    "{code:?}"
                );
                assert!(
                    !code
                        .iter()
                        .any(|i| matches!(i, Instr::Context(_) | Instr::Assign))
                );
            }
            "FieldLocalRead" => {
                assert!(
                    matches!(
                        &ops[..3],
                        [Instr::Return, Instr::StructField(_), Instr::StructField(_)]
                    ),
                    "{code:?}"
                );
                assert!(
                    !code
                        .iter()
                        .any(|i| matches!(i, Instr::Context(_) | Instr::Assign))
                );
            }
            "FieldStructRead" => {
                assert!(
                    matches!(
                        &ops[..5],
                        [
                            Instr::Return,
                            Instr::StructField(_),
                            Instr::StructField(_),
                            Instr::Context(_),
                            ..
                        ]
                    ),
                    "{code:?}"
                );
                assert_eq!(
                    code.iter()
                        .filter(|i| matches!(i, Instr::Context(_)))
                        .count(),
                    2
                );
                assert!(!code.iter().any(|i| matches!(i, Instr::Assign)));
            }
            "FieldNullRead" => {
                assert!(
                    matches!(
                        &ops[..3],
                        [Instr::Return, Instr::Context(_), Instr::Context(_)]
                    ),
                    "{code:?}"
                );
                assert_eq!(
                    calls
                        .iter()
                        .filter_map(|c| c.as_deref())
                        .collect::<Vec<_>>(),
                    ["FieldNull"]
                );
                assert!(!code.iter().any(|i| matches!(i, Instr::Assign)));
            }
            "FieldHandleRead" => {
                assert!(
                    matches!(
                        &ops[..4],
                        [
                            Instr::Return,
                            Instr::Context(_),
                            Instr::Context(_),
                            Instr::StructField(_)
                        ]
                    ),
                    "{code:?}"
                );
            }
            "FieldProbe" => {
                assert_eq!(
                    calls
                        .iter()
                        .filter_map(|c| c.as_deref())
                        .collect::<Vec<_>>(),
                    [
                        "FieldPair",
                        "FieldSelect",
                        "FieldMutate",
                        "FieldPair",
                        "FieldMutate",
                        "FieldSelect",
                        "FieldSelect",
                        "FieldSelect",
                        "OperatorLogicAnd",
                        "FieldSelect",
                        "OperatorLogicAnd",
                        "FieldSelect",
                        "FieldNull"
                    ]
                );
                let pair = calls
                    .iter()
                    .position(|c| c.as_deref() == Some("FieldPair"))
                    .unwrap();
                assert!(matches!(code[pair + 1], Instr::Context(_)));
                let mutate = calls
                    .iter()
                    .position(|c| c.as_deref() == Some("FieldMutate"))
                    .unwrap();
                // Null field contexts finish before the next sibling argument;
                // unlike a method Context, they must not skip FieldMutate.
                for (i, instr) in code.iter().enumerate().take(mutate).skip(pair + 1) {
                    if let Instr::Context(exit) = instr {
                        assert!(target(i, exit.target()) <= mutate);
                    }
                }
                let mut skips = 0;
                for (i, instr) in code.iter().enumerate() {
                    if let Instr::Skip(exit) = instr {
                        skips += 1;
                        let end = target(i, exit.target());
                        assert!(matches!(code[i + 1], Instr::Context(_)));
                        assert!(matches!(code[end - 1], Instr::ObjectField(_)));
                        assert_eq!(
                            calls[i + 1..end]
                                .iter()
                                .filter_map(|c| c.as_deref())
                                .collect::<Vec<_>>(),
                            ["FieldSelect"]
                        );
                    }
                }
                assert_eq!(skips, 2);
                assert!(code.iter().any(|i| matches!(i, Instr::Conditional(_))));
                assert!(code.iter().enumerate().any(|(i, instr)| matches!(instr, Instr::Jump(exit) if i32::from(exit.target()) < 0 && target(i, exit.target()) < i - 1)));
            }
            _ => unreachable!(),
        }
    }
    std::fs::write(directory.join("probe-bytecode.txt"), &listing).unwrap();
    println!("{listing}");
}

//! Feature 6 reads must not widen assignment, out, array or mutable receiver gates.
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::transpile_sources;

fn emit(source: &str) -> Result<String, Vec<redscript_redmod_transpiler::TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let file = sources.push_back("field_reads.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(&sources, &[file], Symbols::with_default_types(), &interner)
        .map(|output| output[0].module.text.clone())
}

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
fn compound_reads_do_not_become_writable_places() {
    let definitions = r#"
        struct Value { public let flag: Bool; public static func Bump(self: script_ref<Value>) {} }
        class Owner { public let child: ref<Owner>; public let value: Value; public let flag: Bool; public let values: [Bool]; }
        func Make() -> ref<Owner> { return new Owner(); }
        func MakeValue() -> Value { return Value(true); }
        func Take(out flag: Bool) {}
    "#;
    for (body, diagnostic) in [
        (
            "owner.value.flag = true;",
            "assignment needs a local, a class-handle field, or a local array's element",
        ),
        (
            "Deref(value).flag = true;",
            "assignment needs a local, a class-handle field, or a local array's element",
        ),
        ("MakeValue().flag = true;", "INVALID_TEMP"),
        (
            "Take(owner.value.flag);",
            "out arguments need a local variable or a class-handle field",
        ),
        (
            "Take(Deref(value).flag);",
            "out arguments need a local variable or a class-handle field",
        ),
        (
            "owner.value.Bump();",
            "AsRef requires a local variable here",
        ),
        (
            "MakeValue().Bump();",
            "AsRef requires a local variable here",
        ),
        (
            "ArrayPush(owner.values, true);",
            "this array operation requires a local array",
        ),
        (
            "AsRef(owner.value);",
            "AsRef requires a local variable here",
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
    for body in [
        "return (new Owner()).flag;",
        "return (owner as Owner).flag;",
    ] {
        let errors = emit(&format!(
            "{definitions} func Test(owner: ref<Owner>, owners: [ref<Owner>]) -> Bool {{ {body} }}"
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
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn field_reads_with_official_compiler() {
    use std::path::{Path, PathBuf};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    use byte::BytesExt as _;
    use redscript_io::{BundleReader, Definition, Instr};
    use redscript_redmod_transpiler::validate::OfficialValidator;

    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "field-reads-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    let generated = emit(SOURCE).unwrap();
    let body = generated
        .lines()
        .filter(|line| !line.starts_with("import function"))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    std::fs::write(directory.join("r6/scripts/authored.script"), &body).unwrap();
    let report = validator.validate_with_root(&directory, &root).unwrap();
    println!("{}\n{}", directory.display(), report.stdout);
    assert!(
        report.stdout.contains("v1.2.2"),
        "profiling layout requires v1.2.2"
    );
    let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
    let reader = BundleReader::new(&bytes).unwrap();
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    // Decode only named authored functions, bounded by their physical records.
    // Official external placeholders and the 14-byte v1.2.2 profiling preamble
    // are not supported by the general bundle decoder.
    let decode = |name: &str| {
        let index = (1..word(84))
            .find(|i| {
                let at = word(80) as usize + *i as usize * 20;
                u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()) == 5
                    && reader.cnames().get(word(at)).unwrap().split(';').next() == Some(name)
            })
            .unwrap();
        let Definition::Function(function) = reader.definitions().get(index).unwrap() else {
            panic!("expected function")
        };
        let redscript_io::FunctionBody::Raw { bytes: raw, .. } = function.body() else {
            panic!("expected raw body")
        };
        assert_eq!(raw[0], 0x33);
        let mut at = 14;
        let mut code = Vec::<Instr>::new();
        while at < raw.len() {
            code.push(raw.read(&mut at, byte::LE).unwrap());
        }
        code
    };
    let mut listing = String::new();
    for name in [
        "FieldDeref",
        "FieldStructRead",
        "FieldLocalRead",
        "FieldHandleRead",
        "FieldNullRead",
        "FieldProbe",
    ] {
        let code = decode(name);
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
                        reader
                            .cnames()
                            .get(word(word(80) as usize + index as usize * 20))
                            .unwrap()
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

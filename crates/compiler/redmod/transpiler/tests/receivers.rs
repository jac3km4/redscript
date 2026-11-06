//! Feature 5: retain receiver Context boundaries, never spill mutable value places.
mod support;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};

use support::try_emit_source as emit;

const SOURCE: &str = r#"
class ReceiverOwner {
    public let child: ref<ReceiverOwner>;
    public let handle: wref<ReceiverOwner>;
    public func Entry(value: Int32) -> Int32 { return value; }
    public let count: Int32;
    public func Next(out steps: Int32) -> ref<ReceiverOwner> {
        steps = steps + 1;
        return this.child;
    }
    public func Read(value: Int32) -> Bool {
        this.count = this.count + value;
        return true;
    }
}
func ReceiverSelect(out steps: Int32, value: ref<ReceiverOwner>) -> ref<ReceiverOwner> {
    steps = steps + 1;
    return value;
}
func ReceiverMutate(out value: ref<ReceiverOwner>) -> Int32 {
    value = new ReceiverOwner();
    return 1;
}
func ReceiverWeak(value: ref<ReceiverOwner>) -> wref<ReceiverOwner> { return value.handle; }
func ReceiverRebind(owner: ref<ReceiverOwner>) -> Int32 {
    owner.handle = owner.child;
    return 1;
}
func ReceiverNull() -> ref<ReceiverOwner> { return null; }
func ReceiverReplaceWeak(out handle: wref<ReceiverOwner>, replacement: ref<ReceiverOwner>) -> ref<ReceiverOwner> {
    handle = replacement;
    return replacement;
}
func ReceiverWeakAssignments(handle: wref<ReceiverOwner>, replacement: ref<ReceiverOwner>) {
    handle.count = 42;
    handle.child = new ReceiverOwner();
    handle.child = ReceiverReplaceWeak(handle, replacement);
}
func ReceiverWriteOut(out value: Int32) { value = 42; }
func ReceiverFieldOut(owner: ref<ReceiverOwner>, handle: wref<ReceiverOwner>) {
    ReceiverWriteOut(owner.count);
    ReceiverWriteOut(owner.child.count);
    ReceiverWriteOut(handle.count);
}
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func OperatorAdd(a: Int32, b: Int32) -> Int32
func ReceiverProbe(enabled: Bool, owner: ref<ReceiverOwner>, other: ref<ReceiverOwner>, steps: Int32) -> Bool {
    ReceiverSelect(steps, owner).Next(steps).Read(ReceiverMutate(owner));
    owner.child.Read(ReceiverMutate(owner));
    owner.handle.Read(ReceiverRebind(owner));
    ReceiverWeak(owner).Read(ReceiverRebind(owner));
    owner.handle.Read(owner.handle.Entry(ReceiverRebind(owner)));
    WeakRefToRef(owner.handle).Read(ReceiverRebind(owner));
    (enabled ? ReceiverSelect(steps, owner) : ReceiverSelect(steps, other)).Read(ReceiverMutate(owner));
    if enabled { ReceiverSelect(steps, owner).Read(ReceiverMutate(owner)); }
    while enabled && ReceiverSelect(steps, owner).Read(ReceiverMutate(owner)) { continue; }
    let result: Bool = enabled && ReceiverSelect(steps, owner).Read(ReceiverMutate(owner));
    return ReceiverNull().Read(ReceiverMutate(owner));
}
"#;

#[test]
fn compound_receivers_stay_at_their_expression_sites() {
    let text = emit(SOURCE).unwrap();
    insta::assert_snapshot!(text);
    assert!(text.contains("ReceiverSelect(steps, owner).Next(steps).Read(ReceiverMutate(owner))"));
    assert!(text.contains("owner.child.Read(ReceiverMutate(owner))"));
    assert!(
        text.contains(
            "(enabled ? ReceiverSelect(steps, owner) : ReceiverSelect(steps, other)).Read("
        )
    );
    assert_eq!(
        text.matches("enabled && ReceiverSelect(steps, owner).Read(ReceiverMutate(owner))")
            .count(),
        2
    );
    assert!(text.contains("return ReceiverNull().Read(ReceiverMutate(owner));"));
    assert_eq!(
        text.matches("owner.handle.Read(ReceiverRebind(owner));")
            .count(),
        2
    );
    assert!(text.contains("ReceiverWeak(owner).Read(ReceiverRebind(owner));"));
    assert!(text.contains("owner.handle.Read(owner.handle.Entry(ReceiverRebind(owner)));"));
    assert!(!text.contains("WeakRefToRef("));
    assert!(text.contains("handle.count = 42;"));
    assert!(text.contains("handle.child = new ReceiverOwner;"));
    assert!(text.contains("handle.child = ReceiverReplaceWeak(handle, replacement);"));

    // Run the full native adapter, not just its parser, including native ref erasure.
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("receivers.script", text.clone());
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
fn array_index_still_requires_a_variable_root() {
    let errors = emit("class ArrayOwner { public let values: [Int32]; } func Make() -> ref<ArrayOwner> { return new ArrayOwner(); } func Test() -> Int32 { return Make().values[0]; }").unwrap_err();
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("array indexing requires a variable-rooted array")),
        "{errors:?}"
    );
}

#[test]
fn array_unary_operations_still_require_a_variable_root() {
    for call in ["ArraySize", "ArrayClear", "ArrayPop", "ArrayLast"] {
        let errors = emit(&format!("class ArrayOwner {{ public let values: [Int32]; }} func Make() -> ref<ArrayOwner> {{ return new ArrayOwner(); }} func Test() {{ {call}(Make().values); }}")).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("requires a local array")),
            "{errors:?}"
        );
    }
}

#[test]
fn struct_aliases_use_typed_borrows_without_receiver_copies() {
    let structure = "struct Value { public let count: Int32; public static func Bump(self: script_ref<Value>) {} } class Holder { public let value: Value; }";
    for source in [
        format!("{structure} func Test(values: [Value]) {{ values[0].Bump(); }}"),
        format!(
            "{structure} func Make() -> Value {{ return Value(0); }} func Test() {{ Make().Bump(); }}"
        ),
    ] {
        let text = emit(&source).unwrap();
        assert!(text.contains("Value.Bump("), "{text}");
    }
}

#[test]
fn temporary_struct_and_class_script_reference_places_remain_rejected() {
    for source in [
        "struct Value { public let count: Int32; } class Owner { public let value: Value; } func Make() -> ref<Owner> { return new Owner(); } func Test() { Make().value.count = 1; }",
        "class Owner { public let count: Int32; } func Test(value: script_ref<Owner>) { Deref(value).count = 1; }",
    ] {
        let errors = emit(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("assignment requires")),
            "{errors:?}"
        );
    }
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn receivers_with_official_compiler() {
    use redscript_io::Instr;
    use redscript_test_support::{OfficialBundle, OfficialCompiler};

    let generated = emit(SOURCE).unwrap();
    let body = generated
        .lines()
        .filter(|line| !line.starts_with("import function"))
        .collect::<Vec<_>>()
        .join("\n");
    let output = OfficialCompiler::from_env().compile("receivers", &body);
    output.assert_version("v1.2.2");
    let directory = &output.directory;
    let bytes = output.bundle_bytes();
    let bundle = OfficialBundle::new(&bytes);
    let cnames = bundle.literal_pool(44);
    let assignments = bundle.function_code_prefix("ReceiverWeakAssignments;");
    std::fs::write(
        directory.join("assignment-bytecode.txt"),
        format!("{assignments:#?}"),
    )
    .unwrap();
    assert_eq!(
        assignments
            .iter()
            .filter(|op| matches!(op, Instr::WeakRefToRef))
            .count(),
        3
    );
    assert_eq!(
        assignments
            .iter()
            .filter(|op| matches!(op, Instr::Assign))
            .count(),
        3
    );
    // The place's promoted receiver is evaluated before the RHS call that
    // rebinds the original weak local. No synthesized local/copy is involved.
    let ops = assignments
        .iter()
        .filter(|op| !matches!(op, Instr::Breakpoint(_)))
        .collect::<Vec<_>>();
    for (i, op) in ops
        .iter()
        .enumerate()
        .filter(|(_, op)| matches!(op, Instr::Assign))
    {
        assert!(matches!(ops[i + 1], Instr::Context(_)), "{op:?}: {ops:?}");
        assert!(matches!(ops[i + 2], Instr::WeakRefToRef), "{ops:?}");
        assert!(matches!(ops[i + 3], Instr::Param(_)), "{ops:?}");
        assert!(matches!(ops[i + 4], Instr::ObjectField(_)), "{ops:?}");
    }
    assert!(
        !assignments.iter().any(|op| matches!(op, Instr::Local(_))),
        "{assignments:?}"
    );
    let out = bundle.function_code_prefix("ReceiverFieldOut;");
    std::fs::write(directory.join("out-bytecode.txt"), format!("{out:#?}")).unwrap();
    assert_eq!(
        out.iter()
            .filter(|op| matches!(op, Instr::ObjectField(_)))
            .count(),
        4
    );
    assert_eq!(
        out.iter()
            .filter(|op| matches!(op, Instr::WeakRefToRef))
            .count(),
        1
    );
    assert_eq!(
        out.iter()
            .filter(|op| matches!(op, Instr::InvokeStatic { .. }))
            .count(),
        3
    );
    assert!(
        !out.iter()
            .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
        "{out:?}"
    );
    let code = bundle.function_code_prefix("ReceiverProbe;");
    let mut listing = String::new();
    let mut offset = 0i32;
    let mut contexts = Vec::new();
    let mut call_names = Vec::new();
    for (index, instr) in code.iter().enumerate() {
        let label = match instr {
            Instr::InvokeStatic {
                function, flags, ..
            } => {
                assert_eq!(*flags, redscript_io::InvokeFlags::default());
                let index: u32 = (*function).into();
                format!("call {} {flags:?}", bundle.definition_name(index))
            }
            Instr::InvokeVirtual {
                function, flags, ..
            } => {
                assert_eq!(*flags, redscript_io::InvokeFlags::default());
                format!("call {} {flags:?}", cnames[u32::from(*function) as usize])
            }
            _ => format!("{instr:?}"),
        };
        call_names.push(
            label
                .strip_prefix("call ")
                .map(|s| s.split(' ').next().unwrap().to_owned()),
        );
        listing.push_str(&format!("{offset}: {label}\n"));
        if let Instr::Context(exit) = instr {
            contexts.push((index, offset + i32::from(exit.target())));
        }
        offset += i32::from(instr.virtual_size());
    }
    std::fs::write(directory.join("probe-bytecode.txt"), &listing).unwrap();
    println!("{listing}");
    assert!(contexts.len() >= 9, "{listing}");
    // Each implicit promotion occurs inside the method Context, before the
    // receiver field/call and before any argument can rebind its source.
    let promotions = code
        .iter()
        .enumerate()
        .filter(|(_, op)| matches!(op, Instr::WeakRefToRef))
        .collect::<Vec<_>>();
    assert_eq!(promotions.len(), 5, "{listing}");
    for (i, _) in promotions {
        assert!(matches!(code[i - 1], Instr::Context(_)), "{listing}");
        assert!(
            matches!(code[i + 1], Instr::Context(_) | Instr::InvokeStatic { .. }),
            "{listing}"
        );
    }
    assert!(
        code.iter().any(|i| matches!(i, Instr::Skip(_))),
        "{listing}"
    );
    assert!(
        code.iter().any(|i| matches!(i, Instr::Conditional(_))),
        "{listing}"
    );
    // Every object Context ends after the invocation's arguments, not after
    // merely computing its receiver. Check real decoded instruction boundaries.
    let mut offsets = Vec::new();
    let mut offset = 0i32;
    for instr in &code {
        offsets.push(offset);
        offset += i32::from(instr.virtual_size());
    }
    offsets.push(offset);
    let mut context_calls = Vec::new();
    for (start, end) in contexts {
        let end = offsets
            .iter()
            .position(|offset| *offset == end)
            .expect("Context exit on instruction boundary");
        assert!(end > start + 1, "{listing}");
        if matches!(code[end - 1], Instr::ObjectField(_)) {
            continue;
        }
        assert!(
            code[start + 1..end]
                .iter()
                .any(|i| matches!(i, Instr::InvokeStatic { .. } | Instr::InvokeVirtual { .. })),
            "{listing}"
        );
        assert!(matches!(code[end - 1], Instr::ParamEnd), "{listing}");
        context_calls.push(
            call_names[start + 1..end]
                .iter()
                .filter_map(|s| s.as_deref())
                .collect::<Vec<_>>(),
        );
    }
    let select = "ReceiverSelect;Int32ReceiverOwner";
    let next = "Next;Int32";
    let read = "Read;Int32";
    let mutate = "ReceiverMutate;ReceiverOwner";
    assert_eq!(
        context_calls,
        [
            vec![select, next, read, mutate],
            vec![select, next],
            vec![read, mutate],
            vec![read, "ReceiverRebind;ReceiverOwner"],
            vec![
                "ReceiverWeak;ReceiverOwner",
                read,
                "ReceiverRebind;ReceiverOwner"
            ],
            vec![read, "Entry;Int32", "ReceiverRebind;ReceiverOwner"],
            vec!["Entry;Int32", "ReceiverRebind;ReceiverOwner"],
            vec![read, "ReceiverRebind;ReceiverOwner"],
            vec![select, select, read, mutate],
            vec![select, read, mutate],
            vec![select, read, mutate],
            vec![select, read, mutate],
            vec!["ReceiverNull;", read, mutate],
        ],
        "{listing}"
    );
    // Each lazy RHS owns the entire Context, not just its argument or receiver.
    let mut skips = 0;
    for (i, instr) in code.iter().enumerate() {
        if let Instr::Skip(exit) = instr {
            skips += 1;
            assert!(matches!(code[i + 1], Instr::Context(_)), "{listing}");
            let end = offsets
                .iter()
                .position(|offset| *offset == offsets[i] + i32::from(exit.target()))
                .unwrap();
            assert_eq!(
                call_names[i + 1..end]
                    .iter()
                    .filter_map(|s| s.as_deref())
                    .collect::<Vec<_>>(),
                [select, read, mutate]
            );
        }
    }
    assert_eq!(skips, 2);
    assert!(code.iter().enumerate().any(|(i, instr)| matches!(instr, Instr::Jump(exit) if i32::from(exit.target()) < 0 && offsets[i] + i32::from(exit.target()) < offsets[i - 1])), "{listing}");
}

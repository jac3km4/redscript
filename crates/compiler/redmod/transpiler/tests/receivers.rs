//! Feature 5: retain receiver Context boundaries, never spill mutable value places.
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::transpile_sources;

fn emit(source: &str) -> Result<String, Vec<redscript_redmod_transpiler::TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let file = sources.push_back("receivers.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(&sources, &[file], Symbols::with_default_types(), &interner)
        .map(|output| output[0].module.text.clone())
}

const SOURCE: &str = r#"
class ReceiverOwner {
    public let child: ref<ReceiverOwner>;
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
func ReceiverNull() -> ref<ReceiverOwner> { return null; }
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func OperatorAdd(a: Int32, b: Int32) -> Int32
func ReceiverProbe(enabled: Bool, owner: ref<ReceiverOwner>, other: ref<ReceiverOwner>, steps: Int32) -> Bool {
    ReceiverSelect(steps, owner).Next(steps).Read(ReceiverMutate(owner));
    owner.child.Read(ReceiverMutate(owner));
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
fn struct_places_remain_rejected() {
    let structure = "struct Value { public let count: Int32; public static func Bump(self: script_ref<Value>) {} } class Holder { public let value: Value; }";
    for source in [
        format!("{structure} func Test(holder: ref<Holder>) {{ holder.value.Bump(); }}"),
        format!("{structure} func Test(values: [Value]) {{ values[0].Bump(); }}"),
        format!(
            "{structure} func Make() -> Value {{ return Value(0); }} func Test() {{ Make().Bump(); }}"
        ),
    ] {
        let errors = emit(&source).unwrap_err();
        assert!(errors.iter().all(|e| e.span.is_some()), "{errors:?}");
        assert!(
            errors.iter().any(|e| e.message.contains("receiver")
                || e.message.contains("AsRef requires a local variable here")),
            "{source}: {errors:?}"
        );
    }
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn receivers_with_official_compiler() {
    use std::path::{Path, PathBuf};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

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
            "receivers-{}-{}",
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
    let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
    let reader = BundleReader::new(&bytes).unwrap();
    // Decode only function records: official external placeholders/profiling
    // records are not necessarily supported by the full bundle reader.
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    let probe = (1..word(84))
        .filter(|i| {
            let at = word(80) as usize + *i as usize * 20;
            u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()) == 5
                && reader
                    .cnames()
                    .get(word(at))
                    .unwrap()
                    .starts_with("ReceiverProbe;")
        })
        .map(|i| reader.definitions().get(i).unwrap())
        .find_map(|def| match def {
            Definition::Function(f)
                if reader
                    .cnames()
                    .get(f.name())
                    .unwrap()
                    .starts_with("ReceiverProbe;") =>
            {
                Some(f)
            }
            _ => None,
        })
        .unwrap();
    // Official v1.2.2 uses a 14-byte profiling preamble, unlike the shared
    // reader's Profile layout. Decode this named probe after that preamble,
    // bounded by its physical record; do not use a guessed virtual code length.
    use byte::BytesExt as _;
    let redscript_io::FunctionBody::Raw { bytes: raw, .. } = probe.body() else {
        panic!("expected serialized probe")
    };
    assert_eq!(raw[0], 0x33);
    let mut at = 14;
    let mut code = Vec::<Instr>::new();
    while at < raw.len() {
        code.push(raw.read(&mut at, byte::LE).unwrap());
    }
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
                let at = word(80) as usize + index as usize * 20;
                format!("call {} {flags:?}", reader.cnames().get(word(at)).unwrap())
            }
            Instr::InvokeVirtual {
                function, flags, ..
            } => {
                assert_eq!(*flags, redscript_io::InvokeFlags::default());
                format!("call {} {flags:?}", reader.cnames().get(*function).unwrap())
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

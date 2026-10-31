use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{Class, ClassFlags, IndexedDefinition, Instr, ScriptBundle, Visibility};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str, expected: &[&str]) -> ScriptBundle<'static> {
    compile_dialect(source, expected, false)
}

fn compile_dialect(source: &str, expected: &[&str], community: bool) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("omitted.script", source);
    let parsed = if community {
        None
    } else {
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        Some(parsed)
    };
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
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = if let Some(parsed) = parsed {
        let (unit, symbols, errors) =
            process_sources(parsed, dependencies, symbols, &mut reporter, &interner);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        (unit, symbols)
    } else {
        let (module, errors) = redscript_community_parser::parse_module(source, file);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let modules = dependencies.into_iter().chain(module).collect::<Vec<_>>();
        let evaluator = redscript_compiler_frontend::Evaluator::from_modules(&modules);
        redscript_compiler_frontend::process_sources(
            modules,
            symbols,
            evaluator,
            &mut reporter,
            &interner,
        )
    };
    assert_eq!(
        reporter
            .reported()
            .iter()
            .map(redscript_compiler_frontend::Diagnostic::code)
            .collect::<Vec<_>>(),
        expected,
        "{source}: {:?}",
        reporter.reported()
    );
    if expected.is_empty() {
        mappings
            .into_monomorphizer(&sources)
            .monomorphize(&unit, &symbols, &mut bundle)
            .unwrap();
    }
    bundle.into_owned()
}

fn code(bundle: &ScriptBundle<'_>, name: &str) -> Vec<Instr> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function) if &*bundle[function.name()] == name => {
                Some(function.body().code_iter().map(Result::unwrap).collect())
            }
            _ => None,
        })
        .unwrap()
}

fn assert_slots(code: &[Instr]) {
    assert!(
        code.windows(5).any(|window| matches!(
            window,
            [
                Instr::InvokeStatic { .. },
                Instr::Nop,
                Instr::I32Const(42),
                Instr::Nop,
                Instr::ParamEnd
            ]
        )),
        "{code:?}"
    );
    assert!(
        !code
            .iter()
            .any(|instr| matches!(instr, Instr::Null | Instr::WeakRefNull))
    );
}

#[test]
fn explicit_and_implicit_optional_slots_survive_bundle_io() {
    let bundle = compile(
        "function Target(optional a: Int32, b: Int32, optional c: String) {} function Run() { Target(, 42, ); Target(, 42); }",
        &[],
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let instructions = code(&bundle, "Run;");
    assert_slots(&instructions);
    assert_eq!(
        instructions
            .iter()
            .filter(|i| matches!(i, Instr::Nop))
            .count(),
        4
    );
}

#[test]
fn community_calls_emit_nop_slots_and_preserve_overload_identity() {
    let bundle = compile_dialect(
        "func Target(a: String, b: Int32, opt c: String) {} func Target(opt a: Int32, b: Int32, opt c: String) {} func Run() { Target(, 42, ); Target(, 42); }",
        &[],
        true,
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let instructions = code(&bundle, "Run;");
    assert_slots(&instructions);
    assert_eq!(
        instructions
            .iter()
            .filter(|i| matches!(i, Instr::Nop))
            .count(),
        4
    );
    let target = instructions
        .iter()
        .find_map(|instr| match instr {
            Instr::InvokeStatic { function, .. } => Some(&bundle[*function]),
            _ => None,
        })
        .unwrap();
    assert!(bundle[target.parameters()[0]].flags().is_optional());
    assert_eq!(
        &*bundle[bundle[bundle[target.parameters()[0]].type_()].name()],
        "Int32"
    );
}

#[test]
fn optionality_filters_candidates_without_losing_overload_identity() {
    let bundle = compile(
        "function Target(a: String, b: Int32, optional c: String) {} function Target(optional a: Int32, b: Int32, optional c: String) {} function Run() { Target(, 42, ); }",
        &[],
    );
    let instructions = code(&bundle, "Run;");
    assert_slots(&instructions);
    let target = instructions
        .iter()
        .find_map(|instr| match instr {
            Instr::InvokeStatic { function, .. } => Some(&bundle[*function]),
            _ => None,
        })
        .unwrap();

    assert_eq!(
        &*bundle[bundle[bundle[target.parameters()[0]].type_()].name()],
        "Int32"
    );
    assert!(bundle[target.parameters()[0]].flags().is_optional());
}

#[test]
fn holes_do_not_contribute_types_or_resolve_ambiguous_overloads() {
    compile(
        "function Target(optional a: Int32, b: Int32) {} function Target(optional a: String, b: Int32) {} function Run() { Target(, 42); }",
        &["MULTIPLE_MATCHING_OVERLOADS"],
    );
    compile(
        "function Target(optional a: Int32, b: Int32) {} function Target(optional a: String, b: String) {} function Run() { Target(, \"value\"); }",
        &[],
    );
}

#[test]
fn native_trailing_comma_retains_its_final_hole() {
    compile(
        "function Target(a: Int32, b: Int32) {} function Run() { Target(42,); }",
        &["REQUIRED_ARGUMENT_OMITTED"],
    );
    compile_dialect(
        "func Target(a: Int32) {} func Run() { Target(42,); }",
        &[],
        true,
    );
}

#[test]
fn required_and_excess_slots_are_diagnostics() {
    compile(
        "function Target(a: Int32, b: Int32) {} function Run() { Target(, 42); }",
        &["REQUIRED_ARGUMENT_OMITTED"],
    );
    compile(
        "function Target(optional a: Int32, b: Int32) {} function Run() { Target(, ); }",
        &["REQUIRED_ARGUMENT_OMITTED"],
    );
    compile(
        "function Target(optional a: Int32) {} function Run() { Target(, , ); }",
        &["INVALID_ARG_COUNT"],
    );
}

#[test]
fn unsupported_call_kinds_fail_closed_and_skip_is_still_rejected() {
    compile(
        "struct Record { var value: Int32; } function Run() { Record(, ); }",
        &["UNSUPPORTED_OMITTED_ARGUMENT"],
    );
    let sources = SourceMap::new();
    let source = "function Target(skip optional value: Int32) {} function Run() { Target(, ); }";
    let file = sources.push_back("skip.script", source);
    let (_, errors) = parse_file_with_metadata(source, file);
    assert!(!errors.is_empty(), "skip requires lazy lowering");
}

#[test]
fn methods_and_optional_out_slots_keep_parameter_positions() {
    let bundle = compile(
        "class Host extends IScriptable { public static function Static(optional a: Int32, b: Int32, optional c: String) {} public final function Method(optional out a: Int32, b: Int32, optional c: String) {} } function Run(host: Host) { Host.Static(, 42, ); host.Method(, 42, ); }",
        &[],
    );
    let instructions = code(&bundle, "Run;Host");
    assert_slots(&instructions);
    assert_eq!(
        instructions
            .windows(4)
            .filter(|window| matches!(
                window,
                [Instr::Nop, Instr::I32Const(42), Instr::Nop, Instr::ParamEnd]
            ))
            .count(),
        2
    );
}

#[test]
fn supplied_arguments_are_evaluated_once_in_written_order() {
    let bundle = compile(
        "function First(): Int32 { return 1; } function Last(): Int32 { return 2; } function Target(optional a: Int32, b: Int32, optional c: Int32, optional d: Int32) {} function Run() { Target(, First(), , Last()); }",
        &[],
    );
    let instructions = code(&bundle, "Run;");
    let calls = instructions
        .iter()
        .filter_map(|instr| match instr {
            Instr::InvokeStatic { function, .. } => Some(&*bundle[bundle[*function].name()]),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(calls, ["Target;Int32Int32Int32Int32", "First;", "Last;"]);
}

#[test]
fn community_parser_accepts_argument_holes() {
    let sources = SourceMap::new();
    for source in [
        "func Run() { Target(, 42); }",
        "func Run() { Target(42, , 43); }",
    ] {
        let file = sources.push_back("community.reds", source);
        let (_, errors) = redscript_community_parser::parse_module(source, file);
        assert!(errors.is_empty(), "{errors:?}");
    }
}

#[test]
#[ignore = "requires freshly compiled official omitted-slot fixtures"]
fn official_bytecode_retains_optional_slots() {
    let bytes = std::fs::read(
        std::env::var_os("OMITTED_OFFICIAL_BUNDLE").expect("set OMITTED_OFFICIAL_BUNDLE"),
    )
    .unwrap();
    // Official definition layouts are not all supported by ScriptBundle. Bound
    // this characterization to the named function's version-14 definition body.
    let word = |offset| u32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize;
    assert_eq!(&bytes[..4], b"REDS");
    assert_eq!(word(4), 14);
    let names = (0..word(48))
        .map(|i| {
            let start = word(32) + word(word(44) + i * 4);
            let end = bytes[start..].iter().position(|b| *b == 0).unwrap() + start;
            std::str::from_utf8(&bytes[start..end]).unwrap()
        })
        .collect::<Vec<_>>();
    let body = (0..word(84))
        .find_map(|i| {
            let header = word(80) + i * 20;
            (u16::from_le_bytes(bytes[header + 16..header + 18].try_into().unwrap()) == 5
                && names[word(header)] == "OmittedRun;")
                .then(|| &bytes[word(header + 8)..word(header + 8) + word(header + 12)])
        })
        .unwrap();
    // Nop, I32Const(42), Nop, ParamEnd, Nop (function terminator).
    assert!(
        body.ends_with(&[0x00, 0x06, 42, 0, 0, 0, 0x00, 0x26, 0x00]),
        "{body:02x?}"
    );
}

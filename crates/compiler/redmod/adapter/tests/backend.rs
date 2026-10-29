use std::collections::HashSet;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner};
use redscript_io::{
    Class, ClassFlags, FieldFlags, Function, IndexedDefinition, Instr, LocalFlags, ScriptBundle,
    Visibility,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn compile(source: &str) -> ScriptBundle<'static> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "helpers.reds",
        "native func Less(a: Int32, b: Int32) -> Bool;
         native func Inc(value: Int32) -> Int32;
         native func OuterStep(); native func InnerStep();
         native func DoCondition() -> Bool;
         native func OuterContinue(); native func InnerContinue(); native func DoContinue();",
    );
    let file = sources.push_back("backend.script", source);
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

    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) =
        process_sources(parsed, dependencies, symbols, &mut reporter, &interner);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_owned()
}

fn function<'a>(bundle: &'a ScriptBundle<'_>, name: &str) -> &'a Function<'a> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function) if &*bundle[function.name()] == name => {
                Some(function)
            }
            _ => None,
        })
        .unwrap()
}

fn call_position(bundle: &ScriptBundle<'_>, code: &[(u32, Instr)], name: &str) -> usize {
    let positions = code
        .iter()
        .enumerate()
        .filter_map(|(i, (_, instr))| match instr {
            Instr::InvokeStatic { function, .. } if &*bundle[bundle[*function].name()] == name => {
                Some(i)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(positions.len(), 1, "{name} must be emitted exactly once");
    positions[0]
}

#[test]
fn nested_native_loops_assemble_fresh_flags_and_continue_to_guarded_headers() {
    let bundle = compile(
        r#"function Run(): Int32 {
            var first: Bool = true;
            var i: Int32;
            for (i = 0; Less(i, 2); OuterStep(), i = Inc(i)) {
                var j: Int32;
                for (j = 0; Less(j, 2); InnerStep(), j = Inc(j)) {
                    switch (j) {
                        case 0:
                            switch (i) {
                                case 0: InnerContinue(); continue;
                                default: break;
                            }
                            break;
                        default: break;
                    }
                }
                do {
                    switch (i) {
                        case 0: DoContinue(); continue;
                        default: break;
                    }
                } while (DoCondition());
                switch (i) {
                    case 0: OuterContinue(); continue;
                    default: break;
                }
            }
            return i;
        }"#,
    );
    // Decode serialized bytecode as well as assembling it; inspect resolved offsets, not labels.
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let run = function(&bundle, "Run;");
    let code = run
        .body()
        .code_iter()
        .with_offsets()
        .map(|(offset, instr)| (offset, instr.unwrap()))
        .collect::<Vec<_>>();
    let locals = run.locals().iter().copied().collect::<HashSet<_>>();
    assert_eq!(locals.len(), run.locals().len());
    assert_eq!(locals.len(), 6); // Three authored locals and three generated first-entry flags.
    let names = locals
        .iter()
        .map(|local| &*bundle[bundle[*local].name()])
        .collect::<HashSet<_>>();
    assert_eq!(names.len(), locals.len());
    for (_, instr) in &code {
        if let Instr::Local(local) = instr {
            assert!(locals.contains(local), "unmapped local: {local:?}");
        }
    }

    let resets = code
        .windows(3)
        .enumerate()
        .filter_map(|(i, window)| match window {
            [
                (_, Instr::Assign),
                (_, Instr::Local(local)),
                (_, Instr::FalseConst),
            ] if bundle[bundle[*local].name()].starts_with("var$") => Some((i, *local)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(resets.len(), 3);
    assert_eq!(
        resets
            .iter()
            .map(|(_, local)| local)
            .collect::<HashSet<_>>()
            .len(),
        3
    );
    let mut heads = Vec::new();
    for ((reset, flag), (header, marker)) in resets.iter().zip([
        ("OuterStep", "OuterContinue"),
        ("InnerStep", "InnerContinue"),
        ("DoCondition", "DoContinue"),
    ]) {
        let local = &bundle[*flag];
        assert_eq!(&*bundle[bundle[local.type_()].name()], "Bool");
        assert_eq!(local.flags(), LocalFlags::default());
        assert!(bundle[local.name()].starts_with("var$"));
        let head = reset + 3;
        assert!(matches!(code[head].1, Instr::JumpIfFalse(_)));
        assert_eq!(code[head + 1].1, Instr::TrueConst);
        let Instr::JumpIfFalse(skip_header) = &code[head + 2].1 else {
            panic!("missing first-entry guard for {header}");
        };
        assert_eq!(code[head + 3].1, Instr::Local(*flag));
        let skip_target = code[head + 2].0 as i32 + i32::from(skip_header.target());
        let set_true = code
            .iter()
            .position(|(offset, _)| *offset as i32 == skip_target)
            .unwrap();
        assert_eq!(code[set_true].1, Instr::Assign);
        assert_eq!(code[set_true + 1].1, Instr::Local(*flag));
        assert_eq!(code[set_true + 2].1, Instr::TrueConst);
        let header_call = call_position(&bundle, &code, header);
        assert!(head + 3 < header_call && header_call < set_true);

        // A marker immediately before each source continue identifies its actual jump,
        // including the continues enclosed by one or two switches.
        let marker_call = call_position(&bundle, &code, marker);
        assert_eq!(code[marker_call + 1].1, Instr::ParamEnd);
        let (offset, Instr::Jump(jump)) = &code[marker_call + 2] else {
            panic!("missing continue after {marker}");
        };
        assert_eq!(
            *offset as i32 + i32::from(jump.target()),
            code[head].0 as i32
        );
        heads.push(head);
    }
    // Nested loop flags are reset at their execution sites inside the outer loop,
    // rather than once in the function prologue.
    assert!(heads[0] < resets[1].0 && heads[0] < resets[2].0);
    assert!(resets[2].0 < call_position(&bundle, &code, "OuterContinue"));
}

#[test]
fn bracket_metadata_hints_and_field_flags_survive_bundle_io() {
    let bundle = compile(
        r#"testonly class Host {
            [category = "First", tooltip = "", category = "Second"]
            [toolitp = "Keep spelling", Tooltip = "Keep case", unknownKey = "λ\\n[raw]"]
            [rangeMin = "-1.5", unsavable = "true"]
            @runtimeProperty("category", "Explicit")
            editable instanceeditable inlined const replicated testonly persistent var value, sibling: Int32;
            var plain: Int32;
            hint value = "Separate hint";
        }
        testonly struct Record {
            [hint = "Property, not directive"]
            inlined var member: Int32;
        }"#,
    );
    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let expected = [
        ("category", "First"),
        ("tooltip", ""),
        ("category", "Second"),
        ("toolitp", "Keep spelling"),
        ("Tooltip", "Keep case"),
        ("unknownKey", r"λ\\n[raw]"),
        ("rangeMin", "-1.5"),
        ("unsavable", "true"),
        ("category", "Explicit"),
    ];
    let mut fields = 0;
    let mut classes = 0;
    for def in bundle.indexed_definitions() {
        match def {
            IndexedDefinition::Class(_, class)
                if matches!(&*bundle[class.name()], "Host" | "Record") =>
            {
                assert!(class.flags().is_test_only());
                classes += 1;
            }
            IndexedDefinition::Field(_, field) => {
                fields += 1;
                let properties = field
                    .attributes()
                    .iter()
                    .map(|p| (p.name(), p.value()))
                    .collect::<Vec<_>>();
                match &*bundle[field.name()] {
                    name @ ("value" | "sibling") => {
                        assert_eq!(properties, expected);
                        assert_eq!(field.hint(), (name == "value").then_some("Separate hint"));
                        let flags = field.flags();
                        assert!(flags.is_editable());
                        assert!(flags.is_instance_editable());
                        assert!(flags.is_inline());
                        assert!(flags.is_const());
                        assert!(flags.is_replicated());
                        assert!(flags.is_test_only());
                        assert!(flags.is_persistent());
                    }
                    "plain" => {
                        assert!(properties.is_empty());
                        assert_eq!(field.hint(), None);
                        assert_eq!(field.flags(), FieldFlags::default());
                    }
                    "member" => {
                        assert_eq!(properties, [("hint", "Property, not directive")]);
                        assert_eq!(field.hint(), None);
                        assert!(field.flags().is_inline());
                    }
                    name => panic!("unexpected field {name}"),
                }
            }
            _ => {}
        }
    }
    assert_eq!(classes, 2);
    assert_eq!(fields, 4);

    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let host = symbols[interner.intern("Host")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert!(host.flags().is_test_only());
    let value = host
        .fields()
        .iter()
        .find(|entry| entry.name() == "value")
        .unwrap();
    let value = value.field();
    assert_eq!(value.hint().map(AsRef::as_ref), Some("Separate hint"));
    assert!(value.flags().is_inline());
}

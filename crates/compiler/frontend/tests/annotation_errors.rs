use std::env;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    Aggregate, AggregateFlags, CompileErrorReporter, Diagnostic, DiagnosticLevel, Field,
    FieldFlags, FunctionType, Method, MethodFlags, Param, ParamFlags, Symbols, Type, TypeDef,
    TypeInterner, TypeSchema, infer_from_sources, ir, predef,
};

#[test]
fn annotations() {
    insta::glob!("data/annotations/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path
            .strip_prefix(&current)
            .unwrap()
            .to_string_lossy()
            .replace("\\", "/");
        let sources = SourceMap::from_files([relative]).unwrap();
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(
            &sources,
            annotation_symbols(&interner),
            &mut reporter,
            &interner,
        );

        let expected: &[(&str, DiagnosticLevel, &str)] =
            match path.file_name().unwrap().to_str().unwrap() {
                "fully-defined-native.reds" => &[(
                    "FULLY_DEFINED_NATIVE_FIELD_ADDITION",
                    DiagnosticLevel::Error,
                    "added",
                )],
                "scripted-struct.reds" => &[(
                    "SCRIPTED_STRUCT_FIELD_ADDITION",
                    DiagnosticLevel::Error,
                    "added",
                )],
                "duplicate-field.reds" => {
                    &[("ADD_FIELD_CONFLICT", DiagnosticLevel::Warning, "existing")]
                }
                "missing-method.reds" => &[
                    ("INVALID_ANN_USE", DiagnosticLevel::Error, "MissingWrapped"),
                    ("INVALID_ANN_USE", DiagnosticLevel::Error, "MissingReplaced"),
                    ("INVALID_ANN_USE", DiagnosticLevel::Error, "Existing"),
                    ("INVALID_ANN_USE", DiagnosticLevel::Error, "Existing"),
                ],
                "nonconflicting-replacements.reds" => &[],
                "valid-addition.reds" => {
                    let aggregate = symbols[interner.intern("NativeTarget")]
                        .schema()
                        .as_aggregate()
                        .unwrap();
                    let (_, field) = aggregate.fields().by_name("added").unwrap();
                    assert_eq!(field.type_(), &Type::nullary(predef::INT32));
                    assert_eq!(aggregate.fields().len(), 2);
                    &[]
                }
                fixture => panic!("unexpected annotation fixture: {fixture}"),
            };
        let actual: Vec<_> = reporter
            .reported()
            .iter()
            .map(|diagnostic| {
                let span = diagnostic.span();
                let file = sources.get(span.file).unwrap();
                (
                    diagnostic.code(),
                    diagnostic.level(),
                    file.span_contents(span).to_owned(),
                )
            })
            .collect();
        let expected: Vec<_> = expected
            .iter()
            .map(|&(code, level, span)| (code, level, span.to_owned()))
            .collect();
        assert_eq!(actual, expected);
        insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
    });
}

#[test]
fn replacement_chain_reports_the_immediately_previous_replacement() {
    let sources = SourceMap::new();
    // Processing order, not lexical path order, determines precedence.
    sources.push_back(
        "z-first.reds",
        "@replaceMethod(NativeTarget)
func Value() -> Int32 { return 1; }",
    );
    let last_file = sources.push_back(
        "a-later.reds",
        "@replaceMethod(NativeTarget)
func Value() -> Int32 { return 2; }

@replaceMethod(NativeTarget)
func Value() -> Int32 { return 3; }",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _) = infer_from_sources(
        &sources,
        annotation_symbols(&interner),
        &mut reporter,
        &interner,
    );

    assert_eq!(unit.method_replacements.len(), 1);
    let retained = unit.method_replacements.values().next().unwrap();
    assert_eq!(retained.span.file, last_file);
    assert_eq!(retained.block.stmts.len(), 1);
    assert!(matches!(
        retained.block.stmts.front(),
        Some(ir::Stmt::Return(Some(expr), _))
            if matches!(&**expr, ir::Expr::Const(ir::Const::I32(3), _))
    ));
    insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
}

#[test]
fn intrinsic_annotations_are_reserved_for_the_boot_library() {
    // The annotation is reserved by file identity, not by path or content: a
    // user file that copies the boot path is still rejected.
    for path in ["user.reds", "boot.reds"] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back(
            path,
            "@intrinsic(IsDefined)
func Fake(a: IScriptable) -> Bool",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(
            &sources,
            annotation_symbols(&interner),
            &mut reporter,
            &interner,
        );

        let reported = reporter.reported();
        assert_eq!(reported.len(), 1, "{path}: {reported:?}");
        assert_eq!(reported[0].code(), "BOOT_ONLY_ANNOTATION");
        assert_eq!(reported[0].span().file, file);
        // The rejected item contributes no symbol, so callers cannot bind it.
        assert!(
            symbols
                .free_functions()
                .all(|entry| entry.name().as_single_component() != Some("Fake")),
            "{path}: rejected intrinsic must not be registered"
        );
    }
}

#[test]
fn intrinsic_annotations_are_accepted_in_the_boot_library() {
    let sources = SourceMap::new();
    let boot = sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols) = infer_from_sources(
        &sources,
        annotation_symbols(&interner),
        &mut reporter,
        &interner,
    );

    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let is_defined = symbols
        .free_functions()
        .find(|entry| entry.name().as_single_component() == Some("IsDefined"))
        .expect("boot library declares IsDefined");
    assert_eq!(
        is_defined.func().intrinsic(),
        Some(ir::Intrinsic::IsDefined)
    );
    assert_eq!(is_defined.func().span().unwrap().file, boot);
}

fn annotation_symbols(interner: &TypeInterner) -> Symbols<'_> {
    let mut symbols = Symbols::with_default_types();
    for (name, flags) in [
        (
            "FullyDefinedNative",
            AggregateFlags::new()
                .with_is_native(true)
                .with_is_fully_defined(true),
        ),
        ("ScriptedStruct", AggregateFlags::new().with_is_struct(true)),
        ("NativeTarget", AggregateFlags::new().with_is_native(true)),
    ] {
        let mut aggregate = Aggregate::default();
        aggregate.set_flags(flags);
        if name == "NativeTarget" {
            aggregate
                .fields_mut()
                .add(
                    "existing",
                    Field::new(
                        FieldFlags::new(),
                        Type::nullary(predef::INT32),
                        [],
                        [],
                        None,
                    ),
                )
                .unwrap();
            for params in [
                vec![],
                vec![Param::new(
                    "value",
                    ParamFlags::new(),
                    Type::nullary(predef::INT32),
                    None,
                )],
            ] {
                aggregate.methods_mut().add(
                    "Value",
                    Method::new(
                        MethodFlags::new().with_is_native(true),
                        FunctionType::new([], params, Type::nullary(predef::INT32)),
                        None,
                        [],
                        None,
                    ),
                );
            }
            aggregate.methods_mut().add(
                "Existing",
                Method::new(
                    MethodFlags::new().with_is_native(true),
                    FunctionType::default(),
                    None,
                    [],
                    None,
                ),
            );
        }
        symbols.add_type(
            interner.intern(name),
            TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
        );
    }
    symbols
}

struct DisplayDiagnostics<'ctx>(Vec<Diagnostic<'ctx>>, &'ctx SourceMap);

impl std::fmt::Display for DisplayDiagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .try_for_each(|diagnostic| writeln!(f, "{}", diagnostic.display(self.1).unwrap()))
    }
}

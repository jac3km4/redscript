use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, DiagnosticLevel, Evaluator, LoweredCompilationUnit, Symbols, TypeInterner,
    ir, parse_files, predef, process_sources_with_policy,
};

fn analyze(
    source: &str,
    check: impl for<'ctx> FnOnce(
        &LoweredCompilationUnit<'ctx>,
        &Symbols<'ctx>,
        &[(String, DiagnosticLevel, &str)],
    ),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("const_script_ref.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    let diagnostics = reporter
        .reported()
        .iter()
        .map(|d| {
            let span = d.span();
            let snippet = if span.file == file {
                &source[span.start as usize..span.end as usize]
            } else {
                ""
            };
            (d.code().to_owned(), d.level(), snippet)
        })
        .collect::<Vec<_>>();
    check(&unit, &symbols, &diagnostics);
}

#[test]
fn writes_through_const_script_ref_are_rejected() {
    for (body, expected_snippet) in [
        ("Deref(r) = Base(1);", "Deref(r)"),
        ("r.x = 1;", "r.x"),
        ("Deref(r).x = 1;", "Deref(r).x"),
        ("MutOut(Deref(r));", "Deref(r)"),
        ("MutOutField(r.x);", "r.x"),
    ] {
        let source = format!(
            "
            struct Base {{ let x: Int32; }}
            func MutOut(out b: Base) {{}}
            func MutOutField(out x: Int32) {{}}
            func Test(r: const_script_ref<Base>) {{
                {body}
            }}
            "
        );
        analyze(&source, |_, _, diagnostics| {
            assert_eq!(
                diagnostics,
                [(
                    "CONST_REF_WRITE".to_owned(),
                    DiagnosticLevel::Error,
                    expected_snippet
                )],
                "{source}"
            );
        });
    }

    for (source, expected_snippet) in [
        (
            "func Test(r: const_script_ref<[Int32]>) { ArrayPush(Deref(r), 1); }",
            "Deref(r)",
        ),
        (
            "func Test(r: const_script_ref<[Int32]>) { r[0] = 1; }",
            "r[0]",
        ),
    ] {
        analyze(source, |_, _, diagnostics| {
            assert_eq!(
                diagnostics,
                [(
                    "CONST_REF_WRITE".to_owned(),
                    DiagnosticLevel::Error,
                    expected_snippet
                )],
                "{source}"
            );
        });
    }
}

#[test]
fn writes_through_covariant_return_const_script_ref_are_rejected() {
    analyze(
        "
        struct Base { let x: Int32; }
        struct Sub extends Base { let y: Int32; }
        class Node {
            let b: Base;
            public func Get() -> const_script_ref<Base> { return AsRef(this.b); }
        }
        class SubNode extends Node {
            let s: Sub;
            public func Get() -> const_script_ref<Sub> { return AsRef(this.s); }
        }
        func Test(n: ref<Node>) {
            n.Get().x = 1;
            Deref(n.Get()) = Base(2);
        }
        ",
        |_, _, diagnostics| {
            assert_eq!(
                diagnostics,
                [
                    ("CONST_REF_WRITE".to_owned(), DiagnosticLevel::Error, "n.Get().x"),
                    (
                        "CONST_REF_WRITE".to_owned(),
                        DiagnosticLevel::Error,
                        "Deref(n.Get())"
                    ),
                ]
            );
        },
    );
}

#[test]
fn rebinding_non_const_local_const_script_ref_is_allowed_but_const_local_is_not() {
    analyze(
        "
        struct Base { let x: Int32; }
        struct Sub extends Base { let y: Int32; }
        func Test(b: Base, s: Sub) -> Int32 {
            let r: const_script_ref<Base> = AsRef(b);
            r = AsRef(s);
            const c: const_script_ref<Base> = AsRef(b);
            c = AsRef(s);
            let _x: Int32 = r.x;
            return c.x;
        }
        ",
        |_, _, diagnostics| {
            assert_eq!(
                diagnostics,
                [("CONST_LOCAL_WRITE".to_owned(), DiagnosticLevel::Error, "c")]
            );
        },
    );
}

#[test]
fn const_and_const_script_ref_cannot_escape_into_mutable_script_ref() {
    for source in [
        "func Test(r: const_script_ref<Int32>) { let m: script_ref<Int32> = r; }",
        "func Take(m: script_ref<Int32>) {} func Test(r: const_script_ref<Int32>) { Take(r); }",
        "func Test(r: const_script_ref<Int32>) -> script_ref<Int32> { return r; }",
    ] {
        analyze(source, |_, _, diagnostics| {
            assert_eq!(diagnostics.len(), 1, "{source}: {diagnostics:?}");
            assert_eq!(diagnostics[0].0, "TYPE_ERR", "{source}");
        });
    }

    for (source, expected_code) in [
        (
            "func Take(m: script_ref<Int32>) {} func Test(r: const_script_ref<Int32>) { Take(Deref(r)); }",
            "CONST_REF_WRITE",
        ),
        (
            "func Take(m: script_ref<Int32>) {} func Test(r: const_script_ref<Int32>) { Take(AsRef(Deref(r))); }",
            "CONST_REF_WRITE",
        ),
        (
            "func Test(r: const_script_ref<Int32>) { let m: script_ref<Int32> = AsRef(Deref(r)); }",
            "CONST_REF_WRITE",
        ),
        (
            "func Test(r: const_script_ref<Int32>) -> script_ref<Int32> { return AsRef(Deref(r)); }",
            "CONST_REF_WRITE",
        ),
        (
            "func Test() { const x: Int32 = 1; let m: script_ref<Int32> = AsRef(x); }",
            "CONST_LOCAL_WRITE",
        ),
        (
            "func Test() -> script_ref<Int32> { const x: Int32 = 1; return AsRef(x); }",
            "CONST_LOCAL_WRITE",
        ),
        (
            "func Test(const r: script_ref<Int32>) { let m: script_ref<Int32> = r; }",
            "CONST_PARAMETER_WRITE",
        ),
    ] {
        analyze(source, |_, _, diagnostics| {
            assert_eq!(diagnostics.len(), 1, "{source}: {diagnostics:?}");
            assert_eq!(diagnostics[0].0, expected_code, "{source}");
            assert_eq!(diagnostics[0].1, DiagnosticLevel::Error, "{source}");
        });
    }

    // Forwarding a const local/param to a const_script_ref produces no warnings or errors.
    analyze(
        "
        func Read(v: const_script_ref<Int32>) -> Int32 { return Deref(v); }
        func Test(const p: Int32) -> Int32 {
            const x: Int32 = 1;
            let r: const_script_ref<Int32> = AsRef(x);
            let _a: Int32 = Read(x);
            let _b: Int32 = Read(p);
            return Read(r);
        }
        ",
        |_, _, diagnostics| assert!(diagnostics.is_empty(), "{diagnostics:?}"),
    );
}

#[test]
fn lvalue_subtyping_into_script_ref_is_only_allowed_for_const_script_ref() {
    analyze(
        "
        struct Base { let x: Int32; }
        struct Sub extends Base { let y: Int32; }
        func MakeSub() -> Sub { let s: Sub; return s; }
        func TakeMut(b: script_ref<Base>) {}
        func TakeConst(b: const_script_ref<Base>) -> Int32 { return b.x; }
        func Test(s: Sub) -> Int32 {
            TakeMut(MakeSub());
            return TakeConst(s);
        }
        ",
        |unit, symbols, diagnostics| {
            assert!(diagnostics.is_empty(), "{diagnostics:?}");
            let test = symbols
                .free_functions()
                .find(|entry| entry.name().as_single_component() == Some("Test"))
                .unwrap();
            let ir::Stmt::Return(Some(expr), _) =
                unit.functions[test.key()].block.stmts.back().unwrap()
            else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            let ir::Expr::Call {
                call: as_ref_call, ..
            } = &call.args()[0]
            else {
                panic!("expected AsRef")
            };
            let ir::Call::FreeFunction {
                function,
                type_args,
                ..
            } = &**as_ref_call
            else {
                panic!("expected free function")
            };
            assert_eq!(symbols[*function].intrinsic(), Some(ir::Intrinsic::AsRef));
            // Preserves the exact runtime type Sub of `s` rather than slicing to Base.
            let ty = type_args[0].coalesced(symbols).unwrap();
            assert_eq!(ty.to_string(), "Sub");
        },
    );

    analyze(
        "
        class Animal {}
        class Dog extends Animal {}
        func TakeMut(a: script_ref<Animal>) {}
        func TakeConst(a: const_script_ref<Animal>) {}
        func Test(d: ref<Dog>) {
            TakeConst(d);
            TakeMut(d);
        }
        ",
        |_, _, diagnostics| {
            assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
            assert_eq!(diagnostics[0].0, "TYPE_ERR");
            assert_eq!(diagnostics[0].2, "d");
        },
    );
}

#[test]
fn conditionals_and_struct_static_forwarders_support_const_script_ref() {
    analyze(
        "
        struct Base {
            let x: Int32;
            public static func ReadX(self: const_script_ref<Base>) -> Int32 {
                return self.x;
            }
        }
        struct Sub extends Base { let a: Int32; }
        class CBase {}
        class CSibA extends CBase {}
        class CSibB extends CBase {}

        func JoinStruct(flag: Bool, a: const_script_ref<Sub>, b: const_script_ref<Base>) -> const_script_ref<Base> {
            return flag ? a : b;
        }
        func JoinClass(flag: Bool, a: const_script_ref<CSibA>, b: const_script_ref<CSibB>) -> const_script_ref<CBase> {
            return flag ? a : b;
        }
        func JoinMixed(flag: Bool, a: script_ref<Sub>, b: const_script_ref<Base>) -> const_script_ref<Base> {
            return flag ? a : b;
        }
        func UseForwarder(b: Base) -> Int32 {
            return b.ReadX();
        }
        ",
        |unit, symbols, diagnostics| {
            assert!(diagnostics.is_empty(), "{diagnostics:?}");
            for (fn_name, expected_pointee) in [
                ("JoinStruct", "Base"),
                ("JoinClass", "CBase"),
                ("JoinMixed", "Base"),
            ] {
                let join = symbols
                    .free_functions()
                    .find(|entry| entry.name().as_single_component() == Some(fn_name))
                    .unwrap();
                let ir::Stmt::Return(Some(expr), _) = &unit.functions[join.key()].block.stmts[0]
                else {
                    panic!("expected return")
                };
                let typ = redscript_compiler_frontend::expr_type(
                    expr,
                    &unit.functions[join.key()],
                    symbols,
                )
                .unwrap();
                assert_eq!(
                    typ.ref_type(),
                    Some(redscript_compiler_frontend::RefType::ConstScript)
                );
                assert_eq!(
                    typ.unwrap_ref().unwrap().upper_bound().unwrap().id().as_str(),
                    expected_pointee
                );
            }
            let _ = predef::CONST_SCRIPT_REF;
        },
    );
}

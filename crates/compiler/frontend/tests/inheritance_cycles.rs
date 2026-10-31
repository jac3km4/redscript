use std::sync::mpsc;
use std::time::Duration;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostic, SourceMapExt, Symbols, TypeInterner,
};
use redscript_compiler_frontend::infer_from_sources;

fn bounded(test: impl FnOnce() + Send + 'static) {
    let (send, recv) = mpsc::channel();
    let worker = std::thread::spawn(move || {
        test();
        send.send(()).unwrap();
    });
    recv.recv_timeout(Duration::from_secs(10))
        .expect("inheritance resolution must terminate");
    worker.join().unwrap();
}

#[test]
fn root_has_no_implicit_base_and_ordinary_classes_still_do() {
    bounded(|| {
        for root in ["class IScriptable {}", "native class IScriptable {}"] {
            let sources = SourceMap::new();
            sources.populate_boot_lib();
            sources.push_back("root.reds", root);
            sources.push_back("child.reds", "class Child {}");
            let interner = TypeInterner::default();
            let mut reporter = CompileErrorReporter::default();
            let (_, symbols) = infer_from_sources(
                &sources,
                Symbols::with_default_types(),
                &mut reporter,
                &interner,
            );
            assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
            let root = interner.intern("IScriptable");
            assert!(symbols[root].schema().base_type().is_none());
            for name in ["Child", "Function0", "Function8"] {
                assert_eq!(
                    symbols[interner.intern(name)]
                        .schema()
                        .base_type()
                        .unwrap()
                        .id(),
                    root,
                    "{name}"
                );
            }
        }
    });
}

#[test]
fn cycles_in_ancestors_terminate_in_any_module_order() {
    bounded(|| {
        for declarations in [
            vec!["class A extends A {}", "class C extends A {}"],
            vec![
                "class A extends B {}",
                "class B extends A {}",
                "class C extends A {}",
            ],
        ] {
            // Exercise every module order, including a descendant before its cycle.
            let orders = if declarations.len() == 2 {
                vec![vec![0, 1], vec![1, 0]]
            } else {
                vec![
                    vec![0, 1, 2],
                    vec![0, 2, 1],
                    vec![1, 0, 2],
                    vec![1, 2, 0],
                    vec![2, 0, 1],
                    vec![2, 1, 0],
                ]
            };
            for order in orders {
                let sources = SourceMap::new();
                sources.populate_boot_lib();
                // Bound checking also traverses ancestry: cycles must be repaired first.
                sources.push_back(
                    "bounds.reds",
                    "class Unrelated {} class Bound<T extends Unrelated> {} \
                     func Check(value: Bound<C>) {}",
                );
                for index in order {
                    sources.push_back(format!("class{index}.reds"), declarations[index]);
                }
                let interner = TypeInterner::default();
                let mut reporter = CompileErrorReporter::default();
                let (_, symbols) = infer_from_sources(
                    &sources,
                    Symbols::with_default_types(),
                    &mut reporter,
                    &interner,
                );
                let cycles = reporter
                    .reported()
                    .iter()
                    .filter_map(|diagnostic| {
                        if let Diagnostic::CircularInheritance(span) = diagnostic {
                            Some(*span)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                assert_eq!(cycles.len(), 1, "{:?}", reporter.reported());
                let span = cycles[0];
                let file = sources.get(span.file).unwrap();
                assert!(matches!(file.span_contents(span), "A" | "B"));
                assert_eq!(
                    symbols[interner.intern("C")]
                        .schema()
                        .base_type()
                        .unwrap()
                        .id(),
                    interner.intern("A"),
                );
                for name in ["A", "C"] {
                    assert!(
                        symbols
                            .base_iter_with_self(interner.intern(name))
                            .take(8)
                            .count()
                            < 8
                    );
                }
            }
        }
    });
}

#[test]
fn explicit_root_self_base_is_still_diagnosed() {
    bounded(|| {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "root.reds",
            "native class IScriptable extends IScriptable {}",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(matches!(
            reporter.reported(),
            [Diagnostic::CircularInheritance(_)]
        ));
        assert!(
            symbols[interner.intern("IScriptable")]
                .schema()
                .base_type()
                .is_none()
        );
    });
}

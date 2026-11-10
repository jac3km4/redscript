use hashbrown::HashMap;
use redscript_community_ast::{self as ast, SourceMap};
use redscript_community_formatter::{FormatCtx, FormatSettings, SyntaxOps};
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner, infer_from_sources};
use redscript_decompiler::{Settings, decompile_function};
use redscript_io::{
    Class, ClassFlags, Function, FunctionFlags, IndexedDefinition, Instr, Jump, ScriptBundle,
    Switch, SwitchLabel, Type, TypeKind, Visibility,
};

fn compile(source: &str) -> Vec<u8> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("switch.reds", source.to_owned());
    let interner = TypeInterner::default();
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(name, Visibility::Public, ClassFlags::default()));
    let (symbols, mappings) =
        CompilationInputs::load(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap()
            .into_inner();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    mappings
        .into_monomorphizer(&sources)
        .monomorphize(&unit, &symbols, &mut bundle)
        .unwrap();
    bundle.into_writeable().to_bytes().unwrap()
}

fn roundtrip(source: &str, check: impl Fn(&ast::ItemDecl<'_>, &str)) {
    let bytes = compile(source);
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let function = bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with("Probe;") =>
            {
                Some(function)
            }
            _ => None,
        })
        .unwrap();
    for settings in [Settings::default(), Settings::default().verbose()] {
        let item = decompile_function(function, &bundle, &settings).unwrap();
        let format_settings = FormatSettings::default();
        let prefixes = HashMap::default();
        let ctx = FormatCtx::new(&format_settings, &prefixes);
        let rendered = format!("{}", item.as_fmt(ctx));
        check(&item, &rendered);
        // The output must remain valid compiler input, not just printable AST.
        // Backend-generated local suffixes are not community identifiers.
        compile(&rendered.replace('$', "_"));
    }
}

fn body<'a, 'src>(item: &'a ast::ItemDecl<'src>) -> &'a ast::Block<'src> {
    let ast::Item::Function(function) = &item.item else {
        panic!("not a function")
    };
    let Some(ast::FunctionBody::Block(block)) = &function.body else {
        panic!("not a block")
    };
    block
}

fn resolve_labels(code: Vec<Instr<usize>>) -> Vec<Instr> {
    let mut offsets = vec![0i16];
    for instr in &code {
        offsets.push(offsets.last().unwrap() + instr.virtual_size() as i16);
    }
    code.into_iter()
        .enumerate()
        .map(|(i, instr)| {
            instr
                .map_labels(|label| Some((offsets[label] - offsets[i]).into()))
                .unwrap()
        })
        .collect()
}

#[test]
fn backwards_dispatch_preserves_shared_body_and_dispatch_order() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("Int32");
    let typ = bundle.define(Type::new(name, TypeKind::Primitive));
    let name = bundle.cnames_mut().add("Probe;");
    // Dispatch visits the last grouped label first, walking backwards to the
    // first label, which links past the shared body to default. This is authored
    // independently of the backend and mirrors the official compiler's layout.
    let code = resolve_labels(vec![
        Instr::Switch(Switch::new(typ, 6)), // 0
        Instr::I32Zero,
        Instr::SwitchLabel(SwitchLabel::new(10, 8)), // 2
        Instr::I32One,
        Instr::SwitchLabel(SwitchLabel::new(2, 8)), // 4
        Instr::I32Const(2),
        Instr::SwitchLabel(SwitchLabel::new(4, 8)), // 6
        Instr::I32Const(3),
        Instr::I32Const(10), // 8: shared body
        Instr::Jump(Jump::new(12)),
        Instr::SwitchDefault, // 10
        Instr::I32Const(30),
        Instr::I32Const(40), // 12: outside switch
        Instr::Return,
        Instr::Nop,
    ]);
    let function =
        Function::new(name, Visibility::Public, FunctionFlags::default()).with_code(code);
    for settings in [Settings::default(), Settings::default().verbose()] {
        let item = decompile_function(&function, &bundle, &settings).unwrap();
        let block = body(&item);
        let ast::Stmt::Switch {
            cases,
            default,
            default_index,
            ..
        } = &block.stmts[0]
        else {
            panic!("{block:?}")
        };
        assert_eq!(cases.len(), 3);
        assert_eq!(*default_index, 3);
        for (case, value) in cases.iter().zip([3, 2, 1]) {
            assert_eq!(
                case.condition,
                ast::Condition::Expr(ast::Expr::Constant(ast::Constant::I32(value)))
            );
        }
        assert!(cases[0].body.is_empty());
        assert!(cases[1].body.is_empty());
        assert_eq!(cases[2].body.len(), 2);
        assert!(matches!(cases[2].body[1], ast::Stmt::Break));
        assert_eq!(default.as_ref().unwrap().len(), 1);
        assert!(
            matches!(&block.stmts[1], ast::Stmt::Expr(expr) if **expr == ast::Expr::Constant(ast::Constant::I32(40)))
        );
    }
}

#[test]
fn backwards_grouped_callable_matchers_keep_lazy_dispatch_order() {
    let bytes = compile(
        "func Matcher1() -> Int32 { return 1; } func Matcher2() -> Int32 { return 2; } func Probe(value: Int32) -> Int32 { switch value { case Matcher1(): case Matcher2(): return 20; default: return 30; } }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let function = bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, f) if bundle[f.name()].starts_with("Probe;") => Some(f),
            _ => None,
        })
        .unwrap();
    let mut code = function.body().code_owned().unwrap();
    let mut offsets = vec![0i16];
    for instr in &code {
        offsets.push(offsets.last().unwrap() + instr.virtual_size() as i16);
    }
    let labels: Vec<_> = code
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| matches!(instr, Instr::SwitchLabel(_)).then_some(i))
        .collect();
    let [first, second] = labels[..] else {
        panic!("expected two labels")
    };
    let Instr::SwitchLabel(label) = &code[second] else {
        unreachable!()
    };
    let body_target = offsets[second] + i16::from(label.body());
    let next = offsets[second] + i16::from(label.next_case());
    code[0] = code[0]
        .clone()
        .map_labels(|_| Some(offsets[second].into()))
        .unwrap();
    code[first] = Instr::SwitchLabel(SwitchLabel::new_with_offset(
        (next - offsets[first]).into(),
        (body_target - offsets[first]).into(),
    ));
    code[second] = Instr::SwitchLabel(SwitchLabel::new_with_offset(
        (offsets[first] - offsets[second]).into(),
        (body_target - offsets[second]).into(),
    ));
    let function = function.clone().with_code(code);
    for settings in [Settings::default(), Settings::default().verbose()] {
        let item = decompile_function(&function, &bundle, &settings).unwrap();
        let rendered = format!(
            "{}",
            item.as_fmt(FormatCtx::new(
                &FormatSettings::default(),
                &HashMap::default()
            ))
        );
        assert!(
            rendered.find("case Matcher2()").unwrap() < rendered.find("case Matcher1()").unwrap(),
            "{rendered}"
        );
        let ast::Stmt::Switch { cases, default, .. } = &body(&item).stmts[0] else {
            panic!("{rendered}")
        };
        assert_eq!(cases.len(), 2);
        assert!(cases[0].body.is_empty(), "{rendered}");
        assert!(
            matches!(cases[1].body[0], ast::Stmt::Return(_)),
            "{rendered}"
        );
        assert!(default.is_some(), "{rendered}");
    }
}

#[test]
fn dispatch_reordering_across_distinct_bodies_is_rejected() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("Int32");
    let typ = bundle.define(Type::new(name, TypeKind::Primitive));
    let name = bundle.cnames_mut().add("Probe;");
    let code = resolve_labels(vec![
        Instr::Switch(Switch::new(typ, 5)),
        Instr::I32Zero,
        Instr::SwitchLabel(SwitchLabel::new(8, 4)),
        Instr::I32One,
        Instr::I32Const(10),
        Instr::SwitchLabel(SwitchLabel::new(2, 7)),
        Instr::I32Const(2),
        Instr::I32Const(20),
        Instr::SwitchDefault,
        Instr::Return,
        Instr::Nop,
    ]);
    let function =
        Function::new(name, Visibility::Public, FunctionFlags::default()).with_code(code);
    for settings in [Settings::default(), Settings::default().verbose()] {
        assert!(decompile_function(&function, &bundle, &settings).is_err());
    }
}

#[test]
fn detached_switch_bodies_preserve_default_position_and_fallthrough() {
    for (labels, expected_index) in [
        (
            "default: result = 30; case 1: result = 10; break; case 2: result = 20; break;",
            0,
        ),
        (
            "case 1: result = 10; break; default: result = 30; case 2: result = 20; break;",
            1,
        ),
        (
            "case 1: result = 10; break; case 2: result = 20; break; default: result = 30;",
            2,
        ),
    ] {
        roundtrip(
            &format!(
                "func Probe(value: Int32) -> Int32 {{ let result: Int32; switch value {{ {labels} }} return result; }}"
            ),
            |item, rendered| {
                let block = body(item);
                let ast::Stmt::Switch {
                    cases,
                    default,
                    default_index,
                    ..
                } = &block.stmts[1]
                else {
                    panic!("{rendered}")
                };
                assert_eq!(*default_index, expected_index, "{rendered}");
                assert_eq!(cases.len(), 2);
                for (case, value) in cases.iter().zip([10, 20]) {
                    assert!(!case.body.is_empty(), "{rendered}");
                    if value == 10 {
                        assert!(
                            matches!(case.body.last(), Some(ast::Stmt::Break)),
                            "{rendered}"
                        );
                    }
                    assert!(
                        format!("{:?}", case.body[0]).contains(&format!("{value}")),
                        "{rendered}"
                    );
                }
                assert_eq!(default.as_ref().unwrap().len(), 1, "{rendered}");
                assert!(
                    matches!(block.stmts.last(), Some(ast::Stmt::Return(_))),
                    "{rendered}"
                );
            },
        );
    }
}

#[test]
fn empty_default_and_grouped_labels_keep_fallthrough_order() {
    roundtrip(
        "func Probe(value: Int32) -> Int32 { let result: Int32; switch value { case 1: result = 10; break; default: case 2: case 3: result = 30; break; } return result; }",
        |item, rendered| {
            let ast::Stmt::Switch {
                cases,
                default_index,
                default,
                ..
            } = &body(item).stmts[1]
            else {
                panic!("{rendered}")
            };
            assert_eq!(*default_index, 1, "{rendered}");
            assert_eq!(cases.len(), 3, "{rendered}");
            assert!(default.as_ref().unwrap().is_empty(), "{rendered}");
            assert!(cases[1].body.is_empty(), "{rendered}");
            assert!(!cases[2].body.is_empty(), "{rendered}");
        },
    );
}

#[test]
fn nested_fallthrough_stops_before_outer_case_and_default() {
    roundtrip(
        "func Probe(value: Int32) -> Int32 { let result: Int32; switch value { case 1: switch value { case 2: result = 20; } case 4: result = 40; break; default: result = 60; } return result; }",
        |item, rendered| {
            let block = body(item);
            let ast::Stmt::Switch {
                cases,
                default,
                default_index,
                ..
            } = &block.stmts[1]
            else {
                panic!("{rendered}")
            };
            assert_eq!(cases.len(), 2, "{rendered}");
            assert_eq!(*default_index, 2, "{rendered}");
            assert_eq!(cases[0].body.len(), 1, "{rendered}");
            let ast::Stmt::Switch {
                cases: inner,
                default: inner_default,
                ..
            } = &cases[0].body[0]
            else {
                panic!("{rendered}")
            };
            assert_eq!(inner.len(), 1, "{rendered}");
            assert!(inner_default.is_none(), "{rendered}");
            assert_eq!(inner[0].body.len(), 1, "{rendered}");
            assert_eq!(
                cases[1].condition,
                ast::Condition::Expr(ast::Expr::Constant(ast::Constant::I32(4)))
            );
            assert_eq!(cases[1].body.len(), 2, "{rendered}");
            assert!(matches!(cases[1].body[1], ast::Stmt::Break), "{rendered}");
            assert_eq!(default.as_ref().unwrap().len(), 1, "{rendered}");
            for (stmts, value) in [
                (&inner[0].body, 20),
                (&cases[1].body, 40),
                (default.as_ref().unwrap(), 60),
            ] {
                let ast::Stmt::Expr(expr) = &stmts[0] else {
                    panic!("{rendered}")
                };
                let ast::Expr::Assign { rhs, .. } = &**expr else {
                    panic!("{rendered}")
                };
                assert_eq!(
                    **rhs,
                    ast::Expr::Constant(ast::Constant::I32(value)),
                    "{rendered}"
                );
            }
            assert!(
                matches!(block.stmts.last(), Some(ast::Stmt::Return(_))),
                "{rendered}"
            );
        },
    );
}

#[test]
fn nested_switch_without_default_stops_at_outer_label() {
    roundtrip(
        "func Probe(value: Int32) -> Int32 { switch value { case 1: switch value { case 2: return 20; case 3: return 30; } case 4: return 40; } return 50; }",
        |item, rendered| {
            let block = body(item);
            let ast::Stmt::Switch { cases, default, .. } = &block.stmts[0] else {
                panic!("{rendered}")
            };
            assert_eq!(cases.len(), 2, "{rendered}");
            assert!(default.is_none(), "{rendered}");
            let ast::Stmt::Switch {
                cases: inner,
                default,
                ..
            } = &cases[0].body[0]
            else {
                panic!("{rendered}")
            };
            assert_eq!(inner.len(), 2, "{rendered}");
            assert!(default.is_none(), "{rendered}");
            assert!(
                matches!(cases[1].body[0], ast::Stmt::Return(_)),
                "{rendered}"
            );
            assert!(
                matches!(block.stmts.last(), Some(ast::Stmt::Return(_))),
                "{rendered}"
            );
        },
    );
}

#[test]
fn ambiguous_detached_final_body_is_not_guessed() {
    let bytes = compile(
        "func Probe(value: Int32) -> Int32 { let result: Int32; switch value { default: result = 30; case 1: result = 10; case 2: result = 20; } return result; }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let function = bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with("Probe;") =>
            {
                Some(function)
            }
            _ => None,
        })
        .unwrap();
    for settings in [Settings::default(), Settings::default().verbose()] {
        assert_eq!(
            decompile_function(function, &bundle, &settings)
                .unwrap_err()
                .to_string(),
            "Detached switch bodies are unsupported"
        );
    }
}

#[test]
fn cyclic_dispatch_is_rejected_without_looping() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("Int32");
    let typ = bundle.define(Type::new(name, TypeKind::Primitive));
    let name = bundle.cnames_mut().add("Probe;");
    let code = resolve_labels(vec![
        Instr::Switch(Switch::new(typ, 2)),
        Instr::I32Zero,
        Instr::SwitchLabel(SwitchLabel::new(2, 4)),
        Instr::I32One,
        Instr::Return,
        Instr::Nop,
    ]);
    let function =
        Function::new(name, Visibility::Public, FunctionFlags::default()).with_code(code);
    for settings in [Settings::default(), Settings::default().verbose()] {
        assert!(decompile_function(&function, &bundle, &settings).is_err());
    }
}

#[test]
fn grouped_labels_and_nested_switches_remain_in_their_own_bodies() {
    roundtrip(
        "func Probe(value: Int32) -> Int32 { let result: Int32; switch value { case 1: case 2: result = 10; break; default: result = 30; case 3: switch value { case 4: result = 40; break; } break; } return result; }",
        |item, rendered| {
            let ast::Stmt::Switch {
                cases,
                default_index,
                ..
            } = &body(item).stmts[1]
            else {
                panic!("{rendered}")
            };
            assert_eq!(cases.len(), 3, "{rendered}");
            assert_eq!(*default_index, 2, "{rendered}");
            assert!(cases[0].body.is_empty(), "{rendered}");
            assert_eq!(cases[1].body.len(), 2, "{rendered}");
            assert!(
                matches!(cases[2].body[0], ast::Stmt::Switch { .. }),
                "{rendered}"
            );
            // A final break may be encoded as a no-op jump and omitted.
            assert!(cases[2].body.len() <= 2, "{rendered}");
        },
    );
}

use redscript_community_ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, LoweredFunction, Symbols, TypeInterner,
    expr_type, ir,
};
use redscript_io::{
    Class, ClassFlags, Function, FunctionFlags, IndexedDefinition, Instr, Parameter,
    ParameterFlags, ScriptBundle, Type, TypeKind, Visibility,
};
use redscript_redmod_adapter::{
    CapabilityError, Error, parse_file, parse_file_with_metadata, process_sources,
};

fn analyze(
    source: &str,
    community: &str,
    inspect: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("community.reds", community);
    let file = sources.push_back("assignments.script", source);
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
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let codes = reporter
        .reported()
        .iter()
        .map(|d| d.code().to_string())
        .collect::<Vec<_>>();
    inspect(&unit, &symbols, &codes);
}

fn function<'a, 'ctx>(
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> &'a LoweredFunction<'ctx> {
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    &unit.functions[entry.key()]
}

fn returned<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> &'a ir::Expr<'ctx> {
    let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
        panic!("expected return")
    };
    expr
}

fn compile(source: &str) -> Vec<u8> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "operators.reds",
        "native func OperatorAssignAdd(out lhs: Int32, rhs: Int32) -> Int32;
         native func OperatorLess(lhs: Int32, rhs: Int32) -> Bool;",
    );
    let file = sources.push_back("assignments.script", source);
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
    let name = bundle.cnames_mut().add("Bool");
    let bool_type = bundle.define(Type::new(name, TypeKind::Primitive));
    for name in ["OperatorLogicAnd", "OperatorLogicOr"] {
        let name = bundle.cnames_mut().add(name);
        let flags = FunctionFlags::default()
            .with_is_native(true)
            .with_is_static(true)
            .with_is_final(true);
        let function = Function::new(name, Visibility::Public, flags)
            .with_source(None)
            .with_return_type(Some(bool_type));
        bundle.define_and_init(function, |bundle, index, function| {
            let parameters = [("lhs", false), ("rhs", true)].map(|(name, lazy)| {
                let name = bundle.cnames_mut().add(name);
                let flags = ParameterFlags::default().with_is_short_circuit(lazy);
                bundle.define(Parameter::new(name, index, bool_type, flags))
            });
            function.with_parameters(parameters)
        });
    }
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
    bundle.into_writeable().to_bytes().unwrap()
}

fn code(bundle: &ScriptBundle<'_>, name: &str) -> Vec<Instr> {
    bundle
        .indexed_definitions()
        .find_map(|def| match def {
            IndexedDefinition::Function(_, function)
                if bundle[function.name()].starts_with(name) =>
            {
                Some(function.body().code_iter().map(Result::unwrap).collect())
            }
            _ => None,
        })
        .unwrap()
}

fn calls(bundle: &ScriptBundle<'_>, code: &[Instr]) -> Vec<String> {
    code.iter()
        .filter_map(|instr| match instr {
            Instr::InvokeStatic { function, .. } => {
                Some(bundle[bundle[*function].name()].to_string())
            }
            _ => None,
        })
        .collect()
}

#[test]
fn chains_have_coerced_result_types_and_keep_written_query_children() {
    let source = "function Chain(out a: Variant, out b: Int32): Variant { return a = b = 7; }";
    analyze(source, "", |unit, symbols, codes| {
        assert!(codes.is_empty(), "{codes:?}");
        let function = function(unit, symbols, "Chain");
        assert_eq!(function.block.stmts.len(), 1);
        assert!(
            function
                .locals
                .iter()
                .all(|local| matches!(local.id, ir::Local::Param(_)))
        );
        let outer = returned(function);
        let ir::Expr::Assign { expr, .. } = outer else {
            panic!("expected typed assignment")
        };
        assert_eq!(
            expr_type(outer, function, symbols).unwrap().to_string(),
            "Variant"
        );
        let ir::Expr::Call { call, .. } = &**expr else {
            panic!("expected conversion")
        };
        let inner = &call.args()[0];
        assert!(matches!(inner, ir::Expr::Assign { .. }));
        assert_eq!(
            expr_type(inner, function, symbols).unwrap().to_string(),
            "Int32"
        );
        assert!(outer.is_prvalue(symbols));
        for written in ["a =", "b =", "7"] {
            let pos = source.rfind(written).unwrap() as u32;
            let found = function.block.find_at(pos).unwrap();
            assert_eq!(found.span().start, pos);
            assert!(!matches!(found, ir::Expr::Assign { .. }));
        }
        struct Count(usize);
        impl<'ctx> ir::Visitor<'ctx> for Count {
            fn visit_assign(
                &mut self,
                place: &ir::Expr<'ctx>,
                value: &ir::Expr<'ctx>,
                _: redscript_community_ast::Span,
            ) {
                self.0 += 1;
                self.visit_expr(place);
                self.visit_expr(value);
            }
        }
        use ir::Visitor;
        let mut count = Count(0);
        count.visit_block(&function.block);
        assert_eq!(count.0, 2);
    });
    let sources = SourceMap::new();
    let file = sources.push_back("assignments.script", source);
    let (module, errors) = parse_file(source, file);
    assert!(module.is_some() && errors.is_empty());
}

#[test]
fn compound_assignments_keep_the_declared_operator_result() {
    analyze(
        "struct Value { var number: Int32; }
         operator +=(out value: Value, rhs: Int32): Int32 { value.number = rhs; return 99; }
         function Compound(out value: Value): Int32 { return value += 7; }",
        "",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let function = function(unit, symbols, "Compound");
            assert_eq!(function.block.stmts.len(), 1);
            let result = returned(function);
            assert!(matches!(result, ir::Expr::Call { .. }));
            assert_eq!(
                expr_type(result, function, symbols).unwrap().to_string(),
                "Int32"
            );
        },
    );
}

#[test]
fn native_reference_assignment_yields_the_pointee_and_community_yields_values() {
    analyze(
        "function Native(value: ref<Int32>): Int32 { return value = 7; }",
        "func Community(out value: Int32) { value = 7; }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let native = function(unit, symbols, "Native");
            let value = returned(native);
            let ir::Expr::Assign { place, .. } = value else {
                panic!("expected value assignment")
            };
            let ir::Expr::Call { call, .. } = &**place else {
                panic!("expected dereferenced place")
            };
            let ir::Call::FreeFunction {
                function: deref, ..
            } = &**call
            else {
                panic!("expected dereference")
            };
            assert_eq!(symbols[*deref].intrinsic(), Some(ir::Intrinsic::Deref));
            assert_eq!(
                expr_type(value, native, symbols).unwrap().to_string(),
                "Int32"
            );
            let community = function(unit, symbols, "Community");
            let ir::Stmt::Expr(value) = &community.block.stmts[0] else {
                panic!("expected assignment")
            };
            assert!(matches!(&**value, ir::Expr::Assign { .. }));
            assert_eq!(
                expr_type(value, community, symbols).unwrap().to_string(),
                "Int32"
            );
        },
    );
    analyze(
        "",
        "func Community(out value: Int32) -> Int32 { return value = 7; }",
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
}

#[test]
fn nested_assignments_emit_native_opcodes_without_receiver_copies_or_readback() {
    let bytes = compile(
        "function IndexTick(out n: Int32): Int32 { n += 1; return n; }
         function RhsTick(out n: Int32): Int32 { n += 1; return n; }
         function Chain(out a: Int32, out b: Int32, out n: Int32): Int32 { return a = b = RhsTick(n); }
         function Indexed(out a: array<Int32>, out n: Int32): Int32 { return a[IndexTick(n)] = RhsTick(n); }
         function Compound(out a: array<Int32>, out n: Int32): Int32 { return a[IndexTick(n)] += RhsTick(n); }
         class Holder extends IScriptable { var number: Int32; }
         function Field(holder: Holder, out n: Int32): Int32 { return holder.number = RhsTick(n); }
         function Ref(value: ref<Int32>, out n: Int32): Int32 { return value = RhsTick(n); }
         function Receiver(holder: Holder, out n: Int32): Holder { n += 1; return holder; }
         function ReceiverCompound(holder: Holder, out n: Int32): Int32 { return Receiver(holder, n).number += RhsTick(n); }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let chain = code(&bundle, "Chain;");
    assert!(matches!(
        &chain[..6],
        [
            Instr::Return,
            Instr::Assign,
            Instr::Param(_),
            Instr::Assign,
            Instr::Param(_),
            Instr::InvokeStatic { .. }
        ]
    ));
    assert_eq!(calls(&bundle, &chain).len(), 1);
    let indexed = code(&bundle, "Indexed;");
    assert!(matches!(
        &indexed[..5],
        [
            Instr::Return,
            Instr::Assign,
            Instr::ArrayElement(_),
            Instr::Param(_),
            Instr::InvokeStatic { .. }
        ]
    ));
    let names = calls(&bundle, &indexed);
    assert_eq!(names.len(), 2);
    assert!(names[0].starts_with("IndexTick;") && names[1].starts_with("RhsTick;"));
    assert_eq!(
        indexed
            .iter()
            .filter(|i| matches!(i, Instr::ArrayElement(_)))
            .count(),
        1
    );
    assert!(!indexed.iter().any(|i| matches!(i, Instr::Local(_))));
    let compound = code(&bundle, "Compound;");
    let names = calls(&bundle, &compound);
    assert_eq!(names.len(), 3);
    assert!(names[0].starts_with("OperatorAssignAdd;"));
    assert!(names[1].starts_with("IndexTick;") && names[2].starts_with("RhsTick;"));
    assert_eq!(
        compound
            .iter()
            .filter(|i| matches!(i, Instr::ArrayElement(_)))
            .count(),
        1
    );
    let field = code(&bundle, "Field;");
    assert!(matches!(
        &field[..6],
        [
            Instr::Return,
            Instr::Assign,
            Instr::Context(_),
            Instr::Param(_),
            Instr::ObjectField(_),
            Instr::InvokeStatic { .. }
        ]
    ));
    assert_eq!(
        field
            .iter()
            .filter(|i| matches!(i, Instr::ObjectField(_)))
            .count(),
        1
    );
    assert_eq!(calls(&bundle, &field).len(), 1);
    let receiver = code(&bundle, "ReceiverCompound;");
    let names = calls(&bundle, &receiver);
    assert_eq!(names.len(), 3);
    assert!(names[0].starts_with("OperatorAssignAdd;"));
    assert!(names[1].starts_with("Receiver;") && names[2].starts_with("RhsTick;"));
    assert_eq!(
        receiver
            .iter()
            .filter(|i| matches!(i, Instr::ObjectField(_)))
            .count(),
        1
    );
    assert!(!receiver.iter().any(|i| matches!(i, Instr::Local(_))));
    let reference = code(&bundle, "Ref;");
    assert!(matches!(
        &reference[..5],
        [
            Instr::Return,
            Instr::Assign,
            Instr::Deref(_),
            Instr::Param(_),
            Instr::InvokeStatic { .. }
        ]
    ));
}

#[test]
fn assignments_stay_inside_lazy_arms_arguments_and_loop_conditions() {
    let bytes = compile(
        "function Tick(out n: Int32): Int32 { n += 1; return n; }
         function Pair(a: Int32, b: Int32): Int32 { return b; }
         function Lazy(flag: Bool, out value: Bool): Bool { return flag && (value = true); }
         function Either(flag: Bool, out value: Bool): Bool { return flag || (value = false); }
         function Ternary(flag: Bool, out value: Int32, out n: Int32): Int32 { return flag ? (value = Tick(n)) : 7; }
         function Arguments(out value: Int32, out n: Int32): Int32 { return Pair(Tick(n), value = Tick(n)); }
         function Loop(out value: Int32, out n: Int32) { while ((value = Tick(n)) < 3) { continue; } }",
    );
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    for name in ["Lazy;", "Either;"] {
        let instructions = code(&bundle, name);
        assert!(matches!(
            &instructions[..6],
            [
                Instr::Return,
                Instr::InvokeStatic { .. },
                Instr::Param(_),
                Instr::Skip(_),
                Instr::Assign,
                Instr::Param(_)
            ]
        ));
        assert_eq!(
            instructions
                .iter()
                .filter(|i| matches!(i, Instr::Assign))
                .count(),
            1
        );
    }
    let ternary = code(&bundle, "Ternary;");
    assert!(matches!(
        &ternary[..6],
        [
            Instr::Return,
            Instr::Conditional(_),
            Instr::Param(_),
            Instr::Assign,
            Instr::Param(_),
            Instr::InvokeStatic { .. }
        ]
    ));
    assert_eq!(calls(&bundle, &ternary).len(), 1);
    let args = code(&bundle, "Arguments;");
    let first_call = args.iter().position(|i| matches!(i, Instr::InvokeStatic { function, .. } if bundle[bundle[*function].name()].starts_with("Tick;"))).unwrap();
    let assignment = args
        .iter()
        .position(|i| matches!(i, Instr::Assign))
        .unwrap();
    assert!(first_call < assignment);
    assert_eq!(calls(&bundle, &args).len(), 3);
    let loop_ = code(&bundle, "Loop;");
    assert!(matches!(
        &loop_[..5],
        [
            Instr::JumpIfFalse(_),
            Instr::InvokeStatic { .. },
            Instr::Assign,
            Instr::Param(_),
            Instr::InvokeStatic { .. }
        ]
    ));
    assert_eq!(
        loop_.iter().filter(|i| matches!(i, Instr::Assign)).count(),
        1
    );
    assert!(loop_.iter().any(|i| matches!(i, Instr::Jump(_))));
}

#[test]
fn invalid_places_types_readonly_writes_and_unsafe_array_prefixes_still_fail() {
    for source in [
        "function Bad(out value: Int32) { (value = 1) = 2; }",
        "function Bad(out value: Int32): Int32 { return value = true; }",
        "function Bad(out value: Int32): Bool { return value = 1; }",
    ] {
        analyze(source, "", |_, _, codes| {
            assert!(!codes.is_empty(), "{source}");
        });
    }
    let source = "function Bad(const value: Int32): Int32 { return value = 1; }";
    let sources = SourceMap::new();
    analyze(source, "", |_, _, codes| assert_eq!(codes, ["OTHER"]));
    for source in [
        "function Bad(flag: Bool) { var a: array<Int32>; return flag ? (a = [1]) : a; }",
        "function Bad() { var a: array<Int32>; Use(Before(), a = [1]); }",
    ] {
        let file = sources.push_back("prefix.script", source);
        let (module, errors) = parse_file_with_metadata(source, file);
        assert!(module.is_none());
        assert!(matches!(
            errors.as_slice(),
            [Error::Capability {
                kind: CapabilityError::UnsafeArrayEvaluation,
                ..
            }]
        ));
    }
}

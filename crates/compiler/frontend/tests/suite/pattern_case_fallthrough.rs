use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, LowerError, Symbols, TypeInterner, infer_from_sources,
};

#[test]
fn safe_switch_fallthrough_remains_valid() {
    for cases in [
        "case 0: case 1: break;",
        "case 0: case 1: break; case let bound: break;",
        "case 0: break; case let bound: break;",
        "case 0: return; case let bound: return;",
        "case let bound: break; case 0:",
        "case let bound: return; case 0: default:",
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "switch.reds",
            format!(
                "func Test(value: Int32) {{ switch value {{ {cases} }} }}\n\
                 native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;"
            ),
        );
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(reporter.reported().is_empty(), "{cases}: {reporter:?}");
    }
}

#[test]
fn derived_destructuring_requires_predecessor_termination() {
    for terminator in ["", "break;", "return;"] {
        let sources = SourceMap::new();
        let file = sources.push_back(
            "destructuring.reds",
            format!(
                "class Base {{ public func IsA(name: CName) -> Bool {{}} }}\n\
                 class Derived extends Base {{ public let field: Int32; }}\n\
                 func Test(value: ref<Base>) {{\n\
                   switch value {{\n\
                     case null: {terminator}\n\
                     case let Derived {{ field }}:\n\
                       let copy = field;\n\
                       break;\n\
                   }}\n\
                 }}\n\
                 native func OperatorEqual(lhs: ref<Base>, rhs: ref<Base>) -> Bool;"
            ),
        );
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        if terminator.is_empty() {
            let [diagnostic] = reporter.reported() else {
                panic!("expected one fallthrough error: {reporter:?}");
            };
            assert!(matches!(
                diagnostic,
                Diagnostic::LoweringError(LowerError::FallthroughIntoPatternCase(_))
            ));
            assert_eq!(diagnostic.code(), "MISSING_BREAK");
            assert_eq!(
                diagnostic.to_string(),
                "case before a `case let` must end with a `break` or `return` statement"
            );
            assert_eq!(
                sources.get(file).unwrap().span_contents(diagnostic.span()),
                "null"
            );
        } else {
            assert!(reporter.reported().is_empty(), "{terminator}: {reporter:?}");
        }
    }
}

#[test]
fn script_ref_pattern_as_cast_is_rejected() {
    for source in [
        "class Base {}
         class Derived extends Base {}
         func Narrow(input: script_ref<Base>) -> Bool {
           if let narrowed as Derived = input {
             return true;
           }
           return false;
         }",
        "class Base {}
         class Derived extends Base {}
         func SwitchNarrow(input: script_ref<Base>) -> Bool {
           switch input {
             case let narrowed as Derived:
               return true;
             default:
               return false;
           }
         }",
        "struct BaseS { public let a: Int32; }
         struct SubS extends BaseS { public let b: Int32; }
         func StructNarrow(input: script_ref<BaseS>) -> Bool {
           if let narrowed as SubS = input {
             return true;
           }
           return false;
         }",
    ] {
        let sources = SourceMap::new();
        sources.push_back("test.reds", source);
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let [diagnostic] = reporter.reported() else {
            panic!("expected one IMPOSSIBLE_DYN_CAST error for {source}: {reporter:?}");
        };
        assert_eq!(diagnostic.code(), "IMPOSSIBLE_DYN_CAST");
    }

    let sources = SourceMap::new();
    sources.push_back(
        "ok.reds",
        "abstract class Scriptable { public func IsA(name: CName) -> Bool { return true; } }
         class Holder extends Scriptable { public let a: Int32; }
         class SubHolder extends Holder { public let b: Int32; }
         func Read(input: script_ref<ref<Holder>>) -> Int32 {
           if let Holder { a } = input {
             return a;
           }
           return 0;
         }
         func ReadArray(input: script_ref<[Int32]>) -> Int32 {
           if let [first, _] = input {
             return first;
           }
           return 0;
         }
         native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
         func CastRef(input: script_ref<Base>, weak_input: script_ref<wref<Base>>) -> Derived {
           let w: wref<Derived> = weak_input as Derived;
           return input as Derived;
         }
         class Base {}
         class Derived extends Base {}",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let is_deref = |expr: &redscript_compiler_frontend::ir::Expr<'_>| {
        matches!(
            expr,
            redscript_compiler_frontend::ir::Expr::Call { call, .. }
            if matches!(
                &**call,
                redscript_compiler_frontend::ir::Call::FreeFunction { function, .. }
                if symbols[*function].intrinsic() == Some(redscript_compiler_frontend::ir::Intrinsic::Deref)
            )
        )
    };
    let read_fn = symbols
        .free_functions()
        .find(|e| e.name().to_string() == "Read")
        .unwrap();
    let read_stmts = &unit.functions[read_fn.key()].block.stmts;
    let redscript_compiler_frontend::ir::Stmt::If { branches, .. } = &read_stmts[1] else {
        panic!("expected branches in Read");
    };
    let redscript_compiler_frontend::ir::Expr::Call { call, .. } = &*branches[0].condition else {
        panic!("expected IsA call in Read");
    };
    let redscript_compiler_frontend::ir::Call::Instance {
        receiver,
        receiver_ref,
        ..
    } = &**call
    else {
        panic!("expected instance call in Read");
    };
    assert_eq!(*receiver_ref, None);
    assert!(
        is_deref(receiver),
        "expected Deref receiver in IsA: {receiver:#?}"
    );
    let redscript_compiler_frontend::ir::Stmt::Expr(assign_expr) = &branches[0].block.stmts[0]
    else {
        panic!("expected assign in Read branch");
    };
    let redscript_compiler_frontend::ir::Expr::Assign {
        expr: field_expr, ..
    } = &**assign_expr
    else {
        panic!("expected assign");
    };
    let redscript_compiler_frontend::ir::Expr::Field {
        receiver,
        receiver_ref,
        ..
    } = &**field_expr
    else {
        panic!("expected field access");
    };
    assert_eq!(*receiver_ref, None);
    assert!(
        is_deref(receiver),
        "expected Deref receiver in field: {receiver:#?}"
    );

    let read_array_fn = symbols
        .free_functions()
        .find(|e| e.name().to_string() == "ReadArray")
        .unwrap();
    let redscript_compiler_frontend::ir::Stmt::If { branches, .. } =
        &unit.functions[read_array_fn.key()].block.stmts[1]
    else {
        panic!("expected branches in ReadArray");
    };
    let redscript_compiler_frontend::ir::Stmt::Expr(assign_expr) = &branches[0].block.stmts[0]
    else {
        panic!("expected assign in ReadArray branch");
    };
    let redscript_compiler_frontend::ir::Expr::Assign {
        expr: index_expr, ..
    } = &**assign_expr
    else {
        panic!("expected assign");
    };
    let redscript_compiler_frontend::ir::Expr::Index { array, .. } = &**index_expr else {
        panic!("expected index access");
    };
    assert!(is_deref(array), "expected Deref array in index: {array:#?}");

    let cast_fn = symbols
        .free_functions()
        .find(|e| e.name().to_string() == "CastRef")
        .unwrap();
    let cast_stmts = &unit.functions[cast_fn.key()].block.stmts;
    let redscript_compiler_frontend::ir::Stmt::Expr(let_expr) = &cast_stmts[0] else {
        panic!("expected assign in CastRef");
    };
    let redscript_compiler_frontend::ir::Expr::Assign { expr: dyn_cast, .. } = &**let_expr else {
        panic!("expected assign");
    };
    let redscript_compiler_frontend::ir::Expr::DynCast {
        expr, expr_type, ..
    } = &**dyn_cast
    else {
        panic!("expected DynCast");
    };
    assert!(
        is_deref(expr),
        "expected Deref inside weak DynCast: {expr:#?}"
    );
    assert_eq!(
        expr_type.coalesced(&symbols).unwrap().ref_type(),
        Some(redscript_compiler_frontend::RefType::Weak)
    );
    let redscript_compiler_frontend::ir::Stmt::Return(Some(ret_expr), _) = &cast_stmts[1] else {
        panic!("expected return in CastRef");
    };
    let redscript_compiler_frontend::ir::Expr::DynCast { expr, .. } = &**ret_expr else {
        panic!("expected DynCast in return");
    };
    assert!(
        is_deref(expr),
        "expected Deref inside strong DynCast: {expr:#?}"
    );

    let native_sources = SourceMap::new();
    let native_file = native_sources.push_back(
        "native_cast.reds",
        "class Base {}
         class Derived extends Base {}
         func CastNative(input: script_ref<Base>) -> Derived {
           return Cast<Derived>(input);
         }",
    );
    native_sources.populate_boot_lib();
    let native_interner = TypeInterner::default();
    let mut native_reporter = CompileErrorReporter::default();
    let modules = redscript_compiler_frontend::parse_files(&native_sources, &mut native_reporter);
    let evaluator = redscript_compiler_frontend::Evaluator::from_modules(&modules);
    let policy =
        redscript_compiler_frontend::Shared::new(redscript_compiler_frontend::SemanticPolicy {
            native_object_cast: true,
            ..redscript_compiler_frontend::SemanticPolicy::default()
        });
    let (native_unit, native_symbols) = redscript_compiler_frontend::process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut native_reporter,
        &native_interner,
        &[(native_file, policy)],
    );
    assert!(
        native_reporter.reported().is_empty(),
        "{:?}",
        native_reporter.reported()
    );
    let cast_native_fn = native_symbols
        .free_functions()
        .find(|e| e.name().to_string() == "CastNative")
        .unwrap();
    let redscript_compiler_frontend::ir::Stmt::Return(Some(ret_expr), _) =
        &native_unit.functions[cast_native_fn.key()].block.stmts[0]
    else {
        panic!("expected return in CastNative");
    };
    let redscript_compiler_frontend::ir::Expr::DynCast { expr, .. } = &**ret_expr else {
        panic!("expected DynCast in CastNative");
    };
    assert!(
        matches!(
            &**expr,
            redscript_compiler_frontend::ir::Expr::Call { call, .. }
            if matches!(
                &**call,
                redscript_compiler_frontend::ir::Call::FreeFunction { function, .. }
                if native_symbols[*function].intrinsic() == Some(redscript_compiler_frontend::ir::Intrinsic::Deref)
            )
        ),
        "expected Deref inside native Cast DynCast: {expr:#?}"
    );
}

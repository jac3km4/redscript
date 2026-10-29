use std::collections::HashMap;

use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, ir,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>),
) {
    analyze_native(source, |_| {}, test);
}

fn analyze_native(
    source: &str,
    rewrite: impl FnOnce(&mut redscript_redmod_ast::Module<'_>),
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>),
) {
    let sources = SourceMap::new();
    let dependency = "native func Mark(value: Int32); native func Less(a: Int32, b: Int32) -> Bool; native func Equal(a: Int32, b: Int32) -> Bool; native func Inc(value: Int32) -> Int32; native func Check(a: Int32, b: Int32) -> Bool; native func Once(value: Bool) -> Bool;";
    let dependency_file = sources.push_back("helpers.reds", dependency);
    let (dependency, errors) =
        redscript_community_parser::parse_module(dependency, dependency_file);
    assert!(errors.is_empty(), "{errors:?}");
    let file = sources.push_back("flow.script", source);
    let (native, errors) = redscript_redmod_parser::parse_module(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let mut native = native.unwrap();
    rewrite(&mut native);
    let parsed = redscript_redmod_adapter::bridge::lower(native).unwrap();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        [parsed],
        [dependency.unwrap()],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    test(&unit, &symbols);
}

#[derive(Debug, PartialEq)]
enum Flow {
    Next,
    Break,
    Continue,
    Return(i32),
}

// Execute the analyzed IR with the backend's structured control-target rules:
// switch consumes break but propagates continue; while consumes both.
struct Machine<'a, 'ctx> {
    symbols: &'a Symbols<'ctx>,
    locals: HashMap<ir::Local, i32>,
    events: Vec<i32>,
    fuel: usize,
}
impl<'a, 'ctx> Machine<'a, 'ctx> {
    fn new(symbols: &'a Symbols<'ctx>) -> Self {
        Self {
            symbols,
            locals: HashMap::new(),
            events: vec![],
            fuel: 10000,
        }
    }

    fn expr(&mut self, expr: &ir::Expr<'ctx>) -> i32 {
        match expr {
            ir::Expr::Const(ir::Const::Bool(b), _) => i32::from(*b),
            ir::Expr::Const(ir::Const::I32(i), _) => *i,
            ir::Expr::Local(local, _) => *self.locals.get(local).expect("initialized local"),
            ir::Expr::Assign { place, expr, .. } => {
                let value = self.expr(expr);
                let ir::Expr::Local(local, _) = **place else {
                    panic!("{place:?}")
                };
                self.locals.insert(local, value);
                value
            }
            ir::Expr::Call { call, .. } => {
                let ir::Call::FreeFunction { function, args, .. } = &**call else {
                    panic!("{call:?}")
                };
                let name = self
                    .symbols
                    .get_free_function(*function)
                    .unwrap()
                    .0
                    .to_string();
                let args: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();
                match name.as_str() {
                    "Mark" => {
                        self.events.push(args[0]);
                        0
                    }
                    "Less" => i32::from(args[0] < args[1]),
                    "Check" => {
                        self.events.push(4);
                        i32::from(args[0] < args[1])
                    }
                    "Once" => {
                        self.events.push(2);
                        args[0]
                    }
                    "Equal" => i32::from(args[0] == args[1]),
                    "Inc" => args[0] + 1,
                    _ => panic!("unknown call {name}"),
                }
            }
            _ => panic!("unsupported expression {expr:?}"),
        }
    }

    fn block(&mut self, block: &ir::Block<'ctx>) -> Flow {
        for stmt in &block.stmts {
            self.fuel = self
                .fuel
                .checked_sub(1)
                .expect("execution did not terminate");
            let flow = match stmt {
                ir::Stmt::Expr(expr) => {
                    self.expr(expr);
                    Flow::Next
                }
                ir::Stmt::InitDefault { local, .. } => {
                    self.locals.insert(*local, 0);
                    Flow::Next
                }
                ir::Stmt::Block(block, _) => self.block(block),
                ir::Stmt::While(branch, _) => {
                    while self.expr(&branch.condition) != 0 {
                        match self.block(&branch.block) {
                            Flow::Break => break,
                            Flow::Return(value) => return Flow::Return(value),
                            Flow::Next | Flow::Continue => {}
                        }
                    }
                    Flow::Next
                }
                ir::Stmt::Branches {
                    branches, default, ..
                } => {
                    if let Some(branch) = branches
                        .iter()
                        .find(|branch| self.expr(&branch.condition) != 0)
                    {
                        self.block(&branch.block)
                    } else if let Some(block) = default {
                        self.block(block)
                    } else {
                        Flow::Next
                    }
                }
                ir::Stmt::Switch {
                    scrutinee,
                    branches,
                    default,
                    ..
                } => {
                    let value = self.expr(scrutinee);
                    let start = branches
                        .iter()
                        .position(|branch| self.expr(&branch.matcher) == value);
                    let mut flow = Flow::Next;
                    if let Some(start) = start {
                        for branch in &branches[start..] {
                            flow = self.block(&branch.block);
                            if flow != Flow::Next {
                                break;
                            }
                        }
                    }
                    if flow == Flow::Next
                        && let Some(block) = default
                    {
                        flow = self.block(block);
                    }
                    if flow == Flow::Break {
                        Flow::Next
                    } else {
                        flow
                    }
                }
                ir::Stmt::Break(_) => Flow::Break,
                ir::Stmt::Continue(_) => Flow::Continue,
                ir::Stmt::Return(expr, _) => {
                    Flow::Return(expr.as_ref().map_or(0, |expr| self.expr(expr)))
                }
                _ => panic!("unsupported statement {stmt:?}"),
            };
            if flow != Flow::Next {
                return flow;
            }
        }
        Flow::Next
    }
}

fn run(source: &str, expected: &[i32], result: i32) {
    analyze(source, |unit, symbols| {
        let function = unit.functions.values().next().unwrap();
        let mut machine = Machine::new(symbols);
        assert_eq!(machine.block(&function.block), Flow::Return(result));
        assert_eq!(machine.events, expected);
    });
}

#[test]
fn for_headers_run_once_per_edge_and_continue_through_switch() {
    run(
        r#"function Run(): Int32 {
        var i: Int32;
        for (Mark(10), i = 0; Mark(20), Less(i, 3); Mark(30), i = Inc(i)) {
            switch (i) {
                case 0: Mark(40); continue;
                case 1: Mark(41); break;
                default: Mark(42); return i;
            }
            Mark(50);
        }
        return 99;
    }"#,
        &[10, 20, 40, 30, 20, 41, 50, 30, 20, 42],
        2,
    );
}

#[test]
fn do_while_continue_evaluates_condition_outside_switch() {
    run(
        r#"function Run(): Int32 {
        var i: Int32 = 0;
        do {
            i = Inc(i);
            switch (i) {
                case 1: Mark(1); continue;
                default: Mark(2); break;
            }
            Mark(3);
        } while (Check(i, 2));
        return i;
    }"#,
        &[1, 4, 2, 3, 4],
        2,
    );
    run(
        "function Run(): Int32 { do { Mark(1); continue; } while (Once(false)); return 3; }",
        &[1, 2],
        3,
    );
    run(
        "function Run(): Int32 { do { Mark(1); break; } while (Once(true)); return 3; }",
        &[1],
        3,
    );
    run(
        "function Run(): Int32 { do { return 3; } while (Once(true)); }",
        &[],
        3,
    );
}

#[test]
fn nested_loops_keep_independent_continue_and_first_entry_targets() {
    run(
        r#"function Run(): Int32 {
        var i: Int32;
        for (i = 0; Less(i, 2); Mark(9), i = Inc(i)) {
            var j: Int32 = 0;
            for (; Less(j, 2); Mark(8), j = Inc(j)) { continue; }
            do { Mark(7); break; } while (Once(true));
            while (true) { Mark(5); break; }
            continue;
        }
        return i;
    }"#,
        &[8, 8, 7, 5, 9, 8, 8, 7, 5, 9],
        2,
    );
}

#[test]
fn lazy_if_and_while_headers_preserve_order() {
    run(
        r#"function Run(): Int32 {
        var i: Int32 = 0;
        while (Check(i, 2)) {
            i = Inc(i);
            if (Equal(i, 1)) { continue; }
            else if (Once(true)) { Mark(4); }
            else { Mark(5); }
        }
        return i;
    }"#,
        &[4, 4, 2, 4, 4],
        2,
    );
}

#[test]
fn absent_headers_false_conditions_breaks_and_returns() {
    run(
        "function Run(): Int32 { for (Mark(1); Mark(2), false; Mark(3)) { Mark(4); } return 5; }",
        &[1, 2],
        5,
    );
    run(
        "function Run(): Int32 { for (;;) { Mark(1); break; } return 5; }",
        &[1],
        5,
    );
    run(
        "function Run(): Int32 { for (;; Mark(2)) { return 5; } }",
        &[],
        5,
    );
    run(
        "function Run(): Int32 { for (;; Mark(2)) { break; } return 5; }",
        &[],
        5,
    );
}

#[test]
fn header_declarations_are_hoisted_once_and_keep_identity() {
    analyze(
        "function Run(): Int32 { x = 4; for (var i: Int32 = 0; Less(i, 2); i = Inc(i)) { var x: Int32; continue; } return x; }",
        |unit, symbols| {
            let function = unit.functions.values().next().unwrap();
            assert_eq!(
                function
                    .locals
                    .iter()
                    .filter(|info| info.name == Some("x"))
                    .count(),
                1
            );
            assert_eq!(
                function
                    .locals
                    .iter()
                    .filter(|info| info.name == Some("i"))
                    .count(),
                1
            );
            assert_eq!(
                function
                    .locals
                    .iter()
                    .filter(|info| info.name.is_none())
                    .count(),
                1
            );
            let mut machine = Machine::new(symbols);
            assert_eq!(machine.block(&function.block), Flow::Return(4));
        },
    );
}

#[test]
fn bare_shared_ast_api_does_not_silently_drop_required_fixups() {
    for (source, kind) in [
        (
            "function Run() { for (;;) { continue; } }",
            redscript_redmod_adapter::CapabilityError::ForLoop,
        ),
        (
            "function Run() { do { continue; } while (false); }",
            redscript_redmod_adapter::CapabilityError::DoWhileLoop,
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("flow.script", source);
        let (parsed, errors) = redscript_redmod_adapter::parse_file(source, file);
        assert!(parsed.is_none());
        assert!(
            matches!(errors.as_slice(), [redscript_redmod_adapter::Error::Capability { kind: actual, .. }] if *actual == kind)
        );
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_some());
        assert!(errors.is_empty(), "{errors:?}");
    }
}

#[test]
fn invalid_headers_report_once_and_do_not_panic_during_fixup() {
    for source in [
        "function Run() { for (;; Missing()) { continue; continue; } }",
        "function Run() { do { continue; continue; } while (Missing()); }",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("bad.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            [parsed.unwrap()],
            [],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(reporter.reported().len(), 1, "{:?}", reporter.reported());
    }
}

#[test]
fn unrelated_errors_do_not_disable_valid_function_fixups() {
    let source = "function Bad() { for (;; Missing()) { continue; } } function Good() { do { continue; } while (false); }";
    let sources = SourceMap::new();
    let file = sources.push_back("flow.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        [parsed.unwrap()],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert_eq!(reporter.reported().len(), 1);
    let good = unit
        .functions
        .iter()
        .find(|(id, _)| symbols.get_free_function(**id).unwrap().0.to_string() == "Good")
        .unwrap()
        .1;
    assert_eq!(good.locals.len(), 1);
    assert!(good.locals[0].name.is_none());
    let mut machine = Machine::new(&symbols);
    assert_eq!(machine.block(&good.block), Flow::Next);
}

#[test]
fn plans_and_hoisting_are_per_source_not_global() {
    let sources = SourceMap::new();
    let dependency = "func Dependency() { while (false) { let x: Int32; } }";
    let dependency_file = sources.push_back("dependency.reds", dependency);
    let (dependency, errors) =
        redscript_community_parser::parse_module(dependency, dependency_file);
    assert!(errors.is_empty(), "{errors:?}");
    let source = "function Run() { for (var i: Int32; false;) { var x: Int32; } do { continue; } while (false); }";
    let file = sources.push_back("flow.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        [parsed.unwrap()],
        [dependency.unwrap()],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let dependency = unit
        .functions
        .values()
        .find(|function| function.span.file == dependency_file)
        .unwrap();
    assert_eq!(dependency.locals.len(), 1);
    assert!(matches!(dependency.block.stmts[0], ir::Stmt::While(_, _)));
    let native = unit
        .functions
        .values()
        .find(|function| function.span.file == file)
        .unwrap();
    assert_eq!(native.locals.len(), 4); // two source locals and two anonymous guards
    assert!(matches!(
        native.block.stmts[0],
        ir::Stmt::InitDefault { .. }
    ));
    assert!(matches!(
        native.block.stmts[1],
        ir::Stmt::InitDefault { .. }
    ));
}

#[test]
fn value_sequences_and_assignment_conditions_remain_typed_capability_failures() {
    for source in [
        "function Run() { for (; x = true;) {} }",
        "function Run() { do {} while (x = true); }",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("bad.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_none());
        assert!(
            matches!(
                errors.as_slice(),
                [redscript_redmod_adapter::Error::Capability { .. }]
            ),
            "{errors:?}"
        );
    }
}

// The parser currently recognizes comma sequences only in for headers. Build
// the equivalent native header shapes by moving real, parsed prefix expressions.
fn sequence_headers(module: &mut redscript_redmod_ast::Module<'_>) {
    use redscript_redmod_ast::{Expr, Item, Statement};
    fn wrap<'src>(
        condition: &mut redscript_redmod_ast::Spanned<Expr<'src>>,
        prefix: redscript_redmod_ast::Spanned<Expr<'src>>,
    ) {
        let span = prefix.1.merge(&condition.1);
        *condition = (Expr::Sequence(vec![prefix, condition.clone()].into()), span);
    }
    fn block(body: &mut redscript_redmod_ast::Block<'_>) {
        let mut result = Vec::new();
        for (mut stmt, mut span) in std::mem::take(body) {
            match &mut stmt {
                Statement::While { condition, body } | Statement::DoWhile { condition, body } => {
                    block(body);
                    let Some((Statement::Expr(prefix), _)) = result.pop() else {
                        panic!("prefix expression")
                    };
                    span = span.merge(&prefix.1);
                    wrap(condition, *prefix);
                }
                Statement::If { blocks, else_ } => {
                    for (condition, body) in blocks {
                        block(body);
                        let (Statement::Expr(prefix), _) = body.remove(0) else {
                            panic!("prefix expression")
                        };
                        wrap(condition, *prefix);
                    }
                    if let Some(body) = else_ {
                        block(body);
                    }
                }
                _ => {}
            }
            result.push((stmt, span));
        }
        *body = result;
    }
    for (decl, _) in &mut module.items {
        if let Item::Function {
            body: Some(body), ..
        } = &mut decl.item
        {
            block(body);
        }
    }
}

#[test]
fn native_sequence_headers_are_lazy_and_repeat_on_continue() {
    let source = r#"function Run(): Int32 {
        var i: Int32 = 0;
        Mark(1);
        while (Less(i, 2)) {
            i = Inc(i);
            if (Equal(i, 1)) { Mark(2); continue; }
            else if (true) { Mark(3); Mark(4); }
            else { Mark(5); }
        }
        Mark(6);
        do { Mark(7); continue; } while (false);
        return i;
    }"#;
    analyze_native(source, sequence_headers, |unit, symbols| {
        let mut machine = Machine::new(symbols);
        assert_eq!(
            machine.block(&unit.functions.values().next().unwrap().block),
            Flow::Return(2)
        );
        assert_eq!(machine.events, [1, 2, 1, 2, 3, 4, 1, 7, 6]);
    });
}

#[test]
fn switch_fallthrough_and_nested_switches_do_not_capture_loop_continue() {
    run(
        r#"function Run(): Int32 {
        var i: Int32 = 0;
        do {
            i = Inc(i);
            switch (i) {
                case 1: Mark(1);
                case 2:
                    switch (i) { case 1: continue; default: break; }
                    Mark(2); break;
                default: return 99;
            }
            Mark(3);
        } while (Check(i, 2));
        return i;
    }"#,
        &[1, 4, 2, 3, 4],
        2,
    );
}

fn assert_written_queries(source: &str, unit: &LoweredCompilationUnit<'_>, symbols: &Symbols<'_>) {
    let function = unit.functions.values().next().unwrap();
    // All numbered markers and their call sites must remain reachable, even if
    // the header now executes before source statements written earlier.
    for (start, _) in source.match_indices("Mark(") {
        let expression = function.block.find_at(start as u32).expect("written call");
        assert!(
            matches!(expression, ir::Expr::Call { call, .. }
            if matches!(&**call, ir::Call::FreeFunction { function, .. }
                if symbols.get_free_function(*function).unwrap().0.to_string() == "Mark")),
            "{expression:?}"
        );
        let start = start + "Mark(".len();
        let end = source[start..].find(')').unwrap() + start;
        let value: i32 = source[start..end].parse().unwrap();
        for pos in start..end {
            assert!(
                matches!(function.block.find_at(pos as u32), Some(ir::Expr::Const(ir::Const::I32(i), span))
                if *i == value && span.start == start as u32 && span.end == end as u32)
            );
        }
    }
    for name in ["Less", "Inc"] {
        for (start, _) in source.match_indices(name) {
            assert!(
                matches!(
                    function.block.find_at(start as u32),
                    Some(ir::Expr::Call { .. })
                ),
                "{name} at {start}"
            );
        }
    }
    for (start, _) in source.match_indices("index") {
        // Declaration names have no expression; every written reference must
        // still resolve to the same hoisted local, never an anonymous guard.
        if source[..start].ends_with("var ") {
            continue;
        }
        assert!(
            matches!(function.block.find_at(start as u32), Some(ir::Expr::Local(local, span))
            if function.locals.iter().any(|info| info.id == *local && info.name == Some("index"))
                && span.start == start as u32 && span.end == (start + "index".len()) as u32),
            "index at {start}"
        );
    }
    let anonymous: Vec<_> = function
        .locals
        .iter()
        .filter(|info| info.name.is_none())
        .map(|info| {
            let span = info.span.expect("generated local location");
            assert_eq!(span.start, span.end);
            info.id
        })
        .collect();
    struct GeneratedSpans(Vec<ir::Local>);
    impl<'ctx> ir::Visitor<'ctx> for GeneratedSpans {
        fn visit_expr(&mut self, expr: &ir::Expr<'ctx>) {
            // These sources have no written Boolean literals. Every Bool here
            // is generated and must be invisible to point queries.
            if matches!(expr, ir::Expr::Const(ir::Const::Bool(_), _)) {
                assert_eq!(expr.span().start, expr.span().end);
            }
            if let ir::Expr::Local(local, span) = expr
                && self.0.contains(local)
            {
                assert_eq!(span.start, span.end);
            }
            match expr {
                ir::Expr::Assign { place, expr, span } => {
                    if place.span().start == place.span().end {
                        assert_eq!(span.start, span.end);
                    }
                    self.visit_expr(place);
                    self.visit_expr(expr);
                }
                ir::Expr::Call { call, .. } => self.visit_call(call, expr.span()),
                _ => {}
            }
        }
    }
    use ir::Visitor;
    GeneratedSpans(anonymous).visit_block(&function.block);
}

#[test]
fn loop_queries_preserve_init_step_condition_body_and_nested_source_identity() {
    let source = r#"function Run() {
        var index: Int32;
        for (Mark(101), index = 0; Mark(102), Less(index, 3); Mark(103), index = Inc(index)) {
            Mark(104);
            do {
                Mark(105);
                for (; Less(index, 2); Mark(106)) { Mark(107); continue; }
                Mark(108);
            } while (Less(index, 1));
            Mark(109);
        }
        do { Mark(110); } while (Less(index, 4));
        Mark(111);
    }"#;
    analyze(source, |unit, symbols| {
        assert_written_queries(source, unit, symbols);
    });
}

#[test]
fn lifted_native_sequence_header_queries_reach_prefixes_and_source_bodies() {
    let source = r#"function Run() {
        var index: Int32 = 0;
        Mark(201);
        while (Less(index, 2)) {
            if (Less(index, 1)) { Mark(202); Mark(203); }
            else if (Less(index, 2)) { Mark(204); Mark(205); }
            index = Inc(index);
        }
        Mark(206);
        do { Mark(207); continue; } while (Less(index, 3));
    }"#;
    analyze_native(source, sequence_headers, |unit, symbols| {
        assert_written_queries(source, unit, symbols);
    });
}

#[test]
fn changed_provenance_returns_a_typed_error_without_consuming_the_header() {
    use redscript_community_ast as ast;
    for malformed in 0..4 {
        let source = "function Run() { for (;; Mark(1)) { continue; } }";
        let sources = SourceMap::new();
        let file = sources.push_back("flow.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty());
        let mut parsed = parsed.unwrap();
        let ast::Item::Function(function) = &mut parsed.module.items[0].0.item else {
            panic!()
        };
        let Some(ast::FunctionBody::Block(body)) = &mut function.body else {
            panic!()
        };
        let ast::Stmt::While(loop_) = &mut body.stmts[0].0 else {
            panic!()
        };
        if malformed == 0 {
            loop_.condition = ast::LetCondition::Expr((
                ast::Expr::Constant(ast::Constant::Bool(false)),
                ast::Span::new(0, 0, file),
            ));
        } else {
            let ast::Stmt::If { blocks, else_ } = &mut loop_.body.stmts[0].0 else {
                panic!()
            };
            match malformed {
                1 => *blocks = vec![].into(),
                2 => *blocks = vec![blocks[0].clone(), blocks[0].clone()].into(),
                _ => *else_ = Some(ast::Block::new(vec![])),
            }
        }
        let dependency = "native func Mark(value: Int32);";
        let dependency_file = sources.push_back("helpers.reds", dependency);
        let (dependency, errors) =
            redscript_community_parser::parse_module(dependency, dependency_file);
        assert!(errors.is_empty());
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, _, errors) = process_sources(
            [parsed],
            [dependency.unwrap()],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        assert!(
            matches!(
                errors.as_slice(),
                [redscript_redmod_adapter::Error::Capability {
                    kind: redscript_redmod_adapter::CapabilityError::ControlFlowProvenance,
                    ..
                }]
            ),
            "{errors:?}"
        );
        let function = unit.functions.values().next().unwrap();
        assert!(function.locals.is_empty());
        let ir::Stmt::While(loop_, _) = &function.block.stmts[0] else {
            panic!()
        };
        assert!(matches!(loop_.block.stmts[0], ir::Stmt::Branches { .. }));
    }
}

#[test]
fn discarded_sequences_preserve_assignment_and_call_order() {
    run(
        "function Run(): Int32 { var i: Int32 = 0; Mark(i), i = Inc(i), Mark(i); if (false) { Mark(99), i = Inc(i); } Mark(2), i = Inc(i); return i; }",
        &[0, 1, 2],
        2,
    );
}

#[derive(Default)]
struct Counts {
    calls: usize,
    continues: usize,
}

impl<'ctx> ir::Visitor<'ctx> for Counts {
    fn visit_call(&mut self, call: &ir::Call<'ctx>, _: redscript_community_ast::Span) {
        self.calls += 1;
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }

    fn visit_continue(&mut self, _: redscript_community_ast::Span) {
        self.continues += 1;
    }
}

#[test]
fn typed_ir_snapshot_keeps_one_step_and_no_rewritten_continue_sites() {
    analyze(
        "function Run() { for (var i: Int32 = 0; Less(i, 2); Mark(1), i = Inc(i)) { if (true) { continue; } continue; } do { continue; } while (false); }",
        |unit, _| {
            let function = unit.functions.values().next().unwrap();
            insta::assert_debug_snapshot!(function.block);
            use ir::Visitor;
            let mut counts = Counts::default();
            counts.visit_block(&function.block);
            assert_eq!(counts.calls, 3); // Less, Mark, Inc, each type checked once.
            assert_eq!(counts.continues, 3);
        },
    );
}

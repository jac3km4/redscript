//! Parity between full REDmod analysis and single-function query analysis.

use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{
    CompileErrorReporter, FieldId, LoweredFunction, QueryTarget, RefType, Symbols, TypeInterner,
    expr_type, ir,
};
use redscript_io::{
    Class, ClassFlags, Function, FunctionFlags, ScriptBundle, Type, TypeKind, Visibility,
};
use redscript_redmod_adapter::{
    CapabilityError, Error, parse_file_with_metadata, process_sources, process_sources_for_query,
};

/// A native class `Native` with `Base() -> Int32` and `Run() -> Void` slots for
/// `@replaceMethod`/`@wrapMethod`, plus the primitives the fixtures use.
fn native_bundle() -> ScriptBundle<'static> {
    let mut bundle = ScriptBundle::default();
    let iscriptable = bundle.cnames_mut().add("IScriptable");
    bundle.define(Class::new(
        iscriptable,
        Visibility::Public,
        ClassFlags::default(),
    ));
    let int32 = bundle.cnames_mut().add("Int32");
    let int32_t = bundle.define(Type::new(int32, TypeKind::Primitive));
    let bool_name = bundle.cnames_mut().add("Bool");
    bundle.define(Type::new(bool_name, TypeKind::Primitive));
    let void = bundle.cnames_mut().add("Void");
    let void_t = bundle.define(Type::new(void, TypeKind::Primitive));

    let native = bundle.cnames_mut().add("Native");
    let parent = bundle.define(Class::new(
        native,
        Visibility::Public,
        ClassFlags::default().with_is_native(true),
    ));
    let mut methods = Vec::new();
    for (name, return_type) in [("Base", int32_t), ("Run", void_t)] {
        let name = bundle.cnames_mut().add(name);
        methods.push(bundle.define(
            Function::new(
                name,
                Visibility::Public,
                FunctionFlags::default().with_is_native(true),
            )
            .with_class(Some(parent))
            .with_return_type(Some(return_type)),
        ));
    }
    bundle[parent] = bundle[parent].clone().with_methods(methods);
    bundle
}

/// Canonical, type-free structural description; see the frontend query suite for
/// why raw `Debug` cannot be compared directly.
#[derive(Default)]
struct Shape {
    tokens: String,
}

impl Shape {
    fn push(&mut self, token: impl std::fmt::Display) {
        self.tokens.push_str(&token.to_string());
        self.tokens.push(';');
    }
}

impl<'ctx> ir::Visitor<'ctx> for Shape {
    fn visit_block(&mut self, block: &ir::Block<'ctx>) {
        self.push(format_args!("block[{}]", block.stmts.len()));
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&mut self, while_loop: &ir::ConditionalBlock<'ctx>, span: Span) {
        self.push(format_args!("while@{}", span.start));
        self.visit_expr(&while_loop.condition);
        self.visit_block(&while_loop.block);
    }

    fn visit_branches(
        &mut self,
        branches: &[ir::ConditionalBlock<'ctx>],
        default: Option<&ir::Block<'ctx>>,
        span: Span,
    ) {
        self.push(format_args!(
            "branches@{}:{}:{}",
            span.start,
            branches.len(),
            default.is_some()
        ));
        for branch in branches {
            self.visit_expr(&branch.condition);
            self.visit_block(&branch.block);
        }
        if let Some(block) = default {
            self.visit_block(block);
        }
    }

    fn visit_switch(
        &mut self,
        scrutinee: &ir::Expr<'ctx>,
        _scrutinee_type: &ir::Type<'ctx>,
        branches: &[ir::Case<'ctx>],
        default_index: usize,
        default: Option<&ir::Block<'ctx>>,
        span: Span,
    ) {
        self.push(format_args!(
            "switch@{}:{}:{}:{}",
            span.start,
            branches.len(),
            default_index,
            default.is_some()
        ));
        self.visit_expr(scrutinee);
        for index in 0..=branches.len() {
            if index == default_index
                && let Some(block) = default
            {
                self.visit_block(block);
            }
            if let Some(branch) = branches.get(index) {
                self.visit_expr(&branch.matcher);
                self.visit_block(&branch.block);
            }
        }
    }

    fn visit_init_array(
        &mut self,
        local: ir::Local,
        elements: &[ir::Expr<'ctx>],
        _element_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("init_array:{local:?}:{}", elements.len()));
        self.visit_local(local, span);
        for element in elements {
            self.visit_expr(element);
        }
    }

    fn visit_init_default(&mut self, local: ir::Local, _typ: &ir::Type<'ctx>, span: Span) {
        self.push(format_args!("init_default:{local:?}"));
        self.visit_local(local, span);
    }

    fn visit_break(&mut self, span: Span) {
        self.push(format_args!("break@{}", span.start));
    }

    fn visit_continue(&mut self, span: Span) {
        self.push(format_args!("continue@{}", span.start));
    }

    fn visit_return(&mut self, expr: Option<&ir::Expr<'ctx>>, span: Span) {
        self.push(format_args!("return@{}:{}", span.start, expr.is_some()));
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_new_class(&mut self, _class: &ir::TypeApp<'ctx>, span: Span) {
        self.push(format_args!("new@{}", span.start));
    }

    fn visit_new_struct(
        &mut self,
        _struct_: &ir::TypeApp<'ctx>,
        args: &[ir::Expr<'ctx>],
        span: Span,
    ) {
        self.push(format_args!("struct@{}", span.start));
        for arg in args {
            self.visit_expr(arg);
        }
    }

    fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, span: Span) {
        self.push(format_args!("closure@{}", span.start));
        for local in &closure.locals {
            self.push(format_args!("closure_local:{:?}:{:?}", local.id, local.name));
        }
        self.visit_block(&closure.block);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, span: Span) {
        let kind = match call {
            ir::Call::FreeFunction { .. } => "free",
            ir::Call::Static { .. } => "static",
            ir::Call::Instance { .. } => "instance",
            ir::Call::Closure { .. } => "closure",
        };
        self.push(format_args!("call:{kind}@{}", span.start));
        if let Some(receiver) = call.receiver() {
            self.visit_expr(receiver);
        }
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }

    fn visit_assign(&mut self, place: &ir::Expr<'ctx>, value: &ir::Expr<'ctx>, span: Span) {
        self.push(format_args!("assign@{}", span.start));
        self.visit_expr(place);
        self.visit_expr(value);
    }

    fn visit_field(
        &mut self,
        receiver: &ir::Expr<'ctx>,
        _receiver_type: &ir::TypeApp<'ctx>,
        _receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("field:{field:?}@{}", span.start));
        self.visit_expr(receiver);
    }

    fn visit_index(
        &mut self,
        array: &ir::Expr<'ctx>,
        _array_type: &ir::Type<'ctx>,
        index: &ir::Expr<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("index@{}", span.start));
        self.visit_expr(array);
        self.visit_expr(index);
    }

    fn visit_conditional(
        &mut self,
        condition: &ir::Expr<'ctx>,
        then_branch: &ir::Expr<'ctx>,
        else_branch: &ir::Expr<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("conditional@{}", span.start));
        self.visit_expr(condition);
        self.visit_expr(then_branch);
        self.visit_expr(else_branch);
    }

    fn visit_dyn_cast(
        &mut self,
        expr: &ir::Expr<'ctx>,
        _expr_type: &ir::Type<'ctx>,
        _target_type: &ir::Type<'ctx>,
        span: Span,
    ) {
        self.push(format_args!("cast@{}", span.start));
        self.visit_expr(expr);
    }

    fn visit_local(&mut self, local: ir::Local, span: Span) {
        self.push(format_args!("local:{local:?}@{}", span.start));
    }

    fn visit_capture(&mut self, capture: ir::Local, span: Span) {
        self.push(format_args!("capture:{capture:?}@{}", span.start));
    }

    fn visit_const(&mut self, constant: &ir::Const<'ctx>) {
        self.push(format_args!("const:{constant:?}"));
    }

    fn visit_null(&mut self, is_weak: bool, span: Span) {
        self.push(format_args!("null:{is_weak}@{}", span.start));
    }
}

fn shape(function: &LoweredFunction<'_>) -> String {
    let mut shape = Shape::default();
    for local in &function.locals {
        shape.push(format_args!(
            "local {:?} purpose={:?} name={:?}",
            local.id, local.temporary_purpose, local.name
        ));
    }
    ir::Visitor::visit_block(&mut shape, &function.block);
    shape.tokens
}

fn last_return<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> Option<&'a ir::Expr<'ctx>> {
    match function.block.stmts.back() {
        Some(ir::Stmt::Return(Some(expr), _)) => Some(expr),
        _ => None,
    }
}

#[test]
fn query_lowering_matches_full_for_patches_and_redmod_loops() {
    let bundle = native_bundle();
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let dependency = "native func Mark(value: Int32); \
        native func Less(a: Int32, b: Int32) -> Bool; \
        native func Inc(value: Int32) -> Int32; \
        native func Once(value: Bool) -> Bool;";
    let dep_id = sources.push_back("helpers.reds", dependency);
    let (dependency, errors) = redscript_community_parser::parse_module(dependency, dep_id);
    assert!(errors.is_empty(), "{errors:?}");
    let dependency = dependency.unwrap();

    let source = r#"
        @replaceMethod(Native)
        function Base() -> Int32 {
          var total: Int32 = 0;
          for (var i: Int32 = 0; Less(i, 3); i = Inc(i)) { total = Inc(total); }
          return total;
        }
        @addMethod(Native)
        function Added(value: Int32) -> Int32 {
          do { value = Inc(value); } while (Once(false));
          return value;
        }
        @wrapMethod(Native)
        function Run() {
          Run();
          for (;;) { break; }
        }
    "#;
    let file = sources.push_back("patches.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();

    let interner = TypeInterner::default();
    let flags = TypeFlagRegistry::default();
    let (symbols, _) = CompilationInputs::load(&bundle, &interner, &flags)
        .unwrap()
        .into_inner();

    let mut reporter = CompileErrorReporter::default();
    let (full_unit, full_symbols, errors) = process_sources(
        [parsed.clone()],
        [dependency.clone()],
        symbols.clone(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let spans = full_unit.all_functions().map(|f| f.span).collect::<Vec<_>>();
    assert!(!spans.is_empty(), "fixture should lower some functions");

    for span in spans {
        let mut reporter = CompileErrorReporter::default();
        let (query_unit, query_symbols, errors) = process_sources_for_query(
            [parsed.clone()],
            [dependency.clone()],
            symbols.clone(),
            QueryTarget {
                file: span.file,
                pos: span.start,
            },
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

        let query_functions = query_unit.all_functions().collect::<Vec<_>>();
        assert_eq!(query_functions.len(), 1, "expected one function at {span:?}");
        let query_function = query_functions[0];
        assert_eq!(query_function.span, span);

        let full_function = full_unit
            .all_functions()
            .find(|function| function.span == span)
            .expect("full unit should contain the queried function");
        assert_eq!(
            shape(query_function),
            shape(full_function),
            "shape mismatch at {span:?}"
        );

        match (last_return(query_function), last_return(full_function)) {
            (Some(query_expr), Some(full_expr)) => assert_eq!(
                format!("{:?}", expr_type(query_expr, query_function, &query_symbols)),
                format!("{:?}", expr_type(full_expr, full_function, &full_symbols)),
                "return type mismatch at {span:?}"
            ),
            (None, None) => {}
            _ => panic!("return shape mismatch at {span:?}"),
        }

        assert!(query_unit.scopes.contains_key(&span.file));
    }
}

/// Collects the spans of promoted `UnsafeArrayEvaluation` capabilities.
fn unsafe_array_spans(errors: &[Error]) -> Vec<Span> {
    errors
        .iter()
        .filter_map(|error| match error {
            Error::Capability {
                kind: CapabilityError::UnsafeArrayEvaluation,
                span,
            } => Some(*span),
            _ => None,
        })
        .collect()
}

#[test]
fn query_mode_promotes_unsafe_array_evaluation_like_full_mode() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let dependency = "native func Earlier() -> Int32; \
        native func Effect() -> Int32; \
        native func Use(value: Int32, values: array<Int32>);";
    let dep_id = sources.push_back("helpers.reds", dependency);
    let (dependency, errors) = redscript_community_parser::parse_module(dependency, dep_id);
    assert!(errors.is_empty(), "{errors:?}");
    let dependency = dependency.unwrap();

    // `Earlier()` is evaluated before the array-literal argument, reordering
    // observable calls, so the resolved-IR pass rejects the prefix.
    let source = "function Bad() { Use(Earlier(), [Effect()]); }";
    let file = sources.push_back("prefix.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();

    let interner = TypeInterner::default();

    let mut reporter = CompileErrorReporter::default();
    let (full_unit, _, full_errors) = process_sources(
        [parsed.clone()],
        [dependency.clone()],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let full_spans = unsafe_array_spans(&full_errors);
    assert_eq!(full_spans.len(), 1, "{full_errors:?}");

    let span = full_unit
        .all_functions()
        .next()
        .expect("fixture should lower a function")
        .span;

    let mut reporter = CompileErrorReporter::default();
    let (_, _, query_errors) = process_sources_for_query(
        [parsed.clone()],
        [dependency.clone()],
        Symbols::with_default_types(),
        QueryTarget {
            file: span.file,
            pos: span.start,
        },
        &mut reporter,
        &interner,
    );
    assert_eq!(
        unsafe_array_spans(&query_errors),
        full_spans,
        "{query_errors:?}"
    );
}

#[test]
fn query_lowering_matches_full_across_multiple_files() {
    let bundle = native_bundle();
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let dependency = "native func Less(a: Int32, b: Int32) -> Bool; \
        native func Inc(value: Int32) -> Int32; \
        native func Once(value: Bool) -> Bool;";
    let dep_id = sources.push_back("helpers.reds", dependency);
    let (dependency, errors) = redscript_community_parser::parse_module(dependency, dep_id);
    assert!(errors.is_empty(), "{errors:?}");
    let dependency = dependency.unwrap();

    let first = r#"
        @replaceMethod(Native)
        function Base() -> Int32 {
          var total: Int32 = 0;
          for (var i: Int32 = 0; Less(i, 3); i = Inc(i)) { total = Inc(total); }
          return total;
        }
    "#;
    let first_id = sources.push_back("first.script", first);
    let (first, errors) = parse_file_with_metadata(first, first_id);
    assert!(errors.is_empty(), "{errors:?}");
    let first = first.unwrap();

    let second = r#"
        @wrapMethod(Native)
        function Run() {
          Run();
          do { } while (Once(false));
        }
        @addMethod(Native)
        function Extra(value: Int32) -> Int32 {
          do { value = Inc(value); } while (Once(false));
          return value;
        }
    "#;
    let second_id = sources.push_back("second.script", second);
    let (second, errors) = parse_file_with_metadata(second, second_id);
    assert!(errors.is_empty(), "{errors:?}");
    let second = second.unwrap();

    let interner = TypeInterner::default();
    let flags = TypeFlagRegistry::default();
    let (symbols, _) = CompilationInputs::load(&bundle, &interner, &flags)
        .unwrap()
        .into_inner();

    let mut reporter = CompileErrorReporter::default();
    let (full_unit, full_symbols, errors) = process_sources(
        [first.clone(), second.clone()],
        [dependency.clone()],
        symbols.clone(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let spans = full_unit.all_functions().map(|f| f.span).collect::<Vec<_>>();
    assert!(!spans.is_empty(), "fixture should lower some functions");

    for span in spans {
        let mut reporter = CompileErrorReporter::default();
        let (query_unit, query_symbols, errors) = process_sources_for_query(
            [first.clone(), second.clone()],
            [dependency.clone()],
            symbols.clone(),
            QueryTarget {
                file: span.file,
                pos: span.start,
            },
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

        let query_functions = query_unit.all_functions().collect::<Vec<_>>();
        assert_eq!(query_functions.len(), 1, "expected one function at {span:?}");
        let query_function = query_functions[0];
        assert_eq!(query_function.span, span);

        let full_function = full_unit
            .all_functions()
            .find(|function| function.span == span)
            .expect("full unit should contain the queried function");
        assert_eq!(
            shape(query_function),
            shape(full_function),
            "shape mismatch at {span:?}"
        );

        match (last_return(query_function), last_return(full_function)) {
            (Some(query_expr), Some(full_expr)) => assert_eq!(
                format!("{:?}", expr_type(query_expr, query_function, &query_symbols)),
                format!("{:?}", expr_type(full_expr, full_function, &full_symbols)),
                "return type mismatch at {span:?}"
            ),
            (None, None) => {}
            _ => panic!("return shape mismatch at {span:?}"),
        }

        assert!(query_unit.scopes.contains_key(&span.file));
    }
}

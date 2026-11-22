use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, FieldId, LoweredCompilationUnit, LoweredFunction, PolyType,
    QueryError, QueryTarget, RefType, Shared, Symbols, Type, TypeInterner, expr_type,
    infer_from_sources, ir, parse_files, predef, process_sources_for_query,
};

fn lower<'ctx>(
    sources: &'ctx SourceMap,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    sources.populate_boot_lib();
    let mut reporter = CompileErrorReporter::default();
    let result = infer_from_sources(
        sources,
        Symbols::with_default_types(),
        &mut reporter,
        interner,
    );
    let diagnostics = reporter.into_reported();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    result
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

fn return_expr<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> &'a ir::Expr<'ctx> {
    match function.block.stmts.back().unwrap() {
        ir::Stmt::Return(Some(expr), _) => expr,
        stmt => panic!("expected a return expression, found {stmt:?}"),
    }
}

#[test]
fn direct_array_bindings_keep_exact_query_spans_without_semantic_reads() {
    let source =
        "func Test() { let values = [1]; const empty: [Int32] = []; let defaulted: Int32; [3]; }";
    let sources = SourceMap::new();
    let file = sources.push_back("query.reds", source);
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    let function = function(&unit, &symbols, "Test");
    for name in ["values", "empty"] {
        let local = function
            .locals
            .iter()
            .find(|local| local.name == Some(name))
            .unwrap();
        let start = source.find(name).unwrap() as u32;
        let expected_span = Span::new(start, start + name.len() as u32, file);
        for pos in start..expected_span.end {
            let expr = function.block.find_at(pos).unwrap();
            assert!(
                matches!(expr, ir::Expr::Local(id, span) if *id == local.id && *span == expected_span)
            );
            assert_eq!(
                expr_type(expr, function, &symbols).unwrap(),
                Type::app(predef::ARRAY, [Type::nullary(predef::INT32)])
            );
        }
    }
    let ir::Stmt::InitArray {
        source_binding: Some(_),
        ..
    } = &function.block.stmts[0]
    else {
        panic!("missing nonempty declaration binding")
    };
    let ir::Stmt::InitDefault {
        source_binding: Some(_),
        ..
    } = &function.block.stmts[1]
    else {
        panic!("missing empty declaration binding")
    };
    assert!(matches!(
        &function.block.stmts[2],
        ir::Stmt::InitDefault {
            source_binding: None,
            ..
        }
    ));
    assert!(matches!(
        &function.block.stmts[3],
        ir::Stmt::InitArray {
            source_binding: None,
            ..
        }
    ));
    assert!(
        function
            .block
            .find_at(source.find("defaulted").unwrap() as u32)
            .is_none()
    );
    assert!(
        function
            .block
            .find_at(source.find("Int32").unwrap() as u32)
            .is_none()
    );
    assert!(
        function
            .block
            .find_at(source.find("[]").unwrap() as u32)
            .is_none()
    );
    assert!(matches!(
        function
            .block
            .find_at(source.find("[1]").unwrap() as u32 + 1),
        Some(ir::Expr::Const(ir::Const::I32(1), _))
    ));

    #[derive(Default)]
    struct LocalOccurrences(Vec<ir::Local>);
    impl<'ctx> ir::Visitor<'ctx> for LocalOccurrences {
        fn visit_local(&mut self, local: ir::Local, _: Span) {
            self.0.push(local);
        }
    }
    let mut occurrences = LocalOccurrences::default();
    ir::Visitor::visit_block(&mut occurrences, &function.block);
    for name in ["values", "empty"] {
        let local = function
            .locals
            .iter()
            .find(|local| local.name == Some(name))
            .unwrap();
        assert_eq!(
            occurrences.0.iter().filter(|id| **id == local.id).count(),
            1
        );
    }
}

#[test]
fn shadowed_direct_array_bindings_in_closures_keep_their_own_identity() {
    let source = "func Test() { let values = [1]; let f: () -> Void = () -> { let values = [values[0]]; let empty: [Int32] = []; }; }";
    let sources = SourceMap::new();
    let file = sources.push_back("query.reds", source);
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    let function = function(&unit, &symbols, "Test");
    let outer = function
        .locals
        .iter()
        .find(|local| local.name == Some("values"))
        .unwrap();
    let ir::Stmt::Expr(expr) = &function.block.stmts[1] else {
        panic!("expected closure declaration")
    };
    let ir::Expr::Assign { expr, .. } = &**expr else {
        panic!("expected closure assignment")
    };
    let ir::Expr::NewClosure { closure, .. } = &**expr else {
        panic!("expected closure")
    };
    let inner = closure
        .locals
        .iter()
        .find(|local| local.name == Some("values"))
        .unwrap();
    let empty = closure
        .locals
        .iter()
        .find(|local| local.name == Some("empty"))
        .unwrap();
    assert_ne!(outer.id, inner.id);
    for (start, local) in [
        (source.find("values").unwrap(), outer),
        (source.find("let values = [values").unwrap() + 4, inner),
        (source.find("empty").unwrap(), empty),
    ] {
        let name = local.name.unwrap();
        let expected_span = Span::new(start as u32, (start + name.len()) as u32, file);
        let expr = function.block.find_at(start as u32).unwrap();
        assert!(
            matches!(expr, ir::Expr::Local(id, span) if *id == local.id && *span == expected_span)
        );
        assert_eq!(
            expr_type(expr, function, &symbols).unwrap(),
            Type::app(predef::ARRAY, [Type::nullary(predef::INT32)])
        );
    }
    let start = source.find("[values[0]]").unwrap() + 1;
    let expr = function.block.find_at(start as u32).unwrap();
    assert!(
        matches!(expr, ir::Expr::Capture(id, span) if *id == outer.id && *span == Span::new(start as u32, (start + "values".len()) as u32, file))
    );
    assert_eq!(
        expr_type(expr, function, &symbols).unwrap(),
        Type::app(predef::ARRAY, [Type::nullary(predef::INT32)])
    );
}

#[test]
fn constants_locals_and_assignment() {
    let sources = SourceMap::new();
    sources.push_back(
        "query.reds",
        r#"
        func Constant() -> Int32 { return 42; }
        func Boolean() -> Bool { return true; }
        func StringValue() -> String { return "hello"; }
        func Local(value: Int32) -> Int32 { return value; }
        func Assigned(value: Int32) -> Int32 { value = 1; return value; }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    for (name, expected) in [
        ("Constant", predef::INT32),
        ("Boolean", predef::BOOL),
        ("StringValue", predef::STRING),
        ("Local", predef::INT32),
        ("Assigned", predef::INT32),
    ] {
        let function = function(&unit, &symbols, name);
        assert_eq!(
            expr_type(return_expr(function), function, &symbols).unwrap(),
            Type::nullary(expected),
            "{name}"
        );
    }
    let assigned = function(&unit, &symbols, "Assigned");
    let ir::Stmt::Expr(expr) = &assigned.block.stmts[0] else {
        panic!("expected assignment");
    };
    assert_eq!(
        expr_type(expr, assigned, &symbols).unwrap(),
        Type::nullary(predef::INT32)
    );
    let ir::Expr::Assign { place, .. } = &**expr else {
        panic!("expected assignment");
    };
    let ir::Expr::Local(local, span) = &**place else {
        panic!("expected local destination");
    };
    let assignment = || ir::Expr::Assign {
        place: ir::Expr::Local(*local, *span).into(),
        expr: ir::Expr::Const(ir::Const::I32(1), *span).into(),
        typ: PolyType::nullary(predef::INT32).into(),
        span: *span,
    };
    let conditional = ir::Expr::Conditional {
        condition: ir::Expr::Const(ir::Const::Bool(true), expr.span()).into(),
        then: assignment().into(),
        else_: assignment().into(),
        span: expr.span(),
    };
    assert_eq!(
        expr_type(&conditional, assigned, &symbols).unwrap(),
        Type::nullary(predef::INT32)
    );
}

#[test]
fn preserves_reference_wrappers() {
    let sources = SourceMap::new();
    sources.push_back(
        "query.reds",
        r#"
        class Base {}
        class Left extends Base {}
        @mixedRef()
        class Mixed {}
        func WeakCast(value: wref<Base>) -> wref<Left> { return value as Left; }
        func Make() -> ref<Mixed> { return new Mixed(); }
        func Cast(value: IScriptable) -> ref<Mixed> { return value as Mixed; }
        func Plain() -> Base { return new Base(); }
        func PlainCast(value: Base) -> Left { return value as Left; }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    for (name, expected) in [
        (
            "WeakCast",
            Type::app(predef::WREF, [Type::nullary(interner.intern("Left"))]),
        ),
        (
            "Make",
            Type::app(predef::REF, [Type::nullary(interner.intern("Mixed"))]),
        ),
        (
            "Cast",
            Type::app(predef::REF, [Type::nullary(interner.intern("Mixed"))]),
        ),
        ("Plain", Type::nullary(interner.intern("Base"))),
        ("PlainCast", Type::nullary(interner.intern("Left"))),
    ] {
        let function = function(&unit, &symbols, name);
        assert_eq!(
            expr_type(return_expr(function), function, &symbols).unwrap(),
            expected,
            "{name}"
        );
        assert!(matches!(
            expr_type(
                return_expr(function),
                function,
                &Symbols::with_default_types()
            ),
            Err(QueryError::MissingType(_))
        ));
    }
}

#[test]
fn conditionals_join_both_branches_and_handle_null() {
    let sources = SourceMap::new();
    sources.push_back(
        "query.reds",
        r#"
        class Base {}
        class Left extends Base {}
        class Right extends Base {}
        func Same(flag: Bool, a: Int32, b: Int32) -> Int32 { return flag ? a : b; }
        func Widen(flag: Bool, a: Left, b: Base) -> Base { return flag ? a : b; }
        func WidenReverse(flag: Bool, a: Base, b: Left) -> Base { return flag ? a : b; }
        func Siblings(flag: Bool, a: Left, b: Right) -> Base { return flag ? a : b; }
        func NullThen(flag: Bool, a: Left) -> Left { return flag ? null : a; }
        func NullElse(flag: Bool, a: Left) -> Left { return flag ? a : null; }
        func WeakNull(flag: Bool, a: wref<Left>) -> wref<Left> { return flag ? a : null; }
        func NestedNull(flag: Bool, a: Left) -> Left {
          return flag ? (flag ? null : null) : a;
        }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    for (name, expected) in [
        ("Same", Type::nullary(predef::INT32)),
        ("Widen", Type::nullary(interner.intern("Base"))),
        ("WidenReverse", Type::nullary(interner.intern("Base"))),
        ("Siblings", Type::nullary(interner.intern("Base"))),
        ("NullThen", Type::nullary(interner.intern("Left"))),
        ("NullElse", Type::nullary(interner.intern("Left"))),
        (
            "WeakNull",
            Type::app(predef::WREF, [Type::nullary(interner.intern("Left"))]),
        ),
        ("NestedNull", Type::nullary(interner.intern("Left"))),
    ] {
        let function = function(&unit, &symbols, name);
        assert_eq!(
            expr_type(return_expr(function), function, &symbols).unwrap(),
            expected,
            "{name}"
        );
    }
}

#[test]
fn substitutes_generic_calls_and_inherited_fields() {
    let sources = SourceMap::new();
    sources.push_back(
        "query.reds",
        r#"
        class Box<T> {
          public let value: T;
          public func Get() -> T { return this.value; }
          public func Echo<U>(value: U) -> U { return value; }
          public static func StaticGet(value: T) -> T { return value; }
        }
        class Derived<T> extends Box<array<T>> {}
        func Identity<T>(value: T) -> T { return value; }
        func Free() -> Int32 { return Identity<Int32>(1); }
        func Field(value: Box<Int32>) -> Int32 { return value.value; }
        func Instance(value: Box<Int32>) -> Int32 { return value.Get(); }
        func Method(value: Box<Int32>) -> String { return value.Echo<String>("hello"); }
        func Static() -> Int32 { return Box.StaticGet(1); }
        func InheritedField(value: Derived<Int32>) -> array<Int32> { return value.value; }
        func InheritedCall(value: Derived<Int32>) -> array<Int32> { return value.Get(); }
        func Generic<T>(value: T) -> T { return Identity<T>(value); }
        func Element(value: array<Int32>) -> Int32 { return value[0]; }
        func Closure(value: (Int32) -> String) -> String { return value(1); }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    for (name, expected) in [
        ("Free", Type::nullary(predef::INT32)),
        ("Field", Type::nullary(predef::INT32)),
        ("Instance", Type::nullary(predef::INT32)),
        ("Method", Type::nullary(predef::STRING)),
        ("Static", Type::nullary(predef::INT32)),
        (
            "InheritedField",
            Type::app(predef::ARRAY, [Type::nullary(predef::INT32)]),
        ),
        (
            "InheritedCall",
            Type::app(predef::ARRAY, [Type::nullary(predef::INT32)]),
        ),
        ("Element", Type::nullary(predef::INT32)),
        ("Closure", Type::nullary(predef::STRING)),
    ] {
        let function = function(&unit, &symbols, name);
        assert_eq!(
            expr_type(return_expr(function), function, &symbols).unwrap(),
            expected,
            "{name}"
        );
    }
    let generic = function(&unit, &symbols, "Generic");
    let typ = expr_type(return_expr(generic), generic, &symbols).unwrap();
    assert!(matches!(typ, Type::Ctx(var) if var.name() == "T"));

    let empty_symbols = Symbols::with_default_types();
    let free = function(&unit, &symbols, "Free");
    assert!(matches!(
        expr_type(return_expr(free), free, &empty_symbols),
        Err(QueryError::MissingFunction(_))
    ));
    let field = function(&unit, &symbols, "Field");
    assert!(matches!(
        expr_type(return_expr(field), field, &empty_symbols),
        Err(QueryError::MissingField(_))
    ));
    let instance = function(&unit, &symbols, "Instance");
    assert!(matches!(
        expr_type(return_expr(instance), instance, &empty_symbols),
        Err(QueryError::MissingMethod(_))
    ));

    let ir::Expr::Call { call, span } = return_expr(free) else {
        panic!("expected a call");
    };
    let ir::Call::FreeFunction { function: id, .. } = &**call else {
        panic!("expected a free function call");
    };
    let missing_type_arg = ir::Expr::call(
        ir::Call::FreeFunction {
            function: *id,
            type_args: [].into(),
            args: [].into(),
        },
        *span,
    );
    assert!(matches!(
        expr_type(&missing_type_arg, free, &symbols),
        Err(QueryError::TypeArgumentCount {
            expected: 1,
            found: 0
        })
    ));
}

#[test]
fn closure_queries_use_the_enclosing_function() {
    let source = r#"
        func Captured(value: Int32) -> () -> Int32 { return () -> value; }
        func Parameter(outer: Int32) -> (String) -> String { return (inner: String) -> inner; }
        func Local() -> () -> Int32 { return () -> { let local = 1; return local; }; }
        func Nested(value: Int32) -> (String) -> () -> String {
          return (inner: String) -> () -> inner;
        }
        func Deep(value: Int32) -> () -> () -> Int32 { return () -> () -> value; }
        func Gaps() -> (String) -> String {
          return (text: String) -> {
            let before = 1;
            let nested = (arg: Int32) -> arg;
            let after = text;
            nested(before);
            return after;
          };
        }
        class Holder {
          public func CaptureThis() -> () -> Holder { return () -> this; }
        }
    "#;
    let sources = SourceMap::new();
    sources.push_back("query.reds", source);
    let interner = TypeInterner::default();
    let (unit, symbols) = lower(&sources, &interner);
    for (marker, name, expected) in [
        ("() -> value;", "value", Type::nullary(predef::INT32)),
        ("-> inner;", "inner", Type::nullary(predef::STRING)),
        ("return local;", "local", Type::nullary(predef::INT32)),
        ("() -> inner;", "inner", Type::nullary(predef::STRING)),
        ("() -> () -> value;", "value", Type::nullary(predef::INT32)),
        ("-> arg;", "arg", Type::nullary(predef::INT32)),
        ("return after;", "after", Type::nullary(predef::STRING)),
        (
            "() -> this;",
            "this",
            Type::nullary(interner.intern("Holder")),
        ),
    ] {
        let pos = (source.find(marker).unwrap() + marker.find(name).unwrap()) as u32;
        let (function, expr) = unit
            .all_functions()
            .find_map(|function| Some((function, function.block.find_at(pos)?)))
            .unwrap();
        assert_eq!(
            expr_type(expr, function, &symbols).unwrap(),
            expected,
            "{marker}"
        );
    }
}

/// A canonical, type-free structural description of a lowered function.
///
/// The raw `Debug` of IR embeds lazily-coalesced type caches (`Uninit`/`Init`)
/// and inference-variable identities, which legitimately differ between two
/// compilations even when the shape is identical. This visitor records only
/// spans, node kinds, local/capture identities and constants.
#[derive(Default)]
struct FunctionShape {
    tokens: String,
}

impl FunctionShape {
    fn push(&mut self, token: impl std::fmt::Display) {
        self.tokens.push_str(&token.to_string());
        self.tokens.push(';');
    }
}

impl<'ctx> ir::Visitor<'ctx> for FunctionShape {
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
            self.push(format_args!(
                "closure_local:{:?}:{:?}",
                local.id, local.name
            ));
        }
        self.push(format_args!("captures:{:?}", closure.captures));
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

fn function_shape(function: &LoweredFunction<'_>) -> String {
    let mut shape = FunctionShape::default();
    for local in &function.locals {
        shape.push(format_args!(
            "local {:?} const={} init={:?} decl={:?} name={:?} purpose={:?} owner={:?} span={:?}",
            local.id,
            local.is_const,
            local.initializer,
            local.declaration,
            local.name,
            local.temporary_purpose,
            local.temporary_owner,
            local.span,
        ));
    }
    ir::Visitor::visit_block(&mut shape, &function.block);
    shape.tokens
}

fn maybe_return_expr<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> Option<&'a ir::Expr<'ctx>> {
    match function.block.stmts.back() {
        Some(ir::Stmt::Return(Some(expr), _)) => Some(expr),
        _ => None,
    }
}

/// Lowers every function of `sources` with the full pipeline, then re-lowers
/// each one through the query pipeline and asserts the IR shape, return
/// expression type, locals, properties and scopes match while unrelated
/// functions are absent.
fn assert_query_parity(sources: &SourceMap) {
    sources.populate_boot_lib();
    let interner = TypeInterner::default();

    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let mut reporter = CompileErrorReporter::default();
    let (full_unit, full_symbols) = infer_from_sources(
        sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let spans = full_unit
        .all_functions()
        .map(|f| f.span)
        .collect::<Vec<_>>();
    assert!(!spans.is_empty(), "fixture should lower some functions");

    for span in spans {
        let mut reporter = CompileErrorReporter::default();
        let evaluator = Evaluator::from_modules(&modules);
        let (query_unit, query_symbols) = process_sources_for_query(
            modules.clone(),
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
            QueryTarget {
                file: span.file,
                pos: span.start,
            },
        );
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

        let query_functions = query_unit.all_functions().collect::<Vec<_>>();
        assert_eq!(
            query_functions.len(),
            1,
            "expected one function at {span:?}"
        );
        let query_function = query_functions[0];
        assert_eq!(query_function.span, span);

        let full_function = full_unit
            .all_functions()
            .find(|function| function.span == span)
            .expect("full unit should contain the queried function");

        assert_eq!(
            function_shape(query_function),
            function_shape(full_function),
            "shape mismatch at {span:?}"
        );
        assert_eq!(query_function.properties, full_function.properties);

        match (
            maybe_return_expr(query_function),
            maybe_return_expr(full_function),
        ) {
            (Some(query_expr), Some(full_expr)) => assert_eq!(
                format!(
                    "{:?}",
                    expr_type(query_expr, query_function, &query_symbols)
                ),
                format!("{:?}", expr_type(full_expr, full_function, &full_symbols)),
                "return type mismatch at {span:?}"
            ),
            (None, None) => {}
            _ => panic!("return shape mismatch at {span:?}"),
        }

        assert!(query_unit.scopes.contains_key(&span.file));
    }
}

#[test]
fn query_lowering_matches_full_compilation() {
    let sources = SourceMap::new();
    sources.push_back(
        "query.reds",
        r#"
        class Base {
          public let value: Int32;
          public func Get() -> Int32 { return this.value; }
        }
        class Derived extends Base {
          public let extra: String;
          public func Label() -> String { return this.extra; }
        }
        class Box<T> {
          public let item: T;
          public func Unwrap() -> T { return this.item; }
        }
        func Free(a: Int32) -> Int32 { return a; }
        func Generic<T>(value: T) -> T { return value; }
        func CallGeneric() -> Int32 { return Generic<Int32>(2); }
        func Conditional(flag: Bool, a: Int32, b: Int32) -> Int32 { return flag ? a : b; }
        func Closure() -> () -> Int32 { return () -> 7; }
        func UseDerived(d: Derived) -> Int32 { return d.Get(); }
        func UseBox(b: Box<Int32>) -> Int32 { return b.Unwrap(); }
        "#,
    );
    assert_query_parity(&sources);
}

#[test]
fn query_lowering_isolates_the_target_file() {
    let sources = SourceMap::new();
    let file_a = sources.push_back(
        "a.reds",
        r#"
        class Base { public func Get() -> Int32 { return 1; } }
        func Alpha() -> Int32 { return 2; }
        "#,
    );
    let file_b = sources.push_back(
        "b.reds",
        r#"
        class Other { public func Get() -> Int32 { return 3; } }
        func Beta() -> Int32 { return 4; }
        "#,
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();

    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let alpha_pos = sources
        .get(file_a)
        .unwrap()
        .source()
        .find("return 2")
        .unwrap() as u32;
    let mut reporter = CompileErrorReporter::default();
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, _) = process_sources_for_query(
        modules.clone(),
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        QueryTarget {
            file: file_a,
            pos: alpha_pos,
        },
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let functions = unit.all_functions().collect::<Vec<_>>();
    assert_eq!(functions.len(), 1);
    assert_eq!(functions[0].span.file, file_a);
    assert!(unit.scopes.contains_key(&file_a));
    assert!(!unit.scopes.contains_key(&file_b));
}

#[test]
fn querying_outside_any_function_lowers_nothing() {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "query.reds",
        r#"
        class Holder {
          public let value: Int32;
          public func Get() -> Int32 { return this.value; }
        }
        func Free() -> Int32 { return 1; }
        "#,
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();

    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    // The class header is outside every function body, so nothing is lowered.
    let header_pos = sources.get(file).unwrap().source().find("Holder").unwrap() as u32;
    let mut reporter = CompileErrorReporter::default();
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, _) = process_sources_for_query(
        modules.clone(),
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        QueryTarget {
            file,
            pos: header_pos,
        },
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_eq!(unit.all_functions().count(), 0);
    assert!(unit.scopes.contains_key(&file));
}

#[test]
fn invalid_queries_return_errors() {
    let sources = SourceMap::new();
    let file = sources.push_back("query.reds", " ");
    let span = Span::new(0, 1, file);
    let function = LoweredFunction {
        block: ir::Block::new([]),
        locals: [].into(),
        span,
        properties: redscript_compiler_frontend::ast::FunctionProperties::default(),
    };
    let symbols = Symbols::with_default_types();
    assert!(matches!(
        expr_type(
            &ir::Expr::Local(ir::Local::Var(0), span),
            &function,
            &symbols
        ),
        Err(QueryError::MissingLocal(ir::Local::Var(0)))
    ));
    assert!(matches!(
        expr_type(&ir::Expr::null(span), &function, &symbols),
        Err(QueryError::UnknownNullType)
    ));
    let conditional = ir::Expr::Conditional {
        condition: ir::Expr::Const(ir::Const::Bool(true), span).into(),
        then: ir::Expr::Const(ir::Const::I32(1), span).into(),
        else_: ir::Expr::Const(ir::Const::Bool(false), span).into(),
        span,
    };
    assert!(matches!(
        expr_type(&conditional, &function, &symbols),
        Err(QueryError::Type(_))
    ));
    let index = ir::Expr::Index {
        array: ir::Expr::null(span).into(),
        array_type: PolyType::nullary(predef::INT32).into(),
        index: ir::Expr::Const(ir::Const::I32(0), span).into(),
        span,
    };
    assert!(matches!(
        expr_type(&index, &function, &symbols),
        Err(QueryError::InvalidArray(_))
    ));
}

fn query_at<'ctx>(
    modules: &[redscript_compiler_frontend::ast::SourceModule<'ctx>],
    symbols: Symbols<'ctx>,
    interner: &'ctx TypeInterner,
    target: QueryTarget,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    let mut reporter = CompileErrorReporter::default();
    let evaluator = Evaluator::from_modules(modules);
    let result = process_sources_for_query(
        modules.iter().cloned(),
        symbols,
        evaluator,
        &mut reporter,
        interner,
        target,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    result
}

#[test]
fn query_targets_respect_span_bounds_and_lower_enclosing_closures() {
    let source = "\
        func Outer() -> Int32 { let f = () -> 9; return f(); }\n\
        \n\
        func Other() -> Int32 { return 0; }\n";
    let sources = SourceMap::new();
    let file = sources.push_back("query.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();

    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let mut reporter = CompileErrorReporter::default();
    let (full_unit, full_symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let outer = function(&full_unit, &full_symbols, "Outer").span;
    let query = |pos: u32| {
        query_at(
            &modules,
            Symbols::with_default_types(),
            &interner,
            QueryTarget { file, pos },
        )
        .0
    };

    for pos in [outer.start, outer.end - 1] {
        let unit = query(pos);
        let functions = unit.all_functions().collect::<Vec<_>>();
        assert_eq!(functions.len(), 1, "position {pos} should lower Outer");
        assert_eq!(functions[0].span, outer);
    }

    let closure_body = source.find("() -> 9").unwrap() as u32 + "() -> ".len() as u32;
    let unit = query(closure_body);
    let functions = unit.all_functions().collect::<Vec<_>>();
    assert_eq!(
        functions.len(),
        1,
        "closure body should lower its enclosing function"
    );
    assert_eq!(functions[0].span, outer);

    let unit = query(outer.end);
    assert_eq!(
        unit.all_functions().count(),
        0,
        "the exclusive span end must lower nothing"
    );
}

#[test]
fn forked_interner_and_cloned_symbols_support_repeated_query_reuse() {
    let base_sources = SourceMap::new();
    base_sources.push_back("base.reds", "class Base { public let value: Int32; }");
    base_sources.populate_boot_lib();
    let base_interner = Shared::new(TypeInterner::default());
    let mut reporter = CompileErrorReporter::default();
    let (_, base_symbols) = infer_from_sources(
        &base_sources,
        Symbols::with_default_types(),
        &mut reporter,
        &base_interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());

    let snap_sources = SourceMap::new();
    let snap_source = "class SnapshotLocal { public let value: Int32; }\n\
        func Target(x: SnapshotLocal) -> Int32 { return x.value; }\n";
    let snap_file = snap_sources.push_back("snap.reds", snap_source);
    let target = QueryTarget {
        file: snap_file,
        pos: snap_source.find("return x.value").unwrap() as u32,
    };

    for _ in 0..2 {
        let child_interner = TypeInterner::fork(&base_interner);
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&snap_sources, &mut reporter);
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        let (unit, _) = query_at(&modules, base_symbols.clone(), &child_interner, target);
        let functions = unit.all_functions().collect::<Vec<_>>();
        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].span.file, snap_file);
        assert!(child_interner.get("SnapshotLocal").is_some());
        assert!(base_interner.get("SnapshotLocal").is_none());
    }
}

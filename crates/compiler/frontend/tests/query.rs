use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, LoweredFunction, PolyType, QueryError, Symbols,
    Type, TypeInterner, expr_type, infer_from_sources, ir, predef,
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
        Type::nullary(predef::VOID)
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
        Type::nullary(predef::VOID)
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

#[test]
fn invalid_queries_return_errors() {
    let sources = SourceMap::new();
    let file = sources.push_back("query.reds", " ");
    let span = Span::new(0, 1, file);
    let function = LoweredFunction {
        block: ir::Block::new([]),
        locals: [].into(),
        span,
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

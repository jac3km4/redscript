use redscript_ast::{
    FileId, LetCondition, Pattern, QueryResult, SourcePattern, SourceStmt, Span, Stmt, Type,
};
use redscript_parser::parse_stmt;

fn parse(source: &str) -> SourceStmt<'_> {
    let (stmt, errors) = parse_stmt(source, FileId::from_i32(7));
    assert!(errors.is_empty(), "{source}: {errors:?}");
    stmt.expect("expected statement")
}

fn pattern<'src>(stmt: &SourceStmt<'src>) -> (SourcePattern<'src>, Span) {
    let Stmt::If { blocks, .. } = stmt else {
        panic!("expected if statement");
    };
    let LetCondition::LetPattern(pattern, _) = &blocks[0].condition else {
        panic!("expected pattern condition");
    };
    pattern.clone()
}

fn assert_span(source: &str, span: Span, expected: &str) {
    assert_eq!(span.file, FileId::from_i32(7));
    assert_eq!(&source[span.start as usize..span.end as usize], expected);
}

fn assert_type_lookup(stmt: &SourceStmt<'_>, source: &str, name: &str) {
    let start = source.find(name).unwrap() as u32;
    for pos in start..start + name.len() as u32 {
        assert!(
            matches!(stmt.find_at(pos), QueryResult::Type(Type::Named { name: found, .. }) if *found == name),
            "expected type {name} at {pos} in {source}"
        );
    }
}

#[test]
fn cast_preserves_inner_span_and_type_lookup() {
    let source = "if let x as Foo = v {}";
    let stmt = parse(source);
    let (Pattern::As(inner, (_, typ_span)), span) = pattern(&stmt) else {
        panic!("expected cast pattern");
    };
    assert_span(source, span, "x as Foo");
    assert_span(source, inner.1, "x");
    assert_span(source, typ_span, "Foo");
    assert_type_lookup(&stmt, source, "Foo");
}

#[test]
fn chained_suffixes_preserve_each_inner_span() {
    let source = "if let x? as Foo? as Bar = v {}";
    let stmt = parse(source);
    let (Pattern::As(bar_inner, _), span) = pattern(&stmt) else {
        panic!("expected outer cast");
    };
    assert_span(source, span, "x? as Foo? as Bar");
    assert_span(source, bar_inner.1, "x? as Foo?");
    let Pattern::Nullable(foo) = &bar_inner.0 else {
        panic!("expected nullable cast");
    };
    assert_span(source, foo.1, "x? as Foo");
    let Pattern::As(nullable, _) = &foo.0 else {
        panic!("expected inner cast");
    };
    assert_span(source, nullable.1, "x?");
    let Pattern::Nullable(name) = &nullable.0 else {
        panic!("expected nullable name");
    };
    assert_span(source, name.1, "x");
    assert_type_lookup(&stmt, source, "Foo");
    assert_type_lookup(&stmt, source, "Bar");
}

#[test]
fn nested_suffixes_preserve_spans_and_type_lookup() {
    let source = "if let Box { items: [x as Foo?] }? as Bar = v {}";
    let stmt = parse(source);
    let (Pattern::As(nullable, _), _) = pattern(&stmt) else {
        panic!("expected outer cast");
    };
    assert_span(source, nullable.1, "Box { items: [x as Foo?] }?");
    let Pattern::Nullable(aggregate) = &nullable.0 else {
        panic!("expected nullable aggregate");
    };
    assert_span(source, aggregate.1, "Box { items: [x as Foo?] }");
    let Pattern::Aggregate(_, fields) = &aggregate.0 else {
        panic!("expected aggregate");
    };
    let Pattern::Array(_, (elements, span)) = &fields[0].1 else {
        panic!("expected array");
    };
    assert_span(source, *span, "[x as Foo?]");
    let Pattern::Nullable(cast) = &elements[0] else {
        panic!("expected nullable element");
    };
    assert_span(source, cast.1, "x as Foo");
    let Pattern::As(name, _) = &cast.0 else {
        panic!("expected element cast");
    };
    assert_span(source, name.1, "x");
    assert_type_lookup(&stmt, source, "Foo");
    assert_type_lookup(&stmt, source, "Bar");
}

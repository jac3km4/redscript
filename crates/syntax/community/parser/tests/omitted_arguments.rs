use std::convert::Infallible;

use redscript_community_ast::{AstVisitor, Expr, FileId, SourceExpr, Span, WithSpan};
use redscript_community_parser::parse_expr;

fn parse(source: &str) -> SourceExpr<'_> {
    let (expr, errors) = parse_expr(source, FileId::from_i32(7));
    assert!(errors.is_empty(), "{source}: {errors:?}");
    expr.unwrap()
}

#[test]
fn positional_slots_have_zero_width_source_spans() {
    for (source, omitted) in [
        ("Target()", vec![]),
        ("Target(42)", vec![false]),
        ("Target(, 42)", vec![true, false]),
        ("Target(42, , 43)", vec![false, true, false]),
        ("Target(42,)", vec![false]),
        ("Target(42, )", vec![false]),
        ("Target(,)", vec![true]),
        ("Target(42,,)", vec![false, true]),
        ("Target(, , )", vec![true, true]),
        ("host.Target<Int32>(, 42, )", vec![true, false]),
    ] {
        let Expr::Call { args, .. } = parse(source) else {
            panic!("expected call");
        };
        assert_eq!(args.len(), omitted.len(), "{source}");
        for ((expr, span), omitted) in args.iter().zip(omitted) {
            assert_eq!(matches!(expr, Expr::Omitted), omitted, "{source}");
            assert_eq!(span.file, FileId::from_i32(7));
            if omitted {
                assert_eq!(span.start, span.end);
                assert!(matches!(
                    source.as_bytes()[span.start as usize],
                    b',' | b')'
                ));
            }
        }
    }
}

#[test]
fn holes_are_not_general_expressions_or_constructor_arguments() {
    for source in [",", "(,)", "[, 42]", "new Record(, 42)"] {
        let (_, errors) = parse_expr(source, FileId::from_i32(7));
        assert!(!errors.is_empty(), "{source}");
    }
}

#[test]
fn unwrapping_and_visiting_preserve_omitted_nodes() {
    #[derive(Default)]
    struct Counter(usize);
    impl<'src> AstVisitor<'src, WithSpan> for Counter {
        type Error = Infallible;

        fn visit_omitted(&mut self) -> Result<(), Self::Error> {
            self.0 += 1;
            Ok(())
        }
    }

    let expr = parse("Target(, Nested(42,,),,)");
    let mut counter = Counter::default();
    counter
        .visit_expr(&(expr.clone(), Span::new(0, 0, FileId::from_i32(7))))
        .unwrap();
    assert_eq!(counter.0, 3);
    let Expr::Call { args, .. } = expr.unwrapped() else {
        panic!("expected call");
    };
    assert!(matches!(args[0], Expr::Omitted));
    assert!(matches!(args[2], Expr::Omitted));
}

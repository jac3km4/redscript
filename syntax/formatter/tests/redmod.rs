use redscript_ast::{Constant, Dialect, Expr, FileId, FunctionBody, Item, SourceMap, Stmt};
use redscript_formatter::{FormatSettings, format_document, parse_document, print_document};

#[test]
fn formats_redmod_constructs() {
    let src = "class A {\n    private override function F() -> Void {\n        for (i = 0; i < 3; i += 1) {\n            var x : Int32 = 1;\n        }\n        for (var j : Int32 = 0; ; ) {\n        }\n    }\n}\n";
    let settings = FormatSettings {
        dialect: Dialect::Redmod,
        ..FormatSettings::default()
    };
    let map = SourceMap::new();
    let id = map.push_back("test.script", src);
    assert_eq!(map.get(id).unwrap().dialect(), Dialect::Redmod);
    let (module, errors) = format_document(src, id, &settings);
    assert!(errors.is_empty(), "{errors:?}");
    let out = module.unwrap().to_string();
    assert!(out.contains("override "), "{out}");
    assert!(out.contains("for (i = 0; i < 3; i += 1) {"), "{out}");
    assert!(out.contains("for (let j: Int32 = 0;;) {"), "{out}");
}

#[test]
fn qualifiers_follow_the_game_order() {
    let src = "class A {
    public const virtual function F() -> Void;
    protected final const override function G() -> Void;
    public static final import function H() -> Void;
}
abstract final class B {}
importonly abstract final class C {}
";
    let settings = FormatSettings {
        dialect: Dialect::Redmod,
        ..FormatSettings::default()
    };
    let (module, errors) = format_document(src, file_id(), &settings);
    assert!(errors.is_empty(), "{errors:?}");
    let out = module.unwrap().to_string();
    assert!(
        out.contains("public const virtual function F() -> Void;"),
        "{out}"
    );
    assert!(
        out.contains("protected const override final function G() -> Void;"),
        "{out}"
    );
    assert!(
        out.contains("public import static final function H() -> Void;"),
        "{out}"
    );
    assert!(out.contains("final abstract class B"), "{out}");
    assert!(out.contains("importonly final abstract class C"), "{out}");
}

const COMMENT_FIXTURES: &[&str] = &[include_str!("data/redmod-constructs.script")];

fn comments_of(source: &str) -> Vec<&str> {
    let mut comments = vec![];
    let mut rest = source;
    while let Some(start) = rest.find("//").into_iter().chain(rest.find("/*")).min() {
        let tail = &rest[start..];
        let len = if tail.starts_with("//") {
            tail.find('\n').unwrap_or(tail.len())
        } else {
            tail.find("*/").map_or(tail.len(), |i| i + 2)
        };
        comments.push(tail[..len].trim_end());
        rest = &tail[len..];
    }
    comments
}

#[test]
fn redmod_round_trip() {
    let settings = FormatSettings {
        dialect: Dialect::Redmod,
        ..FormatSettings::default()
    };
    for src in COMMENT_FIXTURES {
        let id = file_id();
        // declared first so that it outlives both documents (`Document` is invariant)
        #[allow(clippy::needless_late_init)]
        let printed: String;
        let (doc, errors) = parse_document(src, id, Dialect::Redmod);
        assert!(errors.is_empty(), "{errors:?}");
        let doc = doc.unwrap();
        printed = print_document(&doc, &settings).to_string();

        let (again, errors) = parse_document(&printed, id, Dialect::Redmod);
        assert!(errors.is_empty(), "{errors:?}\n{printed}");
        let again = again.unwrap();
        assert_eq!(
            doc.module.clone().unwrapped(),
            again.module.clone().unwrapped(),
            "{printed}"
        );

        // printing is stable
        let reprinted = print_document(&again, &settings).to_string();
        assert_eq!(printed, reprinted);

        for comment in comments_of(src) {
            let normalized = comment.strip_prefix("/*").map_or_else(
                || comment.to_owned(),
                |c| c.strip_suffix("*/").unwrap_or(c).trim().to_owned(),
            );
            assert!(
                printed.contains(&normalized),
                "comment {comment:?} is missing from:\n{printed}"
            );
        }
    }
}

#[test]
fn community_to_redmod_smoke() {
    let src = include_str!("data/control-flow.reds");
    let with_comments = format!("// top comment\n{src}\n// bottom comment\n");
    for src in [src, &with_comments] {
        let id = file_id();
        let (doc, errors) = parse_document(src, id, Dialect::Community);
        assert!(errors.is_empty(), "{errors:?}");
        let doc = doc.unwrap();
        let settings = FormatSettings {
            dialect: Dialect::Redmod,
            ..FormatSettings::default()
        };
        let printed = print_document(&doc, &settings).to_string();
        assert!(printed.contains("function "), "{printed}");
        assert!(!printed.contains("func "), "{printed}");
        for comment in comments_of(src) {
            assert!(
                printed.contains(comment),
                "{comment:?} missing from:\n{printed}"
            );
        }
    }
    let src = include_str!("data/operators.reds");
    let (doc, errors) = parse_document(src, file_id(), Dialect::Community);
    assert!(errors.is_empty(), "{errors:?}");
    let settings = FormatSettings {
        dialect: Dialect::Redmod,
        ..FormatSettings::default()
    };
    print_document(&doc.unwrap(), &settings).to_string();
}

#[test]
fn comments_follow_spans_through_rewrites() {
    let src = "class A {\n  function F() {\n    // keep me\n    let x : Int32 = 1;\n    return;\n  }\n}\n";
    let id = file_id();
    let (doc, errors) = parse_document(src, id, Dialect::Redmod);
    assert!(errors.is_empty(), "{errors:?}");
    let mut doc = doc.unwrap();

    let item = &mut doc.module.items[0].0;
    let Item::Class(class) = &mut item.item else {
        panic!("expected class");
    };
    let Item::Function(func) = &mut class.items[0].0.item else {
        panic!("expected function");
    };
    let Some(FunctionBody::Block(block)) = &mut func.body else {
        panic!("expected block");
    };
    // rebuild the first statement with a new value but the same span
    let (stmt, span) = block.stmts[0].clone();
    let Stmt::Let { name, typ, .. } = stmt else {
        panic!("expected let");
    };
    let rebuilt = Stmt::Let {
        name,
        typ,
        value: Some(Box::new((Expr::Constant(Constant::I32(42)), span))),
    };
    block.stmts[0] = (rebuilt, span);

    let printed = print_document(&doc, &FormatSettings::default()).to_string();
    assert!(printed.contains("// keep me"), "{printed}");
    assert_eq!(printed.matches("// keep me").count(), 1, "{printed}");
    assert!(printed.contains("= 42;"), "{printed}");
}

#[test]
fn header_comments_stay_above_module() {
    let cases = [
        (
            "// license header
/* block */
module A.B

// item comment
func F() {}
",
            Dialect::Community,
        ),
        (
            "// license header
/* block */
@module \"A.B\"

// item comment
function F() {}
",
            Dialect::Redmod,
        ),
    ];
    for (src, dialect) in cases {
        let (doc, errors) = parse_document(src, file_id(), dialect);
        assert!(errors.is_empty(), "{errors:?}");
        let doc = doc.unwrap();
        for target in [Dialect::Community, Dialect::Redmod] {
            let settings = FormatSettings {
                dialect: target,
                ..FormatSettings::default()
            };
            let printed = print_document(&doc, &settings).to_string();
            let header = printed.find("// license header").unwrap();
            let block = printed.find("/* block */").unwrap();
            let module = printed.find("module").unwrap();
            let item = printed.find("// item comment").unwrap();
            assert!(
                header < block && block < module && module < item,
                "{printed}"
            );
        }
    }
}

fn file_id() -> FileId {
    SourceMap::new().push_back("test.script", "")
}

#[test]
fn nested_assignments_keep_their_parentheses() {
    let src = "func F() {\n  let x = (a = b) + 1;\n  let y = 1 + (a = b);\n  let z = (a = b).c;\n  let w = -(a = b);\n  let v = (a = b) ? 1 : 2;\n  let u = (a = b) as C;\n  G(a = b);\n  a = b;\n}\n";
    for dialect in [Dialect::Community, Dialect::Redmod] {
        let settings = FormatSettings {
            dialect,
            ..FormatSettings::default()
        };
        let (document, errors) = parse_document(src, file_id(), Dialect::Community);
        assert!(errors.is_empty(), "{errors:?}");
        let mut document = document.unwrap();
        document.module.dialect = dialect;
        let out = print_document(&document, &settings).to_string();
        for expected in [
            "(a = b) + 1;",
            "1 + (a = b);",
            "(a = b).c;",
            "-(a = b);",
            "(a = b) ? 1 : 2;",
            "(a = b) as C;",
            "G(a = b);",
            "a = b;
}",
        ] {
            assert!(out.contains(expected), "{expected} in {out}");
        }
    }
    // the community output parses back to the same tree
    let settings = FormatSettings::default();
    let (module, errors) = format_document(src, file_id(), &settings);
    assert!(errors.is_empty(), "{errors:?}");
    let out = module.unwrap().to_string();
    let (again, errors) = format_document(&out, file_id(), &settings);
    assert!(errors.is_empty(), "{errors:?}");
    assert_eq!(again.unwrap().to_string(), out);
}

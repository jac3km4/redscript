use redscript_ast::{Dialect, SourceMap};
use redscript_formatter::{FormatSettings, parse_document, print_document};

const FIXTURES: &[(&str, Dialect)] = &[
    (
        include_str!("data/trailing-comments.reds"),
        Dialect::Community,
    ),
    (
        include_str!("data/trailing-comments.script"),
        Dialect::Redmod,
    ),
];

/// Comments that follow an enum variant or the closing brace of an enum are hoisted before the
/// next item, since variants are not nodes.
/// A comment after a `for` initializer is rewritten as a block comment, since the rest of the
/// header follows on the same line.
const HOISTED: &[&str] = &[
    "// enum open",
    "// red",
    "// green",
    "// enum close",
    "// init comment",
];

fn comments_of(source: &str) -> Vec<String> {
    let mut comments = vec![];
    let mut rest = source;
    while let Some(start) = rest.find("//").into_iter().chain(rest.find("/*")).min() {
        let tail = &rest[start..];
        let (len, text) = if let Some(line) = tail.strip_prefix("//") {
            let len = tail.find('\n').unwrap_or(tail.len());
            (len, line[..len - 2].trim().to_owned())
        } else {
            let len = tail.find("*/").map_or(tail.len(), |i| i + 2);
            (len, tail[2..len - 2].trim().to_owned())
        };
        comments.push(text);
        rest = &tail[len..];
    }
    comments.sort();
    comments
}

fn format(source: &str, dialect: Dialect) -> String {
    let map = SourceMap::new();
    let id = map.push_back(
        if dialect == Dialect::Redmod {
            "test.script"
        } else {
            "test.reds"
        },
        source,
    );
    let settings = FormatSettings {
        dialect,
        ..FormatSettings::default()
    };
    let (doc, errors) = parse_document(source, id, dialect);
    assert!(errors.is_empty(), "{errors:?}");
    print_document(&doc.unwrap(), &settings).to_string()
}

#[test]
fn trailing_comments_round_trip() {
    for &(src, dialect) in FIXTURES {
        let printed = format(src, dialect);
        if printed.contains('\r') {
            eprintln!("Input contains CRLF: {}", src.contains("\r\n"));
            eprintln!("Output contains CRLF: {}", printed.contains("\r\n"));
            eprintln!("Input:  {src:?}");
            eprintln!("Output: {printed:?}");
        }
        assert!(!printed.contains('\r'));

        // each comment is printed exactly once
        assert_eq!(comments_of(src), comments_of(&printed), "{printed}");

        // trailing comments stay on the line of their statement
        for line in src.lines() {
            let Some((code, comment)) = line.split_once("//") else {
                continue;
            };
            let code = code.trim();
            let comment = format!("//{comment}");
            if !(code.ends_with(';') || code.ends_with('}') || code.ends_with("*/"))
                || HOISTED.contains(&comment.as_str())
            {
                continue;
            }
            assert!(
                printed.lines().any(|l| {
                    l.ends_with(comment.trim_end()) && !l.trim_start().starts_with("//")
                }),
                "trailing {comment:?} left its line in:\n{printed}"
            );
        }
    }
}

#[test]
fn trailing_comments_are_idempotent() {
    for &(src, dialect) in FIXTURES {
        let once = format(src, dialect);
        let twice = format(&once, dialect);
        assert_eq!(once, twice);
    }
}

#[test]
fn block_comment_mid_line_is_kept_once() {
    let printed = format(
        "func A() {\n  let a = b /* x */ + c;\n}\n",
        Dialect::Community,
    );
    assert_eq!(printed.matches("/* x */").count(), 1, "{printed}");
}

#[test]
fn for_init_comment_with_block_terminator() {
    let cases = [
        (
            "function A() {\n  for (j = 0; // c */ x\n    j < 3; j += 1) {\n  }\n}\n",
            Dialect::Redmod,
        ),
        (
            "function A() {\n  for (j = 0; // a /* b */ c\n    j < 3; j += 1) {\n  }\n}\n",
            Dialect::Redmod,
        ),
    ];
    for (src, dialect) in cases {
        let printed = format(src, dialect);
        // re-parses (checked by `format`) and is stable
        assert_eq!(printed, format(&printed, dialect));
        assert_eq!(printed.matches("/*").count(), 1, "{printed}");
        assert_eq!(printed.matches("*/").count(), 1, "{printed}");
    }
}

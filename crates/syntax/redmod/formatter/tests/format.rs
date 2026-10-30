use redscript_community_ast::FileId;

fn file() -> FileId {
    FileId::from_i32(7)
}

#[test]
fn native_indentation_preserves_comments_literals_and_override() {
    let source = "class Test {\r\n// comment {\r\noverride function Run() {\r\nvar text: String = \"first\n  second\\\"; // }\r\nUse(\"assets\\widgets\", \"\\u{22}\");\r\n/* block\n unmodified */\r\n}\r\n}\r\n";
    let expected = "class Test {\r\n    // comment {\r\n    override function Run() {\r\n        var text: String = \"first\n  second\\\"; // }\r\n        Use(\"assets\\widgets\", \"\\u{22}\");\r\n        /* block\n unmodified */\r\n    }\r\n}\r\n";
    let formatted = redscript_redmod_formatter::format_source(source, file()).unwrap();
    assert_eq!(formatted, expected);
    // Snapshot LF for readability; the equality above checks exact mixed endings.
    insta::assert_snapshot!(formatted.replace("\r\n", "\n"));
    assert_eq!(
        redscript_redmod_formatter::format_source(&formatted, file()).unwrap(),
        formatted
    );
}

#[test]
fn native_indentation_preserves_body_and_trailing_doc_comments() {
    let source = "/// host\nclass Host {\n/// method\nfunction Run() {\n/// Explain next call\nUse(/// argument\n1);\n/// trailing body\n}\n/// trailing class\n}\n/// trailing file";
    let expected = "/// host\nclass Host {\n    /// method\n    function Run() {\n        /// Explain next call\n        Use(/// argument\n            1);\n        /// trailing body\n    }\n    /// trailing class\n}\n/// trailing file";
    for (source, expected) in [
        (source.to_owned(), expected.to_owned()),
        (source.replace('\n', "\r\n"), expected.replace('\n', "\r\n")),
        (
            "/// only trailing docs".to_owned(),
            "/// only trailing docs".to_owned(),
        ),
    ] {
        let formatted = redscript_redmod_formatter::format_source(&source, file()).unwrap();
        assert_eq!(formatted, expected);
        assert_eq!(
            redscript_redmod_formatter::format_source(&formatted, file()).unwrap(),
            formatted
        );
    }
}

#[test]
fn indentation_accepts_native_syntax_without_semantic_projection() {
    let source = "[meta = true]\npublic override class C {\ndefault x = 1;\nhint x = \"text\";\noperator +(skip x, y: mixedRef<C>) -> const C;\nfunction F() {\nconst var a, b: Int32 = 1;\ndo {\nUse(, 1,);\n} while (false);\nswitch (a) { default: break; case 1: break; default: break; }\n}\n}\n";
    let formatted = redscript_redmod_formatter::format_source(source, file()).unwrap();
    assert!(formatted.contains("        const var a, b: Int32 = 1;"));
    assert_eq!(
        redscript_redmod_formatter::format_source(&formatted, file()).unwrap(),
        formatted
    );
    assert!(
        redscript_redmod_parser::parse_module(&formatted, file())
            .1
            .is_empty()
    );
}

#[test]
fn indentation_preserves_rebased_literal_and_comment_spellings() {
    let source = "class C {\r\n// comment\r\nfunction F() {\r\nUse(-2147483648, -9223372036854775808l, 1e001f, .5E-01, 1E+001d);\r\nUse(\"\\\\u{XYZ}\\\\n\\\\\", 'raw\\\\name\\\\');\r\n}\r\n}\r\n";
    let output = redscript_redmod_formatter::format_source(source, file()).unwrap();
    assert_eq!(
        output,
        source
            .replace("// comment", "    // comment")
            .replace("function F()", "    function F()")
            .replace("Use(", "        Use(")
            .replacen("}\r\n", "    }\r\n", 1)
    );
    assert_eq!(
        redscript_redmod_formatter::format_source(&output, file()).unwrap(),
        output
    );
}

#[test]
fn invalid_source_returns_parser_diagnostics() {
    let sources = redscript_community_ast::SourceMap::new();
    let source = "function Broken() {\n#\n}\n";
    let file = sources.push_back("invalid.script", source);
    let errors = redscript_redmod_formatter::format_source(source, file).unwrap_err();
    assert_eq!(
        errors,
        redscript_redmod_parser::parse_module(source, file).1
    );
    let diagnostics: String = errors
        .iter()
        .map(|error| error.display(&sources).unwrap().to_string())
        .collect();
    insta::assert_snapshot!(diagnostics);
}

#[test]
fn switch_bodies_and_nested_labels_are_indented() {
    let expected = "function F() {\n    switch (x) {\n        case true ? 1 : 2:\n        case 3:\n            // body\n            if (true) {\n                Use();\n            }\n            switch (y) {\n                default:\n                    break;\n            }\n            break;\n        default:\n            /* trailing body */\n    }\n}\n";
    assert_indentation(expected);
}

#[test]
fn existing_multiline_lists_and_chains_use_continuation_indents() {
    let expected = "class C {\n    function F(\n        x: Int32,\n        y: Int32\n    ) {\n        Use(\n            x,\n            Nested(\n                y\n            )\n        );\n        Use([\n                1,\n                2\n        ]);\n        this\n            .First()\n            .Second();\n    }\n}\n";
    assert_indentation(expected);
}

#[test]
fn multiline_chained_calls_keep_their_continuation_base() {
    assert_indentation(
        "function F() {\n    this\n        .First(\n            x\n        )\n        .Second();\n}\n",
    );
}

#[test]
fn compact_empty_switch_labels_do_not_underflow() {
    assert_indentation(
        "function F(){switch(x){case 0:case 0:case 0:switch(x){case 0:switch(x){case 0:case 0:case 0:case 0:case 0:}case 0:switch(x){case 0:case 0:case 0:}case 0:case 0:case 0:}case 0:}\n}\n",
    );
}

fn assert_indentation(expected: &str) {
    for expected in [expected.to_owned(), expected.replace('\n', "\r\n")] {
        let source = expected
            .split_inclusive('\n')
            .map(|line| line.trim_start_matches(' '))
            .collect::<String>();
        let formatted = redscript_redmod_formatter::format_source(&source, file()).unwrap();
        assert_eq!(formatted, expected);
        assert_eq!(
            redscript_redmod_formatter::format_source(&formatted, file()).unwrap(),
            formatted
        );
        let tokens = |source: &str| {
            let (tokens, errors) =
                redscript_redmod_parser::lex_with_lf_and_comments(source, file());
            assert!(errors.is_empty());
            tokens
                .unwrap()
                .into_iter()
                .filter(|(token, _)| *token != redscript_redmod_parser::Token::Whitespace)
                .map(|(token, _)| token.to_string())
                .collect::<Vec<_>>()
        };
        assert_eq!(tokens(&source), tokens(&formatted));
    }
}

#[test]
fn indentation_is_idempotent_for_trivia_and_line_endings() {
    for source in [
        "",
        "// only trivia {\n\t\n",
        "function Run() {\n\tif (true) {\nCall();\n\t}\n}",
        "function Run() {\r\n\tCall();\r\n}\r\n",
        "/* multiline\n untouched {\n */\nfunction Run() {}",
    ] {
        let formatted = redscript_redmod_formatter::format_source(source, file()).unwrap();
        assert_eq!(
            redscript_redmod_formatter::format_source(&formatted, file()).unwrap(),
            formatted
        );
        let tokens = |source: &str| {
            let (tokens, errors) =
                redscript_redmod_parser::lex_with_lf_and_comments(source, file());
            assert!(errors.is_empty());
            tokens
                .unwrap()
                .into_iter()
                .filter(|(token, _)| *token != redscript_redmod_parser::Token::Whitespace)
                .map(|(token, _)| token.to_string())
                .collect::<Vec<_>>()
        };
        assert_eq!(tokens(source), tokens(&formatted));
    }
}

use redscript_ast::SourceMap;
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

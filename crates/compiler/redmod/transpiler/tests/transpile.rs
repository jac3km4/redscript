use std::collections::BTreeMap;

use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, LoweredCompilationUnit, Symbols, TypeInterner, parse_files,
    process_sources,
};
use redscript_redmod_transpiler::{
    DeclarationPath, ResolvedDeclaration as R, TranspileModule, transpile,
};

fn analyze<'ctx>(
    sources: &'ctx SourceMap,
    interner: &'ctx TypeInterner,
) -> (
    Vec<ast::SourceModule<'ctx>>,
    LoweredCompilationUnit<'ctx>,
    Symbols<'ctx>,
) {
    let selected = sources.files().map(|(id, _)| id).collect::<Vec<_>>();
    sources.populate_boot_lib();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(sources, &mut reporter);
    let (unit, symbols) = process_sources(
        modules.clone(),
        Symbols::with_default_types(),
        Evaluator::from_modules(&modules),
        &mut reporter,
        interner,
    );
    let diagnostics = reporter.into_reported();
    assert!(diagnostics.iter().all(|d| !d.is_error()), "{diagnostics:?}");
    let modules = modules
        .into_iter()
        .filter(|m| {
            m.items
                .first()
                .is_some_and(|(_, span)| selected.contains(&span.file))
        })
        .collect();
    (modules, unit, symbols)
}

// Authored fixtures have unique declarations. Tests associate by names/structure,
// never by source span. Real callers can bind overloads during resolution.
fn bindings<'ctx>(
    module: &ast::SourceModule<'ctx>,
    symbols: &Symbols<'ctx>,
) -> BTreeMap<DeclarationPath, R<'ctx>> {
    let mut result = BTreeMap::new();
    let prefix = module
        .path
        .as_ref()
        .map(|p| format!("{}.", p.segments.join(".")))
        .unwrap_or_default();
    for (i, (decl, _)) in module.items.iter().enumerate() {
        match &decl.item {
            ast::Item::Function(function) => {
                let candidates: Vec<_> = symbols
                    .free_functions()
                    .filter(|e| e.name().to_string() == format!("{prefix}{}", function.name.0))
                    .collect();
                if let Some((annotation, _)) = decl
                    .annotations
                    .iter()
                    .find(|(a, _)| a.name == "wrapMethod")
                {
                    let [(ast::Expr::Ident(parent), _)] = &annotation.args[..] else {
                        panic!("fixture parent")
                    };
                    let (id, def) = symbols
                        .types()
                        .find(|(id, _)| id.as_str() == *parent)
                        .unwrap();
                    let method = def
                        .schema()
                        .as_aggregate()
                        .unwrap()
                        .methods()
                        .iter()
                        .find(|m| **m.name() == function.name.0)
                        .unwrap();
                    result.insert(
                        DeclarationPath(vec![i]),
                        R::Wrapper(
                            redscript_compiler_frontend::MethodId::new(id, *method.key()),
                            0,
                        ),
                    );
                    continue;
                }
                // Fixture native operator overloads are selected by parameter count/types.
                let entry = candidates
                    .iter()
                    .find(|e| {
                        e.func()
                            .type_()
                            .params()
                            .iter()
                            .zip(function.params.iter())
                            .all(|(p, (s, _))| {
                                s.typ.as_ref().is_none_or(|(t, _)| match t {
                                    ast::Type::Named { name, .. } => p.type_().to_string() == *name,
                                    _ => true,
                                })
                            })
                    })
                    .unwrap();
                result.insert(DeclarationPath(vec![i]), R::Function(*entry.key()));
            }
            ast::Item::Class(class) | ast::Item::Struct(class) => {
                let (id, def) = symbols
                    .types()
                    .find(|(id, _)| id.as_str() == format!("{prefix}{}", class.name.0))
                    .unwrap();
                result.insert(DeclarationPath(vec![i]), R::Type(id));
                let agg = def.schema().as_aggregate().unwrap();
                for (j, (decl, _)) in class.items.iter().enumerate() {
                    match &decl.item {
                        ast::Item::Function(f) => {
                            let method = agg
                                .methods()
                                .iter()
                                .find(|m| **m.name() == f.name.0)
                                .unwrap();
                            result.insert(
                                DeclarationPath(vec![i, j]),
                                R::Method(redscript_compiler_frontend::MethodId::new(
                                    id,
                                    *method.key(),
                                )),
                            );
                        }
                        ast::Item::Let(f) => {
                            let field = agg.fields().iter().find(|m| m.name() == f.name.0).unwrap();
                            result.insert(
                                DeclarationPath(vec![i, j]),
                                R::Field(redscript_compiler_frontend::FieldId::new(
                                    id,
                                    field.index(),
                                )),
                            );
                        }
                        _ => panic!("unsupported fixture member"),
                    }
                }
            }
            ast::Item::Enum(enm) => {
                let (id, _) = symbols
                    .types()
                    .find(|(id, _)| id.as_str() == format!("{prefix}{}", enm.name.0))
                    .unwrap();
                result.insert(DeclarationPath(vec![i]), R::Type(id));
            }
            ast::Item::Import(_) => {}
            _ => panic!("unsupported fixture declaration"),
        }
    }
    result
}

fn convert(source: &str) -> String {
    convert_mapped(source).text
}

fn convert_mapped(source: &str) -> redscript_redmod_transpiler::TranspiledModule {
    let sources = SourceMap::new();
    sources.push_back("authored.reds", source);
    let interner = TypeInterner::default();
    let (modules, unit, symbols) = analyze(&sources, &interner);
    let module = &modules[0];
    let request = TranspileModule {
        source: module,
        bindings: bindings(module, &symbols),
    };
    let result = transpile(&[request], &unit, &symbols).unwrap();
    assert!(!result[0].mappings.is_empty());
    for mapping in &result[0].mappings {
        assert!(result[0].text.get(mapping.generated.clone()).is_some());
    }
    result.into_iter().next().unwrap()
}

#[test]
fn fresh_array_declarations_construct_in_place_and_assignments_keep_copies() {
    let output = convert(
        "func Tick() -> Int32 { return 1; } \
         func Test(again: Bool) { let values = [Tick(), Tick()]; \
         while again { let empty: [Int32] = []; ArrayPush(empty, 1); \
         let values = [values[0]]; } values = [values[0], Tick()]; }",
    );
    for expected in [
        "values.Resize(2);\n    values[0] = Tick();\n    values[1] = Tick();",
        "while (again) {\n        emptyLiteral.Clear();",
        "values_2Literal.Resize(1);\n        values_2Literal[0] = values[0];\n        values_2 = values_2Literal;",
        "valuesLiteral.Resize(2);\n    valuesLiteral[0] = values[0];",
        "values = valuesLiteral;",
    ] {
        assert!(output.contains(expected), "{expected}: {output}");
    }
    assert!(
        output.contains("emptyLiteral.Clear();\n        empty = emptyLiteral;"),
        "{output}"
    );
    let sources = SourceMap::new();
    let file = sources.push_back("direct.script", output.clone());
    let (parsed, errors) = redscript_redmod_parser::parse_module(&output, file);
    assert!(parsed.is_some() && errors.is_empty(), "{errors:?}");
}

#[test]
fn contextual_temporary_names_reserve_authored_names_and_shadowed_owners() {
    let output = convert(
        "native func OperatorAssignAdd(out a: Int32, b: Int32) -> Int32; \
         native func OperatorLess(a: Int32, b: Int32) -> Bool; \
         func Items() -> [Int32] { return [1]; } \
         func Test() { let firstLiteral = 9; let first: [Int32]; first = [1]; \
         for item in Items() { for item in Items() {} } }",
    );
    for expected in [
        "var firstLiteral: Int32 = 9;",
        "var firstLiteral_2: array<Int32>;",
        "var itemIndex: Int32 = 0;",
        "var item_2Index: Int32;",
        "var itemIterable: array<Int32> = Items();",
        "item_2Iterable = Items();",
    ] {
        assert!(output.contains(expected), "{expected}: {output}");
    }
}

#[test]
fn generated_guards_simplify_only_verified_boolean_negation() {
    let output = convert(
        "native func OperatorLogicNot(value: Bool) -> Bool; \
         native func OperatorLess(a: Float, b: Float) -> Bool; \
         class Widget {} \
         func Check(values: [Int32]) -> Bool { return true; } \
         func Get(values: [Int32]) -> ref<Widget> { return null; } \
         func Number(values: [Int32]) -> Float { return 0.0; } \
         func Test(x: Float) { \
         while Check([1]) == true {} \
         while Check([2]) != false {} \
         while !Check([3]) {} \
         while IsDefined(Get([4])) {} \
         while Number([5]) < x {} }",
    );
    for expected in [
        "if (Check(arrayLiteral) != true)",
        "if (Check(arrayLiteral_2) == false)",
        "if (Check(arrayLiteral_3))",
        "if (Get(arrayLiteral_4) == NULL)",
        "if (!(Number(arrayLiteral_5) < x))",
    ] {
        assert!(output.contains(expected), "{expected}: {output}");
    }
}

#[test]
fn target_control_flow_and_parameter_keywords_are_renamed() {
    let output = convert("func Test(skip: Int32) -> Int32 { let do = skip; return do; }");
    assert!(output.contains("function Test(skip_: Int32)"), "{output}");
    assert!(output.contains("var do_: Int32 = skip_;"), "{output}");
    assert!(output.contains("return do_;"), "{output}");
}

#[test]
fn reserved_authored_names_keep_hygienic_recognizable_stems() {
    let output = convert(
        "func Test(name: Int32) -> Int32 { let name_ = 2; \
         if true { let name = name_; } else { let name = 3; } return name; }",
    );
    assert!(output.contains("function Test(name__2: Int32)"), "{output}");
    assert!(output.contains("var name_: Int32 = 2;"), "{output}");
    assert!(output.contains("var name__3: Int32;"), "{output}");
    assert!(output.contains("var name__4: Int32;"), "{output}");
    assert!(output.contains("return name__2;"), "{output}");
    assert!(!output.contains("__rst_"), "{output}");
}

#[test]
fn numeric_defaults_and_typed_literals_avoid_redundant_syntax() {
    let output = convert(
        "func Test() { let zero: Int32; let small: Int16 = 3; \
         let negative: Int8 = -2; let wide: Int64 = 4; \
         let unsigned: Uint32 = 5u; let precise: Double = 1.5d; \
         let minimum: Int32 = -2147483648; }",
    );
    assert!(output.contains("var zero: Int32 = 0;"), "{output}");
    assert!(
        output.contains("var small: Int16 = Cast<Int16>(3);"),
        "{output}"
    );
    assert!(
        output.contains("var negative: Int8 = Cast<Int8>(-2);"),
        "{output}"
    );
    assert!(
        output.contains("var wide: Int64 = Cast<Int64>(4);"),
        "{output}"
    );
    assert!(
        output.contains("var unsigned: Uint32 = Cast<Uint32>(5);"),
        "{output}"
    );
    assert!(
        output.contains("var precise: Double = Cast<Double>(1.5);"),
        "{output}"
    );
    assert!(
        output.contains("var minimum: Int32 = (-2147483647 - 1);"),
        "{output}"
    );
    assert!(!output.contains("Cast<Int32>(0)"), "{output}");
}

#[test]
fn declarations_are_spaced_and_members_and_mappings_are_indented() {
    let output = convert_mapped(
        "class Widget { let first: Int32; let second: Int32; \
         /// Read the field.\n public func Read() -> Int32 { return this.first; } \
         public func Reset() { this.first = 0; } } func Run() {}",
    );
    assert_eq!(
        output.text,
        "class Widget extends IScriptable {\n\
         \x20   var first: Int32;\n\
         \x20   var second: Int32;\n\n\
         \x20   /// Read the field.\n\
         \x20   public function Read() -> Int32 {\n\
         \x20       return this.first;\n\
         \x20   }\n\n\
         \x20   public function Reset() -> Void {\n\
         \x20       this.first = 0;\n\
         \x20   }\n\
         }\n\n\
         function Run() -> Void {\n\
         }\n",
    );
    assert!(output.mappings.iter().any(|mapping| {
        &output.text[mapping.generated.clone()] == "        return this.first;\n"
    }));
}

#[test]
fn value_assignments_are_exported_as_nested_expressions_with_source_mappings() {
    let source = "func Chain(out a: Int32, out b: Int32) -> Int32 { return a = b = 7; }";
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("assignment.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    let module = modules
        .iter()
        .find(|module| module.span().is_some_and(|span| span.file == file))
        .unwrap()
        .clone();
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, symbols) = redscript_compiler_frontend::process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let request = TranspileModule {
        source: &module,
        bindings: bindings(&module, &symbols),
    };
    let output = transpile(&[request], &unit, &symbols).unwrap();
    assert!(
        output[0].text.contains("return a = b = 7;"),
        "{}",
        output[0].text
    );
    assert!(
        output[0]
            .mappings
            .iter()
            .any(|mapping| mapping.source.file == file)
    );
    let (parsed, errors) = redscript_redmod_parser::parse_module(&output[0].text, file);
    assert!(parsed.is_some() && errors.is_empty(), "{errors:?}");
}

#[test]
fn explicit_enum_storage_is_exported_without_narrowing() {
    let output = convert(
        "enum U: Uint32 { Max = 4294967295l } enum L: Int64 { Max = 9223372036854775807l }",
    );
    assert!(output.contains("enum U: Uint32 {"), "{output}");
    assert!(output.contains("Max = 4294967295,"));
    assert!(output.contains("enum L: Int64 {"));
    assert!(output.contains("Max = 9223372036854775807,"));
    let output = convert("enum Old { A, B }");
    assert!(output.contains("enum Old {"));
    assert!(!output.contains("enum Old:"));
}

#[test]
fn lazy_branches_and_loop_back_edges_keep_initialization_in_place() {
    let output = convert(
        r#"
native func OperatorLess(a: Int32, b: Int32) -> Bool
native func OperatorAssignAdd(out a: Int32, b: Int32) -> Int32
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
func Tick() -> Bool = true
func Check(values: [Int32]) -> Bool = true
func Test(enabled: Bool, values: [Int32]) -> Bool {
  let count = 0;
  while Check([count]) {
    let reset: Int32;
    count += 1;
    switch count {
      case 1: continue;
      default: break;
    }
    if count < 3 { continue; }
    break;
  }
  return enabled && (Tick() ? Check(values) : Tick());
}
"#,
    );

    insta::assert_snapshot!(output);
}

#[test]
fn loop_guards_preserve_prefixes_and_nested_control_flow() {
    let output = convert(
        r#"
func Check(values: [Int32]) -> Bool = true
func Tick() {}
func Nested(values: [Int32]) {
    while Check([1]) {
        while Check([2]) { continue; }
        switch 1 { case 1: continue; default: break; }
        if Check(values) { continue; }
        break;
    }
}
func WithTail(flag: Bool) {
    while true {
        if flag { Tick(); } else { break; }
        Tick();
    }
}
func SwitchBreak(flag: Bool) {
    while true {
        switch 1 { case 1: if flag { Tick(); } else { break; } }
        Tick();
    }
}
func ConditionalLoop(flag: Bool) {
    while flag { if flag { Tick(); } else { break; } }
}
func ElseEffects(flag: Bool) {
    while true { if flag { continue; } else { Tick(); break; } }
}
func MultipleBranches(flag: Bool) {
    while true { if flag { continue; } else if flag { Tick(); } else { break; } }
}
func GuardExpression(flag: Bool, values: [Int32]) {
    while true { if flag ? Check(values) : false { continue; } else { break; } }
}
"#,
    );
    assert!(output.contains("if (!(flag ? Check(values) : false))"));
    assert_eq!(output.matches(".Resize(1);").count(), 2);
    let sources = SourceMap::new();
    let file = sources.push_back("emitted.script", output.clone());
    let (module, errors) = redscript_redmod_parser::parse_module(&output, file);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(module.is_some());
    insta::assert_snapshot!(output);
}

#[test]
fn flattened_loop_guards_keep_break_and_body_source_mappings() {
    use redscript_redmod_transpiler::package::source_at;

    let source = "func Test(flag: Bool) { while true { if flag { continue; } else { break; } } }";
    let output = convert_mapped(source);
    for token in ["break;", "continue;"] {
        let generated = output.text.find(token).unwrap();
        let mapped = source_at(&output.mappings, generated).unwrap();
        assert_eq!(mapped.start as usize, source.find(token).unwrap());
        assert_eq!(&source[mapped.start as usize..mapped.end as usize], token);
    }
}

#[test]
fn expression_printing_preserves_precedence_and_associativity() {
    let output = convert(
        r#"
native func OperatorAdd(a: Int32, b: Int32) -> Int32
native func OperatorAdd(a: String, b: String) -> String
native func Cast(a: Int32) -> Float
native func OperatorSubtract(a: Int32, b: Int32) -> Int32
native func OperatorMultiply(a: Int32, b: Int32) -> Int32
native func OperatorNeg(a: Int32) -> Int32
native func OperatorLogicNot(a: Bool) -> Bool
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func OperatorAssignAdd(out a: Int32, b: Int32) -> Int32
func Sum(a: Int32, b: Int32, c: Int32) -> Int32 = a + b * c
func Product(a: Int32, b: Int32, c: Int32) -> Int32 = (a + b) * c
func RightSum(a: Int32, b: Int32, c: Int32) -> Int32 = a + (b + c)
func Difference(a: Int32, b: Int32, c: Int32) -> Int32 = a - (b - c)
func LeftDifference(a: Int32, b: Int32, c: Int32) -> Int32 = (a - b) - c
func Negate(a: Int32, b: Int32) -> Int32 = -(a + b)
func DoubleNegate(a: Int32) -> Int32 = -(-a)
func Choose(a: Bool, b: Bool, c: Bool) -> Bool = a && (b ? c : a)
func NestedChoice(a: Bool, b: Bool, c: Bool) -> Bool = (a ? b : c) ? a : b
func NotChoice(a: Bool, b: Bool, c: Bool) -> Bool = !(a ? b : c)
func Assign(a: Int32, b: Int32, c: Int32) -> Int32 { a = b + c; return a; }
func Compound(a: Int32, b: Int32, c: Int32) -> Int32 { a += b += c; return a; }
func CastChoice(a: Bool, b: Int32, c: Int32) -> Float = Cast<Float>(a ? b : c)
class Base {}
class Child extends Base {}
func CastReceiver(a: Bool, b: Base, c: Base) -> Child = (a ? b : c) as Child
func Defined(a: Base, flag: Bool) -> Bool = IsDefined(a) && flag
func Same(a: Base, b: Base, flag: Bool) -> Bool = Equals(a, b) && flag
func StringIdentity(a: Bool, b: String, c: String) -> String = ToString(a ? b : c) + "!"
"#,
    );
    for expression in [
        "a + b * c",
        "(a + b) * c",
        "a + (b + c)",
        "a - (b - c)",
        "a - b - c",
        "-(a + b)",
        "-(-a)",
        "a && (b ? c : a)",
        "(a ? b : c) ? a : b",
        "!(a ? b : c)",
        "a = b + c",
        "a += b += c",
        "Cast<Float>(a ? b : c)",
        "(a ? b : c) as Child",
        "a != NULL && flag",
        "a == b && flag",
        "(a ? b : c) + \"!\"",
    ] {
        assert!(output.contains(&format!("{expression};")), "{output}");
    }
    let sources = SourceMap::new();
    let file = sources.push_back("emitted.script", output.clone());
    let (module, errors) = redscript_redmod_parser::parse_module(&output, file);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(module.is_some());
    insta::assert_snapshot!(output);
}

#[test]
fn declaration_metadata_and_reference_kinds_are_retained() {
    let output = convert(
        r#"
module Example
import Example.*
class Widget {
  public let weak: wref<Widget>;
  public func Identity(value: Widget) -> Widget = value
}
@if(ModuleExists("Example"))
func Conditional() -> String = "present"
"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn function_profile_and_const_return_are_retained() {
    let output = convert(
        r#"@profile("Authored label")
func Get() -> const Int32 = 7;
class Host {
    @profile("Member label")
    public func Get() -> const Int32 = 8;
}"#,
    );
    assert!(
        output.contains(r#"[profile = "Authored label"]"#),
        "{output}"
    );
    assert!(output.contains(r#"[profile = "Member label"]"#), "{output}");
    assert_eq!(output.matches("-> const Int32").count(), 2, "{output}");
}

#[test]
fn unhandled_qualifiers_are_rejected_instead_of_silently_dropped() {
    for qualifier in [
        "editable",
        "instanceeditable",
        "inlined",
        "replicated",
        "testonly",
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "qualifiers.reds",
            format!("class Widget {{ {qualifier} let value: Int32; }}"),
        );
        let interner = TypeInterner::default();
        let (modules, unit, symbols) = analyze(&sources, &interner);
        let diagnostics = transpile(
            &[TranspileModule {
                source: &modules[0],
                bindings: bindings(&modules[0], &symbols),
            }],
            &unit,
            &symbols,
        )
        .unwrap_err();
        assert_eq!(diagnostics.len(), 1, "{qualifier}: {diagnostics:?}");
        let span = diagnostics[0].span.unwrap();
        let source = sources.get(span.file).unwrap().source();
        assert_eq!(
            &source[span.start as usize..span.end as usize],
            format!("{qualifier} let value: Int32;")
        );
        if qualifier == "editable" {
            insta::assert_snapshot!(diagnostics[0].display(&sources).to_string());
        }
    }
}

#[test]
fn typed_lowering_golden() {
    let text = convert(include_str!("data/typed.reds"));

    insta::assert_snapshot!(text);
    // This tests only the local parser's syntax acceptance, not official compatibility.
    let (_, errors) = redscript_redmod_parser::parse_module(
        &text,
        ast::SourceMap::new().push_back("generated.script", ""),
    );
    assert!(errors.is_empty(), "{errors:?}\n{text}");
}

#[test]
fn unsupported_intrinsic_reports_source_and_no_artifacts() {
    let sources = SourceMap::new();
    sources.push_back(
        "unsupported.reds",
        "func Test(a: [Int32]) { ArraySort(a); }",
    );
    let interner = TypeInterner::default();
    let (modules, unit, symbols) = analyze(&sources, &interner);
    let result = transpile(
        &[TranspileModule {
            source: &modules[0],
            bindings: bindings(&modules[0], &symbols),
        }],
        &unit,
        &symbols,
    )
    .unwrap_err();

    assert_eq!(result[0].declaration, Some(DeclarationPath(vec![0])));
    insta::assert_snapshot!(
        result
            .iter()
            .map(|d| d.display(&sources).to_string())
            .collect::<String>()
    );
}

#[test]
fn missing_identity_is_not_recovered_from_spans() {
    let sources = SourceMap::new();
    sources.push_back("identity.reds", "func A() {} func B() {}");
    let interner = TypeInterner::default();
    let (modules, unit, symbols) = analyze(&sources, &interner);
    let errors = transpile(
        &[TranspileModule {
            source: &modules[0],
            bindings: BTreeMap::new(),
        }],
        &unit,
        &symbols,
    )
    .unwrap_err();
    assert!(errors[0].message.contains("explicit declaration binding"));
    let valid = TranspileModule {
        source: &modules[0],
        bindings: bindings(&modules[0], &symbols),
    };
    let invalid = TranspileModule {
        source: &modules[0],
        bindings: BTreeMap::new(),
    };
    assert!(transpile(&[valid, invalid], &unit, &symbols).is_err());
}

#[test]
fn local_identity_survives_shadowing_and_reserved_names() {
    let output = convert(
        "func Test(name: Int32) -> Int32 { if true { let name = 1; } else { let name = 2; } return name; }",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn readable_names_reserve_authored_names_before_disambiguating() {
    let output = convert(
        r#"
func Test(value: Int32) -> Int32 {
    let value_2 = 2;
    let __rst_v3 = 3;
    if true { let value = 1; }
    let NULL = 4;
    let Test = value_2;
    return value;
}
"#,
    );
    assert!(output.contains("function Test(value: Int32)"));
    assert!(output.contains("var value_2: Int32 = 2;"));
    assert!(output.contains("var value_3: Int32;"));
    assert!(output.contains("var __rst_v3: Int32 = 3;"));
    assert!(output.contains("var Test_2: Int32 = value_2;"));
    insta::assert_snapshot!(output);
}

#[test]
fn temporary_names_describe_purpose_and_reserve_authored_names() {
    let output = convert(
        r#"
native func OperatorAssignAdd(out a: Int32, b: Int32) -> Int32
native func OperatorLess(a: Int32, b: Int32) -> Bool
func Items() -> [Int32] = [1]
func Test() {
    let first = [1];
    let second = [2];
    let arrayLiteral = 3;
    let arrayLiteral_2 = 4;
    let index = 5;
    let iterable = 6;
    for item in Items() { for other in first {} }
}
"#,
    );
    for declaration in [
        "var first: array<Int32>;",
        "var second: array<Int32>;",
        "var itemIndex: Int32 = 0;",
        "var otherIndex: Int32;",
        "var itemIterable: array<Int32> = Items();",
        "var item: Int32;",
        "var other: Int32;",
    ] {
        assert!(output.contains(declaration), "{output}");
    }
    assert!(!output.contains("__rst_v"));
    insta::assert_snapshot!(output);
}

#[test]
fn declarations_merge_only_with_unconditional_first_initializers() {
    let source = r#"
func Tick() -> Int32 = 1
func Test(flag: Bool) -> Int32 {
    let count = Tick();
    let zero: Int32;
    let text: String;
    let empty: [Int32];
    let values = [count];
    if flag { let branchValue = Tick(); }
    while flag { let repeated = Tick(); let reset: Int32; continue; }

    if flag { let count = count; }
    count = Tick();
    return count;
}
"#;
    let output = convert_mapped(source);
    for expected in [
        "var count: Int32 = Tick();",
        "var zero: Int32 = 0;",
        "var text: String = \"\";",
        "var values: array<Int32>;",
        "var branchValue: Int32;",
        "var repeated: Int32;",
        "var reset: Int32;",
        "var count_2: Int32;",
        "empty.Clear();",
        "reset = 0;",
        "count_2 = count;",
    ] {
        assert!(output.text.contains(expected), "{}", output.text);
    }
    assert_eq!(output.text.matches("var count:").count(), 1);
    let generated = output.text.find("var count: Int32 = Tick();").unwrap();
    let mapped =
        redscript_redmod_transpiler::package::source_at(&output.mappings, generated).unwrap();
    assert!(source[mapped.start as usize..mapped.end as usize].contains("let count = Tick()"));
    for (generated_token, source_statement) in [
        ("Tick();", "let count = Tick()"),
        ("var zero: Int32", "let zero: Int32"),
    ] {
        let offset = if generated_token == "Tick();" {
            generated + output.text[generated..].find(generated_token).unwrap()
        } else {
            output.text.find(generated_token).unwrap()
        };
        let span =
            redscript_redmod_transpiler::package::source_at(&output.mappings, offset).unwrap();
        assert!(source[span.start as usize..span.end as usize].contains(source_statement));
    }
    let sources = SourceMap::new();
    let file = sources.push_back("emitted.script", output.text.clone());
    let (module, errors) = redscript_redmod_parser::parse_module(&output.text, file);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(module.is_some());
    insta::assert_snapshot!(output.text);
}

#[test]
fn callback_bool_returns_are_preserved() {
    let output = convert(
        "func Effect() -> Bool = true class Widget { protected cb func OnEvent() -> Bool { return Effect(); } }",
    );
    assert!(output.contains("return Effect();"), "{output}");
    assert!(!output.contains("__rst_result"), "{output}");
    assert!(!output.contains("return;"), "{output}");
}

#[test]
fn user_function_named_like_intrinsic_stays_a_function() {
    let output = convert(
        "module Custom func ToString(value: Int32, other: Int32) -> String = \"custom\" func Test() -> String = ToString(1, 2)",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn inherited_method_uses_resolved_override() {
    let output = convert(
        "class Base { public func Value() -> Int32 = 1 } class Child extends Base { public func Value() -> Int32 = 2 }",
    );
    insta::assert_snapshot!(output);
}

#[test]
fn wrapper_original_uses_resolved_alias_and_hygienic_names() {
    let sources = SourceMap::new();
    let selected = sources.push_back("wrapper.reds", "@wrapMethod(EngineWidget) public func Value(name: Int32) -> Int32 { let Value = name; return wrappedMethod(Value); }");
    use redscript_compiler_frontend::{
        Aggregate, FunctionType, Method, MethodFlags, Param, ParamFlags, Type, TypeDef, TypeSchema,
        Visibility, predef,
    };
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    let (dependencies, modules): (Vec<_>, Vec<_>) = modules
        .into_iter()
        .partition(|m| m.items[0].1.file != selected);
    let evaluator = Evaluator::from_modules(&dependencies);
    let (_, mut symbols) = process_sources(
        dependencies,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
    );
    let mut aggregate = Aggregate::default();
    aggregate.methods_mut().add(
        "Value",
        Method::new(
            MethodFlags::default().with_visibility(Visibility::Public),
            FunctionType::new(
                [],
                [Param::new(
                    "name",
                    ParamFlags::default(),
                    Type::nullary(predef::INT32),
                    None,
                )],
                Type::nullary(predef::INT32),
            ),
            None,
            [],
            None,
        ),
    );
    symbols.add_type(
        interner.intern("EngineWidget"),
        TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
    );
    let (unit, symbols) = process_sources(
        modules.clone(),
        symbols,
        Evaluator::from_modules(&modules),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let source = &modules[0];
    let output = transpile(
        &[TranspileModule {
            source,
            bindings: bindings(source, &symbols),
        }],
        &unit,
        &symbols,
    )
    .unwrap();

    insta::assert_snapshot!(output[0].text);
}

#[test]
fn rejected_features_have_source_diagnostics() {
    for (name, source, expected) in [
        (
            "generics",
            "func Identity<Item>(value: Item) -> Item = value",
            "generic",
        ),
        (
            "lambda",
            "func Test() { let closure = () -> 1; }",
            "function",
        ),
        ("mixed-ref", "@mixedRef() class Widget {}", "mixedRef"),
        (
            "wide-enum",
            "enum Wide { Value = 2147483648l }",
            "outside verified target range",
        ),
        (
            "array-place",
            "class Widget { let items: [Int32]; public func Value() -> Int32 { return 1; } public func Test() { ArrayPush(this.items, this.Value()); } }",
            "this array operation requires a local array",
        ),
        (
            "out-place",
            "func Set(out value: Int32) {} struct Value { public let count: Int32; } class Owner { public let value: Value; } func Make() -> ref<Owner> { return new Owner(); } func Test() { Set(Make().value.count); }",
            "out arguments require a local variable or a writable field",
        ),
        (
            "struct-reset",
            "native struct Record { let value: Int32; } func Test() { let record: Record; }",
            "cannot default-initialize Record",
        ),
    ] {
        let sources = SourceMap::new();
        sources.push_back("rejected.reds", source);
        let interner = TypeInterner::default();
        let (modules, unit, symbols) = analyze(&sources, &interner);
        let result = transpile(
            &[TranspileModule {
                source: &modules[0],
                bindings: bindings(&modules[0], &symbols),
            }],
            &unit,
            &symbols,
        )
        .unwrap_err();
        assert!(result[0].message.contains(expected), "{name}: {result:?}");
        insta::assert_snapshot!(
            name,
            result
                .iter()
                .map(|d| d.display(&sources).to_string())
                .collect::<String>()
        );
    }
}

#[test]
fn enum_return_uses_typed_intermediate() {
    let sources = SourceMap::new();
    sources.push_back(
        "enum.reds",
        "enum Choice { First = 1, } func Test() -> Choice = Choice.First",
    );
    let interner = TypeInterner::default();
    let (modules, unit, symbols) = analyze(&sources, &interner);
    let enum_id = symbols
        .types()
        .find(|(id, _)| id.as_str() == "Choice")
        .unwrap()
        .0;
    let function = *symbols
        .free_functions()
        .find(|e| e.name().to_string() == "Test")
        .unwrap()
        .key();
    let output = transpile(
        &[TranspileModule {
            source: &modules[0],
            bindings: [
                (DeclarationPath(vec![0]), R::Type(enum_id)),
                (DeclarationPath(vec![1]), R::Function(function)),
            ]
            .into(),
        }],
        &unit,
        &symbols,
    )
    .unwrap();

    insta::assert_snapshot!(output[0].text);
}

#[test]
fn raw_string_emission_preserves_decoded_values() {
    // Community input decodes escapes; REDmod output must not re-escape them.
    let output = convert(r#"func Test() -> String = "assets\\widgets\\\nraw\\u{22}\\""#);
    assert!(
        output.contains("\"assets\\widgets\\\nraw\\u{22}\\\""),
        "{output}"
    );
    insta::assert_snapshot!(output);
}

#[test]
fn embedded_string_quote_is_a_typed_transpile_error() {
    let sources = SourceMap::new();
    sources.push_back("quote.reds", r#"func Test() -> String = "say\"hello""#);
    let interner = TypeInterner::default();
    let (modules, unit, symbols) = analyze(&sources, &interner);
    let diagnostics = transpile(
        &[TranspileModule {
            source: &modules[0],
            bindings: bindings(&modules[0], &symbols),
        }],
        &unit,
        &symbols,
    )
    .unwrap_err();
    assert_eq!(
        diagnostics[0].literal_error,
        Some(redscript_redmod_transpiler::LiteralError::EmbeddedDelimiter)
    );
    insta::assert_snapshot!(diagnostics[0].to_string());
}

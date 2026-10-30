use redscript_redmod_ast::{Declaration, FileId, Item, Module};
use redscript_redmod_formatter::{print_item_decl, print_member_decl, print_module};

fn file() -> FileId {
    FileId::from_i32(7)
}

fn module(source: &str) -> Module<'_> {
    let (module, errors) = redscript_redmod_parser::parse_module(source, file());
    assert!(errors.is_empty(), "{source}\n{errors:?}");
    module.unwrap()
}

// Compare the entire native tree, ignoring only source locations. No shared AST
// projection or semantic normalization should hide a lost group/modifier/label.
fn syntax(tree: &impl std::fmt::Debug) -> String {
    let debug = format!("{tree:?}");
    let mut rest = debug.as_str();
    let mut output = String::new();
    while let Some(start) = rest.find("Span {") {
        output.push_str(&rest[..start]);
        let end = rest[start..].find('}').unwrap() + start + 1;
        rest = &rest[end..];
    }
    output.push_str(rest);
    output
}

fn roundtrip(source: &str) -> String {
    let ast = module(source);
    let output = print_module(&ast).unwrap();
    let reparsed = module(&output);
    assert_eq!(syntax(&ast), syntax(&reparsed), "{output}");
    assert_eq!(print_module(&reparsed).unwrap(), output);
    output
}

#[test]
fn minima_nested_enums_and_integer_receivers_roundtrip() {
    let output = roundtrip(
        "class C { enum E: Int32 { Min = -2147483648, Max = 2147483647, Next, } }\n\
         function F() { Use(-2147483648, -9223372036854775808l, (1).Member, (-2147483648).Member, (-9223372036854775808l).Member); }",
    );
    assert!(output.contains("        Min = -2147483648,"));
    assert!(output.contains("        Next,\n    }\n}"));
    assert!(!output.contains("2147483647 - 1"));
}

#[test]
fn full_native_grammar() {
    let output = roundtrip(
        r#"@module "Example.Mod";
/// using docs
@using "Other.Mod";
/// aggregate docs
[first = "raw\path", second = 2]
[third = true]
@arbitrary(C, "value")
@another(2, false)
public abstract final importonly import exec persistent quest static editable instanceeditable inlined replicated testonly override export virtual constexpr mutable const class C extends mixedRef<Base> {
    [category = "General"]
    @custom()
    protected editable persistent var first, second: array<weak<C>>;
    var ids: Int32[3];
    var names: name;
    var mixed: mixedRef;
    var typedMixed: mixedRef<C>;
    var script: script_ref<name>;
    var weakref: wref<C>;
    default first = NULL;
    hint first = "hint\text";
    private override const function Run(optional out const skip x, y: Int32, z: weak<C>) -> const ref<C> {
        const var i, j: Int32 = 0;
        ;
        { var nested: name = 'raw\name'; }
        for (var k, l: Int32 = 0; k < 3; k += 1, l += 2) { continue; }
        for (i = 0, j = 1; i < 3, j < 4; i += 1) {}
        for (;;) {}
        do { i += 1; continue; } while (i < 2);
        while (i < 3) { i += 1; }
        if (true) { return NULL; } else if (false) { break; } else { return new C; }
        switch (i) { default: i = 1; case 1: case 2: break; default: return; case 3: ; }
        i = j = 2;
        First(), i = 3, Last(i);
        Use(, 1, , 3, );
        Use(,);
        Use();
        Use([1, 2], (Int32)(i), Cast<Float>(j), (this as C).value, true ? 1 : 2);
        (new C).Run(); (new C)[0]; (1).ToString();
        this.Run(); super.Run(); other.Run(); wrappedMethod();
        return new C;
    }
    event Tick(x: Int32) -> const Bool;
    implicit cast(value: C) -> const name;
    cast(value: name) -> C {}
    struct Nested { var x, y: Int32 = 1; }
}
enum E: Uint8 { A = -1, B, C = 4, D, }
@wrapMethod(C)
public function Run() { Run(); wrappedMethod(); }
"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn declaration_groups_and_transitions() {
    let output = roundtrip(
        r#"@module "Groups";
@using "First";
@using "Second";
/// documented import
@using "Third";
@using "Fourth";
@using "Fifth";
var first: Int32;
public static var second, third: Int32;
function First();
function Second() {}
class C {
    var first: Int32;
    private persistent var second: Int32;
    function First();
    event Second();
    struct Nested {
        var x: Int32;
        var y: Int32;
    }
    enum E { A, B, }
    var third: Int32;
    var fourth: Int32;
}
struct Empty {}
enum E { A, }
function Last();
"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn declaration_prefixes_break_field_groups() {
    let output = roundtrip(
        r#"class C {
    var first: Int32;
    /// documented field
    var second: Int32;
    /// another documented field
    var third: Int32;
    var fourth: Int32;
    var fifth: Int32;
    [category = "General"]
    var sixth: Int32;
    [category = "General"]
    var seventh: Int32;
    @custom()
    var eighth: Int32;
    @custom()
    var ninth: Int32;
    /// combined prefix
    [category = "General"]
    @custom()
    public editable var tenth: Int32;
    var last: Int32;
}
"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn directive_groups_do_not_infer_field_associations() {
    let output = roundtrip(
        r#"class C {
    var first: Int32;
    var second: Int32;
    default first = 1;
    default second = 2;
    hint first = "first";
    hint second = "second";
    default first = 3;
    /// documented default
    default second = 4;
    default first = 5;
    [category = "General"]
    hint first = "metadata";
    @custom()
    hint second = "annotated";
    hint first = "plain";
    hint second = "plain";
    function Run();
    default first = 6;
    var last: Int32;
}
"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn standalone_declarations_have_no_group_padding() {
    for source in ["", "class Empty {}", "struct Empty {}", "enum Empty {}"] {
        let output = roundtrip(source);
        assert!(!output.starts_with('\n'));
        assert!(!output.ends_with("\n\n"));
        if let Some((decl, _)) = module(source).items.first() {
            assert_eq!(print_item_decl(decl).unwrap(), output);
            assert_eq!(print_member_decl(decl).unwrap(), output);
        }
    }
}

#[test]
fn all_operator_declarations_and_expressions() {
    for op in [
        "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "&", "|", "^", "!",
        "~", "+=", "-=", "*=", "/=", "&=", "|=", "[]",
    ] {
        roundtrip(&format!("operator {op}(skip a, b: Int32) -> Int32;"));
    }
    for op in [
        "+=", "-=", "*=", "/=", "|=", "&=", "||", "&&", "|", "^", "&", "==", "!=", "<", "<=", ">",
        ">=", "+", "-", "*", "/", "%",
    ] {
        roundtrip(&format!("function Run() {{ a {op} b; }}"));
    }
    for op in ["~", "!", "-"] {
        roundtrip(&format!("function Run() {{ {op}a; }}"));
    }
}

#[test]
fn expression_precedence_and_postfix_roundtrip() {
    roundtrip(
        r#"function F() {
        var values: array<Int32[2]>[3];
        a = b = c + d * e;
        Use(a + (b - c), -(a + b), !(a && b), ~(a | b));
        Use(a ? b ? c : d : e, (a ? b : c) ? d : e, a ? b : c ? d : e);
        Use((Int32)-x, (Float)(Int32)x, Cast<Float>((Int32)(x + y)));
        Use(((this as C) as Base), ((Int32)(x)).value, Cast<C>(x).Run());
        (new C)(); (new C)[0]; (new C).Run();
        (2l).ToString(); (3.0).ToString();
        factory()(); values[0][1] = [1, 2][0];
        (a ? b : c)();
    }"#,
    );
}

#[test]
fn raw_literals_and_native_spelling() {
    let output = roundtrip(
        r#"function Test() {
        Use("assets\", "\n\t\u{22}\\", "first
  second", 'Name with "quotes"', 'assets\', n"can't", n"can't\", n"can't\n\u{22}", n"can't
  second", R"path", T"id", 2.5d, 3l, NULL);
        var x: name = 'name';
        var y: weak<C>;
        var z: mixedRef<C>;
    }"#,
    );
    insta::assert_snapshot!(output);
}

#[test]
fn native_declaration_entry_points_need_no_callback() {
    let source = "@unrecognized(Target) public override static event Run() -> Int32;";
    let (decl, errors) = redscript_redmod_parser::parse_item_decl(source, file());
    assert!(errors.is_empty(), "{errors:?}");
    let decl: Declaration<'_> = decl.unwrap();
    let global = print_item_decl(&decl).unwrap();
    assert_eq!(global, print_member_decl(&decl).unwrap());
    assert_eq!(syntax(&decl), syntax(&module(&global).items[0].0));
    assert!(global.contains("public override static event Run() -> Int32;"));
}

#[test]
fn documentation_and_contextual_names_survive() {
    let output = roundtrip(
        "/// first\r\n/// second\r\n//// ordinary comment\r\nclass name {\n/// member\nfunction operator() {}\nvar cast: name;\n}",
    );
    assert!(output.starts_with("/// first\n/// second\n"));
    let ast = module("class C { function Run() {} }");
    let Item::Aggregate { items, .. } = &ast.items[0].0.item else {
        panic!()
    };
    assert_eq!(
        print_item_decl(&items[0].0).unwrap(),
        print_member_decl(&items[0].0).unwrap()
    );
}

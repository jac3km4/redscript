//! Tests for the 0.5.x compatibility mode of the community dialect, and for making sure the
//! default mode is unaffected.

use redscript_ast::{
    BinOp, Constant, Dialect, Expr, FileId, Item, ItemQualifiers, Module, Stmt, Type, UnOp,
    Visibility,
};
use similar_asserts::assert_eq;

use super::*;

const COMPAT: ParseOptions = ParseOptions::compat(Dialect::Community);
const STRICT: ParseOptions = ParseOptions::new(Dialect::Community);

fn file() -> FileId {
    FileId::from_i32(0)
}

fn expr(src: &str, opts: ParseOptions) -> Expr<'_> {
    let (res, errs) = parse_expr_with_options(src, file(), opts);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn expr_errors(src: &str, opts: ParseOptions) -> usize {
    parse_expr_with_options(src, file(), opts).1.len()
}

fn stmt(src: &str, opts: ParseOptions) -> Stmt<'_> {
    let (res, errs) = parse_stmt_with_options(src, file(), opts);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn stmt_errors(src: &str, opts: ParseOptions) -> usize {
    parse_stmt_with_options(src, file(), opts).1.len()
}

fn module(src: &str, opts: ParseOptions) -> Module<'_> {
    let (res, errs) = parse_module_with_options(src, file(), opts);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn module_errors(src: &str, opts: ParseOptions) -> usize {
    parse_module_with_options(src, file(), opts).1.len()
}

fn tokens(src: &str, opts: ParseOptions) -> (Vec<Token<'_>>, usize) {
    let (toks, errs) = lex_with_options(src, file(), opts);
    let toks = toks
        .unwrap_or_default()
        .into_iter()
        .map(|(t, _)| t)
        .collect();
    (toks, errs.len())
}

fn ident(name: &str) -> Expr<'_> {
    Expr::Ident(name)
}

fn int(n: i32) -> Expr<'static> {
    Expr::Constant(Constant::I32(n))
}

fn cast<'a>(e: Expr<'a>, ty: &'a str) -> Expr<'a> {
    Expr::DynCast {
        expr: Box::new(e),
        typ: Box::new(Type::plain(ty)),
    }
}

fn member<'a>(e: Expr<'a>, member: &'a str) -> Expr<'a> {
    Expr::Member {
        expr: Box::new(e),
        member,
    }
}

fn binop<'a>(lhs: Expr<'a>, op: BinOp, rhs: Expr<'a>) -> Expr<'a> {
    Expr::BinOp {
        lhs: Box::new(lhs),
        op,
        rhs: Box::new(rhs),
    }
}

fn assign<'a>(lhs: Expr<'a>, rhs: Expr<'a>) -> Expr<'a> {
    Expr::Assign {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn ternary<'a>(cond: Expr<'a>, then: Expr<'a>, else_: Expr<'a>) -> Expr<'a> {
    Expr::Conditional {
        cond: Box::new(cond),
        then: Box::new(then),
        else_: Box::new(else_),
    }
}

// 1. doc comments

#[test]
fn doc_comments_anywhere() {
    let src = r#"
/// before the module
module Test

class A {
  /// on a field
  let x: Int32;

  @if(true)
  /// after an annotation
  func F() {
    /// inside a body
    let a = 1;
    /// between statements
    a += 1;
    /// last in a body
  }
  /// last in a class
}

enum E {
  A = 0,
  /// last in an enum
}
/// end of file
"#;
    let res = module(src, COMPAT);
    assert_eq!(res.items.len(), 2);
    let Item::Class(class) = &res.items[0].item else {
        panic!("expected a class");
    };
    // `///` comments right before a declaration stay documentation, even after annotations
    assert_eq!(&class.items[0].doc[..], ["/// on a field"]);
    assert_eq!(&class.items[1].doc[..], ["/// after an annotation"]);
    assert!(res.items[0].doc.is_empty());
    assert!(module_errors(src, STRICT) > 0);
}

#[test]
fn misplaced_doc_comments_become_line_comments() {
    let src = "/// a
module A
/// b
func F() {
  /// c
  let x = 1;
}
/// d";
    let (toks, errs) = lex_with_lf_and_comments_and_options(src, file(), COMPAT);
    assert_eq!(errs.len(), 0);
    let comments = toks
        .unwrap()
        .into_iter()
        .filter_map(|(tok, _)| match tok {
            Token::LineComment(str) => Some(format!("line {str}")),
            Token::DocComment(str) => Some(format!("doc {str}")),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        comments,
        ["line // a", "doc /// b", "line // c", "line // d"]
    );

    // without comments, the misplaced ones are dropped
    let (toks, errs) = tokens(src, COMPAT);
    assert_eq!(errs, 0);
    assert_eq!(
        toks.iter()
            .filter(|tok| matches!(tok, Token::DocComment(_) | Token::LineComment(_)))
            .count(),
        1
    );
    let (toks, _) = tokens(src, STRICT);
    assert_eq!(toks[0], Token::DocComment("/// a"));
}

#[test]
fn doc_comments_in_class_bodies() {
    let src = r#"
class A extends B {
  /// on a method
  public func F() -> Int32 {
    /// in a body
    let x: Int32;
    return x;
  }
  /// on a field
  let y: Int32;
}
"#;
    let res = module(src, COMPAT);
    let Item::Class(class) = &res.items[0].item else {
        panic!("expected a class");
    };
    assert_eq!(&class.items[0].doc[..], ["/// on a method"]);
    assert_eq!(&class.items[1].doc[..], ["/// on a field"]);
    assert!(module_errors(src, STRICT) > 0);
}

// 2. `as` in the postfix chain

#[test]
fn as_in_postfix_chain() {
    assert_eq!(expr("x as T.f", COMPAT), member(cast(ident("x"), "T"), "f"));
    assert_eq!(
        expr("x as T.M()", COMPAT),
        Expr::Call {
            expr: Box::new(member(cast(ident("x"), "T"), "M")),
            type_args: [].into(),
            args: [].into(),
        }
    );
    assert_eq!(
        expr("x as T[i]", COMPAT),
        Expr::Index {
            expr: Box::new(cast(ident("x"), "T")),
            index: Box::new(ident("i")),
        }
    );
    assert_eq!(
        expr("x as A as B", COMPAT),
        cast(cast(ident("x"), "A"), "B")
    );
    assert_eq!(
        expr("a + b as T", COMPAT),
        binop(ident("a"), BinOp::Add, cast(ident("b"), "T"))
    );
    assert!(expr_errors("x as T.f", STRICT) > 0);
}

#[test]
fn unary_operators_apply_to_casts() {
    assert_eq!(
        expr("!x as T", COMPAT),
        Expr::UnOp {
            op: UnOp::Not,
            expr: Box::new(cast(ident("x"), "T")),
        }
    );
    assert_eq!(
        expr("-x as T", COMPAT),
        Expr::UnOp {
            op: UnOp::Neg,
            expr: Box::new(cast(ident("x"), "T")),
        }
    );
    assert_eq!(
        expr("!x as T", STRICT),
        cast(
            Expr::UnOp {
                op: UnOp::Not,
                expr: Box::new(ident("x")),
            },
            "T"
        )
    );
}

// 3. assignments

#[test]
fn compound_assignment_with_ternary() {
    assert_eq!(
        expr("a += b ? 1 : 2", COMPAT),
        binop(
            ident("a"),
            BinOp::AssignAdd,
            ternary(ident("b"), int(1), int(2))
        )
    );
    assert_eq!(
        expr("a += b ? 1 : 2", STRICT),
        ternary(
            binop(ident("a"), BinOp::AssignAdd, ident("b")),
            int(1),
            int(2)
        )
    );
    // other operators keep their precedence
    assert_eq!(
        expr("a -= b + c", COMPAT),
        binop(
            ident("a"),
            BinOp::AssignSub,
            binop(ident("b"), BinOp::Add, ident("c"))
        )
    );
    assert_eq!(expr("a -= b + c", COMPAT), expr("a -= b + c", STRICT));
}

#[test]
fn chained_assignments_are_right_associative() {
    assert_eq!(
        expr("a = b = c", COMPAT),
        assign(ident("a"), assign(ident("b"), ident("c")))
    );
    assert_eq!(
        expr("a = b += c", COMPAT),
        assign(ident("a"), binop(ident("b"), BinOp::AssignAdd, ident("c")))
    );
    assert_eq!(expr("a = b ? c : d", COMPAT), expr("a = b ? c : d", STRICT));
    assert!(expr_errors("a = b = c", STRICT) > 0);
}

// 4. qualifiers

#[test]
fn qualifiers_in_any_order() {
    let src = "static public final func F() {}";
    let res = module(src, COMPAT);
    assert_eq!(res.items[0].visibility, Some(Visibility::Public));
    assert_eq!(
        res.items[0].qualifiers,
        ItemQualifiers::STATIC | ItemQualifiers::FINAL
    );
    assert!(module_errors(src, STRICT) > 0);
}

#[test]
fn duplicate_qualifiers_are_tolerated() {
    let src = "public static public private static func F() {}";
    let res = module(src, COMPAT);
    assert_eq!(res.items[0].visibility, Some(Visibility::Public));
    assert_eq!(res.items[0].qualifiers, ItemQualifiers::STATIC);
    assert!(module_errors(src, STRICT) > 0);
}

#[test]
fn param_qualifiers_on_items_are_ignored() {
    let src = "class A { opt public let x: Int32; out func F() {} }";
    let res = module(src, COMPAT);
    let Item::Class(class) = &res.items[0].item else {
        panic!("expected a class");
    };
    assert_eq!(class.items[0].visibility, Some(Visibility::Public));
    assert_eq!(class.items[0].qualifiers, ItemQualifiers::empty());
    assert_eq!(class.items[1].qualifiers, ItemQualifiers::empty());
    assert!(module_errors(src, STRICT) > 0);
}

// 5. keywords as names

#[test]
fn keywords_as_names() {
    let src = r#"
func default(default: Int32, case: Int32) -> Int32 {
  let null: Int32 = default;
  switch default {
    case 1:
      return default;
    default:
      return 2;
  }
}

class A {
  let default: Int32;
  func case() {}
}

enum E {
  default = 0,
  case = 1,
}
"#;
    let res = module(src, COMPAT);
    let Item::Function(func) = &res.items[0].item else {
        panic!("expected a function");
    };
    assert_eq!(func.name, "default");
    assert_eq!(func.params[0].name, "default");
    assert_eq!(func.params[1].name, "case");
    assert!(module_errors(src, STRICT) > 0);
}

#[test]
fn default_variable_does_not_break_switch() {
    let res = stmt(
        "switch x { case 1: default = 2; break; default: f(default); }",
        COMPAT,
    );
    let Stmt::Switch { cases, default, .. } = res else {
        panic!("expected a switch");
    };
    assert_eq!(cases.len(), 1);
    assert_eq!(cases[0].body.len(), 2);
    assert_eq!(default.unwrap().len(), 1);
    assert!(stmt_errors("let default = 1;", STRICT) > 0);
    assert_eq!(
        stmt("let default = 1;", COMPAT),
        Stmt::Let {
            name: "default",
            typ: None,
            value: Some(Box::new(int(1))),
        }
    );
}

#[test]
fn default_variable_as_conditional_branch() {
    assert_eq!(
        expr("c ? default : x", COMPAT),
        ternary(ident("c"), ident("default"), ident("x"))
    );
    assert_eq!(
        expr("c ? x : default", COMPAT),
        ternary(ident("c"), ident("x"), ident("default"))
    );
    assert_eq!(
        expr("c ? d ? x : default : default", COMPAT),
        ternary(
            ident("c"),
            ternary(ident("d"), ident("x"), ident("default")),
            ident("default")
        )
    );
    // a switch arm after a conditional is still a switch arm
    let res = stmt(
        "switch x { case 1: y = c ? default : 2; break; default: f(c ? 1 : default); }",
        COMPAT,
    );
    let Stmt::Switch { cases, default, .. } = res else {
        panic!("expected a switch");
    };
    assert_eq!(cases.len(), 1);
    assert_eq!(default.unwrap().len(), 1);
    assert!(expr_errors("c ? default : x", STRICT) > 0);
}

// 6. lexer

#[test]
fn ge_after_generic_close() {
    let src = "let x: array<Int32>= [];";
    assert_eq!(
        stmt(src, COMPAT),
        Stmt::Let {
            name: "x",
            typ: Some(Box::new(Type::Named {
                name: "array",
                args: [Type::plain("Int32")].into(),
            })),
            value: Some(Box::new(Expr::ArrayLit([].into()))),
        }
    );
    assert!(
        stmt("let x: array<array<Int32>>= [];", COMPAT)
            .eq(&stmt("let x: array<array<Int32>> = [];", STRICT))
    );
    // comparisons are unaffected
    assert_eq!(expr("a < b >= c", COMPAT), expr("a < b >= c", STRICT));
    // so is the `:` of a conditional followed by a comparison
    assert_eq!(
        expr("G(c ? p : q < a, b >= d)", COMPAT),
        expr("G(c ? p : q < a, b >= d)", STRICT)
    );
    assert_eq!(
        expr("c ? p : q < a >= d", COMPAT),
        expr("c ? p : q < a >= d", STRICT)
    );
    // fields and locals named with a keyword
    assert!(
        stmt("let default: array<Int32>= [];", COMPAT)
            .eq(&stmt("let default: array<Int32> = [];", COMPAT))
    );
    assert_eq!(
        expr("a >= b", COMPAT),
        binop(ident("a"), BinOp::Ge, ident("b"))
    );
    assert!(stmt_errors(src, STRICT) > 0);
}

#[test]
fn signed_min_literals() {
    assert_eq!(
        expr("-2147483648", COMPAT),
        Expr::Constant(Constant::I32(i32::MIN))
    );
    assert_eq!(
        expr("-9223372036854775808l", COMPAT),
        Expr::Constant(Constant::I64(i64::MIN))
    );
    // other negative literals stay negations
    assert_eq!(
        expr("-1", COMPAT),
        Expr::UnOp {
            op: UnOp::Neg,
            expr: Box::new(int(1)),
        }
    );
    assert_eq!(
        expr("a -2147483647", COMPAT),
        binop(ident("a"), BinOp::Sub, int(2_147_483_647))
    );
    assert!(expr_errors("-2147483648", STRICT) > 0);
    assert!(expr_errors("-9223372036854775808l", STRICT) > 0);
}

#[test]
fn number_literals() {
    let (toks, errs) = tokens("007 .5 1.5u 1.5l 1ud 2ul 3u 4l 5d 6. 1.5e3", COMPAT);
    assert_eq!(errs, 0);
    assert_eq!(
        toks,
        [
            Token::Int(7),
            Token::Float(0.5),
            Token::Float(1.5),
            Token::Float(1.5),
            Token::Double(1.),
            Token::Ulong(2),
            Token::Uint(3),
            Token::Long(4),
            Token::Double(5.),
            Token::Float(6.),
            Token::Float(1500.),
        ]
    );

    let (toks, _) = tokens("007", STRICT);
    assert_ne!(toks, [Token::Int(7)]);
    let (toks, _) = tokens(".5", STRICT);
    assert_eq!(toks[0], Token::Period);
    assert!(tokens("1.5u", STRICT).1 > 0);
    let (toks, _) = tokens("1ud", STRICT);
    assert_eq!(toks, [Token::Uint(1), Token::Ident("d")]);
}

#[test]
fn member_access_is_not_a_float() {
    assert_eq!(expr("a.b", COMPAT), member(ident("a"), "b"));
    assert_eq!(
        expr("a[0].b", COMPAT),
        member(
            Expr::Index {
                expr: Box::new(ident("a")),
                index: Box::new(int(0)),
            },
            "b"
        )
    );
}

#[test]
fn long_enum_values() {
    let src = "enum E { A = 10l, B = -5l, C = 3 }";
    let res = module(src, COMPAT);
    let Item::Enum(enum_) = &res.items[0].item else {
        panic!("expected an enum");
    };
    let values = enum_.variants.iter().map(|v| v.value).collect::<Vec<_>>();
    assert_eq!(values, [Some(10), Some(-5), Some(3)]);
    assert!(module_errors(src, STRICT) > 0);
}

// 7. interpolation

#[test]
fn interpolation_with_unbalanced_parens_in_string() {
    let src = r#"s"a \(F(")")) b""#;
    let res = expr(src, COMPAT);
    let Expr::InterpolatedString(parts) = res else {
        panic!("expected an interpolated string");
    };
    assert_eq!(parts.len(), 3);
    assert!(expr_errors(src, STRICT) > 0);
}

#[test]
fn compat_has_no_effect_on_redmod() {
    let opts = ParseOptions::compat(Dialect::Redmod);
    let (toks, _) = tokens("/// doc", opts);
    assert_eq!(toks, [Token::DocComment("/// doc")]);
    assert!(expr_errors("x as T.f", opts) > 0);
}

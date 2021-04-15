use std::rc::Rc;
use std::str::FromStr;

use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{BinOp, Constant, Expr, Ident, Literal, Pos, Seq, SourceAst, SwitchCase, TypeName, UnOp};
use redscript::definition::Visibility;
use strum::EnumString;

use crate::source_map::File;
use crate::{Import, ModulePath};

#[derive(Debug)]
pub struct SourceModule {
    pub path: Option<ModulePath>,
    pub imports: Vec<Import>,
    pub entries: Vec<SourceEntry>,
}

#[derive(Debug)]
pub enum SourceEntry {
    Class(ClassSource),
    Function(FunctionSource),
    GlobalLet(FieldSource),
}

#[derive(Debug)]
pub struct ClassSource {
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub base: Option<Ident>,
    pub members: Vec<MemberSource>,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum MemberSource {
    Function(FunctionSource),
    Field(FieldSource),
}

#[derive(Debug)]
pub struct FieldSource {
    pub declaration: Declaration,
    pub type_: TypeName,
}

#[derive(Debug)]
pub struct FunctionSource {
    pub declaration: Declaration,
    pub type_: Option<TypeName>,
    pub parameters: Vec<ParameterSource>,
    pub body: Option<Seq<SourceAst>>,
}

#[derive(Debug)]
pub struct ParameterSource {
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub type_: TypeName,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Qualifier {
    Public,
    Protected,
    Private,
    Static,
    Final,
    Const,
    Native,
    Exec,
    Callback,
    Out,
    Optional,
}

#[derive(Debug)]
pub struct Qualifiers(Vec<Qualifier>);

impl Qualifiers {
    pub fn visibility(&self) -> Option<Visibility> {
        self.0.iter().find_map(|q| match q {
            Qualifier::Public => Some(Visibility::Public),
            Qualifier::Protected => Some(Visibility::Protected),
            Qualifier::Private => Some(Visibility::Private),
            _ => None,
        })
    }

    pub fn contain(&self, qualifier: Qualifier) -> bool {
        self.0.contains(&qualifier)
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub annotations: Vec<Annotation>,
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct Annotation {
    pub name: AnnotationName,
    pub values: Vec<Ident>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum AnnotationName {
    ReplaceMethod,
    ReplaceGlobal,
    AddMethod,
    AddField,
}

pub fn parse_file(file: &File) -> Result<SourceModule, ParseError<LineCol>> {
    lang::module(file.source(), file.byte_offset())
}

pub fn parse_str(str: &str) -> Result<SourceModule, ParseError<LineCol>> {
    lang::module(str, Pos::ZERO)
}

peg::parser! {
    grammar lang(offset: Pos) for str {
        use peg::ParseLiteral;

        rule pos() -> Pos = pos:position!() { offset + pos }
        rule _() = quiet!{ ([' ' | '\n' | '\r' | '\t'] / comment() / line_comment())* }
        rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ("," _)) {v}
        rule dotsep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ("." _)) {v}

        rule comment_start() = "/*"
        rule comment_end() = "*/"
        rule comment_content()
            = comment() / (!comment_start() !comment_end() [_])
        rule comment()
            = comment_start() comment_content()* comment_end()

        rule line_comment() = "//" $(!['\n'] [_])*

        rule qualifier() -> Qualifier
            = keyword("public") { Qualifier::Public }
            / keyword("protected") { Qualifier::Protected }
            / keyword("private") { Qualifier::Private }
            / keyword("static") { Qualifier::Static }
            / keyword("final") { Qualifier::Final }
            / keyword("const") { Qualifier::Const }
            / keyword("native") { Qualifier::Native }
            / keyword("exec") { Qualifier::Exec }
            / keyword("cb") { Qualifier::Callback }
            / keyword("out") { Qualifier::Out }
            / keyword("opt") { Qualifier::Optional }

        rule literal_type() -> Literal
            = "n" { Literal::Name }
            / "r" { Literal::Resource }
            / "t" { Literal::TweakDbId }

        rule annotation() -> Annotation
            = pos:pos() "@" ident:ident() _ "(" _ values:commasep(<ident()>) _ ")" {?
                AnnotationName::from_str(ident.as_ref()).map(|name| {
                    Annotation { name, values, pos }
                }).map_err(|_| "annotation")
            }

        rule qualifiers() -> Qualifiers = qs:qualifier() ** _ { Qualifiers(qs) }

        rule ident() -> Ident
            = quiet!{
                x:$(['a'..='z' | 'A'..='Z' | '_']) xs:$(['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*)
                { Ident::new(format!("{}{}", x, xs)) }
            } / expected!("identifier")

        rule keyword(id: &'static str) -> () =
            ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']

        rule number() -> Constant
            = n:$(['0'..='9' | '.']+) postfix:$(['u'])?
            { if n.contains('.') { Constant::Float(n.parse().unwrap()) }
              else if postfix == Some("u") { Constant::Uint(n.parse().unwrap()) }
              else { Constant::Int(n.parse().unwrap()) }
            }

        rule escaped_char() -> String
            = !['\\' | '\"'] c:$([_]) { String::from(c) }
            / r#"\n"# { String::from('\n') }
            / r#"\r"# { String::from('\r') }
            / r#"\t"# { String::from('\t') }
            / r#"\'"# { String::from('\'') }
            / r#"\""# { String::from('\"') }
            / r#"\\"# { String::from('\\') }
            / "\\u{" u:$(['a'..='f' | 'A'..='F' | '0'..='9']*<1,6>) "}" {
                String::from(char::from_u32(u32::from_str_radix(u, 16).unwrap()).unwrap())
            }

        pub rule escaped_string() -> String
            = "\"" s:escaped_char()* "\"" { s.join("") }

        rule constant() -> Constant
            = keyword("true") { Constant::Bool(true) }
            / keyword("false") { Constant::Bool(false) }
            / n:number() { n }
            / type_:literal_type()? str:escaped_string()
                { Constant::String(type_.unwrap_or(Literal::String), Rc::new(str)) }

        rule seq() -> Seq<SourceAst> = exprs:(stmt() ** _) { Seq::new(exprs) }

        rule type_() -> TypeName
            = name:ident() args:type_args()? { TypeName { name, arguments: args.unwrap_or_default() } }
        rule type_args() -> Vec<TypeName> = "<" _ args:commasep(<type_()>) _ ">" { args }

        rule let_type() -> TypeName = ":" _ type_:type_() { type_ }
        rule func_type() -> TypeName = "->" _ type_:type_() { type_ }

        rule initializer() -> Expr<SourceAst> = "=" _ val:expr() { val }

        rule let() -> Expr<SourceAst>
            = pos:pos() keyword("let") _ name:ident() _ type_:let_type()? _ val:initializer()? _ ";"
            { Expr::Declare(name, type_, val.map(Box::new), pos) }

        rule decl(inner: rule<()>) -> Declaration
            = pos:pos() annotations:(annotation() ** _) _ qualifiers:qualifiers() _ inner() _ name:ident()
            { Declaration { annotations, qualifiers, name, pos } }

        rule field() -> FieldSource
            = declaration:decl(<keyword("let")>) _ type_:let_type() _ ";"
            { FieldSource { declaration, type_ }}

        pub rule function() -> FunctionSource
            = declaration:decl(<keyword("func")>) _ "(" _ parameters:commasep(<param()>) _ ")" _ type_:func_type()? _ body:function_body()?
            { FunctionSource { declaration, type_, parameters, body } }
        rule function_body() -> Seq<SourceAst>
            = "{" _ body:seq() _ "}" { body }
            / pos:pos() "=" _ expr:expr() { Seq::new(vec![Expr::Return(Some(Box::new(expr)), pos)]) }

        rule param() -> ParameterSource
            = qualifiers:qualifiers() _ name:ident() _ type_:let_type()
            { ParameterSource { qualifiers, name, type_ } }

        rule extends() -> Ident = keyword("extends") _ name:ident() { name }

        pub rule class() -> ClassSource
            = pos:pos() qualifiers:qualifiers() _ keyword("class") _ name:ident() _ base:extends()? _ "{" _ members:member()**_ _ "}"
            { ClassSource { qualifiers, name, base, members, pos } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Function(fun) }
            / field:field() { MemberSource::Field(field) }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }
            / field:field() { SourceEntry::GlobalLet(field) }


        rule import() -> Import
            = pos:pos() keyword("import") _ parts: dotsep(<ident()>) _ "." _ "*"
                { Import::All(ModulePath::new(parts), pos) }
            / pos:pos() keyword("import") _ parts: dotsep(<ident()>) _ "." _ "{" _ names:commasep(<ident()>) _ "}"
                { Import::Selected(ModulePath::new(parts), names, pos) }
            / pos:pos() keyword("import") _ parts: dotsep(<ident()>)
                { Import::Exact(ModulePath::new(parts), pos) }

        rule module_path() -> ModulePath  =
            keyword("module") _ parts:dotsep(<ident()>) { ModulePath { parts } }

        pub rule module() -> SourceModule =
            _ path:module_path()? _ imports:(import() ** _) entries:(source_entry() ** _) _
            { SourceModule { path, imports, entries } }

        rule switch() -> Expr<SourceAst>
            = keyword("switch") _ matcher:expr() _ "{" _ cases:(case() ** _) _ default:default()? _ "}" _ ";"?
            { Expr::Switch(Box::new(matcher), cases, default) }

        rule case() -> SwitchCase<SourceAst>
            = keyword("case") _ matcher:expr() _ ":" _ body:seq()
            { SwitchCase { matcher, body } }

        rule default() -> Seq<SourceAst>
            = keyword("default") _ ":" _ body:seq() { body }

        rule while_() -> Expr<SourceAst>
            = pos:pos() keyword("while") _ cond:expr() _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::While(Box::new(cond), body, pos) }

        rule for_() -> Expr<SourceAst>
            = pos:pos() keyword("for") _ ident:ident() _ keyword("in") _ array:expr() _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::ForIn(ident, Box::new(array), body, pos) }

        rule if_() -> Expr<SourceAst>
            = pos:pos() keyword("if") _ cond:expr() _ "{" _ if_:seq() _ "}" _ else_:else_()? _ ";"?
            { Expr::If(Box::new(cond), if_, else_, pos) }
        rule else_() -> Seq<SourceAst>
            = keyword("else") _ "{" _ body:seq() _ "}" { body }

        pub rule stmt() -> Expr<SourceAst>
            = while_: while_() { while_ }
            / for_: for_() { for_ }
            / if_: if_() { if_ }
            / switch: switch() { switch }
            / pos:pos() keyword("return") _ val:expr()? _ ";" { Expr::Return(val.map(Box::new), pos) }
            / pos:pos() keyword("break") _ ";" { Expr::Break(pos) }
            / let_:let() { let_ }
            / expr:expr() _ ";" { expr }

        pub rule expr() -> Expr<SourceAst> = precedence!{
            x:@ _ pos:pos() "?" _ y:expr() _ ":" _ z:expr()
                { Expr::Conditional(Box::new(x), Box::new(y), Box::new(z), pos) }
            x:@ _ pos:pos() "=" _ y:(@) { Expr::Assign(Box::new(x), Box::new(y), pos) }
            x:@ _ pos:pos() "+=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignAdd, pos) }
            x:@ _ pos:pos() "-=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignSubtract, pos) }
            x:@ _ pos:pos() "*=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignMultiply, pos) }
            x:@ _ pos:pos() "/=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignDivide, pos) }
            --
            x:(@) _ pos:pos() "||" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicOr, pos) }
            x:(@) _ pos:pos() "&&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicAnd, pos) }
            x:(@) _ pos:pos() "|" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Or, pos) }
            x:(@) _ pos:pos() "^" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Xor, pos) }
            x:(@) _ pos:pos() "&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::And, pos) }
            --
            x:(@) _ pos:pos() "==" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Equal, pos) }
            x:(@) _ pos:pos() "!=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::NotEqual, pos) }
            --
            x:(@) _ pos:pos() "<" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Less, pos) }
            x:(@) _ pos:pos() "<=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LessEqual, pos) }
            x:(@) _ pos:pos() ">" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Greater, pos) }
            x:(@) _ pos:pos() ">=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::GreaterEqual, pos) }
            --
            x:(@) _ pos:pos() "+" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Add, pos) }
            x:(@) _ pos:pos() "-" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Subtract, pos) }
            --
            x:(@) _ pos:pos() "*" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Multiply, pos) }
            x:(@) _ pos:pos() "/" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Divide, pos) }
            x:(@) _ pos:pos() "%" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Modulo, pos) }
            --
            pos:pos() "!" _ x:@ { Expr::UnOp(Box::new(x), UnOp::LogicNot, pos) }
            pos:pos() "~" _ x:@ { Expr::UnOp(Box::new(x), UnOp::BitNot, pos) }
            pos:pos() "-" _ x:@ { Expr::UnOp(Box::new(x), UnOp::Neg, pos) }
            pos:pos() keyword("new") _ id:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::New(TypeName::basic_owned(id.to_owned()), params, pos) }
            --
            expr:(@) _ pos:pos() "[" _ idx:expr() _ "]"
                { Expr::ArrayElem(Box::new(expr), Box::new(idx), pos) }
            expr:(@) _ pos:pos() "." _ ident:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::MethodCall(Box::new(expr), ident, params, pos) }
            expr:(@) _ pos:pos() "." _ ident:ident()
                { Expr::Member(Box::new(expr), ident, pos) }
            expr:(@) _ pos:pos() keyword("as") _ type_:type_()
                { Expr::Cast(type_, Box::new(expr), pos) }
            pos:pos() "[" _ exprs:commasep(<expr()>)_ "]" { Expr::ArrayLit(exprs, None, pos) }
            "(" _ v:expr() _ ")" { v }
            keyword("null") { Expr::Null }
            pos:pos() keyword("this") { Expr::This(pos) }
            pos:pos() keyword("super") { Expr::Super(pos) }
            pos:pos() cons:constant() { Expr::Constant(cons, pos) }
            pos:pos() id:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::Call(id, params, pos) }
            pos:pos() id:ident()
                { Expr::Ident(id, pos) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ternary_op() {
        let expr = lang::expr("3.0 ? 5.0 : 5 + 4", Pos::ZERO).unwrap();
        assert_eq!(
            format!("{:?}", expr),
            "Conditional(Constant(Float(3.0), Pos(0)), Constant(Float(5.0), Pos(6)), BinOp(Constant(Int(5), Pos(12)), \
             Constant(Int(4), Pos(16)), Add, Pos(14)), Pos(4))"
        );
    }

    #[test]
    fn parse_simple_class() {
        let module = lang::module(
            "public class A extends IScriptable {
                private const let m_field: Int32;

                public func GetField() -> Int32 {
                    return this.m_field;
                }
             }",
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", module.entries),
            r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: Owned("A"), base: Some(Owned("IScriptable")), members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), name: Owned("m_field"), pos: Pos(53) }, type_: TypeName { name: Owned("Int32"), arguments: [] } }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public]), name: Owned("GetField"), pos: Pos(104) }, type_: Some(TypeName { name: Owned("Int32"), arguments: [] }), parameters: [], body: Some(Seq { exprs: [Return(Some(Member(This(Pos(165)), Owned("m_field"), Pos(169))), Pos(158))] }) })], pos: Pos(0) })]"#
        );
    }

    #[test]
    fn parse_simple_func() {
        let module = lang::module(
            "public static func GetField(optimum: Uint64) -> Uint64 {
                return this.m_field > optimum ? this.m_field : optimum;
             }",
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", module.entries),
            r#"[Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), name: Owned("GetField"), pos: Pos(0) }, type_: Some(TypeName { name: Owned("Uint64"), arguments: [] }), parameters: [ParameterSource { qualifiers: Qualifiers([]), name: Owned("optimum"), type_: TypeName { name: Owned("Uint64"), arguments: [] } }], body: Some(Seq { exprs: [Return(Some(Conditional(BinOp(Member(This(Pos(80)), Owned("m_field"), Pos(84)), Ident(Owned("optimum"), Pos(95)), Greater, Pos(93)), Member(This(Pos(105)), Owned("m_field"), Pos(109)), Ident(Owned("optimum"), Pos(120)), Pos(103))), Pos(73))] }) })]"#
        );
    }

    #[test]
    fn parse_simple_loop() {
        let stmt = lang::stmt(
            "while i < 1000 {
                this.counter += Object.CONSTANT;
                i += 1;
             }",
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"While(BinOp(Ident(Owned("i"), Pos(6)), Constant(Int(1000), Pos(10)), Less, Pos(8)), Seq { exprs: [BinOp(Member(This(Pos(33)), Owned("counter"), Pos(37)), Member(Ident(Owned("Object"), Pos(49)), Owned("CONSTANT"), Pos(55)), AssignAdd, Pos(46)), BinOp(Ident(Owned("i"), Pos(82)), Constant(Int(1), Pos(87)), AssignAdd, Pos(84))] }, Pos(0))"#
        );
    }

    #[test]
    fn parse_simple_if_else() {
        let stmt = lang::stmt(
            "if this.m_fixBugs {
                this.NoBugs();
             } else {
                this.Bugs();
             }",
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"If(Member(This(Pos(3)), Owned("m_fixBugs"), Pos(7)), Seq { exprs: [MethodCall(This(Pos(36)), Owned("NoBugs"), [], Pos(40))] }, Some(Seq { exprs: [MethodCall(This(Pos(89)), Owned("Bugs"), [], Pos(93))] }), Pos(0))"#
        );
    }

    #[test]
    fn parse_switch_case() {
        let stmt = lang::stmt(
            r#"switch value {
                 case "0":
                 case "1":
                    Log("0 or 1");
                 case "2":
                    break;
                 default:
                    Log("default");
            }"#,
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"Switch(Ident(Owned("value"), Pos(7)), [SwitchCase { matcher: Constant(String(String, "0"), Pos(37)), body: Seq { exprs: [] } }, SwitchCase { matcher: Constant(String(String, "1"), Pos(64)), body: Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "0 or 1"), Pos(93))], Pos(89))] } }, SwitchCase { matcher: Constant(String(String, "2"), Pos(126)), body: Seq { exprs: [Break(Pos(151))] } }], Some(Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "default"), Pos(208))], Pos(204))] }))"#
        );
    }

    #[test]
    fn parse_with_comment() {
        let module = lang::module(
            r#"
            /* this is a multiline comment
               blah blah blah
            */
            class Test {
                private let m_field /* cool stuff */: String;
            }
            "#,
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", module.entries),
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: Owned("Test"), base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: Owned("m_field"), pos: Pos(130) }, type_: TypeName { name: Owned("String"), arguments: [] } })], pos: Pos(101) })]"#
        );
    }

    #[test]
    fn parse_with_line_comment() {
        let module = lang::module(
            r#"
            class Test { // line comment
                // private let m_comment_field: String;
                private let m_field: String;
            }
            "#,
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", module.entries),
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: Owned("Test"), base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: Owned("m_field"), pos: Pos(114) }, type_: TypeName { name: Owned("String"), arguments: [] } })], pos: Pos(13) })]"#
        );
    }

    #[test]
    fn parse_escaped_string() {
        let escaped = lang::escaped_string(
            r#""This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n""#,
            Pos::ZERO,
        );

        assert_eq!(
            escaped,
            Ok(String::from(
                "This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n"
            ))
        );
    }

    #[test]
    fn fail_mangled_string() {
        let mangled = lang::escaped_string(
            r#""These are invalid escape characters: \a \\" \u{1234567}""#,
            Pos::ZERO,
        );

        assert!(mangled.is_err());
    }
}

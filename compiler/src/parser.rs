use std::rc::Rc;
use std::str::FromStr;

use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{BinOp, Constant, Expr, Ident, Literal, Pos, Seq, SourceAst, SwitchCase, TypeName, UnOp};
use redscript::definition::Visibility;
use strum::EnumString;

#[derive(Debug)]
pub enum SourceEntry {
    Class(ClassSource),
    Function(FunctionSource),
    GlobalLet(FieldSource),
}

#[derive(Debug)]
pub struct ClassSource {
    pub qualifiers: Qualifiers,
    pub name: String,
    pub base: Option<String>,
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
    pub name: String,
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
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct Annotation {
    pub name: AnnotationName,
    pub values: Vec<String>,
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

pub fn parse(input: &str) -> Result<Vec<SourceEntry>, ParseError<LineCol>> {
    lang::source(input)
}

peg::parser! {
    grammar lang() for str {
        use peg::ParseLiteral;

        rule _() = quiet!{ ([' ' | '\n' | '\r' | '\t'] / comment() / line_comment())* }
        rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ("," _)) {v}

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
            = pos:position!() "@" ident:ident() _ "(" _ values:commasep(<ident()>) _ ")" {?
                AnnotationName::from_str(&ident).map(|name| {
                    Annotation { name, values, pos: Pos::new(pos) }
                }).map_err(|_| "annotation")
            }

        rule qualifiers() -> Qualifiers = qs:qualifier() ** _ { Qualifiers(qs) }

        rule ident() -> String
            = quiet!{
                x:$(['a'..='z' | 'A'..='Z' | '_']) xs:$(['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*)
                { format!("{}{}", x, xs) }
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
            = name:ident() args:type_args()? { TypeName { name: Ident::new(name), arguments: args.unwrap_or_default() } }
        rule type_args() -> Vec<TypeName> = "<" _ args:commasep(<type_()>) _ ">" { args }

        rule let_type() -> TypeName = ":" _ type_:type_() { type_ }
        rule func_type() -> TypeName = "->" _ type_:type_() { type_ }

        rule initializer() -> Expr<SourceAst> = "=" _ val:expr() { val }

        rule let() -> Expr<SourceAst>
            = pos:position!() keyword("let") _ name:ident() _ type_:let_type()? _ val:initializer()? _ ";"
            { Expr::Declare(Ident::new(name), type_, val.map(Box::new), Pos::new(pos)) }

        rule decl(inner: rule<()>) -> Declaration
            = pos:position!() annotations:(annotation() ** _) _ qualifiers:qualifiers() _ inner() _ name:ident()
            { Declaration { annotations, qualifiers, name, pos: Pos::new(pos) } }

        rule field() -> FieldSource
            = declaration:decl(<keyword("let")>) _ type_:let_type() _ ";"
            { FieldSource { declaration, type_ }}

        pub rule function() -> FunctionSource
            = declaration:decl(<keyword("func")>) _ "(" _ parameters:commasep(<param()>) _ ")" _ type_:func_type()? _ body:function_body()?
            { FunctionSource { declaration, type_, parameters, body } }
        rule function_body() -> Seq<SourceAst>
            = "{" _ body:seq() _ "}" { body }
            / pos:position!() "=" _ expr:expr() { Seq::new(vec![Expr::Return(Some(Box::new(expr)), Pos::new(pos))]) }

        rule param() -> ParameterSource
            = qualifiers:qualifiers() _ name:ident() _ type_:let_type()
            { ParameterSource { qualifiers, name, type_ } }

        rule extends() -> String = keyword("extends") _ name:ident() { name }

        pub rule class() -> ClassSource
            = pos:position!() qualifiers:qualifiers() _ keyword("class") _ name:ident() _ base:extends()? _ "{" _ members:member()**_ _ "}"
            { ClassSource { qualifiers, name, base, members, pos: Pos::new(pos) } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Function(fun) }
            / field:field() { MemberSource::Field(field) }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }
            / field:field() { SourceEntry::GlobalLet(field) }

        pub rule source() -> Vec<SourceEntry> = _ decls:(source_entry() ** _) _ { decls }

        rule switch() -> Expr<SourceAst>
            = keyword("switch") _ matcher:expr() _ "{" _ cases:(case() ** _) _ default:default()? _ "}" _ ";"?
            { Expr::Switch(Box::new(matcher), cases, default) }

        rule case() -> SwitchCase<SourceAst>
            = keyword("case") _ matcher:expr() _ ":" _ body:seq()
            { SwitchCase { matcher, body } }

        rule default() -> Seq<SourceAst>
            = keyword("default") _ ":" _ body:seq() { body }

        rule while_() -> Expr<SourceAst>
            = pos:position!() keyword("while") _ cond:expr() _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::While(Box::new(cond), body, Pos::new(pos)) }

        rule for_() -> Expr<SourceAst>
            = pos:position!() keyword("for") _ ident:ident() _ keyword("in") _ array:expr() _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::ForIn(Ident::new(ident), Box::new(array), body, Pos::new(pos)) }

        rule if_() -> Expr<SourceAst>
            = pos:position!() keyword("if") _ cond:expr() _ "{" _ if_:seq() _ "}" _ else_:else_()? _ ";"?
            { Expr::If(Box::new(cond), if_, else_, Pos::new(pos)) }
        rule else_() -> Seq<SourceAst>
            = keyword("else") _ "{" _ body:seq() _ "}" { body }

        pub rule stmt() -> Expr<SourceAst>
            = while_: while_() { while_ }
            / for_: for_() { for_ }
            / if_: if_() { if_ }
            / switch: switch() { switch }
            / pos:position!() keyword("return") _ val:expr()? _ ";" { Expr::Return(val.map(Box::new), Pos::new(pos)) }
            / pos:position!() keyword("break") _ ";" { Expr::Break(Pos::new(pos)) }
            / let_:let() { let_ }
            / expr:expr() _ ";" { expr }

        pub rule expr() -> Expr<SourceAst> = precedence!{
            x:@ _ pos:position!() "?" _ y:expr() _ ":" _ z:expr()
                { Expr::Conditional(Box::new(x), Box::new(y), Box::new(z), Pos::new(pos)) }
            x:@ _ pos:position!() "=" _ y:(@) { Expr::Assign(Box::new(x), Box::new(y), Pos::new(pos)) }
            x:@ _ pos:position!() "+=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignAdd, Pos::new(pos)) }
            x:@ _ pos:position!() "-=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignSubtract, Pos::new(pos)) }
            x:@ _ pos:position!() "*=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignMultiply, Pos::new(pos)) }
            x:@ _ pos:position!() "/=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignDivide, Pos::new(pos)) }
            --
            x:(@) _ pos:position!() "||" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicOr, Pos::new(pos)) }
            x:(@) _ pos:position!() "&&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicAnd, Pos::new(pos)) }
            x:(@) _ pos:position!() "|" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Or, Pos::new(pos)) }
            x:(@) _ pos:position!() "^" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Xor, Pos::new(pos)) }
            x:(@) _ pos:position!() "&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::And, Pos::new(pos)) }
            --
            x:(@) _ pos:position!() "==" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Equal, Pos::new(pos)) }
            x:(@) _ pos:position!() "!=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::NotEqual, Pos::new(pos)) }
            --
            x:(@) _ pos:position!() "<" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Less, Pos::new(pos)) }
            x:(@) _ pos:position!() "<=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LessEqual, Pos::new(pos)) }
            x:(@) _ pos:position!() ">" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Greater, Pos::new(pos)) }
            x:(@) _ pos:position!() ">=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::GreaterEqual, Pos::new(pos)) }
            --
            x:(@) _ pos:position!() "+" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Add, Pos::new(pos)) }
            x:(@) _ pos:position!() "-" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Subtract, Pos::new(pos)) }
            --
            x:(@) _ pos:position!() "*" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Multiply, Pos::new(pos)) }
            x:(@) _ pos:position!() "/" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Divide, Pos::new(pos)) }
            x:(@) _ pos:position!() "%" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Modulo, Pos::new(pos)) }
            --
            pos:position!() "!" _ x:@ { Expr::UnOp(Box::new(x), UnOp::LogicNot, Pos::new(pos)) }
            pos:position!() "~" _ x:@ { Expr::UnOp(Box::new(x), UnOp::BitNot, Pos::new(pos)) }
            pos:position!() "-" _ x:@ { Expr::UnOp(Box::new(x), UnOp::Neg, Pos::new(pos)) }
            pos:position!() keyword("new") _ id:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::New(TypeName::basic_owned(Rc::new(id)), params, Pos::new(pos)) }
            --
            expr:(@) _ pos:position!() "[" _ idx:expr() _ "]"
                { Expr::ArrayElem(Box::new(expr), Box::new(idx), Pos::new(pos)) }
            expr:(@) _ pos:position!() "." _ ident:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::MethodCall(Box::new(expr), Ident::new(ident), params, Pos::new(pos)) }
            expr:(@) _ pos:position!() "." _ ident:ident()
                { Expr::Member(Box::new(expr), Ident::new(ident), Pos::new(pos)) }
            expr:(@) _ pos:position!() keyword("as") _ type_:type_()
                { Expr::Cast(type_, Box::new(expr), Pos::new(pos)) }
            pos:position!() "[" _ exprs:commasep(<expr()>)_ "]" { Expr::ArrayLit(exprs, None, Pos::new(pos)) }
            "(" _ v:expr() _ ")" { v }
            keyword("null") { Expr::Null }
            pos:position!() keyword("this") { Expr::This(Pos::new(pos)) }
            pos:position!() keyword("super") { Expr::Super(Pos::new(pos)) }
            pos:position!() cons:constant() { Expr::Constant(cons, Pos::new(pos)) }
            pos:position!() id:ident() _ "(" _ params:commasep(<expr()>) _ ")"
                { Expr::Call(Ident::new(id), params, Pos::new(pos)) }
            pos:position!() id:ident()
                { Expr::Ident(Ident::new(id), Pos::new(pos)) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ternary_op() {
        let expr = lang::expr("3.0 ? 5.0 : 5 + 4").unwrap();
        assert_eq!(
            format!("{:?}", expr),
            "Conditional(Constant(Float(3.0), Pos(0)), Constant(Float(5.0), Pos(6)), BinOp(Constant(Int(5), Pos(12)), \
             Constant(Int(4), Pos(16)), Add, Pos(14)), Pos(4))"
        );
    }

    #[test]
    fn parse_simple_class() {
        let class = lang::source(
            "public class A extends IScriptable {
                private const let m_field: Int32;

                public func GetField() -> Int32 {
                    return this.m_field;
                }
             }",
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", class),
            r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: "A", base: Some("IScriptable"), members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), name: "m_field", pos: Pos(53) }, type_: TypeName { name: Owned("Int32"), arguments: [] } }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public]), name: "GetField", pos: Pos(104) }, type_: Some(TypeName { name: Owned("Int32"), arguments: [] }), parameters: [], body: Some(Seq { exprs: [Return(Some(Member(This(Pos(165)), Owned("m_field"), Pos(169))), Pos(158))] }) })], pos: Pos(0) })]"#
        );
    }

    #[test]
    fn parse_simple_func() {
        let class = lang::source(
            "public static func GetField(optimum: Uint64) -> Uint64 {
                return this.m_field > optimum ? this.m_field : optimum;
             }",
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", class),
            r#"[Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), name: "GetField", pos: Pos(0) }, type_: Some(TypeName { name: Owned("Uint64"), arguments: [] }), parameters: [ParameterSource { qualifiers: Qualifiers([]), name: "optimum", type_: TypeName { name: Owned("Uint64"), arguments: [] } }], body: Some(Seq { exprs: [Return(Some(Conditional(BinOp(Member(This(Pos(80)), Owned("m_field"), Pos(84)), Ident(Owned("optimum"), Pos(95)), Greater, Pos(93)), Member(This(Pos(105)), Owned("m_field"), Pos(109)), Ident(Owned("optimum"), Pos(120)), Pos(103))), Pos(73))] }) })]"#
        );
    }

    #[test]
    fn parse_simple_loop() {
        let stmt = lang::stmt(
            "while i < 1000 {
                this.counter += Object.CONSTANT;
                i += 1;
             }",
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
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"Switch(Ident(Owned("value"), Pos(7)), [SwitchCase { matcher: Constant(String(String, "0"), Pos(37)), body: Seq { exprs: [] } }, SwitchCase { matcher: Constant(String(String, "1"), Pos(64)), body: Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "0 or 1"), Pos(93))], Pos(89))] } }, SwitchCase { matcher: Constant(String(String, "2"), Pos(126)), body: Seq { exprs: [Break(Pos(151))] } }], Some(Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "default"), Pos(208))], Pos(204))] }))"#
        );
    }

    #[test]
    fn parse_with_comment() {
        let stmt = lang::source(
            r#"
            /* this is a multiline comment
               blah blah blah
            */
            class Test {
                private let m_field /* cool stuff */: String;
            }
            "#,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: "Test", base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: "m_field", pos: Pos(130) }, type_: TypeName { name: Owned("String"), arguments: [] } })], pos: Pos(101) })]"#
        );
    }

    #[test]
    fn parse_with_line_comment() {
        let stmt = lang::source(
            r#"
            class Test { // line comment
                // private let m_comment_field: String;
                private let m_field: String;
            }
            "#,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", stmt),
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: "Test", base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: "m_field", pos: Pos(114) }, type_: TypeName { name: Owned("String"), arguments: [] } })], pos: Pos(13) })]"#
        );
    }

    #[test]
    fn parse_escaped_string() {
        let escaped = lang::escaped_string(r#""This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n""#);

        assert_eq!(escaped, Ok(String::from(  "This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n")));
    }

    #[test]
    fn fail_mangled_string() {
        let mangled = lang::escaped_string(r#""These are invalid escape characters: \a \\" \u{1234567}""#);

        assert!(mangled.is_err());
    }
}

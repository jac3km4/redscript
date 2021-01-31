use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{BinOp, Expr, Ident, LiteralType, Seq, SwitchCase, TypeName, UnOp};
use redscript::definition::Visibility;

#[derive(Debug)]
pub enum SourceEntry {
    Class(ClassSource),
    Function(FunctionSource),
}
#[derive(Debug)]
pub struct ClassSource {
    pub qualifiers: Qualifiers,
    pub name: String,
    pub base: Option<String>,
    pub members: Vec<MemberSource>,
}

#[derive(Debug)]
pub enum MemberSource {
    Function(FunctionSource),
    Field(Declaration, TypeName),
}

#[derive(Debug)]
pub struct FunctionSource {
    pub declaration: Declaration,
    pub type_: Option<TypeName>,
    pub parameters: Vec<ParameterSource>,
    pub body: Option<Seq>,
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
}

#[derive(Debug)]
pub struct Annotation {
    pub name: String,
    pub value: String,
}

impl Annotation {
    pub fn get_replace_target(&self) -> Option<&str> {
        if self.name == "replaceMethod" {
            Some(&self.value)
        } else {
            None
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<SourceEntry>, ParseError<LineCol>> {
    lang::source(input)
}

peg::parser! {
    grammar lang() for str {
        use peg::ParseLiteral;

        rule _() = ([' ' | '\n' | '\r' | '\t'] / comment())*
        rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ("," _)) {v}

        rule comment_start() = "/*"
        rule comment_end() = "*/"
        rule comment_content()
            = comment() / (!comment_start() !comment_end() [_])
        rule comment()
            = comment_start() comment_content()* comment_end()

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

        rule literal_type() -> LiteralType
            = "n" { LiteralType::Name }
            / "r" { LiteralType::Resource }
            / "t" { LiteralType::TweakDbId }

        rule annotation() -> Annotation
            = "@" name:ident() _ "(" _ value:ident() _ ")" { Annotation { name, value } }

        rule qualifiers() -> Qualifiers = qs:qualifier() ** _ { Qualifiers(qs) }

        rule ident() -> String
            = x:$(['a'..='z' | 'A'..='Z' | '_']) xs:$(['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*)
            { format!("{}{}", x, xs) }

        rule keyword(id: &'static str) -> () =
            ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']

        rule number() -> Expr
            = n:$(['0'..='9' | '.']+) postfix:$(['u'])?
            { if n.contains(".") { Expr::FloatLit(n.parse().unwrap()) }
              else if postfix == Some("u") { Expr::UintLit(n.parse().unwrap()) }
              else { Expr::IntLit(n.parse().unwrap()) }
            }

        rule seq() -> Seq = exprs:(stmt() ** _) { Seq::new(exprs) }

        rule type_() -> TypeName
            = name:ident() args:type_args()? { TypeName { name, arguments: args.unwrap_or(vec![]) } }
        rule type_args() -> Vec<TypeName> = "<" _ args:commasep(<type_()>) _ ">" { args }

        rule let_type() -> TypeName = ":" _ type_:type_() { type_ }
        rule func_type() -> TypeName = "->" _ type_:type_() { type_ }

        rule decl(inner: rule<()>) -> Declaration
            = annotations:(annotation() ** _) _ qualifiers:qualifiers() _ inner() _ name:ident()
            { Declaration { annotations, qualifiers, name } }

        rule let() -> Expr
            = keyword("let") _ name:ident() _ type_:let_type()? _ val:initializer()? _ ";"
            { Expr::Declare(Ident::new(name), type_, val.map(Box::new)) }

        rule initializer() -> Expr = "=" _ val:expr() { val }

        pub rule function() -> FunctionSource
            = declaration:decl(<keyword("func")>) _ "(" _ parameters:commasep(<param()>) _ ")" _ type_:func_type()? _ body:function_body()?
            { FunctionSource { declaration, type_, parameters, body } }
        rule function_body() -> Seq = "{" _ body:seq() _ "}" { body }

        rule param() -> ParameterSource
            = qualifiers:qualifiers() _ name:ident() _ type_:let_type()
            { ParameterSource { qualifiers, name, type_ } }

        rule extends() -> String = keyword("extends") _ name:ident() { name }

        pub rule class() -> ClassSource
            = qualifiers:qualifiers() _ keyword("class") _ name:ident() _ base:extends()? _ "{" _ members:member()**_ _ "}"
            { ClassSource { qualifiers, name, base, members } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Function(fun) }
            / decl:decl(<keyword("let")>) _ type_:let_type() _ ";" { MemberSource::Field(decl, type_) }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }

        pub rule source() -> Vec<SourceEntry> = _ decls:(source_entry() ** _) _ { decls }

        rule switch() -> Expr
            = keyword("switch") _ matcher:expr() _ "{" _ cases:(case() ** _) _ default:default()? _ "}" _ ";"?
            { Expr::Switch(Box::new(matcher), cases, default) }

        rule case() -> SwitchCase
            = keyword("case") _ matcher:expr() _ ":" _ body:seq()
            { SwitchCase(matcher, body) }

        rule default() -> Seq
            = keyword("default") _ ":" _ body:seq() { body }

        rule while_() -> Expr
            = keyword("while") _ cond:expr() _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::While(Box::new(cond), body) }

        rule if_() -> Expr
            = keyword("if") _ cond:expr() _ "{" _ if_:seq() _ "}" _ else_:else_()? ";"?
            { Expr::If(Box::new(cond), if_, else_) }
        rule else_() -> Seq
            = keyword("else") _ "{" _ body:seq() _ "}" { body }

        pub rule stmt() -> Expr
            = while_: while_() { while_ }
            / if_: if_() { if_ }
            / switch: switch() { switch }
            / keyword("return") _ val:expr()? ";" { Expr::Return(val.map(Box::new)) }
            / keyword("break") _ ";" { Expr::Break }
            / let_:let() { let_ }
            / expr:expr() _ ";" { expr }

        pub rule expr() -> Expr = precedence!{
            x:@ _ "?" _ y:expr() _ ":" _ z:expr() { Expr::Conditional(Box::new(x), Box::new(y), Box::new(z)) }
            x:@ _ "=" _ y:(@) { Expr::Assign(Box::new(x), Box::new(y)) }
            x:@ _ "+=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignAdd) }
            x:@ _ "-=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignSub) }
            x:@ _ "*=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignMultiply) }
            x:@ _ "/=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignDivide) }
            --
            x:(@) _ "||" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicOr) }
            x:(@) _ "&&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicAnd) }
            x:(@) _ "|" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Or) }
            x:(@) _ "^" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Xor) }
            x:(@) _ "&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::And) }
            --
            x:(@) _ "==" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Equal) }
            x:(@) _ "!=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::NotEqual) }
            --
            x:(@) _ "<" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Less) }
            x:(@) _ "<=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LessEqual) }
            x:(@) _ ">" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Greater) }
            x:(@) _ ">=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::GreaterEqual) }
            --
            x:(@) _ "+" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Add) }
            x:(@) _ "-" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Subtract) }
            --
            x:(@) _ "*" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Multiply) }
            x:(@) _ "/" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Divide) }
            x:(@) _ "%" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Modulo) }
            --
            "!" _ x:@ { Expr::UnOp(Box::new(x), UnOp::LogicNot) }
            "~" _ x:@ { Expr::UnOp(Box::new(x), UnOp::BitNot) }
            "-" _ x:@ { Expr::UnOp(Box::new(x), UnOp::Neg) }
            keyword("new") _ id:ident() _ "(" _ params:commasep(<expr()>) _ ")" { Expr::New(Ident::new(id), params) }
            --
            expr:(@) _ "[" _ idx:expr() _ "]" { Expr::ArrayElem(Box::new(expr), Box::new(idx)) }
            expr:(@) _ "." _ ident:ident() _ "(" _ params:commasep(<expr()>) _ ")" { Expr::MethodCall(Box::new(expr), Ident::new(ident), params) }
            expr:(@) _ "." _ ident:ident() { Expr::Member(Box::new(expr), Ident::new(ident)) }
            expr:(@) _ keyword("as") _ type_:type_() { Expr::Cast(type_, Box::new(expr)) }
            "(" _ v:expr() _ ")" { v }
            keyword("true") { Expr::True }
            keyword("false") { Expr::False }
            keyword("null") { Expr::Null }
            keyword("this") { Expr::This }
            lit_type:literal_type()? "\"" str:$((!['"'] [_])*) "\"" { Expr::StringLit(lit_type.unwrap_or(LiteralType::String), str.to_owned()) }
            n:number() { n }
            id:ident() _ "(" _ params:commasep(<expr()>) _ ")" { Expr::Call(Ident::new(id), params) }
            id:ident() { Expr::Ident(Ident::new(id)) }
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
            "Conditional(FloatLit(3.0), FloatLit(5.0), BinOp(IntLit(5), IntLit(4), Add))"
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
            r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: "A", base: Some("IScriptable"), members: [Field(Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), name: "m_field" }, TypeName { name: "Int32", arguments: [] }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public]), name: "GetField" }, type_: Some(TypeName { name: "Int32", arguments: [] }), parameters: [], body: Some(Seq { exprs: [Return(Some(Member(This, Ident("m_field"))))] }) })] })]"#
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
            r#"[Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), name: "GetField" }, type_: Some(TypeName { name: "Uint64", arguments: [] }), parameters: [ParameterSource { qualifiers: Qualifiers([]), name: "optimum", type_: TypeName { name: "Uint64", arguments: [] } }], body: Some(Seq { exprs: [Return(Some(Conditional(BinOp(Member(This, Ident("m_field")), Ident(Ident("optimum")), Greater), Member(This, Ident("m_field")), Ident(Ident("optimum")))))] }) })]"#
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
            r#"While(BinOp(Ident(Ident("i")), IntLit(1000), Less), Seq { exprs: [BinOp(Member(This, Ident("counter")), Member(Ident(Ident("Object")), Ident("CONSTANT")), AssignAdd), BinOp(Ident(Ident("i")), IntLit(1), AssignAdd)] })"#
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
            r#"If(Member(This, Ident("m_fixBugs")), Seq { exprs: [MethodCall(This, Ident("NoBugs"), [])] }, Some(Seq { exprs: [MethodCall(This, Ident("Bugs"), [])] }))"#
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
            r#"Switch(Ident(Ident("value")), [SwitchCase(StringLit(String, "0"), Seq { exprs: [] }), SwitchCase(StringLit(String, "1"), Seq { exprs: [Call(Ident("Log"), [StringLit(String, "0 or 1")])] }), SwitchCase(StringLit(String, "2"), Seq { exprs: [Break] })], Some(Seq { exprs: [Call(Ident("Log"), [StringLit(String, "default")])] }))"#
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
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: "Test", base: None, members: [Field(Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: "m_field" }, TypeName { name: "String", arguments: [] })] })]"#
        );
    }
}

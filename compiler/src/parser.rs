use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{BinOp, Expr, Ident, Seq, TypeName, UnOp};
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
    Field(Declaration),
}

#[derive(Debug)]
pub struct FunctionSource {
    pub declaration: Declaration,
    pub parameters: Vec<Declaration>,
    pub body: Option<Seq>,
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
    pub type_: TypeName,
    pub name: String,
}

#[derive(Debug)]
pub struct Annotation {
    pub name: String,
    pub value: String,
}

impl Annotation {
    pub fn get_insert_target(&self) -> Option<&str> {
        if self.name == "insert" {
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
        rule _() = [' ' | '\n' | '\r']*
        rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ("," _)) {v}

        rule qualifier() -> Qualifier
            = "public" { Qualifier::Public }
            / "protected" { Qualifier::Protected }
            / "private" { Qualifier::Private }
            / "static" { Qualifier::Static }
            / "final" { Qualifier::Final }
            / "const" { Qualifier::Const }
            / "native" { Qualifier::Native }
            / "exec" { Qualifier::Exec }
            / "cb" { Qualifier::Callback }

        rule annotation() -> Annotation
            = "@" name:ident() _ "(" _ value:ident() _ ")" { Annotation { name, value } }

        rule qualifiers() -> Qualifiers = qs:qualifier() ** _ { Qualifiers(qs) }

        rule ident() -> String
            = x:$(['a'..='z' | 'A'..='Z' | '_']) xs:$(['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*)
            { format!("{}{}", x, xs) }

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

        rule decl() -> Declaration
            = annotations:(annotation() ** _) _ qualifiers:qualifiers() _ type_:type_() _ name:ident()
            { Declaration { annotations, qualifiers, type_, name } }

        pub rule function() -> FunctionSource
            = declaration:decl() _ "(" parameters:commasep(<decl()>) ")" _ body:function_body()?
            { FunctionSource { declaration, parameters, body } }
        rule function_body() -> Seq = "{" _ body:seq() _ "}" { body }

        rule extends() -> String = "extends" _ name:ident() { name }

        pub rule class() -> ClassSource
            = qualifiers:qualifiers() _ "class" _ name:ident() _ base:extends()? _ "{" _ members:member()**_ _ "}"
            { ClassSource { qualifiers, name, base, members } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Function(fun) }
            / decl:decl() _ ";" { MemberSource::Field(decl) }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }

        pub rule source() -> Vec<SourceEntry> = _ decls:(source_entry() ** _) _ { decls }

        rule initializer() -> Expr = "=" _ val:expr() { val }

        rule while_() -> Expr
            = "while" _ "(" _ cond:expr() _ ")" _ "{" _ body:seq() _ "}" _ ";"?
            { Expr::While(Box::new(cond), body) }

        rule if_() -> Expr
            = "if" _ "(" _ cond:expr() _ ")" _ "{" _ if_:seq() _ "}" _ else_:else_()? ";"?
            { Expr::If(Box::new(cond), if_, else_) }
        rule else_() -> Seq
            = "else" _ "{" _ body:seq() _ "}" { body }

        pub rule stmt() -> Expr
            = while_: while_() { while_ }
            / if_: if_() { if_ }
            / "return" _ val:expr()? ";" { Expr::Return(val.map(Box::new)) }
            / "break" _ ";" { Expr::Break }
            / decl:decl() _ val:initializer()? _ ";" { Expr::Declare(decl.type_, Ident::new(decl.name), val.map(Box::new)) }
            / expr:expr() _ ";" { expr }

        pub rule expr() -> Expr = precedence!{
            x:@ _ "?" _ y:expr() _ ":" _ z:expr() { Expr::Conditional(Box::new(x), Box::new(y), Box::new(z)) }
            x:@ _ "=" _ y:(@) { Expr::Assign(Box::new(x), Box::new(y)) }
            x:@ _ "+=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignAdd) }
            x:@ _ "-=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignSub) }
            x:@ _ "*=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignMul) }
            x:@ _ "/=" _ y:(@) { Expr::BinOp(Box::new(x), Box::new(y), BinOp::AssignDiv) }
            --
            x:(@) _ "||" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicOr) }
            x:(@) _ "&&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LogicAnd) }
            x:(@) _ "|" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Or) }
            x:(@) _ "^" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Xor) }
            x:(@) _ "&" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::And) }
            --
            x:(@) _ "==" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Eq) }
            x:(@) _ "!=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Neq) }
            --
            x:(@) _ "<" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Less) }
            x:(@) _ "<=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::LessOrEqual) }
            x:(@) _ ">" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Greater) }
            x:(@) _ ">=" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::GreaterOrEqual) }
            --
            x:(@) _ "+" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Add) }
            x:(@) _ "-" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Sub) }
            --
            x:(@) _ "*" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Mul) }
            x:(@) _ "/" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Div) }
            x:(@) _ "%" _ y:@ { Expr::BinOp(Box::new(x), Box::new(y), BinOp::Mod) }
            --
            "!" _ x:@ { Expr::UnOp(Box::new(x), UnOp::LogicNot) }
            "~" _ x:@ { Expr::UnOp(Box::new(x), UnOp::BitNot) }
            "-" _ x:@ { Expr::UnOp(Box::new(x), UnOp::Neg) }
            "new" _ id:ident() _ "(" _ params:commasep(<expr()>) _ ")" { Expr::New(Ident::new(id), params) }
            --
            expr:(@) _ "[" _ idx:expr() _ "]" { Expr::ArrayElem(Box::new(expr), Box::new(idx)) }
            expr:(@) _ "." _ ident:ident() _ "(" _ params:commasep(<expr()>) _ ")" { Expr::MethodCall(Box::new(expr), Ident::new(ident), params) }
            expr:(@) _ "." _ ident:ident() { Expr::Member(Box::new(expr), Ident::new(ident)) }
            "cast" _ "<" _ type_:type_() _ ">" _ "(" _ expr:expr() _ ")" { Expr::Cast(type_, Box::new(expr)) }
            "(" _ v:expr() _ ")" { v }
            "true" { Expr::True }
            "false" { Expr::False }
            "null" { Expr::Null }
            "this" { Expr::This }
            "\"" str:$((!['"'] [_])*) "\"" { Expr::StringLit(str.to_owned()) }
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
                private const int32 m_field;

                public static int32 GetField() {
                    return m_field;
                }
             }",
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", class),
            r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: "A", base: Some("IScriptable"), members: [Field(Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), type_: TypeName { name: "int32", arguments: [] }, name: "m_field" }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), type_: TypeName { name: "int32", arguments: [] }, name: "GetField" }, parameters: [], body: Some(Seq { exprs: [Return(Some(Ident(Ident("m_field"))))] }) })] })]"#
        );
    }

    #[test]
    fn parse_simple_loop() {
        let stmt = lang::stmt(
            "while (i < 1000) {
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
            "if (this.m_fixBugs) {
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
}

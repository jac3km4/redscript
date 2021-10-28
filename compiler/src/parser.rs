use std::str::FromStr;

use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{BinOp, Constant, Expr, Ident, Literal, Pos, Seq, SourceAst, Span, SwitchCase, TypeName, UnOp};
use redscript::definition::Visibility;
use redscript::Ref;
use strum::EnumString;

use crate::source_map::File;
use crate::symbol::{Import, ModulePath};

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
    Enum(EnumSource),
}

impl SourceEntry {
    pub fn annotations(&self) -> &[Annotation] {
        match self {
            SourceEntry::Function(fun) => &fun.declaration.annotations,
            SourceEntry::GlobalLet(field) => &field.declaration.annotations,
            _ => &[],
        }
    }

    pub fn conditionals(&self) -> impl Iterator<Item = &Expr<SourceAst>> {
        self.annotations()
            .iter()
            .filter(|ann| ann.kind == AnnotationKind::If)
            .filter_map(|ann| ann.args.first())
    }
}

#[derive(Debug)]
pub struct ClassSource {
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub base: Option<Ident>,
    pub members: Vec<MemberSource>,
    pub span: Span,
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
    pub span: Span,
}

#[derive(Debug)]
pub struct ParameterSource {
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub type_: TypeName,
}

#[derive(Debug)]
pub struct EnumSource {
    pub name: Ident,
    pub members: Vec<EnumMember>,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumMember {
    pub name: Ident,
    pub value: i64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Qualifier {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
    Const,
    Native,
    Exec,
    Callback,
    Out,
    Optional,
    Quest,
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
    pub span: Span,
}

#[derive(Debug)]
pub struct Annotation {
    pub kind: AnnotationKind,
    pub args: Vec<Expr<SourceAst>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum AnnotationKind {
    ReplaceMethod,
    WrapMethod,
    ReplaceGlobal,
    AddMethod,
    AddField,
    If,
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
            / keyword("abstract") { Qualifier::Abstract }
            / keyword("static") { Qualifier::Static }
            / keyword("final") { Qualifier::Final }
            / keyword("const") { Qualifier::Const }
            / keyword("native") { Qualifier::Native }
            / keyword("exec") { Qualifier::Exec }
            / keyword("cb") { Qualifier::Callback }
            / keyword("out") { Qualifier::Out }
            / keyword("opt") { Qualifier::Optional }
            / keyword("quest") { Qualifier::Quest }

        rule literal_type() -> Literal
            = "n" { Literal::Name }
            / "r" { Literal::Resource }
            / "t" { Literal::TweakDbId }

        rule annotation() -> Annotation
            = pos:pos() "@" ident:ident() _ "(" _ args:commasep(<expr()>) _ ")" end:pos() {?
                AnnotationKind::from_str(ident.as_ref()).map(|kind| {
                    Annotation { kind, args, span: Span::new(pos, end) }
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
            = n:$(['0'..='9' | '.']+) postfix:$(['u' | 'l' | 'd'])?
            {? if postfix == Some("d") { n.parse::<f64>().or(Err("valid double")).map(Constant::F64) }
               else if n.contains('.') { n.parse::<f32>().or(Err("valid float")).map(Constant::F32) }
               else if postfix == Some("l") { n.parse::<i64>().or(Err("valid 64-bit int")).map(Constant::I64) }
               else if postfix == Some("u") { n.parse::<u32>().or(Err("valid 32-bit uint")).map(Constant::U32) }
               else { n.parse::<i32>().or(Err("valid 32-bit int")).map(Constant::I32) }
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

        rule string_contents() -> String
            = s:escaped_char()* { s.join("") }

        pub rule escaped_string() -> String
            = "\"" s:string_contents() "\"" { s }

        rule interpolation() -> Expr<SourceAst>
            = r#"\("# _ expr:expr() _ ")" { expr }

        rule string_part() -> (Expr<SourceAst>, Ref<String>)
            = e:interpolation() s:string_contents() { (e, Ref::new(s)) }

        pub rule interpolated_string() -> (Ref<String>, Vec<(Expr<SourceAst>, Ref<String>)>)
            = "s\""  prefix:string_contents() parts:string_part()* "\"" { (Ref::new(prefix), parts) }

        rule constant() -> Constant
            = keyword("true") { Constant::Bool(true) }
            / keyword("false") { Constant::Bool(false) }
            / n:number() { n }
            / type_:literal_type()? str:escaped_string()
                { Constant::String(type_.unwrap_or(Literal::String), Ref::new(str)) }

        rule seq() -> Seq<SourceAst> = exprs:(stmt() ** _) { Seq::new(exprs) }

        rule type_() -> TypeName
            = name:ident() args:type_args()? { TypeName { name, arguments: args.unwrap_or_default() } }
        rule type_args() -> Vec<TypeName> = "<" _ args:commasep(<type_()>) _ ">" { args }

        rule let_type() -> TypeName = ":" _ type_:type_() { type_ }
        rule func_type() -> TypeName = "->" _ type_:type_() { type_ }

        rule initializer() -> Expr<SourceAst> = "=" _ val:expr() { val }

        rule let() -> Expr<SourceAst>
            = pos:pos() keyword("let") _ name:ident() _ type_:let_type()? _ val:initializer()? _ ";" end:pos()
            { Expr::Declare(name, type_, val.map(Box::new), Span::new(pos, end)) }

        rule decl(inner: rule<()>) -> Declaration
            = pos:pos() annotations:(annotation() ** _) _ qualifiers:qualifiers() _ inner() _ name:ident() end:pos()
            { Declaration { annotations, qualifiers, name, span: Span::new(pos, end) } }

        rule field() -> FieldSource
            = declaration:decl(<keyword("let")>) _ type_:let_type() _ ";"
            { FieldSource { declaration, type_ }}

        pub rule function() -> FunctionSource
            = pos:pos() declaration:decl(<keyword("func")>) _ "(" _ parameters:commasep(<param()>) _ ")" _ type_:func_type()? _ body:function_body()? end:pos()
            { FunctionSource { declaration, type_, parameters, body, span: Span::new(pos, end) } }
        rule function_body() -> Seq<SourceAst>
            = "{" _ body:seq() _ "}" { body }
            / pos:pos() "=" _ expr:expr() _ ";"? end:pos() { Seq::new(vec![Expr::Return(Some(Box::new(expr)), Span::new(pos, end))]) }

        rule param() -> ParameterSource
            = qualifiers:qualifiers() _ name:ident() _ type_:let_type()
            { ParameterSource { qualifiers, name, type_ } }

        rule extends() -> Ident = keyword("extends") _ name:ident() { name }

        pub rule class() -> ClassSource
            = pos:pos() qualifiers:qualifiers() _ keyword("class") _ name:ident() _ base:extends()? _ "{" _ members:member()**_ _ "}" end:pos()
            { ClassSource { qualifiers, name, base, members, span: Span::new(pos, end) } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Function(fun) }
            / field:field() { MemberSource::Field(field) }

        pub rule enum_() -> EnumSource
            = pos:pos() keyword("enum") _ name:ident() _ "{" _ members:commasep(<enum_member()>) _ ","? _ "}" end:pos()
            { EnumSource { name, members, span: Span::new(pos, end) } }

        rule enum_member() -> EnumMember
            = name:ident() _ "=" _ value:number()
            {? match value {
                 Constant::I32(value) => Ok(EnumMember { name, value: value.into() }),
                 Constant::I64(value) => Ok(EnumMember { name, value }),
                 _ => Err("int")
               }
            }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }
            / field:field() { SourceEntry::GlobalLet(field) }
            / enum_:enum_() { SourceEntry::Enum(enum_) }

        rule import() -> Import
            = pos:pos() keyword("import") _ parts: dotsep(<ident()>) _ "." _ "*" end:pos()
                { Import::All(ModulePath::new(parts), Span::new(pos, end)) }
            / pos:pos() keyword("import") _ parts: dotsep(<ident()>) _ "." _ "{" _ names:commasep(<ident()>) _ "}" end:pos()
                { Import::Selected(ModulePath::new(parts), names, Span::new(pos, end)) }
            / pos:pos() keyword("import") _ parts: dotsep(<ident()>) end:pos()
                { Import::Exact(ModulePath::new(parts), Span::new(pos, end)) }

        rule module_path() -> ModulePath  =
            keyword("module") _ parts:dotsep(<ident()>) { ModulePath { parts } }

        pub rule module() -> SourceModule =
            _ path:module_path()? _ imports:(import() ** _) _ entries:(source_entry() ** _) _
            { SourceModule { path, imports, entries } }

        rule switch() -> Expr<SourceAst>
            = pos:pos() keyword("switch") _ matcher:expr() _ "{" _ cases:(case() ** _) _ default:default()? _ "}" _ ";"? end:pos()
            { Expr::Switch(Box::new(matcher), cases, default, Span::new(pos, end)) }

        rule case() -> SwitchCase<SourceAst>
            = keyword("case") _ matcher:expr() _ ":" _ body:seq()
            { SwitchCase { matcher, body } }

        rule default() -> Seq<SourceAst>
            = keyword("default") _ ":" _ body:seq() { body }

        rule while_() -> Expr<SourceAst>
            = pos:pos() keyword("while") _ cond:expr() _ "{" _ body:seq() _ "}" _ ";"? end:pos()
            { Expr::While(Box::new(cond), body, Span::new(pos, end)) }

        rule for_() -> Expr<SourceAst>
            = pos:pos() keyword("for") _ ident:ident() _ keyword("in") _ array:expr() _ "{" _ body:seq() _ "}" _ ";"? end:pos()
            { Expr::ForIn(ident, Box::new(array), body, Span::new(pos, end)) }

        rule if_() -> Expr<SourceAst>
            = pos:pos() keyword("if") _ cond:expr() _ "{" _ if_:seq() _ "}" _ else_:else_()? _ ";"? end:pos()
            { Expr::If(Box::new(cond), if_, else_, Span::new(pos, end)) }
        rule else_() -> Seq<SourceAst>
            = keyword("else") _ "{" _ body:seq() _ "}" { body }

        pub rule stmt() -> Expr<SourceAst>
            = while_: while_() { while_ }
            / for_: for_() { for_ }
            / if_: if_() { if_ }
            / switch: switch() { switch }
            / pos:pos() keyword("return") _ val:expr()? _ ";" end:pos() { Expr::Return(val.map(Box::new), Span::new(pos, end)) }
            / pos:pos() keyword("break") _ ";" end:pos() { Expr::Break(Span::new(pos, end)) }
            / let_:let() { let_ }
            / expr:expr() _ ";" { expr }

        pub rule expr() -> Expr<SourceAst> = precedence!{
            x:@ _ "?" _ y:expr() _ ":" _ z:expr() {
                let span = x.span().merge(z.span());
                Expr::Conditional(Box::new(x), Box::new(y), Box::new(z), span)
            }
            x:@ _ "=" _ y:(@) {
                let span = x.span().merge(y.span());
                Expr::Assign(Box::new(x), Box::new(y), span)
            }
            x:@ _ pos:pos() "+=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignAdd) }
            x:@ _ pos:pos() "-=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignSubtract) }
            x:@ _ pos:pos() "*=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignMultiply) }
            x:@ _ pos:pos() "/=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignDivide) }
            x:@ _ pos:pos() "|=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignOr) }
            x:@ _ pos:pos() "&=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignAnd) }
            --
            x:(@) _ pos:pos() "||" end:pos() _ y:@ { binop(x, y, BinOp::LogicOr) }
            x:(@) _ pos:pos() "&&" end:pos() _ y:@ { binop(x, y, BinOp::LogicAnd) }
            x:(@) _ pos:pos() "|" end:pos() _ y:@ { binop(x, y, BinOp::Or) }
            x:(@) _ pos:pos() "^" end:pos() _ y:@ { binop(x, y, BinOp::Xor) }
            x:(@) _ pos:pos() "&" end:pos() _ y:@ { binop(x, y, BinOp::And) }
            --
            x:(@) _ pos:pos() "==" end:pos() _ y:@ { binop(x, y, BinOp::Equal) }
            x:(@) _ pos:pos() "!=" end:pos() _ y:@ { binop(x, y, BinOp::NotEqual) }
            --
            x:(@) _ pos:pos() "<" end:pos() _ y:@ { binop(x, y, BinOp::Less) }
            x:(@) _ pos:pos() "<=" end:pos() _ y:@ { binop(x, y, BinOp::LessEqual) }
            x:(@) _ pos:pos() ">" end:pos() _ y:@ { binop(x, y, BinOp::Greater) }
            x:(@) _ pos:pos() ">=" end:pos() _ y:@ { binop(x, y, BinOp::GreaterEqual) }
            --
            x:(@) _ pos:pos() "+" end:pos() _ y:@ { binop(x, y, BinOp::Add) }
            x:(@) _ pos:pos() "-" end:pos() _ y:@ { binop(x, y, BinOp::Subtract) }
            --
            x:(@) _ pos:pos() "*" end:pos() _ y:@ { binop(x, y, BinOp::Multiply) }
            x:(@) _ pos:pos() "/" end:pos() _ y:@ { binop(x, y, BinOp::Divide) }
            x:(@) _ pos:pos() "%" end:pos() _ y:@ { binop(x, y, BinOp::Modulo) }
            --
            "!" _ expr:@ { unop(expr, UnOp::LogicNot) }
            "~" _ expr:@ { unop(expr, UnOp::BitNot) }
            "-" _ expr:@ { unop(expr, UnOp::Neg) }

            pos:pos() keyword("new") _ id:ident() _ "(" _ params:commasep(<expr()>) _ ")" end:pos() {
                Expr::New(TypeName::basic_owned(id.to_owned()), params, Span::new(pos, end))
            }
            --
            expr:(@) _ "[" _ idx:expr() _ "]" high:pos() {
                let span = expr.span();
                Expr::ArrayElem(Box::new(expr), Box::new(idx), Span { high, ..span })
            }
            expr:(@) _ "." _ ident:ident() _ "(" _ params:commasep(<expr()>) _ ")" high:pos() {
                let span = expr.span();
                Expr::MethodCall(Box::new(expr), ident, params, Span { high, ..span })
            }
            expr:(@) _ "." _ ident:ident() high:pos() {
                let span = expr.span();
                Expr::Member(Box::new(expr), ident, Span { high, ..span })
            }
            expr:(@) _ keyword("as") _ type_:type_() high:pos() {
                let span = expr.span();
                Expr::Cast(type_, Box::new(expr), Span { high, ..span })
            }
            pos:pos() "[" _ exprs:commasep(<expr()>)_ "]" end:pos() {
                Expr::ArrayLit(exprs, None, Span::new(pos, end))
            }
            "(" _ v:expr() _ ")" { v }
            pos:pos() keyword("null") end:pos() {
                Expr::Null(Span::new(pos, end))
            }
            pos:pos() keyword("this") end:pos() {
                Expr::This(Span::new(pos, end)) }
            pos:pos() keyword("super") end:pos() {
                Expr::Super(Span::new(pos, end))
            }
            pos:pos() str:interpolated_string() end:pos() {
                Expr::InterpolatedString(str.0, str.1, Span::new(pos, end))
            }
            pos:pos() cons:constant() end:pos() {
                Expr::Constant(cons, Span::new(pos, end))
            }
            pos:pos() id:ident() _ "(" _ params:commasep(<expr()>) _ ")" end:pos() {
                Expr::Call(id, params, Span::new(pos, end))
            }
            pos:pos() id:ident() end:pos() {
                Expr::Ident(id, Span::new(pos, end))
            }
        }
    }
}

#[inline]
fn binop(lhs: Expr<SourceAst>, rhs: Expr<SourceAst>, op: BinOp) -> Expr<SourceAst> {
    let span = lhs.span().merge(rhs.span());
    Expr::BinOp(Box::new(lhs), Box::new(rhs), op, span)
}

#[inline]
fn unop(expr: Expr<SourceAst>, op: UnOp) -> Expr<SourceAst> {
    let span = expr.span();
    Expr::UnOp(Box::new(expr), op, span)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ternary_op() {
        let expr = lang::expr("3.0 ? 5.0 : 5 + 4", Pos::ZERO).unwrap();
        assert_eq!(
            format!("{:?}", expr),
            "Conditional(Constant(F32(3.0), Span { low: Pos(0), high: Pos(3) }), Constant(F32(5.0), Span { low: Pos(6), high: Pos(9) }), BinOp(Constant(I32(5), Span { low: Pos(12), high: Pos(13) }), Constant(I32(4), Span { low: Pos(16), high: Pos(17) }), Add, Span { low: Pos(12), high: Pos(17) }), Span { low: Pos(0), high: Pos(17) })"
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
            r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: Owned("A"), base: Some(Owned("IScriptable")), members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), name: Owned("m_field"), span: Span { low: Pos(53), high: Pos(78) } }, type_: TypeName { name: Owned("Int32"), arguments: [] } }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public]), name: Owned("GetField"), span: Span { low: Pos(104), high: Pos(124) } }, type_: Some(TypeName { name: Owned("Int32"), arguments: [] }), parameters: [], body: Some(Seq { exprs: [Return(Some(Member(This(Span { low: Pos(165), high: Pos(169) }), Owned("m_field"), Span { low: Pos(165), high: Pos(177) })), Span { low: Pos(158), high: Pos(178) })] }), span: Span { low: Pos(104), high: Pos(196) } })], span: Span { low: Pos(0), high: Pos(211) } })]"#
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
            r#"[Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), name: Owned("GetField"), span: Span { low: Pos(0), high: Pos(27) } }, type_: Some(TypeName { name: Owned("Uint64"), arguments: [] }), parameters: [ParameterSource { qualifiers: Qualifiers([]), name: Owned("optimum"), type_: TypeName { name: Owned("Uint64"), arguments: [] } }], body: Some(Seq { exprs: [Return(Some(Conditional(BinOp(Member(This(Span { low: Pos(80), high: Pos(84) }), Owned("m_field"), Span { low: Pos(80), high: Pos(92) }), Ident(Owned("optimum"), Span { low: Pos(95), high: Pos(102) }), Greater, Span { low: Pos(80), high: Pos(102) }), Member(This(Span { low: Pos(105), high: Pos(109) }), Owned("m_field"), Span { low: Pos(105), high: Pos(117) }), Ident(Owned("optimum"), Span { low: Pos(120), high: Pos(127) }), Span { low: Pos(80), high: Pos(127) })), Span { low: Pos(73), high: Pos(128) })] }), span: Span { low: Pos(0), high: Pos(143) } })]"#
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
            r#"While(BinOp(Ident(Owned("i"), Span { low: Pos(6), high: Pos(7) }), Constant(I32(1000), Span { low: Pos(10), high: Pos(14) }), Less, Span { low: Pos(6), high: Pos(14) }), Seq { exprs: [BinOp(Member(This(Span { low: Pos(33), high: Pos(37) }), Owned("counter"), Span { low: Pos(33), high: Pos(45) }), Member(Ident(Owned("Object"), Span { low: Pos(49), high: Pos(55) }), Owned("CONSTANT"), Span { low: Pos(49), high: Pos(64) }), AssignAdd, Span { low: Pos(33), high: Pos(64) }), BinOp(Ident(Owned("i"), Span { low: Pos(82), high: Pos(83) }), Constant(I32(1), Span { low: Pos(87), high: Pos(88) }), AssignAdd, Span { low: Pos(82), high: Pos(88) })] }, Span { low: Pos(0), high: Pos(104) })"#
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
            r#"If(Member(This(Span { low: Pos(3), high: Pos(7) }), Owned("m_fixBugs"), Span { low: Pos(3), high: Pos(17) }), Seq { exprs: [MethodCall(This(Span { low: Pos(36), high: Pos(40) }), Owned("NoBugs"), [], Span { low: Pos(36), high: Pos(49) })] }, Some(Seq { exprs: [MethodCall(This(Span { low: Pos(89), high: Pos(93) }), Owned("Bugs"), [], Span { low: Pos(89), high: Pos(100) })] }), Span { low: Pos(0), high: Pos(116) })"#
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
            r#"Switch(Ident(Owned("value"), Span { low: Pos(7), high: Pos(12) }), [SwitchCase { matcher: Constant(String(String, "0"), Span { low: Pos(37), high: Pos(40) }), body: Seq { exprs: [] } }, SwitchCase { matcher: Constant(String(String, "1"), Span { low: Pos(64), high: Pos(67) }), body: Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "0 or 1"), Span { low: Pos(93), high: Pos(101) })], Span { low: Pos(89), high: Pos(102) })] } }, SwitchCase { matcher: Constant(String(String, "2"), Span { low: Pos(126), high: Pos(129) }), body: Seq { exprs: [Break(Span { low: Pos(151), high: Pos(157) })] } }], Some(Seq { exprs: [Call(Owned("Log"), [Constant(String(String, "default"), Span { low: Pos(208), high: Pos(217) })], Span { low: Pos(204), high: Pos(218) })] }), Span { low: Pos(0), high: Pos(233) })"#
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
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: Owned("Test"), base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: Owned("m_field"), span: Span { low: Pos(130), high: Pos(149) } }, type_: TypeName { name: Owned("String"), arguments: [] } })], span: Span { low: Pos(101), high: Pos(189) } })]"#
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
            r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: Owned("Test"), base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: Owned("m_field"), span: Span { low: Pos(114), high: Pos(133) } }, type_: TypeName { name: Owned("String"), arguments: [] } })], span: Span { low: Pos(13), high: Pos(156) } })]"#
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

    #[test]
    fn parse_interpolated_string() {
        let str = lang::interpolated_string(
            r#"s"My name is \(name) and I am \(currentYear - birthYear) years old""#,
            Pos::ZERO,
        )
        .unwrap();
        assert_eq!(
            format!("{:?}", str),
            r#"("My name is ", [(Ident(Owned("name"), Span { low: Pos(15), high: Pos(19) }), " and I am "), (BinOp(Ident(Owned("currentYear"), Span { low: Pos(32), high: Pos(43) }), Ident(Owned("birthYear"), Span { low: Pos(46), high: Pos(55) }), Subtract, Span { low: Pos(32), high: Pos(55) }), " years old")])"#
        );
    }
}

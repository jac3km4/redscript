use std::str::FromStr;

use peg::error::ParseError;
use peg::str::LineCol;
use redscript::ast::{
    BinOp, Constant, Expr, Ident, Literal, Param, Pos, Seq, SourceAst, Span, SwitchCase, TypeName, TypeParam, UnOp,
    Variance,
};
use redscript::definition::Visibility;
use redscript::Str;
use strum::EnumString;

use crate::source_map::File;

#[derive(Debug)]
pub struct SourceModule {
    pub path: ModulePath,
    pub imports: Vec<Import>,
    pub entries: Vec<SourceEntry>,
}

#[derive(Debug)]
pub enum SourceEntry {
    Class(ClassSource),
    Struct(ClassSource),
    Enum(EnumSource),
    Function(FunctionSource),
    GlobalLet(FieldSource),
}

impl SourceEntry {
    pub fn annotations(&self) -> &[Annotation] {
        match self {
            Self::Function(fun) => &fun.decl.annotations,
            Self::GlobalLet(field) => &field.declaration.annotations,
            _ => &[],
        }
    }
}

#[derive(Debug)]
pub struct ClassSource {
    pub tparams: Vec<TypeParam>,
    pub qualifiers: Qualifiers,
    pub name: Ident,
    pub base: Option<TypeName>,
    pub members: Vec<MemberSource>,
    pub span: Span,
}

#[derive(Debug)]
pub enum MemberSource {
    Method(FunctionSource),
    Field(FieldSource),
}

#[derive(Debug)]
pub struct FieldSource {
    pub declaration: Declaration,
    pub type_: TypeName,
    pub default: Option<Expr<SourceAst>>,
}

#[derive(Debug)]
pub struct FunctionSource {
    pub tparams: Vec<TypeParam>,
    pub decl: Declaration,
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
    ImportOnly,
    Persistent,
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
    RuntimeProperty,
}

#[derive(Debug)]
pub enum Import {
    Exact(Vec<Annotation>, ModulePath, Span),
    Selected(Vec<Annotation>, ModulePath, Vec<Ident>, Span),
    All(Vec<Annotation>, ModulePath, Span),
}

impl Import {
    pub fn annotations(&self) -> &[Annotation] {
        match self {
            Self::Exact(anns, _, _) | Self::Selected(anns, _, _, _) | Self::All(anns, _, _) => anns,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    parts: Vec<Ident>,
}

impl ModulePath {
    #[inline]
    pub fn new(parts: Vec<Ident>) -> Self {
        Self { parts }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Ident> + Clone {
        self.parts.iter()
    }
}

impl AsRef<[Ident]> for ModulePath {
    #[inline]
    fn as_ref(&self) -> &[Ident] {
        &self.parts
    }
}

impl IntoIterator for ModulePath {
    type IntoIter = std::vec::IntoIter<Ident>;
    type Item = Ident;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.parts.into_iter()
    }
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
            / keyword("importonly") { Qualifier::ImportOnly }
            / keyword("persistent") { Qualifier::Persistent }

        rule type_arg_ident() -> &'static str
            = keyword("Cast")
            / keyword("FromVariant")
            / keyword("IntEnum")

        rule literal_type() -> Literal
            = "n" { Literal::Name }
            / "r" { Literal::Resource }
            / "t" { Literal::TweakDbId }

        rule annotation() -> Annotation
            = pos:pos() "@" ident:ident() _ "(" _ args:commasep(<expr()>) _ ")" end:pos() {?
                AnnotationKind::from_str(ident.as_ref()).map(|kind| {
                    Annotation { kind, args, span: Span::new(pos, end) }
                }).map_err(|_| "valid annotation")
            }

        rule qualifiers() -> Qualifiers = qs:qualifier() ** _ { Qualifiers(qs) }

        rule ident() -> Ident
            = quiet!{
                x:$(['a'..='z' | 'A'..='Z' | '_']) xs:$(['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']*)
                { Ident::from(x) + xs }
            } / expected!("identifier")

        rule keyword(id: &'static str) -> &'static str =
            ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_'] { id }

        rule number() -> Constant
            = str:$(['-']? ['0'..='9' | '.']+) unsigned: $(['u'])? postfix:$(['l' | 'd'])?
            {? if postfix == Some("d") { str.parse().or(Err("valid double")).map(Constant::F64) }
               else if str.contains('.') { str.parse().or(Err("valid float")).map(Constant::F32) }
               else if postfix == Some("l") && unsigned.is_some() { str.parse().or(Err("valid unsigned 64-bit int")).map(Constant::U64) }
               else if unsigned.is_some() { str.parse().or(Err("valid unsigned 32-bit int")).map(Constant::U32) }
               else if postfix == Some("l") { str.parse().or(Err("valid 64-bit int")).map(Constant::I64) }
               else { str.parse().or(Err("valid 32-bit int")).map(Constant::I32) }
            }

        rule escaped_char() -> char
            = !['\\' | '\"'] c:[_] { c }
            / r#"\n"# { '\n' }
            / r#"\r"# { '\r' }
            / r#"\t"# { '\t' }
            / r#"\'"# { '\'' }
            / r#"\""# { '\"' }
            / r#"\\"# { '\\' }
            / "\\u{" u:$(['a'..='f' | 'A'..='F' | '0'..='9']*<1,6>) "}" {
                char::from_u32(u32::from_str_radix(u, 16).unwrap()).unwrap()
            }

        rule string_contents() -> String
            = s:escaped_char()* { s.into_iter().collect() }

        pub rule escaped_string() -> String
            = "\"" s:string_contents() "\"" { s }

        rule interpolation() -> Expr<SourceAst>
            = r#"\("# _ expr:expr() _ ")" { expr }

        rule string_part() -> (Expr<SourceAst>, Str)
            = e:interpolation() s:string_contents() { (e, Str::from(s)) }

        pub rule interpolated_string() -> (Str, Vec<(Expr<SourceAst>, Str)>)
            = "s\""  prefix:string_contents() parts:string_part()* "\"" { (Str::from(prefix), parts) }

        rule constant() -> Constant
            = keyword("true") { Constant::Bool(true) }
            / keyword("false") { Constant::Bool(false) }
            / n:number() { n }
            / type_:literal_type()? str:escaped_string()
                { Constant::String(type_.unwrap_or(Literal::String), Str::from(str)) }

        rule seq() -> Seq<SourceAst> = exprs:(stmt() ** _) { Seq::new(exprs) }

        rule type_args() -> Vec<TypeName> = "<" _ args:commasep(<type_()>) _ ">" { args }
        rule type_() -> TypeName
            = name:ident() args:type_args()? { TypeName::new(name, args.unwrap_or_default()) }
            / "[" _ inner:type_() _ "]" { TypeName::of_array(inner) }
            / "(" _ params:commasep(<type_()>) _ ")" _ "->" _ ret:type_() { TypeName::of_function(params, ret) }

        rule let_type() -> TypeName = ":" _ type_:type_() { type_ }
        rule func_type() -> TypeName = "->" _ type_:type_() { type_ }

        rule initializer() -> Expr<SourceAst> = "=" _ val:expr() { val }

        rule let() -> Expr<SourceAst>
            = pos:pos() keyword("let") _ name:ident() _ type_:let_type()? _ val:initializer()? _ ";" end:pos()
            { Expr::Declare(name, type_.map(Box::new), val.map(Box::new), Span::new(pos, end)) }

        rule decl<A>(inner: rule<A>) -> Declaration
            = pos:pos() annotations:(annotation() ** _) _ qualifiers:qualifiers() _ inner() _ name:ident() end:pos()
            { Declaration { annotations, qualifiers, name, span: Span::new(pos, end) } }

        rule field() -> FieldSource
            = declaration:decl(<keyword("let")>) _ type_:let_type() _ default:initializer()? _ ";"
            { FieldSource { declaration, type_, default }}

        pub rule function() -> FunctionSource
            = pos:pos() decl:decl(<keyword("func")>) _ tparams:tparams()? _ "(" _ parameters:commasep(<param()>) _ ")" _ type_:func_type()? _ body:function_body()? ";"? end:pos()
            { FunctionSource { tparams: tparams.unwrap_or_default(), decl, type_, parameters, body, span: Span::new(pos, end) } }
        rule function_body() -> Seq<SourceAst>
            = "{" _ body:seq() _ "}" { body }
            / pos:pos() "=" _ expr:expr() _ end:pos() { Seq::new(vec![Expr::Return(Some(Box::new(expr)), Span::new(pos, end))]) }

        rule param() -> ParameterSource
            = qualifiers:qualifiers() _ name:ident() _ type_:let_type()
            { ParameterSource { qualifiers, name, type_ } }

        rule extends() -> TypeName = keyword("extends") _ name:type_() { name }

        rule variance() -> Variance
            = "+" { Variance::Co }
            / "-" { Variance::Contra }

        rule tparam() -> TypeParam
            = variance:variance()? _ name:ident() _ extends:extends()?
            { TypeParam { variance: variance.unwrap_or(Variance::In), name, extends } }

        rule tparams() -> Vec<TypeParam>
            = "<" _ tparams:commasep(<tparam()>) _ ">"
            { tparams }

        pub rule class() -> ClassSource
            = pos:pos() qualifiers:qualifiers() _ keyword("class") _ name:ident() _ tparams:tparams()? _ base:extends()? _ "{" _ members:member()**_ _ "}" end:pos()
            { ClassSource { qualifiers, tparams: tparams.unwrap_or_default(), name, base, members, span: Span::new(pos, end) } }

        pub rule struct_() -> ClassSource
            = pos:pos() qualifiers:qualifiers() _ keyword("struct") _ name:ident() _ "{" _ members:member()**_ _ "}" end:pos()
            { ClassSource { qualifiers, tparams: vec![], name, base: None, members, span: Span::new(pos, end) } }

        rule member() -> MemberSource
            = fun:function() { MemberSource::Method(fun) }
            / field:field() { MemberSource::Field(field) }
            / expected!("a method or a field")

        pub rule enum_() -> EnumSource
            = pos:pos() keyword("enum") _ name:ident() _ "{" _ members:commasep(<enum_member()>) _ ","? _ "}" end:pos()
            { EnumSource { name, members, span: Span::new(pos, end) } }

        rule enum_member() -> EnumMember
            = name:ident() _ "=" _ value:number()
            {? match value {
                 Constant::I32(value) => Ok(EnumMember { name, value: value.into() }),
                 Constant::I64(value) => Ok(EnumMember { name, value }),
                 _ => Err("signed 64-bit int")
               }
            }

        pub rule source_entry() -> SourceEntry
            = fun:function() { SourceEntry::Function(fun) }
            / class:class() { SourceEntry::Class(class) }
            / struct_:struct_() { SourceEntry::Struct(struct_) }
            / field:field() { SourceEntry::GlobalLet(field) }
            / enum_:enum_() { SourceEntry::Enum(enum_) }
            / expected!("a top-level definition")

        rule import() -> Import
            = pos:pos() annotations:(annotation() ** _) _ keyword("import") _ parts: dotsep(<ident()>) _ "." _ "*" end:pos()
                { Import::All(annotations, ModulePath::new(parts), Span::new(pos, end)) }
            / pos:pos() annotations:(annotation() ** _) _ keyword("import") _ parts: dotsep(<ident()>) _ "." _ "{" _ names:commasep(<ident()>) _ "}" end:pos()
                { Import::Selected(annotations, ModulePath::new(parts), names, Span::new(pos, end)) }
            / pos:pos() annotations:(annotation() ** _) _ keyword("import") _ parts: dotsep(<ident()>) end:pos()
                { Import::Exact(annotations, ModulePath::new(parts), Span::new(pos, end)) }

        rule module_path() -> ModulePath  =
            keyword("module") _ parts:dotsep(<ident()>) { ModulePath { parts } }

        pub rule module() -> SourceModule =
            _ path:module_path()? _ imports:(import() ** _) _ entries:(source_entry() ** _) _
            { SourceModule { path: path.unwrap_or_default(), imports, entries } }

        rule switch() -> Expr<SourceAst>
            = pos:pos() keyword("switch") _ matcher:expr() _ "{" _ cases:(case() ** _) _ default:default()? _ "}" _ ";"? end:pos()
            { Expr::Switch(Box::new(matcher), cases, default, (), Span::new(pos, end)) }

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
            / keyword("else") _ body:if_() { Seq::new(vec![body]) }

        pub rule stmt() -> Expr<SourceAst>
            = while_: while_() { while_ }
            / for_: for_() { for_ }
            / if_: if_() { if_ }
            / switch: switch() { switch }
            / pos:pos() keyword("return") _ val:expr()? _ ";" end:pos() { Expr::Return(val.map(Box::new), Span::new(pos, end)) }
            / pos:pos() keyword("break") _ ";" end:pos() { Expr::Break(Span::new(pos, end)) }
            / let_:let() { let_ }
            / expr:expr() _ ";" { expr }
            / expected!("a statement")

        rule lparam() -> Param
            = name:ident() _ typ:(":" _ typ:type_() { typ })?
            { Param { name, typ } }

        pub rule expr() -> Expr<SourceAst> = precedence!{
            // TODO: added due to syntactic ambiguity with <> operators, revise later
            pos:pos() ident:type_arg_ident() _ type_args:type_args()? _ "(" _ params:commasep(<expr()>) _ ")" end:pos() {
                Expr::Call(Expr::Ident(Ident::from_static(ident), Span::new(pos, end)).into(), (), type_args.unwrap_or_default().into(), params.into(), (), Span::new(pos, end))
            }
            --
            x:@ _ "?" _ y:expr() _ ":" _ z:expr() {
                let span = x.span().merge(z.span());
                Expr::Conditional(Box::new(x), Box::new(y), Box::new(z), span)
            }
            x:@ _ "=" _ y:(@) {
                let span = x.span().merge(y.span());
                Expr::Assign(Box::new(x), Box::new(y), (), span)
            }
            x:@ _ pos:pos() "+=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignAdd) }
            x:@ _ pos:pos() "-=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignSubtract) }
            x:@ _ pos:pos() "*=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignMultiply) }
            x:@ _ pos:pos() "/=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignDivide) }
            x:@ _ pos:pos() "|=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignOr) }
            x:@ _ pos:pos() "&=" end:pos() _ y:(@) { binop(x, y, BinOp::AssignAnd) }
            --
            x:(@) _ pos:pos() "||" end:pos() _ y:@ { binop(x, y, BinOp::LogicOr) }
            --
            x:(@) _ pos:pos() "&&" end:pos() _ y:@ { binop(x, y, BinOp::LogicAnd) }
            --
            x:(@) _ pos:pos() "|" end:pos() _ y:@ { binop(x, y, BinOp::Or) }
            --
            x:(@) _ pos:pos() "^" end:pos() _ y:@ { binop(x, y, BinOp::Xor) }
            --
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
                Expr::New(TypeName::new(id, vec![]), params.into(), Span::new(pos, end))
            }
            --
            expr:(@) _ "[" _ idx:expr() _ "]" high:pos() {
                let span = expr.span();
                Expr::ArrayElem(Box::new(expr), Box::new(idx), (), Span { high, ..span })
            }
            expr:(@) _ "." _ ident:ident() high:pos() {
                let span = expr.span();
                Expr::Member(Box::new(expr), ident, Span { high, ..span })
            }
            expr:(@) _ keyword("as") _ type_:type_() high:pos() {
                let span = expr.span();
                Expr::DynCast(type_, Box::new(expr), Span { high, ..span })
            }
            pos:pos() "[" _ exprs:commasep(<expr()>)_ "]" end:pos() {
                Expr::ArrayLit(exprs.into(), (), Span::new(pos, end))
            }
            pos:pos() "(" _ params:commasep(<lparam()>) _ ")" _ "->" _ expr:expr() end:pos() {
                Expr::Lambda(params.into(), expr.into(), Span::new(pos, end))
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
            expr:(@) _ type_args:type_args()? _ "(" _ params:commasep(<expr()>) _ ")" end:pos() {
                let span = expr.span();
                Expr::Call(expr.into(), (), type_args.unwrap_or_default().into(), params.into(), (), Span::new(span.low, end))
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

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn parse_ternary_op() {
//         let expr = lang::expr("3.0 ? 5.0 : 5 + 4", Pos::ZERO).unwrap();
//         assert_eq!(
//             format!("{:?}", expr),
//             "Conditional(Constant(F32(3.0), Span { low: Pos(0), high: Pos(3) }), Constant(F32(5.0), Span { low: Pos(6), high: Pos(9) }), BinOp(Constant(I32(5), Span { low: Pos(12), high: Pos(13) }), Constant(I32(4), Span { low: Pos(16), high: Pos(17) }), Add, Span { low: Pos(12), high: Pos(17) }), Span { low: Pos(0), high: Pos(17) })"
//         );
//     }

//     #[test]
//     fn parse_simple_class() {
//         let module = lang::module(
//             "public class A extends IScriptable {
//                 private const let m_field: Int32;

//                 public func GetField() -> Int32 {
//                     return this.m_field;
//                 }
//              }",
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", module.entries),
//             r#"[Class(ClassSource { qualifiers: Qualifiers([Public]), name: "A", base: Some("IScriptable"), members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private, Const]), name: "m_field", span: Span { low: Pos(53), high: Pos(78) } }, type_: TypeName { name: "Int32", arguments: None }, default: None }), Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public]), name: "GetField", span: Span { low: Pos(104), high: Pos(124) } }, type_: Some(TypeName { name: "Int32", arguments: None }), parameters: [], body: Some(Seq { exprs: [Return(Some(Member(This(Span { low: Pos(165), high: Pos(169) }), "m_field", Span { low: Pos(165), high: Pos(177) })), Span { low: Pos(158), high: Pos(178) })] }), span: Span { low: Pos(104), high: Pos(196) } })], span: Span { low: Pos(0), high: Pos(211) } })]"#
//         );
//     }

//     #[test]
//     fn parse_simple_func() {
//         let module = lang::module(
//             "public static func GetField(optimum: Uint64) -> Uint64 {
//                 return this.m_field > optimum ? this.m_field : optimum;
//              }",
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", module.entries),
//             r#"[Function(FunctionSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Public, Static]), name: "GetField", span: Span { low: Pos(0), high: Pos(27) } }, type_: Some(TypeName { name: "Uint64", arguments: None }), parameters: [ParameterSource { qualifiers: Qualifiers([]), name: "optimum", type_: TypeName { name: "Uint64", arguments: None } }], body: Some(Seq { exprs: [Return(Some(Conditional(BinOp(Member(This(Span { low: Pos(80), high: Pos(84) }), "m_field", Span { low: Pos(80), high: Pos(92) }), Ident("optimum", Span { low: Pos(95), high: Pos(102) }), Greater, Span { low: Pos(80), high: Pos(102) }), Member(This(Span { low: Pos(105), high: Pos(109) }), "m_field", Span { low: Pos(105), high: Pos(117) }), Ident("optimum", Span { low: Pos(120), high: Pos(127) }), Span { low: Pos(80), high: Pos(127) })), Span { low: Pos(73), high: Pos(128) })] }), span: Span { low: Pos(0), high: Pos(143) } })]"#
//         );
//     }

//     #[test]
//     fn parse_simple_loop() {
//         let stmt = lang::stmt(
//             "while i < 1000 {
//                 this.counter += Object.CONSTANT;
//                 i += 1;
//              }",
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", stmt),
//             r#"While(BinOp(Ident("i", Span { low: Pos(6), high: Pos(7) }), Constant(I32(1000), Span { low: Pos(10), high: Pos(14) }), Less, Span { low: Pos(6), high: Pos(14) }), Seq { exprs: [BinOp(Member(This(Span { low: Pos(33), high: Pos(37) }), "counter", Span { low: Pos(33), high: Pos(45) }), Member(Ident("Object", Span { low: Pos(49), high: Pos(55) }), "CONSTANT", Span { low: Pos(49), high: Pos(64) }), AssignAdd, Span { low: Pos(33), high: Pos(64) }), BinOp(Ident("i", Span { low: Pos(82), high: Pos(83) }), Constant(I32(1), Span { low: Pos(87), high: Pos(88) }), AssignAdd, Span { low: Pos(82), high: Pos(88) })] }, Span { low: Pos(0), high: Pos(104) })"#
//         );
//     }

//     #[test]
//     fn parse_simple_if_else() {
//         let stmt = lang::stmt(
//             "if this.m_fixBugs {
//                 this.NoBugs();
//              } else {
//                 this.Bugs();
//              }",
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", stmt),
//             r#"If(Member(This(Span { low: Pos(3), high: Pos(7) }), "m_fixBugs", Span { low: Pos(3), high: Pos(17) }), Seq { exprs: [MethodCall(This(Span { low: Pos(36), high: Pos(40) }), "NoBugs", [], Span { low: Pos(36), high: Pos(49) })] }, Some(Seq { exprs: [MethodCall(This(Span { low: Pos(89), high: Pos(93) }), "Bugs", [], Span { low: Pos(89), high: Pos(100) })] }), Span { low: Pos(0), high: Pos(116) })"#
//         );
//     }

//     #[test]
//     fn parse_switch_case() {
//         let stmt = lang::stmt(
//             r#"switch value {
//                  case "0":
//                  case "1":
//                     Log("0 or 1");
//                  case "2":
//                     break;
//                  default:
//                     Log("default");
//             }"#,
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", stmt),
//             r#"Switch(Ident("value", Span { low: Pos(7), high: Pos(12) }), [SwitchCase { matcher: Constant(String(String, "0"), Span { low: Pos(37), high: Pos(40) }), body: Seq { exprs: [] } }, SwitchCase { matcher: Constant(String(String, "1"), Span { low: Pos(64), high: Pos(67) }), body: Seq { exprs: [Call("Log", [], [Constant(String(String, "0 or 1"), Span { low: Pos(93), high: Pos(101) })], Span { low: Pos(89), high: Pos(102) })] } }, SwitchCase { matcher: Constant(String(String, "2"), Span { low: Pos(126), high: Pos(129) }), body: Seq { exprs: [Break(Span { low: Pos(151), high: Pos(157) })] } }], Some(Seq { exprs: [Call("Log", [], [Constant(String(String, "default"), Span { low: Pos(208), high: Pos(217) })], Span { low: Pos(204), high: Pos(218) })] }), Span { low: Pos(0), high: Pos(233) })"#
//         );
//     }

//     #[test]
//     fn parse_with_comment() {
//         let module = lang::module(
//             r#"
//             /* this is a multiline comment
//                blah blah blah
//             */
//             class Test {
//                 private let m_field /* cool stuff */: String;
//             }
//             "#,
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", module.entries),
//             r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: "Test", base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: "m_field", span: Span { low: Pos(130), high: Pos(149) } }, type_: TypeName { name: "String", arguments: None }, default: None })], span: Span { low: Pos(101), high: Pos(189) } })]"#
//         );
//     }

//     #[test]
//     fn parse_with_line_comment() {
//         let module = lang::module(
//             r#"
//             class Test { // line comment
//                 // private let m_comment_field: String;
//                 private let m_field: String;
//             }
//             "#,
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", module.entries),
//             r#"[Class(ClassSource { qualifiers: Qualifiers([]), name: "Test", base: None, members: [Field(FieldSource { declaration: Declaration { annotations: [], qualifiers: Qualifiers([Private]), name: "m_field", span: Span { low: Pos(114), high: Pos(133) } }, type_: TypeName { name: "String", arguments: None }, default: None })], span: Span { low: Pos(13), high: Pos(156) } })]"#
//         );
//     }

//     #[test]
//     fn parse_escaped_string() {
//         let escaped = lang::escaped_string(
//             r#""This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n""#,
//             Pos::ZERO,
//         );

//         assert_eq!(
//             escaped,
//             Ok(String::from(
//                 "This is a backslash \'\\\' \"escaped\" string \t\u{03BB}\r\n"
//             ))
//         );
//     }

//     #[test]
//     fn fail_mangled_string() {
//         let mangled = lang::escaped_string(
//             r#""These are invalid escape characters: \a \\" \u{1234567}""#,
//             Pos::ZERO,
//         );

//         assert!(mangled.is_err());
//     }

//     #[test]
//     fn parse_interpolated_string() {
//         let str = lang::interpolated_string(
//             r#"s"My name is \(name) and I am \(currentYear - birthYear) years old""#,
//             Pos::ZERO,
//         )
//         .unwrap();
//         assert_eq!(
//             format!("{:?}", str),
//             r#"("My name is ", [(Ident("name", Span { low: Pos(15), high: Pos(19) }), " and I am "), (BinOp(Ident("currentYear", Span { low: Pos(32), high: Pos(43) }), Ident("birthYear", Span { low: Pos(46), high: Pos(55) }), Subtract, Span { low: Pos(32), high: Pos(55) }), " years old")])"#
//         );
//     }

//     #[test]
//     fn parse_complex_logic() {
//         let str = lang::expr(r#"(true || false && false) && ((true || false) && true)"#, Pos::ZERO).unwrap();
//         assert_eq!(
//             format!("{:?}", str),
//             r#"BinOp(BinOp(Constant(Bool(true), Span { low: Pos(1), high: Pos(5) }), BinOp(Constant(Bool(false), Span { low: Pos(9), high: Pos(14) }), Constant(Bool(false), Span { low: Pos(18), high: Pos(23) }), LogicAnd, Span { low: Pos(9), high: Pos(23) }), LogicOr, Span { low: Pos(1), high: Pos(23) }), BinOp(BinOp(Constant(Bool(true), Span { low: Pos(30), high: Pos(34) }), Constant(Bool(false), Span { low: Pos(38), high: Pos(43) }), LogicOr, Span { low: Pos(30), high: Pos(43) }), Constant(Bool(true), Span { low: Pos(48), high: Pos(52) }), LogicAnd, Span { low: Pos(30), high: Pos(52) }), LogicAnd, Span { low: Pos(1), high: Pos(52) })"#
//         );
//     }
// }

use std::fmt;
use std::mem;

use redscript_community_ast::{
    Aggregate, Annotation, AstKind, Enum, EnumVariant, Field, Function, FunctionBody, Import, Item,
    ItemDecl, ItemQualifiers, Module, NodeId, Param, ParamQualifiers, Path, Type, TypeParam,
    Variance, Visibility, Wrapper,
};

use crate::layout::{SepBy, SepByMultiline};
use crate::{FormatCtx, Formattable, SyntaxOps};

impl<K: AstKind> Formattable for Module<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if let Some(path) = &self.path {
            writeln!(f, "module {}", path.as_fmt(ctx))?;
        }
        writeln!(f)?;
        format_items(self.items.iter().map(Wrapper::as_wrapped), f, ctx)
    }
}

impl<K: AstKind> Formattable for ItemDecl<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            ctx.node_prefix(NodeId::item_decl(self), Some(ctx.indent()))
        )?;

        for doc in &self.doc[..] {
            writeln!(f, "{}{}", ctx.indent(), doc)?;
        }
        for ann in &self.annotations[..] {
            writeln!(f, "{}{}", ctx.indent(), ann.as_wrapped().as_fmt(ctx))?;
        }
        if let Item::Function(function) = &self.item
            && let Some(label) = &function.properties.profile
        {
            writeln!(
                f,
                "{}@profile(\"{}\")",
                ctx.indent(),
                str::escape_default(label)
            )?;
        }
        write!(f, "{}", ctx.indent())?;
        if let Some(visibility) = &self.visibility {
            write!(f, "{} ", visibility.as_fmt(ctx))?;
        }
        writeln!(
            f,
            "{}{}",
            self.qualifiers.as_fmt(ctx),
            self.item.as_fmt(ctx)
        )
    }
}

impl<K: AstKind> Formattable for Item<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Item::Import(import) => write!(f, "{}", import.as_fmt(ctx)),
            Item::Class(class) => write!(f, "class {}", class.as_fmt(ctx)),
            Item::Struct(struct_) => write!(f, "struct {}", struct_.as_fmt(ctx)),
            Item::Function(func) => write!(f, "{}", func.as_fmt(ctx)),
            Item::Let(field) => write!(f, "{}", field.as_fmt(ctx)),
            Item::Default(default) => write!(
                f,
                "default {} = {};",
                default.target.as_wrapped(),
                (*default.value).as_wrapped().as_fmt(ctx)
            ),
            Item::Enum(enum_) => write!(f, "{}", enum_.as_fmt(ctx)),
        }
    }
}

impl Formattable for Import<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Import::Exact(path) => write!(f, "import {}", path.as_fmt(ctx)),
            Import::Select(path, items) => write!(
                f,
                "import {}.{{{}}}",
                path.as_fmt(ctx),
                SepBy::new(items.iter().map(Wrapper::as_wrapped), ", ", ctx)
            ),
            Import::All(path) => write!(f, "import {}.*", path.as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for Aggregate<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", self.name.as_wrapped())?;
        if !self.type_params.is_empty() {
            write!(
                f,
                "<{}>",
                SepBy::new(self.type_params.iter().map(Wrapper::as_wrapped), ", ", ctx)
            )?;
        }
        if let Some(extends) = &self.extends {
            write!(f, " extends {}", (**extends).as_wrapped().as_fmt(ctx))?;
        }
        writeln!(f, " {{")?;
        format_items(
            self.items.iter().map(Wrapper::as_wrapped),
            f,
            ctx.indented(),
        )?;
        write!(f, "{}}}", ctx.indent())
    }
}

impl<K: AstKind> Formattable for Field<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "let {}: {}",
            self.name.as_wrapped(),
            (*self.typ).as_wrapped().as_fmt(ctx)
        )?;
        if let Some(value) = &self.default {
            write!(f, " = {}", (**value).as_wrapped().as_fmt(ctx))?;
        }
        write!(f, ";")
    }
}

impl<K: AstKind> Formattable for Function<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if let Some(conversion) = self.properties.conversion {
            let qualifier = match conversion {
                redscript_community_ast::ConversionKind::Explicit => "explicit",
                redscript_community_ast::ConversionKind::Implicit => "implicit",
            };
            write!(f, "{qualifier} ")?;
        }
        write!(f, "func {}", self.name.as_wrapped())?;
        if !self.type_params.is_empty() {
            write!(
                f,
                "<{}>",
                SepBy::new(self.type_params.iter().map(Wrapper::as_wrapped), ", ", ctx)
            )?;
        }
        write!(
            f,
            "({})",
            SepByMultiline::new(self.params.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )?;
        if let Some(typ) = &self.return_type {
            write!(
                f,
                " -> {}{}",
                if self.properties.is_const_return {
                    "const "
                } else {
                    ""
                },
                (**typ).as_wrapped().as_fmt(ctx)
            )?;
        }
        match &self.body {
            Some(FunctionBody::Block(block)) => write!(f, " {}", block.as_fmt(ctx)),
            Some(FunctionBody::Inline(expr)) => {
                write!(f, " = {};", (**expr).as_wrapped().as_fmt(ctx))
            }
            None => write!(f, ";"),
        }
    }
}

impl<K: AstKind> Formattable for Enum<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "enum {}", self.name.as_wrapped())?;
        if let Some(storage) = &self.storage {
            write!(f, ": {}", storage.as_wrapped().as_fmt(ctx))?;
        }
        writeln!(f, " {{")?;
        for variant in &self.variants[..] {
            let ctx = ctx.indented();
            writeln!(f, "{}{},", ctx.indent(), variant.as_wrapped().as_fmt(ctx))?;
        }
        write!(f, "{}}}", ctx.indent())
    }
}

impl Formattable for EnumVariant<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(value) = &self.value {
            let suffix = if i32::try_from(*value).is_ok() {
                ""
            } else {
                "l"
            };
            write!(f, " = {value}{suffix}")?;
        }
        Ok(())
    }
}

impl<K: AstKind> Formattable for FunctionBody<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            FunctionBody::Block(block) => write!(f, "{}", block.as_fmt(ctx)),
            FunctionBody::Inline(expr) => write!(f, "{}", (**expr).as_wrapped().as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for Param<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if self.qualifiers.contains(ParamQualifiers::OUT) {
            write!(f, "out ")?;
        };
        if self.qualifiers.contains(ParamQualifiers::OPTIONAL) {
            write!(f, "opt ")?;
        };
        if self.qualifiers.contains(ParamQualifiers::CONST) {
            write!(f, "const ")?;
        };
        if self.qualifiers.contains(ParamQualifiers::SKIP) {
            write!(f, "skip ")?;
        };
        if let Some(typ) = &self.typ {
            write!(f, "{}: {}", self.name, typ.as_wrapped().as_fmt(ctx))
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl<K: AstKind> Formattable for Type<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Type::Named { name, args } if args.is_empty() => write!(f, "{name}"),
            Type::Named { name, args } => write!(
                f,
                "{}<{}>",
                name,
                SepBy::new(args.iter().map(Wrapper::as_wrapped), ", ", ctx)
            ),
            Type::Array(el) => write!(f, "[{}]", (**el).as_wrapped().as_fmt(ctx)),
            Type::StaticArray(el, size) => {
                write!(f, "[{}; {}]", (**el).as_wrapped().as_fmt(ctx), size)
            }
            Type::Fn {
                params,
                return_type,
            } => {
                write!(
                    f,
                    "({}) -> {}",
                    SepBy::new(params.iter().map(Wrapper::as_wrapped), ", ", ctx),
                    (**return_type).as_wrapped().as_fmt(ctx)
                )
            }
        }
    }
}

impl<K: AstKind> Formattable for Annotation<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "@{}({})",
            self.name,
            SepBy::new(self.args.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )
    }
}

impl Formattable for Path<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            SepBy::new(self.segments.iter().map(Wrapper::as_wrapped), ".", ctx)
        )
    }
}

impl Formattable for ItemQualifiers {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        if self.contains(ItemQualifiers::ABSTRACT) {
            write!(f, "abstract ")?;
        }
        if self.contains(ItemQualifiers::CALLBACK) {
            write!(f, "cb ")?;
        }
        if self.contains(ItemQualifiers::CONST) {
            write!(f, "const ")?;
        }
        if self.contains(ItemQualifiers::EDITABLE) {
            write!(f, "editable ")?;
        }
        if self.contains(ItemQualifiers::EXEC) {
            write!(f, "exec ")?;
        }
        if self.contains(ItemQualifiers::FINAL) {
            write!(f, "final ")?;
        }
        if self.contains(ItemQualifiers::IMPORT_ONLY) {
            write!(f, "importonly ")?;
        }
        if self.contains(ItemQualifiers::INSTANCE_EDITABLE) {
            write!(f, "instanceeditable ")?;
        }
        if self.contains(ItemQualifiers::INLINE) {
            write!(f, "inlined ")?;
        }
        if self.contains(ItemQualifiers::NATIVE) {
            write!(f, "native ")?;
        }
        if self.contains(ItemQualifiers::PERSISTENT) {
            write!(f, "persistent ")?;
        }
        if self.contains(ItemQualifiers::QUEST) {
            write!(f, "quest ")?;
        }
        if self.contains(ItemQualifiers::REPLICATED) {
            write!(f, "replicated ")?;
        }
        if self.contains(ItemQualifiers::STATIC) {
            write!(f, "static ")?;
        }
        if self.contains(ItemQualifiers::TEST_ONLY) {
            write!(f, "testonly ")?;
        }
        Ok(())
    }
}

impl Formattable for Visibility {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Public => write!(f, "public"),
            Self::Protected => write!(f, "protected"),
            Self::Private => write!(f, "private"),
        }
    }
}

impl<K: AstKind> Formattable for TypeParam<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}{}", self.variance.as_fmt(ctx), self.name.as_wrapped())?;
        if let Some(upper_bound) = &self.upper_bound {
            write!(f, " extends {}", (**upper_bound).as_wrapped().as_fmt(ctx))?;
        }
        Ok(())
    }
}

impl Formattable for Variance {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Covariant => write!(f, "+"),
            Self::Contravariant => write!(f, "-"),
            Self::Invariant => Ok(()),
        }
    }
}

fn format_items<'a, 'c: 'a, K: AstKind + 'a>(
    items: impl IntoIterator<Item = &'a ItemDecl<'c, K>>,
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    let mut it = items.into_iter();
    let Some(first) = it.next() else {
        return Ok(());
    };
    let mut discriminant = mem::discriminant(&first.item);
    let mut annotated = !first.annotations.is_empty();
    write!(f, "{}", first.as_fmt(ctx))?;

    for decl in it {
        let decl = decl.as_wrapped();
        let decl_is_annotated = !decl.annotations.is_empty();
        if !matches!(decl.item, Item::Import(_) | Item::Let(_) | Item::Default(_))
            || discriminant != mem::discriminant(&decl.item)
            || annotated != decl_is_annotated
            || decl_is_annotated
        {
            writeln!(f)?;
        }
        discriminant = mem::discriminant(&decl.item);
        annotated = decl_is_annotated;
        write!(f, "{}", decl.as_fmt(ctx))?;
    }

    Ok(())
}

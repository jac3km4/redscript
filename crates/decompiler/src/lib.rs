use std::borrow::Cow;

use control_flow::build_control_flow;
pub use decompiler::Verbosity;
use decompiler::decompile_block;
use error::{Error, Result};
use redscript_community_ast as ast;
use redscript_io::{
    Class, CodeIter, Definition, Enum, Field, Function, Parameter, PoolItemIndex, ScriptBundle,
    TypeIndex, TypeKind, Visibility,
};

mod control_flow;
mod decompiler;
mod error;
mod location;
#[cfg(test)]
mod tests;

#[derive(Debug, Default)]
pub struct Settings {
    verbosity: Verbosity,
    omit_function_bodies: bool,
}

impl Settings {
    pub fn verbose(mut self) -> Self {
        self.verbosity = Verbosity::Verbose;
        self
    }

    pub fn omit_function_bodies(mut self) -> Self {
        self.omit_function_bodies = true;
        self
    }
}

pub fn decompile_all<'ctx: 'i, 'i>(
    bundle: &'ctx ScriptBundle<'i>,
    settings: &Settings,
) -> impl Iterator<Item = Result<ast::ItemDecl<'i>>> {
    bundle.definitions().filter_map(move |def| match def {
        Definition::Class(class) => Some(decompile_class(class, bundle, settings)),
        Definition::Function(function) if function.class().is_none() => {
            Some(decompile_function(function, bundle, settings))
        }
        Definition::Enum(enum_) => Some(decompile_enum(enum_, bundle)),
        _ => None,
    })
}

pub fn decompile_class<'ctx: 'i, 'i>(
    class: &'ctx Class,
    bundle: &'ctx ScriptBundle<'i>,
    settings: &Settings,
) -> Result<ast::ItemDecl<'i>> {
    let name = extract_mangled_name(expect_borrowed(
        bundle.try_get_item_hint(class.name(), "class name")?,
    ));

    let base = class
        .base()
        .map(|base| {
            let base = bundle.try_get_item(base)?;
            let base_name = extract_mangled_name(expect_borrowed(
                bundle.try_get_item_hint(base.name(), "base class name")?,
            ));
            Ok::<_, Error>(Box::new(ast::Type::plain(base_name)))
        })
        .transpose()?;

    let fields = class
        .fields()
        .iter()
        .map(|&field_idx| decompile_field(bundle.try_get_item(field_idx)?, bundle));

    let methods = class
        .methods()
        .iter()
        .map(|&func_idx| decompile_function(bundle.try_get_item(func_idx)?, bundle, settings));

    let members = fields.chain(methods).collect::<Result<Vec<_>>>()?;
    let aggregate = ast::Aggregate::new(name, [], base, members);
    let item = if class.flags().is_struct() {
        ast::Item::Struct(aggregate)
    } else {
        ast::Item::Class(aggregate)
    };
    let qualifiers = extract_class_qualifiers(class);
    Ok(ast::ItemDecl::new([], None, qualifiers, [], item))
}

pub fn decompile_enum<'ctx: 'i, 'i>(
    enum_: &'ctx Enum,
    bundle: &'ctx ScriptBundle<'i>,
) -> Result<ast::ItemDecl<'i>> {
    let name = extract_mangled_name(expect_borrowed(
        bundle.try_get_item_hint(enum_.name(), "enum name")?,
    ));

    let variants = enum_
        .values()
        .iter()
        .map(|&value_idx| {
            let variant = bundle.try_get_item(value_idx)?;
            let name = expect_borrowed(bundle.try_get_item_hint(variant.name(), "variant name")?);
            Ok(ast::EnumVariant::new(name, Some(variant.value())))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(ast::ItemDecl::new(
        [],
        None,
        ast::ItemQualifiers::empty(),
        [],
        ast::Item::Enum(ast::Enum::new(name, variants)),
    ))
}

pub fn decompile_function<'ctx: 'i, 'i>(
    func: &'ctx Function<'i>,
    bundle: &'ctx ScriptBundle<'i>,
    settings: &Settings,
) -> Result<ast::ItemDecl<'i>> {
    let demangled_name = extract_mangled_name(expect_borrowed(
        bundle.try_get_item_hint(func.name(), "function name")?,
    ));
    let name = demangled_name;
    let visibility = extract_visibility(func.visibility());

    let block = if func.body().is_empty() || settings.omit_function_bodies {
        None
    } else {
        let code = func.body().code_iter().collect::<Result<Vec<_>, _>>()?;
        let cf_block = build_control_flow(CodeIter::new(&code))?;
        let block = decompile_block(func, bundle, &code, &cf_block, settings.verbosity)?;
        Some(ast::FunctionBody::Block(block))
    };

    let params = func
        .parameters()
        .iter()
        .map(|&p| {
            let param = bundle.try_get_item(p)?;
            let name = expect_borrowed(bundle.try_get_item_hint(param.name(), "parameter name")?);
            let typ = extract_type(param.type_(), bundle)?;
            let qualifiers = extract_param_qualifiers(param);
            Ok(ast::Param::new(name, Some(typ), qualifiers))
        })
        .collect::<Result<Vec<_>>>()?;

    let return_t = func
        .return_type()
        .map(|typ| extract_type(typ, bundle).map(Box::new))
        .transpose()?;

    let item = ast::Item::Function(ast::Function::new(name, [], params, return_t, block));
    let qualifiers = extract_func_qualifiers(func);
    Ok(ast::ItemDecl::new(
        [],
        Some(visibility),
        qualifiers,
        [],
        item,
    ))
}

pub fn decompile_field<'i>(
    field: &Field<'i>,
    bundle: &ScriptBundle<'i>,
) -> Result<ast::ItemDecl<'i>> {
    let name = expect_borrowed(bundle.try_get_item_hint(field.name(), "field name")?);
    let visibility = extract_visibility(field.visibility());
    let typ = extract_type(field.type_(), bundle)?.into();
    let qualifiers = extract_field_qualifiers(field);

    Ok(ast::ItemDecl::new(
        [],
        Some(visibility),
        qualifiers,
        [],
        ast::Item::Let(ast::Field {
            name,
            typ,
            default: None,
        }),
    ))
}

fn extract_type<'i>(typ: TypeIndex, bundle: &ScriptBundle<'i>) -> Result<ast::Type<'i>> {
    let typ = bundle.try_get_item(typ)?;
    match typ.kind() {
        TypeKind::Primitive | TypeKind::Class => Ok(ast::Type::plain(expect_borrowed(
            bundle.try_get_item_hint(typ.name(), "type name")?,
        ))),
        TypeKind::Ref(inner) => extract_type(inner, bundle),
        TypeKind::WeakRef(inner) => Ok(ast::Type::Named {
            name: "wref",
            args: [extract_type(inner, bundle)?].into(),
        }),
        TypeKind::Array(inner) => Ok(ast::Type::Array(extract_type(inner, bundle)?.into())),
        TypeKind::StaticArray { element_type, size } => Ok(ast::Type::StaticArray(
            extract_type(element_type, bundle)?.into(),
            size as usize,
        )),
        TypeKind::ScriptRef(inner) => Ok(ast::Type::Named {
            name: "script_ref",
            args: [extract_type(inner, bundle)?].into(),
        }),
    }
}

fn extract_visibility(v: Visibility) -> ast::Visibility {
    match v {
        Visibility::Public => ast::Visibility::Public,
        Visibility::Protected => ast::Visibility::Protected,
        Visibility::Private => ast::Visibility::Private,
    }
}

fn extract_class_qualifiers(class: &Class) -> ast::ItemQualifiers {
    let mut qs = ast::ItemQualifiers::empty();
    if class.flags().is_abstract() {
        qs |= ast::ItemQualifiers::ABSTRACT;
    }
    if class.flags().is_final() {
        qs |= ast::ItemQualifiers::FINAL;
    }
    if class.flags().is_import_only() {
        qs |= ast::ItemQualifiers::IMPORT_ONLY;
    }
    if class.flags().is_native() {
        qs |= ast::ItemQualifiers::NATIVE;
    }
    qs
}

fn extract_func_qualifiers(func: &Function<'_>) -> ast::ItemQualifiers {
    let mut qs = ast::ItemQualifiers::empty();
    if func.flags().is_callback() {
        qs |= ast::ItemQualifiers::CALLBACK;
    }
    if func.flags().is_const() {
        qs |= ast::ItemQualifiers::CONST;
    }
    if func.flags().is_exec() {
        qs |= ast::ItemQualifiers::EXEC;
    }
    if func.flags().is_final() && func.class().is_some() {
        qs |= ast::ItemQualifiers::FINAL;
    }
    if func.flags().is_native() {
        qs |= ast::ItemQualifiers::NATIVE;
    }
    if func.flags().is_static() && func.class().is_some() {
        qs |= ast::ItemQualifiers::STATIC;
    }
    qs
}

fn extract_field_qualifiers(field: &Field<'_>) -> ast::ItemQualifiers {
    let mut qs = ast::ItemQualifiers::empty();
    if field.flags().is_const() {
        qs |= ast::ItemQualifiers::CONST;
    }
    if field.flags().is_native() {
        qs |= ast::ItemQualifiers::NATIVE;
    }
    if field.flags().is_persistent() {
        qs |= ast::ItemQualifiers::PERSISTENT;
    }
    qs
}

fn extract_param_qualifiers(param: &Parameter) -> ast::ParamQualifiers {
    let mut qs = ast::ParamQualifiers::empty();
    if param.flags().is_optional() {
        qs |= ast::ParamQualifiers::OPTIONAL;
    }
    if param.flags().is_out() {
        qs |= ast::ParamQualifiers::OUT;
    }
    if param.flags().is_const() {
        qs |= ast::ParamQualifiers::CONST;
    }
    qs
}

fn extract_mangled_name(mangled_name: &str) -> &str {
    mangled_name
        .split_once(';')
        .map(|(name, _)| name)
        .unwrap_or(mangled_name)
}

fn expect_borrowed<'b>(cow: &Cow<'b, str>) -> &'b str {
    match cow {
        Cow::Borrowed(s) => s,
        Cow::Owned(_) => unreachable!(),
    }
}

trait BundleOps<'i>: Sized {
    fn try_get_item<I>(&self, index: I) -> Result<&I::Output>
    where
        I: PoolItemIndex<'i> + Into<u32> + Copy;

    fn try_get_item_hint<I>(&self, index: I, what: &'static str) -> Result<&I::Output>
    where
        I: PoolItemIndex<'i> + Into<u32> + Copy;
}

impl<'i> BundleOps<'i> for ScriptBundle<'i> {
    #[inline]
    fn try_get_item<I>(&self, index: I) -> Result<&I::Output>
    where
        I: PoolItemIndex<'i> + Into<u32> + Copy,
    {
        self.try_get_item_hint(index, std::any::type_name::<I::Output>())
    }

    fn try_get_item_hint<I>(&self, index: I, what: &'static str) -> Result<&I::Output>
    where
        I: PoolItemIndex<'i> + Into<u32> + Copy,
    {
        self.get_item(index)
            .ok_or(Error::MissingPoolItem(index.into(), what))
    }
}

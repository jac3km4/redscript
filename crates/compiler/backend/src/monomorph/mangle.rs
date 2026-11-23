use std::fmt;

use redscript_compiler_frontend::utils::ScopedMap;
use redscript_compiler_frontend::{
    FreeFunction, FunctionKind, Method, MonoType, QualifiedName, Symbols, TypeSchema, ir, predef,
};

use crate::inputs::Signature;

#[derive(Debug)]
pub(super) struct MangledType<'a, 'ctx> {
    typ: &'a MonoType<'ctx>,
    symbols: &'a Symbols<'ctx>,
}

impl<'a, 'ctx> MangledType<'a, 'ctx> {
    #[inline]
    pub(super) fn new(typ: &'a MonoType<'ctx>, symbols: &'a Symbols<'ctx>) -> Self {
        Self { typ, symbols }
    }
}

impl fmt::Display for MangledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.symbols[self.typ.id()].schema() {
            TypeSchema::Aggregate(agg) => {
                if agg.flags().is_class_handle() {
                    write!(f, "ref:")?;
                }
                if let Some(name) = agg.named_implementation(self.typ) {
                    write!(f, "{name}")
                } else {
                    write!(f, "{}", self.typ)
                }
            }
            TypeSchema::Enum(_) => {
                write!(f, "{}", self.typ)
            }
            TypeSchema::Primitive => match (
                self.typ.id(),
                self.typ.id().static_array_size(),
                self.typ.args(),
            ) {
                (_, Some(size), [arg]) => {
                    write!(f, "{}[{}]", MangledType::new(arg, self.symbols), size)
                }
                (id, _, [arg]) if id == predef::REF || id == predef::WREF => {
                    write!(f, "{id}:{arg}")
                }
                (id, _, [arg]) => write!(f, "{id}:{}", MangledType::new(arg, self.symbols)),
                (id, _, _) => write!(f, "{id}"),
            },
        }
    }
}

#[derive(Debug)]
pub(super) struct MangledFreeFunction<'a, 'ctx, K> {
    name: &'a QualifiedName<'ctx>,
    signature: &'a Signature<'ctx, K>,
    func: &'a FreeFunction<'ctx>,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, K> MangledFreeFunction<'a, 'ctx, K> {
    pub(super) fn new(
        name: &'a QualifiedName<'ctx>,
        signature: &'a Signature<'ctx, K>,
        func: &'a FreeFunction<'ctx>,
        env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
    ) -> Self {
        Self {
            name,
            signature,
            func,
            env,
        }
    }
}

impl<K> fmt::Display for MangledFreeFunction<'_, '_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.name, self.signature.display_args())?;
        if !self.func.flags().is_cast()
            && !self.func.flags().is_operator()
            && (self.func.flags().is_exec() || self.func.flags().is_native())
        {
            return Ok(());
        };
        write!(f, ";{}", MangledParams::new(self.func, self.env))?;
        if self.func.flags().is_cast()
            || self.name.as_ref().last() == Some(&"OperatorArray")
            || (self.func.flags().is_operator()
                && self
                    .func
                    .type_()
                    .params()
                    .iter()
                    .any(|param| param.flags().is_short_circuit()))
        {
            let rt = self
                .func
                .type_()
                .return_type()
                .unwrap_ref_or_self()
                .assume_mono(self.env);
            write!(f, ";{rt}",)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(super) struct MangledMethod<'a, 'ctx, K> {
    name: &'a str,
    signature: &'a Signature<'ctx, K>,
    method: &'a Method<'ctx>,
    base: Option<&'a Method<'ctx>>,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, K> MangledMethod<'a, 'ctx, K> {
    pub(super) fn new(
        name: &'a str,
        signature: &'a Signature<'ctx, K>,
        method: &'a Method<'ctx>,
        base: Option<&'a Method<'ctx>>,
        env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
    ) -> Self {
        Self {
            name,
            signature,
            method,
            base,
            env,
        }
    }
}

impl<K> fmt::Display for MangledMethod<'_, '_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.name, self.signature.display_args())?;
        if self.base.is_some_and(|m| m.flags().is_native())
            || self.method.flags().is_callback()
            || self.method.flags().is_native()
        {
            return Ok(());
        };
        write!(f, ";{}", MangledParams::new(self.method, self.env))
    }
}

#[derive(Debug)]
struct MangledParams<'a, 'ctx, F> {
    func: F,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, F> MangledParams<'a, 'ctx, F> {
    fn new(func: F, env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>) -> Self {
        Self { func, env }
    }
}

impl<'ctx, F: FunctionKind<'ctx>> fmt::Display for MangledParams<'_, 'ctx, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.func.type_().params().iter().try_for_each(|param| {
            if param.flags().is_out() {
                write!(f, "Out")?;
            }
            if param.flags().is_short_circuit() {
                write!(f, "Skip")?;
            }
            let typ = param.type_().unwrap_ref_or_self().assume_mono(self.env);
            write!(f, "{typ}",)
        })
    }
}

#[derive(Debug)]
pub(super) struct MangledConstant<'a, 'ctx> {
    const_: &'a ir::Const<'ctx>,
    symbols: &'a Symbols<'ctx>,
}

impl<'a, 'ctx> MangledConstant<'a, 'ctx> {
    pub(super) fn new(const_: &'a ir::Const<'ctx>, symbols: &'a Symbols<'ctx>) -> Self {
        Self { const_, symbols }
    }
}

impl fmt::Display for MangledConstant<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.const_ {
            ir::Const::Str(cow)
            | ir::Const::CName(cow)
            | ir::Const::ResRef(cow)
            | ir::Const::TweakDbId(cow) => write!(f, "{cow}"),
            ir::Const::EnumVariant(field_id) => {
                let (name, _) = self
                    .symbols
                    .get_enum_variant(*field_id)
                    .expect("field should exist");
                write!(f, "{}.{}", field_id.parent().simple_name(), name)
            }
            ir::Const::F32(val) => write!(f, "{val}"),
            ir::Const::F64(val) => write!(f, "{val}"),
            ir::Const::I8(val) => write!(f, "{val}"),
            ir::Const::I16(val) => write!(f, "{val}"),
            ir::Const::I32(val) => write!(f, "{val}"),
            ir::Const::I64(val) => write!(f, "{val}"),
            ir::Const::U8(val) => write!(f, "{val}"),
            ir::Const::U16(val) => write!(f, "{val}"),
            ir::Const::U32(val) => write!(f, "{val}"),
            ir::Const::U64(val) => write!(f, "{val}"),
            ir::Const::Bool(val) => write!(f, "{val}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_frontend::{
        FreeFunctionFlags, FunctionType, MethodFlags, Param, ParamFlags, Type, TypeId,
        TypeParamVar, Variance,
    };
    use similar_asserts::assert_eq;

    use super::*;

    #[test]
    fn type_mangling() {
        let symbols = Symbols::with_default_types();

        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::STRING), &symbols).to_string(),
            "String"
        );
        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::INT32), &symbols).to_string(),
            "Int32"
        );
        assert_eq!(
            MangledType::new(
                &MonoType::new(predef::ARRAY, [MonoType::nullary(predef::INT8)]),
                &symbols
            )
            .to_string(),
            "array:Int8"
        );
        assert_eq!(
            MangledType::new(
                &MonoType::new(TypeId::static_array(1), [MonoType::nullary(predef::UINT8)]),
                &symbols
            )
            .to_string(),
            "Uint8[1]"
        );
        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::ISCRIPTABLE), &symbols).to_string(),
            "ref:IScriptable"
        );
    }

    #[test]
    fn function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default(),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &ScopedMap::default()
            )
            .to_string(),
            "foo;Int32"
        );
    }

    #[test]
    fn native_function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default().with_is_native(true),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &ScopedMap::default()
            )
            .to_string(),
            "foo"
        );
    }

    #[test]
    fn generic_function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default(),
            FunctionType::new([], [param("a", ctx("A"))], Type::nullary(predef::VOID)),
            [],
            None,
        );

        let sig = Signature::new((), [MonoType::nullary(predef::INT32)]);
        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &[("A", MonoType::nullary(predef::INT32))]
                    .into_iter()
                    .collect(),
            )
            .to_string(),
            "foo<Int32>;Int32"
        );
    }

    #[test]
    fn method_mangling() {
        let func = Method::new(
            MethodFlags::default(),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            None,
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledMethod::new("foo", &sig, &func, None, &ScopedMap::default()).to_string(),
            "foo;Int32"
        );
    }

    #[test]
    fn native_method_mangling() {
        let func = Method::new(
            MethodFlags::default().with_is_native(true),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            None,
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledMethod::new("foo", &sig, &func, None, &ScopedMap::default()).to_string(),
            "foo"
        );
    }

    #[test]
    fn generic_method_mangling() {
        let func = Method::new(
            MethodFlags::default(),
            FunctionType::new([], [param("a", ctx("A"))], Type::nullary(predef::VOID)),
            None,
            [],
            None,
        );

        let sig = Signature::new((), [MonoType::nullary(predef::INT32)]);
        assert_eq!(
            MangledMethod::new(
                "foo",
                &sig,
                &func,
                None,
                &[("A", MonoType::nullary(predef::INT32))]
                    .into_iter()
                    .collect(),
            )
            .to_string(),
            "foo<Int32>;Int32"
        );
    }

    fn param<'ctx>(name: &'ctx str, typ: Type<'ctx>) -> Param<'ctx> {
        Param::new(name, ParamFlags::default(), typ, None)
    }

    fn ctx<'ctx>(name: &'ctx str) -> Type<'ctx> {
        Type::Ctx(TypeParamVar::new(name, Variance::Invariant, None, None).into())
    }
}

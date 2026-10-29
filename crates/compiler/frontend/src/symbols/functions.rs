use std::fmt;
use std::rc::Rc;

use derive_where::derive_where;
use redscript_ast::Span;

use super::{FreeFunctionFlags, MethodFlags, MethodId, ParamFlags};
use crate::types::{CtxVar, Type};
use crate::{Immutable, TypeKind, ir, predef};

#[derive(Debug, Clone)]
pub struct FunctionType<'ctx> {
    type_params: Box<[Rc<CtxVar<'ctx>>]>,
    params: Box<[Param<'ctx>]>,
    return_type: Type<'ctx>,
}

impl<'ctx> FunctionType<'ctx> {
    #[inline]
    pub fn new(
        type_params: impl Into<Box<[Rc<CtxVar<'ctx>>]>>,
        params: impl Into<Box<[Param<'ctx>]>>,
        ret_t: Type<'ctx>,
    ) -> Self {
        Self {
            type_params: type_params.into(),
            params: params.into(),
            return_type: ret_t,
        }
    }

    pub fn with_params(self, params: impl Into<Box<[Param<'ctx>]>>) -> Self {
        Self {
            params: params.into(),
            ..self
        }
    }

    pub fn with_return_type(self, ret_t: Type<'ctx>) -> Self {
        Self {
            return_type: ret_t,
            ..self
        }
    }

    #[inline]
    pub fn type_params(&self) -> &[Rc<CtxVar<'ctx>>] {
        &self.type_params
    }

    #[inline]
    pub fn type_vars(&self) -> impl ExactSizeIterator<Item = &'ctx str> + use<'_, 'ctx> {
        self.type_params.iter().map(|v| v.name())
    }

    #[inline]
    pub fn params(&self) -> &[Param<'ctx>] {
        &self.params
    }

    #[inline]
    pub fn param_types(&self) -> impl ExactSizeIterator<Item = &Type<'ctx>> + use<'_, 'ctx> {
        self.params.iter().map(Param::type_)
    }

    #[inline]
    pub fn unwrapped_param_types(
        &self,
    ) -> impl ExactSizeIterator<Item = &Type<'ctx>> + use<'_, 'ctx> {
        self.param_types().map(Type::unwrap_ref_or_self)
    }

    #[inline]
    pub fn return_type(&self) -> &Type<'ctx> {
        &self.return_type
    }

    pub fn min_args(&self) -> usize {
        self.params
            .iter()
            .rev()
            .skip_while(|p| p.flags.is_optional())
            .count()
    }

    #[inline]
    pub fn max_args(&self) -> usize {
        self.params.len()
    }
}

impl Default for FunctionType<'_> {
    fn default() -> Self {
        Self {
            type_params: Box::default(),
            params: Box::default(),
            return_type: Type::nullary(predef::VOID),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct FreeFunction<'ctx> {
    flags: FreeFunctionFlags,
    type_: FunctionType<'ctx>,
    intrinsic: Option<ir::Intrinsic>,
    method_alias: Option<MethodId<'ctx>>,
    doc: Box<[&'ctx str]>,
    span: Option<Span>,
}

impl<'ctx> FreeFunction<'ctx> {
    pub fn new(
        flags: FreeFunctionFlags,
        type_: FunctionType<'ctx>,
        doc: impl Into<Box<[&'ctx str]>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            flags,
            type_,
            doc: doc.into(),
            span,
            ..Default::default()
        }
    }

    pub fn new_intrinsic(
        type_: FunctionType<'ctx>,
        intrinsic: ir::Intrinsic,
        doc: impl Into<Box<[&'ctx str]>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            type_,
            intrinsic: Some(intrinsic),
            doc: doc.into(),
            span,
            ..Default::default()
        }
    }

    pub fn new_alias(type_: FunctionType<'ctx>, alias: MethodId<'ctx>, span: Option<Span>) -> Self {
        Self {
            type_,
            span,
            method_alias: Some(alias),
            ..Default::default()
        }
    }

    #[inline]
    pub fn flags(&self) -> FreeFunctionFlags {
        self.flags
    }

    #[inline]
    pub fn type_(&self) -> &FunctionType<'ctx> {
        &self.type_
    }

    #[inline]
    pub fn intrinsic(&self) -> Option<ir::Intrinsic> {
        self.intrinsic
    }

    #[inline]
    pub fn method_alias(&self) -> Option<MethodId<'ctx>> {
        self.method_alias
    }

    #[inline]
    pub fn doc(&self) -> &[&'ctx str] {
        &self.doc
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Method<'ctx> {
    flags: MethodFlags,
    typ: FunctionType<'ctx>,
    overriden: Option<MethodId<'ctx>>,
    doc: Box<[&'ctx str]>,
    span: Option<Span>,
    aliased_method: Option<MethodId<'ctx>>,
}

impl<'ctx> Method<'ctx> {
    pub fn new(
        flags: MethodFlags,
        typ: FunctionType<'ctx>,
        overridden: Option<MethodId<'ctx>>,
        doc: impl Into<Box<[&'ctx str]>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            flags,
            typ,
            overriden: overridden,
            doc: doc.into(),
            span,
            aliased_method: None,
        }
    }

    pub fn with_aliased(mut self, alias: MethodId<'ctx>) -> Self {
        self.aliased_method = Some(alias);
        self
    }

    #[inline]
    pub fn flags(&self) -> MethodFlags {
        self.flags
    }

    #[inline]
    pub fn flags_mut(&mut self) -> &mut MethodFlags {
        &mut self.flags
    }

    #[inline]
    pub fn type_(&self) -> &FunctionType<'ctx> {
        &self.typ
    }

    #[inline]
    pub fn overriden(&self) -> Option<MethodId<'ctx>> {
        self.overriden
    }

    pub fn set_overriden(&mut self, overridden: MethodId<'ctx>) {
        self.overriden = Some(overridden);
    }

    #[inline]
    pub fn doc(&self) -> &[&'ctx str] {
        &self.doc
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }

    pub fn is_user_defined(&self) -> bool {
        self.span().is_some()
    }

    #[inline]
    pub fn aliased_method(&self) -> Option<MethodId<'ctx>> {
        self.aliased_method
    }
}

pub trait FunctionKind<'ctx> {
    fn type_(&self) -> &FunctionType<'ctx>;
    fn intrinsic(&self) -> Option<ir::Intrinsic> {
        None
    }
}

impl<'ctx> FunctionKind<'ctx> for FreeFunction<'ctx> {
    #[inline]
    fn type_(&self) -> &FunctionType<'ctx> {
        &self.type_
    }

    #[inline]
    fn intrinsic(&self) -> Option<ir::Intrinsic> {
        self.intrinsic
    }
}

impl<'ctx> FunctionKind<'ctx> for Method<'ctx> {
    #[inline]
    fn type_(&self) -> &FunctionType<'ctx> {
        &self.typ
    }
}

impl<'ctx, A: FunctionKind<'ctx>> FunctionKind<'ctx> for &A {
    #[inline]
    fn type_(&self) -> &FunctionType<'ctx> {
        (*self).type_()
    }

    #[inline]
    fn intrinsic(&self) -> Option<ir::Intrinsic> {
        (*self).intrinsic()
    }
}

#[derive_where(Debug, Clone)]
pub struct Param<'ctx, K: TypeKind = Immutable> {
    name: &'ctx str,
    flags: ParamFlags,
    typ: K::Type<'ctx>,
    span: Option<Span>,
}

impl<'ctx, K: TypeKind> Param<'ctx, K> {
    pub fn new(name: &'ctx str, flags: ParamFlags, typ: K::Type<'ctx>, span: Option<Span>) -> Self {
        Self {
            name,
            flags,
            typ,
            span,
        }
    }

    #[inline]
    pub fn name(&self) -> &'ctx str {
        self.name
    }

    #[inline]
    pub fn flags(&self) -> ParamFlags {
        self.flags
    }

    #[inline]
    pub fn type_(&self) -> &K::Type<'ctx> {
        &self.typ
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl<K: TypeKind> fmt::Display for Param<'_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.flags.is_optional() {
            write!(f, "opt ")?;
        }
        if self.flags.is_const() {
            write!(f, "const ")?;
        }
        if self.flags.is_out() {
            write!(f, "out ")?;
        }
        write!(f, "{}: {}", self.name, self.typ)
    }
}

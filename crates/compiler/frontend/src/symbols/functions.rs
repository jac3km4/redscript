use std::fmt;

use redscript_community_ast::{FunctionProperties, Span};

use super::{FreeFunctionFlags, MethodFlags, MethodId, ParamFlags};
use crate::types::{Type, TypeParamVar};
use crate::{Shared, ir, predef};

#[derive(Debug, Clone)]
pub struct FunctionType<'ctx> {
    type_params: Box<[Shared<TypeParamVar<'ctx>>]>,
    params: Box<[Param<'ctx>]>,
    return_type: Type<'ctx>,
    properties: FunctionProperties<'ctx>,
    invalid_signature: bool,
}

impl<'ctx> FunctionType<'ctx> {
    #[inline]
    pub fn new(
        type_params: impl Into<Box<[Shared<TypeParamVar<'ctx>>]>>,
        params: impl Into<Box<[Param<'ctx>]>>,
        ret_t: Type<'ctx>,
    ) -> Self {
        Self {
            type_params: type_params.into(),
            params: params.into(),
            return_type: ret_t,
            properties: FunctionProperties::default(),
            invalid_signature: false,
        }
    }

    /// Invalid signatures retain their written slots for analysis only. Unresolved
    /// slots use `Type::Nothing` as storage, not as an inferable bottom type.
    /// Consumers must not lower bodies or compare/select these as ordinary signatures.
    pub fn with_invalid_signature(mut self, invalid: bool) -> Self {
        self.invalid_signature = invalid;
        self
    }

    pub fn is_valid(&self) -> bool {
        !self.invalid_signature
    }

    pub fn with_properties(mut self, properties: FunctionProperties<'ctx>) -> Self {
        self.properties = properties;
        self
    }

    pub fn properties(&self) -> &FunctionProperties<'ctx> {
        &self.properties
    }

    pub fn is_const_return(&self) -> bool {
        self.properties.is_const_return
    }

    pub fn conversion(&self) -> Option<redscript_community_ast::ConversionKind> {
        self.properties.conversion
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
    pub fn type_params(&self) -> &[Shared<TypeParamVar<'ctx>>] {
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
            properties: FunctionProperties::default(),
            invalid_signature: false,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct FreeFunction<'ctx> {
    flags: FreeFunctionFlags,
    type_: FunctionType<'ctx>,
    intrinsic: Option<ir::Intrinsic>,
    wrap_target: Option<MethodId<'ctx>>,
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

    /// Whether this intrinsic still has a parameter explicitly typed
    /// `IScriptable`, i.e. whether [`Self::use_serializable_root`] would change
    /// its signature.
    pub(super) fn needs_serializable_root(&self) -> bool {
        self.intrinsic.is_some()
            && self
                .type_
                .params
                .iter()
                .any(|param| param.typ == Type::nullary(predef::ISCRIPTABLE))
    }

    /// Point explicitly `IScriptable`-typed intrinsic parameters at the engine
    /// root. Intrinsic signatures are compiler-owned, so this cannot rewrite a
    /// user-written annotation or a generic bound.
    pub(super) fn use_serializable_root(&mut self) {
        if self.intrinsic.is_none() {
            return;
        }
        for param in self.type_.params.iter_mut() {
            if param.typ == Type::nullary(predef::ISCRIPTABLE) {
                param.typ = Type::nullary(predef::ISERIALIZABLE);
            }
        }
    }

    pub fn new_wrap_target(
        type_: FunctionType<'ctx>,
        target: MethodId<'ctx>,
        span: Option<Span>,
    ) -> Self {
        Self {
            type_,
            span,
            wrap_target: Some(target),
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
    pub fn wrap_target(&self) -> Option<MethodId<'ctx>> {
        self.wrap_target
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
    overridden: Option<MethodId<'ctx>>,
    doc: Box<[&'ctx str]>,
    span: Option<Span>,
    forwarded_static_method: Option<MethodId<'ctx>>,
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
            overridden,
            doc: doc.into(),
            span,
            forwarded_static_method: None,
        }
    }

    pub fn with_forwarded_static_method(mut self, method: MethodId<'ctx>) -> Self {
        self.forwarded_static_method = Some(method);
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
    pub fn overridden(&self) -> Option<MethodId<'ctx>> {
        self.overridden
    }

    pub fn set_overridden(&mut self, overridden: MethodId<'ctx>) {
        self.overridden = Some(overridden);
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
    pub fn forwarded_static_method(&self) -> Option<MethodId<'ctx>> {
        self.forwarded_static_method
    }
}

pub trait FunctionKind<'ctx> {
    fn type_(&self) -> &FunctionType<'ctx>;

    fn intrinsic(&self) -> Option<ir::Intrinsic> {
        None
    }

    fn scope_priority(&self) -> u32 {
        0
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

    #[inline]
    fn scope_priority(&self) -> u32 {
        u32::from(self.flags().is_static_forwarder())
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

    #[inline]
    fn scope_priority(&self) -> u32 {
        (*self).scope_priority()
    }
}

#[derive(Debug, Clone)]
pub struct Param<'ctx, T = Type<'ctx>> {
    name: &'ctx str,
    flags: ParamFlags,
    typ: T,
    span: Option<Span>,
}

impl<'ctx, T> Param<'ctx, T> {
    pub fn new(name: &'ctx str, flags: ParamFlags, typ: T, span: Option<Span>) -> Self {
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
    pub fn type_(&self) -> &T {
        &self.typ
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl<T: fmt::Display> fmt::Display for Param<'_, T> {
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
        if self.flags.is_short_circuit() {
            write!(f, "skip ")?;
        }
        write!(f, "{}: {}", self.name, self.typ)
    }
}

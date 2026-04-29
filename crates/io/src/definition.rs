use std::borrow::Cow;
use std::{iter, mem};

use bitfield_struct::bitfield;
use byte::ctx::Endianess;
use byte::{BytesExt, Measure, TryRead, TryWrite};

use crate::index::{
    CNameIndex, ClassIndex, EnumIndex, EnumValueIndex, FieldIndex, FunctionIndex, LocalIndex,
    NzPoolIndex, ParameterIndex, PoolIndex, SourceFileIndex, TypeIndex,
};
use crate::instr::Instr;
use crate::util::{self, FlagDependent, OptionalIndex, Prefixed};
use crate::{ENDIANESS, Offset, Str};

#[derive(Debug, Default, Clone, Copy, TryRead, TryWrite, Measure)]
/// Header information for a definition in the pool.
pub struct DefinitionHeader {
    name: CNameIndex,
    parent: u32,
    offset: u32,
    size: u32,
    type_tag: u16,
    padding: [u8; 2],
}

impl DefinitionHeader {
    #[inline]
    pub(crate) fn from_defintion(def: &Definition<'_>, size: u32, offset: u32) -> Self {
        DefinitionHeader {
            name: def.name(),
            parent: def.parent(),
            offset,
            size,
            type_tag: def.tag(),
            padding: [0; 2],
        }
    }

    #[inline]
    pub(crate) fn offset(&self) -> u32 {
        self.offset
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Represents a definition in the pool.
pub enum Definition<'i> {
    Type(Type),
    Class(Box<Class>),
    EnumMember(EnumMember),
    Enum(Box<Enum>),
    Bitfield,
    Function(Box<Function<'i>>),
    Parameter(Parameter),
    Local(Local),
    Field(Box<Field<'i>>),
    SourceFile(Box<SourceFile<'i>>),
}

impl Definition<'_> {
    pub(crate) const UNDEFINED: Self =
        Definition::Type(Type::new(CNameIndex::UNDEFINED, TypeKind::Primitive));

    fn tag(&self) -> u16 {
        match self {
            Definition::Type(_) => 0,
            Definition::Class(_) => 1,
            Definition::EnumMember(_) => 2,
            Definition::Enum(_) => 3,
            Definition::Bitfield => 4,
            Definition::Function(_) => 5,
            Definition::Parameter(_) => 6,
            Definition::Local(_) => 7,
            Definition::Field(_) => 8,
            Definition::SourceFile(_) => 9,
        }
    }

    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        match self {
            Definition::Type(t) => t.name,
            Definition::Class(c) => c.name,
            Definition::EnumMember(v) => v.name,
            Definition::Enum(e) => e.name,
            Definition::Function(f) => f.name,
            Definition::Parameter(p) => p.name,
            Definition::Local(l) => l.name,
            Definition::Field(f) => f.name,
            Definition::SourceFile(_) | Definition::Bitfield => CNameIndex::UNDEFINED,
        }
    }

    fn parent(&self) -> u32 {
        match self {
            Definition::Type(_)
            | Definition::Class(_)
            | Definition::Enum(_)
            | Definition::Bitfield
            | Definition::SourceFile(_) => 0,
            Definition::EnumMember(v) => v.enum_.into(),
            Definition::Function(f) => f.class.map_or(0, u32::from),
            Definition::Parameter(p) => p.function.into(),
            Definition::Local(l) => l.function.into(),
            Definition::Field(f) => f.class.into(),
        }
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> Definition<'static> {
        match self {
            Definition::Type(t) => Definition::Type(t),
            Definition::Class(c) => Definition::Class(c.clone()),
            Definition::EnumMember(v) => Definition::EnumMember(v),
            Definition::Enum(e) => Definition::Enum(e.clone()),
            Definition::Bitfield => Definition::Bitfield,
            Definition::Function(f) => Definition::Function(Box::new(f.into_owned())),
            Definition::Parameter(p) => Definition::Parameter(p),
            Definition::Local(l) => Definition::Local(l),
            Definition::Field(f) => Definition::Field(Box::new(f.into_owned())),
            Definition::SourceFile(f) => Definition::SourceFile(Box::new(f.into_owned())),
        }
    }
}

impl<'i, Ctx: Endianess> TryRead<'i, (Ctx, DefinitionHeader)> for Definition<'i> {
    fn try_read(
        bytes: &'i [u8],
        (ctx, header): (Ctx, DefinitionHeader),
    ) -> byte::Result<(Self, usize)> {
        let DefinitionHeader {
            name,
            parent,
            size,
            type_tag,
            ..
        } = header;

        let in_bytes = &bytes[..size as _];
        let offset = &mut 0;

        const BAD_INDEX: byte::Error = byte::Error::BadInput {
            err: "definition pool index set to zero",
        };

        let result = match type_tag {
            0 => {
                debug_assert!(parent == 0);
                Definition::Type(Type {
                    name,
                    ..in_bytes.read(offset, ctx)?
                })
            }
            1 => {
                debug_assert!(parent == 0);
                Definition::Class(
                    Class {
                        name,
                        ..in_bytes.read(offset, ctx)?
                    }
                    .into(),
                )
            }
            2 => Definition::EnumMember(EnumMember {
                name,
                enum_: NzPoolIndex::new(parent).ok_or(BAD_INDEX)?,
                value: in_bytes.read(offset, ctx)?,
            }),
            3 => {
                debug_assert!(parent == 0);
                Definition::Enum(
                    Enum {
                        name,
                        ..in_bytes.read(offset, ctx)?
                    }
                    .into(),
                )
            }
            4 => Definition::Bitfield,
            5 => Definition::Function(
                Function {
                    name,
                    class: NzPoolIndex::new(parent),
                    ..in_bytes.read(offset, ctx)?
                }
                .into(),
            ),
            6 => Definition::Parameter(Parameter {
                name,
                function: NzPoolIndex::new(parent).ok_or(BAD_INDEX)?,
                ..in_bytes.read(offset, ctx)?
            }),
            7 => Definition::Local(Local {
                name,
                function: NzPoolIndex::new(parent).ok_or(BAD_INDEX)?,
                ..in_bytes.read(offset, ctx)?
            }),
            8 => Definition::Field(
                Field {
                    name,
                    class: NzPoolIndex::new(parent).ok_or(BAD_INDEX)?,
                    ..in_bytes.read(offset, ctx)?
                }
                .into(),
            ),
            9 => {
                debug_assert!(parent == 0 && u32::from(name) == 0);
                Definition::SourceFile(Box::new(in_bytes.read(offset, ctx)?))
            }
            _ => {
                return Err(byte::Error::BadInput {
                    err: "invalid definition type",
                });
            }
        };
        Ok((result, *offset))
    }
}

impl<Ctx: Endianess> TryWrite<Ctx> for Definition<'_> {
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> byte::Result<usize> {
        let offset = &mut 0;
        match self {
            Definition::Type(t) => bytes.write(offset, t, ctx)?,
            Definition::Class(c) => bytes.write(offset, c.as_ref(), ctx)?,
            Definition::EnumMember(v) => bytes.write(offset, v, ctx)?,
            Definition::Enum(e) => bytes.write(offset, e.as_ref(), ctx)?,
            Definition::Bitfield => {}
            Definition::Function(f) => bytes.write(offset, f.as_ref(), ctx)?,
            Definition::Parameter(p) => bytes.write(offset, p, ctx)?,
            Definition::Local(l) => bytes.write(offset, l, ctx)?,
            Definition::Field(f) => bytes.write(offset, f.as_ref(), ctx)?,
            Definition::SourceFile(f) => bytes.write(offset, f.as_ref(), ctx)?,
        };
        Ok(*offset)
    }
}

impl<Ctx: Copy> Measure<Ctx> for Definition<'_> {
    fn measure(&self, ctx: Ctx) -> usize {
        match self {
            Definition::Type(typ) => typ.measure(ctx),
            Definition::Class(class) => class.measure(ctx),
            Definition::EnumMember(v) => v.measure(ctx),
            Definition::Enum(e) => e.measure(ctx),
            Definition::Bitfield => 0,
            Definition::Function(f) => f.measure(ctx),
            Definition::Parameter(p) => p.measure(ctx),
            Definition::Local(l) => l.measure(ctx),
            Definition::Field(f) => f.measure(ctx),
            Definition::SourceFile(f) => f.measure(ctx),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// A definition with its corresponding index.
pub enum IndexedDefinition<'b, 'i> {
    Type(TypeIndex, &'b Type),
    Class(ClassIndex, &'b Class),
    EnumMember(EnumValueIndex, &'b EnumMember),
    Enum(EnumIndex, &'b Enum),
    Bitfield,
    Function(FunctionIndex, &'b Function<'i>),
    Parameter(ParameterIndex, &'b Parameter),
    Local(LocalIndex, &'b Local),
    Field(FieldIndex, &'b Field<'i>),
    SourceFile(SourceFileIndex, &'b SourceFile<'i>),
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a type definition.
pub struct Type {
    #[byte(skip)]
    name: CNameIndex,
    kind: TypeKind,
}

impl Type {
    #[inline]
    pub const fn new(name: CNameIndex, kind: TypeKind) -> Self {
        Self { name, kind }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the kind of this type.
    pub fn kind(&self) -> TypeKind {
        self.kind
    }
}

impl From<Type> for Definition<'_> {
    #[inline]
    fn from(t: Type) -> Self {
        Definition::Type(t)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryRead, TryWrite, Measure)]
#[byte(tag_type = u8)]
/// Specifies the kind of a type.
pub enum TypeKind {
    #[byte(tag = 0x00)]
    Primitive,
    #[byte(tag = 0x01)]
    Class,
    #[byte(tag = 0x02)]
    Ref(TypeIndex),
    #[byte(tag = 0x03)]
    WeakRef(TypeIndex),
    #[byte(tag = 0x04)]
    Array(TypeIndex),
    #[byte(tag = 0x05)]
    StaticArray { element_type: TypeIndex, size: u32 },
    #[byte(tag = 0x06)]
    ScriptRef(TypeIndex),
}

#[derive(Debug, Default, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a class definition.
pub struct Class {
    #[byte(skip)]
    name: CNameIndex,
    visiblity: Visibility,
    flags: ClassFlags,
    #[byte(ctx = OptionalIndex(ctx))]
    base: Option<ClassIndex>,
    #[byte(ctx = Prefixed(ctx), skip_if = !flags.has_functions())]
    methods: Vec<FunctionIndex>,
    #[byte(ctx = Prefixed(ctx), skip_if = !flags.has_fields())]
    fields: Vec<FieldIndex>,
    #[byte(ctx = Prefixed(ctx), skip_if = !flags.has_overrides())]
    overrides: Vec<FieldIndex>,
}

impl Class {
    /// Creates a new instance.
    pub fn new(name: CNameIndex, visiblity: Visibility, flags: ClassFlags) -> Self {
        Self {
            name,
            visiblity,
            flags,
            base: None,
            methods: vec![],
            fields: vec![],
            overrides: vec![],
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the visibility.
    pub fn visibility(&self) -> Visibility {
        self.visiblity
    }

    #[inline]
    /// Gets the flags.
    pub fn flags(&self) -> ClassFlags {
        self.flags
    }

    #[inline]
    /// Gets the base class index.
    pub fn base(&self) -> Option<ClassIndex> {
        self.base
    }

    #[inline]
    /// Gets the list of methods.
    pub fn methods(&self) -> &[FunctionIndex] {
        &self.methods
    }

    #[inline]
    /// Gets the list of fields.
    pub fn fields(&self) -> &[FieldIndex] {
        &self.fields
    }

    #[inline]
    /// Gets a mutable list of fields.
    pub fn fields_mut(&mut self) -> &mut Vec<FieldIndex> {
        &mut self.fields
    }

    #[inline]
    /// Gets the list of overrides.
    pub fn overrides(&self) -> &[FieldIndex] {
        &self.overrides
    }

    /// Returns a copy of this object with the `base` updated.
    pub fn with_base(mut self, base: Option<ClassIndex>) -> Self {
        self.base = base;
        self
    }

    /// Returns a copy of this object with the `methods` updated.
    pub fn with_methods(mut self, methods: impl Into<Vec<FunctionIndex>>) -> Self {
        self.methods = methods.into();
        self.flags.set_has_functions(!self.methods.is_empty());
        self
    }

    /// Adds to the `methods` of this object.
    pub fn add_method(&mut self, method: FunctionIndex) {
        self.methods.push(method);
        self.flags.set_has_functions(true);
    }

    /// Returns a copy of this object with the `fields` updated.
    pub fn with_fields(mut self, fields: impl Into<Vec<FieldIndex>>) -> Self {
        self.fields = fields.into();
        self.flags.set_has_fields(!self.fields.is_empty());
        self
    }

    /// Adds to the `fields` of this object.
    pub fn add_field(&mut self, field: FieldIndex) {
        self.fields.push(field);
        self.flags.set_has_fields(true);
    }

    /// Returns a copy of this object with the `overrides` updated.
    pub fn with_overrides(mut self, overrides: impl Into<Vec<FieldIndex>>) -> Self {
        self.overrides = overrides.into();
        self.flags.set_has_overrides(!self.overrides.is_empty());
        self
    }
}

impl From<Class> for Definition<'_> {
    #[inline]
    fn from(c: Class) -> Self {
        Definition::Class(Box::new(c))
    }
}

#[bitfield(u16)]
#[derive(PartialEq, Eq)]
/// Flags indicating properties of a class.
pub struct ClassFlags {
    pub is_native: bool,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_struct: bool,
    has_functions: bool,
    has_fields: bool,
    pub is_import_only: bool,
    pub is_test_only: bool,
    has_overrides: bool,
    #[bits(7)]
    __: u8,
}

util::impl_bitfield_read_write!(ClassFlags);

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a member of an enumeration.
pub struct EnumMember {
    #[byte(skip)]
    name: CNameIndex,
    #[byte(skip)]
    enum_: EnumIndex,
    value: i64,
}

impl EnumMember {
    #[inline]
    /// Creates a new instance.
    pub fn new(name: CNameIndex, enum_: EnumIndex, value: i64) -> Self {
        Self { name, enum_, value }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the enum index.
    pub fn enum_(&self) -> EnumIndex {
        self.enum_
    }

    #[inline]
    /// Gets the value.
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl From<EnumMember> for Definition<'_> {
    #[inline]
    fn from(v: EnumMember) -> Self {
        Definition::EnumMember(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents an enumeration definition.
pub struct Enum {
    #[byte(skip)]
    name: CNameIndex,
    visiblity: Visibility,
    size: u8,
    #[byte(ctx = Prefixed(ctx))]
    values: Vec<EnumValueIndex>,
    is_native: bool,
}

impl Enum {
    /// Creates a new instance.
    pub fn new(name: CNameIndex, visiblity: Visibility, size: u8) -> Self {
        Self {
            name,
            visiblity,
            size,
            values: vec![],
            is_native: false,
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the visibility.
    pub fn visibility(&self) -> Visibility {
        self.visiblity
    }

    #[inline]
    /// Gets the size.
    pub fn size(&self) -> u8 {
        self.size
    }

    #[inline]
    /// Gets the list of enum values.
    pub fn values(&self) -> &[EnumValueIndex] {
        &self.values
    }

    #[inline]
    /// Returns whether this is native.
    pub fn is_native(&self) -> bool {
        self.is_native
    }

    /// Returns a copy of this object with the `values` updated.
    pub fn with_values(mut self, values: impl Into<Vec<EnumValueIndex>>) -> Self {
        self.values = values.into();
        self
    }

    /// Returns a copy of this object with the `is_native` updated.
    pub fn with_is_native(mut self, is_native: bool) -> Self {
        self.is_native = is_native;
        self
    }
}

impl From<Enum> for Definition<'_> {
    #[inline]
    fn from(e: Enum) -> Self {
        Definition::Enum(Box::new(e))
    }
}

#[derive(Debug, Clone, PartialEq, TryRead, TryWrite, Measure)]
/// Represents a function or method definition.
pub struct Function<'i> {
    #[byte(skip)]
    name: CNameIndex,
    #[byte(ctx = OptionalIndex(ctx), skip)]
    class: Option<ClassIndex>,
    visibility: Visibility,
    flags: FunctionFlags,
    #[byte(ctx = FlagDependent(ctx), skip_if = flags.is_native())]
    source: Option<SourceReference>,
    #[byte(ctx = FlagDependent(ctx), skip_if = !flags.has_return_value())]
    return_type: Option<TypeIndex>,
    #[byte(ctx = FlagDependent(ctx), skip_if = !flags.has_return_value())]
    is_const_return: bool,
    #[byte(ctx = FlagDependent(ctx), skip_if = !flags.has_base_method())]
    base_method: Option<FunctionIndex>,
    #[byte(ctx = Prefixed(ctx), skip_if = !flags.has_parameters())]
    parameters: Vec<ParameterIndex>,
    #[byte(ctx = Prefixed(ctx), skip_if = !flags.has_locals())]
    locals: Vec<LocalIndex>,
    #[byte(ctx = FlagDependent(ctx), skip_if = !flags.is_operator())]
    operator: Option<CNameIndex>,
    #[byte(ctx = FlagDependent(ctx), skip_if = !flags.is_cast())]
    cast_cost: u8,
    #[byte(ctx = (ctx, *flags))]
    body: FunctionBody<'i>,
}

impl<'i> Function<'i> {
    /// Creates a new instance.
    pub fn new(name: CNameIndex, visibility: Visibility, flags: FunctionFlags) -> Self {
        Self {
            name,
            class: None,
            visibility,
            flags,
            source: Some(SourceReference::DEFAULT),
            return_type: None,
            is_const_return: false,
            base_method: None,
            parameters: vec![],
            locals: vec![],
            operator: None,
            cast_cost: 0,
            body: FunctionBody::default(),
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the class index.
    pub fn class(&self) -> Option<ClassIndex> {
        self.class
    }

    #[inline]
    /// Gets the visibility.
    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    #[inline]
    /// Gets the flags.
    pub fn flags(&self) -> FunctionFlags {
        self.flags
    }

    #[inline]
    /// Gets the source reference.
    pub fn source(&self) -> Option<SourceReference> {
        self.source
    }

    #[inline]
    /// Gets the return type.
    pub fn return_type(&self) -> Option<TypeIndex> {
        self.return_type
    }

    #[inline]
    /// Returns whether the return type is const.
    pub fn is_const_return(&self) -> bool {
        self.is_const_return
    }

    #[inline]
    /// Gets the base method index.
    pub fn base_method(&self) -> Option<FunctionIndex> {
        self.base_method
    }

    #[inline]
    /// Gets the list of parameters.
    pub fn parameters(&self) -> &[ParameterIndex] {
        &self.parameters
    }

    #[inline]
    /// Gets a mutable list of parameters.
    pub fn parameters_mut(&mut self) -> &mut Vec<ParameterIndex> {
        &mut self.parameters
    }

    #[inline]
    /// Gets the list of local variables.
    pub fn locals(&self) -> &[LocalIndex] {
        &self.locals
    }

    #[inline]
    /// Gets the operator.
    pub fn operator(&self) -> Option<CNameIndex> {
        self.operator
    }

    #[inline]
    /// Gets the cast cost.
    pub fn cast_cost(&self) -> u8 {
        self.cast_cost
    }

    #[inline]
    /// Gets the function body.
    pub fn body(&self) -> &FunctionBody<'i> {
        &self.body
    }

    /// Returns a copy of this object with the `name` updated.
    pub fn with_name(mut self, name: CNameIndex) -> Self {
        self.name = name;
        self
    }

    /// Returns a copy of this object with the `flags` updated.
    pub fn with_flags(mut self, flags: FunctionFlags) -> Self {
        self.flags = flags;
        self
    }

    /// Returns a copy of this object with the `class` updated.
    pub fn with_class(mut self, class: Option<ClassIndex>) -> Self {
        self.class = class;
        self
    }

    /// Returns a copy of this object with the `source` updated.
    pub fn with_source(mut self, source: Option<SourceReference>) -> Self {
        assert!(
            !self.flags.is_native() || source.is_none(),
            "should not set source for a native function"
        );
        self.source = source;
        self
    }

    /// Returns a copy of this object with the `return_type` updated.
    pub fn with_return_type(mut self, return_type: Option<TypeIndex>) -> Self {
        self.return_type = return_type;
        self.flags.set_has_return_value(return_type.is_some());
        self
    }

    /// Returns a copy of this object with the `const_return_type` updated.
    pub fn with_const_return_type(mut self, return_type: TypeIndex) -> Self {
        self.is_const_return = true;
        self.with_return_type(Some(return_type))
    }

    /// Returns a copy of this object with the `base_method` updated.
    pub fn with_base_method(mut self, base_method: Option<FunctionIndex>) -> Self {
        self.base_method = base_method;
        self.flags.set_has_base_method(base_method.is_some());
        self
    }

    /// Returns a copy of this object with the `parameters` updated.
    pub fn with_parameters(mut self, parameters: impl Into<Vec<ParameterIndex>>) -> Self {
        self.parameters = parameters.into();
        self.flags.set_has_parameters(!self.parameters.is_empty());
        self
    }

    /// Returns a copy of this object with the `locals` updated.
    pub fn with_locals(mut self, locals: impl Into<Vec<LocalIndex>>) -> Self {
        self.set_locals(locals);
        self
    }

    /// Sets the `locals` of this object.
    pub fn set_locals(&mut self, locals: impl Into<Vec<LocalIndex>>) {
        self.locals = locals.into();
        self.flags.set_has_locals(!self.locals.is_empty());
    }

    /// Returns a copy of this object with the `operator` updated.
    pub fn with_operator(mut self, operator: Option<CNameIndex>) -> Self {
        self.operator = operator;
        self.flags.set_is_operator(operator.is_some());
        self
    }

    /// Returns a copy of this object with the `cast_cost` updated.
    pub fn with_cast_cost(mut self, cast_cost: u8) -> Self {
        self.cast_cost = cast_cost;
        self.flags.set_is_cast(cast_cost != 0);
        self
    }

    /// Returns a copy of this object with the `body` updated.
    pub fn with_body(mut self, body: FunctionBody<'i>) -> Self {
        self.set_body(body);
        self
    }

    /// Returns a copy of this object with the `code` updated.
    pub fn with_code(self, code: Vec<Instr>) -> Self {
        self.with_body(FunctionBody::Code(code))
    }

    /// Sets the `body` of this object.
    pub fn set_body(&mut self, body: FunctionBody<'i>) {
        self.flags.set_has_body(!body.is_empty());
        if matches!(self.body, FunctionBody::Code(_)) {
            self.flags.set_has_extra_parameters(false);
        }
        self.body = body;
    }

    /// Sets the `code` of this object.
    pub fn set_code(&mut self, code: Vec<Instr>) {
        self.set_body(FunctionBody::Code(code));
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> Function<'static> {
        debug_assert_eq!(self.flags().has_base_method(), self.base_method.is_some());
        debug_assert_eq!(self.flags().has_return_value(), self.return_type.is_some());
        debug_assert_eq!(self.flags().has_parameters(), !self.parameters.is_empty());
        debug_assert_eq!(self.flags().has_locals(), !self.locals.is_empty());
        debug_assert_eq!(self.flags().has_body(), !self.body.is_empty());
        debug_assert_eq!(self.flags().is_operator(), self.operator.is_some());
        debug_assert_eq!(self.flags().is_cast(), self.cast_cost != 0);
        debug_assert_eq!(self.flags().is_native(), self.source.is_none());

        Function {
            name: self.name,
            class: self.class,
            visibility: self.visibility,
            flags: self.flags,
            source: self.source,
            return_type: self.return_type,
            is_const_return: self.is_const_return,
            base_method: self.base_method,
            parameters: self.parameters,
            locals: self.locals,
            operator: self.operator,
            cast_cost: self.cast_cost,
            body: self.body.into_owned(),
        }
    }
}

impl<'i> From<Function<'i>> for Definition<'i> {
    #[inline]
    fn from(f: Function<'i>) -> Self {
        Definition::Function(Box::new(f))
    }
}

#[bitfield(u32)]
#[derive(PartialEq, Eq)]
/// Flags indicating properties of a function.
pub struct FunctionFlags {
    pub is_static: bool,
    pub is_exec: bool,
    pub is_timer: bool,
    pub is_final: bool,
    pub is_native: bool,
    #[bits(1)]
    __: u8,
    pub is_callback: bool,
    is_operator: bool,
    has_return_value: bool,
    has_base_method: bool,
    has_parameters: bool,
    has_locals: bool,
    has_body: bool,
    is_cast: bool,
    pub is_implicit_cast: bool,
    #[bits(4)]
    __: u8,
    pub is_const: bool,
    pub is_thread_safe: bool,
    pub is_quest: bool,
    has_extra_parameters: bool,
    #[bits(9)]
    __: u16,
}

util::impl_bitfield_read_write!(FunctionFlags);

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a function parameter.
pub struct Parameter {
    #[byte(skip)]
    name: CNameIndex,
    #[byte(skip)]
    function: FunctionIndex,
    type_: TypeIndex,
    flags: ParameterFlags,
}

impl Parameter {
    #[inline]
    /// Creates a new instance.
    pub fn new(
        name: CNameIndex,
        function: FunctionIndex,
        type_: TypeIndex,
        flags: ParameterFlags,
    ) -> Self {
        Self {
            name,
            function,
            type_,
            flags,
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the function index.
    pub fn function(&self) -> FunctionIndex {
        self.function
    }

    #[inline]
    /// Gets the type index.
    pub fn type_(&self) -> TypeIndex {
        self.type_
    }

    #[inline]
    /// Gets the flags.
    pub fn flags(&self) -> ParameterFlags {
        self.flags
    }

    /// Returns a copy of this object with the `function` updated.
    pub fn with_function(mut self, function: FunctionIndex) -> Self {
        self.function = function;
        self
    }
}

impl From<Parameter> for Definition<'_> {
    #[inline]
    fn from(p: Parameter) -> Self {
        Definition::Parameter(p)
    }
}

#[bitfield(u8)]
#[derive(PartialEq, Eq)]
/// Flags indicating properties of a parameter.
pub struct ParameterFlags {
    pub is_optional: bool,
    pub is_out: bool,
    pub is_short_circuit: bool,
    pub is_const: bool,
    #[bits(4)]
    __: u8,
}

util::impl_bitfield_read_write!(ParameterFlags);

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a local variable in a function.
pub struct Local {
    #[byte(skip)]
    name: CNameIndex,
    #[byte(skip)]
    function: FunctionIndex,
    type_: TypeIndex,
    flags: LocalFlags,
}

impl Local {
    #[inline]
    /// Creates a new instance.
    pub fn new(
        name: CNameIndex,
        function: FunctionIndex,
        type_: TypeIndex,
        flags: LocalFlags,
    ) -> Self {
        Self {
            name,
            function,
            type_,
            flags,
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the function index.
    pub fn function(&self) -> FunctionIndex {
        self.function
    }

    #[inline]
    /// Gets the type index.
    pub fn type_(&self) -> TypeIndex {
        self.type_
    }

    #[inline]
    /// Gets the flags.
    pub fn flags(&self) -> LocalFlags {
        self.flags
    }

    /// Returns a copy of this object with the `function` updated.
    pub fn with_function(mut self, function: FunctionIndex) -> Self {
        self.function = function;
        self
    }
}

impl From<Local> for Definition<'_> {
    #[inline]
    fn from(l: Local) -> Self {
        Definition::Local(l)
    }
}

#[bitfield(u8)]
#[derive(PartialEq, Eq)]
/// Flags indicating properties of a local variable.
pub struct LocalFlags {
    pub is_const: bool,
    #[bits(7)]
    __: u8,
}

util::impl_bitfield_read_write!(LocalFlags);

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a field in a class or struct.
pub struct Field<'i> {
    #[byte(skip)]
    name: CNameIndex,
    #[byte(skip)]
    class: ClassIndex,
    visibility: Visibility,
    type_: TypeIndex,
    flags: FieldFlags,
    #[byte(ctx = FlagDependent(Prefixed(ctx)), skip_if = !flags.has_hint())]
    hint: Option<Str<'i>>,
    #[byte(ctx = Prefixed(ctx))]
    attributes: Vec<Property<'i>>,
    #[byte(ctx = Prefixed(ctx))]
    defaults: Vec<Property<'i>>,
}

impl<'i> Field<'i> {
    /// Creates a new instance.
    pub fn new(
        name: CNameIndex,
        class: ClassIndex,
        visibility: Visibility,
        type_: TypeIndex,
        flags: FieldFlags,
    ) -> Self {
        Self {
            name,
            class,
            visibility,
            type_,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> CNameIndex {
        self.name
    }

    #[inline]
    /// Gets the class index.
    pub fn class(&self) -> ClassIndex {
        self.class
    }

    #[inline]
    /// Gets the visibility.
    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    #[inline]
    /// Gets the type index.
    pub fn type_(&self) -> TypeIndex {
        self.type_
    }

    #[inline]
    /// Gets the flags.
    pub fn flags(&self) -> FieldFlags {
        self.flags
    }

    #[inline]
    /// Gets the hint string.
    pub fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    #[inline]
    /// Gets the attributes.
    pub fn attributes(&self) -> &[Property<'i>] {
        &self.attributes
    }

    #[inline]
    /// Gets the default values.
    pub fn defaults(&self) -> &[Property<'i>] {
        &self.defaults
    }

    /// Returns a copy of this object with the `hint` updated.
    pub fn with_hint(mut self, hint: impl Into<Str<'i>>) -> Self {
        self.hint = Some(hint.into());
        self.flags.set_has_hint(true);
        self
    }

    /// Returns a copy of this object with the `attributes` updated.
    pub fn with_attributes(mut self, attributes: impl Into<Vec<Property<'i>>>) -> Self {
        self.attributes = attributes.into();
        self
    }

    /// Returns a copy of this object with the `defaults` updated.
    pub fn with_defaults(mut self, defaults: impl Into<Vec<Property<'i>>>) -> Self {
        self.set_defaults(defaults);
        self
    }

    /// Sets the `defaults` of this object.
    pub fn set_defaults(&mut self, defaults: impl Into<Vec<Property<'i>>>) {
        self.defaults = defaults.into();
        self.flags.set_has_default(!self.defaults.is_empty());
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> Field<'static> {
        Field {
            name: self.name,
            class: self.class,
            visibility: self.visibility,
            type_: self.type_,
            flags: self.flags,
            hint: self.hint.map(|s| s.into_owned().into()),
            attributes: self
                .attributes
                .into_iter()
                .map(Property::into_owned)
                .collect(),
            defaults: self
                .defaults
                .into_iter()
                .map(Property::into_owned)
                .collect(),
        }
    }
}

impl<'i> From<Field<'i>> for Definition<'i> {
    fn from(f: Field<'i>) -> Self {
        Definition::Field(Box::new(f))
    }
}

#[bitfield(u16)]
#[derive(PartialEq, Eq)]
/// Flags indicating properties of a field.
pub struct FieldFlags {
    pub is_native: bool,
    pub is_editable: bool,
    pub is_inline: bool,
    pub is_const: bool,
    pub is_replicated: bool,
    has_hint: bool,
    pub is_instance_editable: bool,
    has_default: bool,
    pub is_persistent: bool,
    pub is_test_only: bool,
    pub is_browsable: bool,
    #[bits(5)]
    __: u8,
}

util::impl_bitfield_read_write!(FieldFlags);

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a source file reference.
pub struct SourceFile<'i> {
    index: u32,
    path_hash: u32,
    code_crc: u32,
    #[byte(ctx = Prefixed(ctx))]
    path: Str<'i>,
}

impl<'i> SourceFile<'i> {
    #[inline]
    /// Creates a new instance.
    pub fn new(index: u32, path_hash: u32, code_crc: u32, path: impl Into<Str<'i>>) -> Self {
        Self {
            index,
            path_hash,
            code_crc,
            path: path.into(),
        }
    }

    #[inline]
    /// Gets the pool index.
    pub fn index(&self) -> u32 {
        self.index
    }

    #[inline]
    /// Gets the hash of the path.
    pub fn path_hash(&self) -> u32 {
        self.path_hash
    }

    #[inline]
    /// Gets the CRC of the code.
    pub fn code_crc(&self) -> u32 {
        self.code_crc
    }

    #[inline]
    /// Gets the file path.
    pub fn path(&self) -> &str {
        &self.path
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> SourceFile<'static> {
        SourceFile {
            index: self.index,
            path_hash: self.path_hash,
            code_crc: self.code_crc,
            path: self.path.into_owned().into(),
        }
    }
}

impl<'i> From<SourceFile<'i>> for Definition<'i> {
    #[inline]
    fn from(f: SourceFile<'i>) -> Self {
        Definition::SourceFile(Box::new(f))
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Represents the body of a function.
pub enum FunctionBody<'i> {
    Raw {
        max_offset: u32,
        bytes: Cow<'i, [u8]>,
    },
    Code(Vec<Instr>),
}

impl FunctionBody<'_> {
    /// Returns true if this is empty.
    pub fn is_empty(&self) -> bool {
        match self {
            &FunctionBody::Raw { max_offset, .. } => max_offset == 0,
            FunctionBody::Code(instructions) => instructions.is_empty(),
        }
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> FunctionBody<'static> {
        match self {
            FunctionBody::Raw { max_offset, bytes } => FunctionBody::Raw {
                max_offset,
                bytes: Cow::Owned(bytes.into_owned()),
            },
            FunctionBody::Code(instructions) => FunctionBody::Code(instructions),
        }
    }

    #[inline]
    /// Returns an iterator over the code instructions.
    pub fn code_iter(&self) -> FunctionBodyIter<'_> {
        FunctionBodyIter::new(self)
    }

    /// Returns an owned vector of the code instructions.
    pub fn code_owned(&self) -> byte::Result<Vec<Instr>> {
        match self {
            FunctionBody::Raw { .. } => self.code_iter().collect::<Result<Vec<_>, _>>(),
            FunctionBody::Code(instructions) => Ok(instructions.clone()),
        }
    }
}

impl Default for FunctionBody<'_> {
    fn default() -> Self {
        FunctionBody::Code(vec![])
    }
}

impl<'i, Ctx: Endianess> TryRead<'i, (Ctx, FunctionFlags)> for FunctionBody<'i> {
    fn try_read(
        bytes: &'i [u8],
        (ctx, flags): (Ctx, FunctionFlags),
    ) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        let max_offset: u32 = if flags.has_body() {
            bytes.read(offset, ctx)?
        } else {
            0
        };
        Ok((
            FunctionBody::Raw {
                max_offset,
                bytes: Cow::Borrowed(&bytes[*offset..]),
            },
            bytes.len(),
        ))
    }
}

impl<Ctx: Endianess> TryWrite<(Ctx, FunctionFlags)> for FunctionBody<'_> {
    fn try_write(&self, out: &mut [u8], (ctx, flags): (Ctx, FunctionFlags)) -> byte::Result<usize> {
        let offset = &mut 0;
        match self {
            FunctionBody::Raw { max_offset, bytes } => {
                if flags.has_body() {
                    out.write(offset, max_offset, ctx)?;
                }
                out.write(offset, bytes.as_ref(), ())?;
            }
            FunctionBody::Code(instructions) if flags.has_body() => {
                let max_offset: u32 = instructions
                    .iter()
                    .map(|i| u32::from(i.virtual_size()))
                    .sum();
                out.write(offset, &max_offset, ctx)?;
                for instr in instructions {
                    out.write(offset, instr, ctx)?;
                }
            }
            _ => {}
        }
        Ok(*offset)
    }
}

impl<Ctx: Copy> Measure<(Ctx, FunctionFlags)> for FunctionBody<'_> {
    fn measure(&self, (ctx, flags): (Ctx, FunctionFlags)) -> usize {
        match self {
            FunctionBody::Raw { bytes, .. } if flags.has_body() => {
                mem::size_of::<u32>() + bytes.len()
            }
            FunctionBody::Raw { bytes, .. } => bytes.len(),
            FunctionBody::Code(instructions) if flags.has_body() => {
                mem::size_of::<u32>() + instructions.iter().map(|i| i.measure(ctx)).sum::<usize>()
            }
            FunctionBody::Code(_) => 0,
        }
    }
}

#[derive(Debug, Clone)]
/// An iterator over the body of a function.
pub enum FunctionBodyIter<'a> {
    Raw {
        virtual_offset: u32,
        max_offset: u32,
        bytes: &'a [u8],
    },
    Code(CodeIter<'a, Offset>),
}

impl<'a> FunctionBodyIter<'a> {
    fn new(body: &'a FunctionBody<'_>) -> Self {
        match body {
            FunctionBody::Raw { bytes, max_offset } => FunctionBodyIter::Raw {
                bytes,
                max_offset: *max_offset,
                virtual_offset: 0,
            },
            FunctionBody::Code(instructions) => FunctionBodyIter::Code(CodeIter::new(instructions)),
        }
    }

    /// Gets the current virtual offset.
    pub fn virtual_offset(&self) -> u32 {
        match self {
            FunctionBodyIter::Raw {
                virtual_offset: offset,
                ..
            }
            | FunctionBodyIter::Code(CodeIter {
                virtual_offset: offset,
                ..
            }) => *offset,
        }
    }

    /// Returns an iterator yielding virtual offsets and instructions.
    pub fn with_offsets(mut self) -> impl Iterator<Item = (u32, byte::Result<Instr>)> + use<'a> {
        iter::from_fn(move || Some((self.virtual_offset(), self.next()?)))
    }
}

impl Iterator for FunctionBodyIter<'_> {
    type Item = byte::Result<Instr>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FunctionBodyIter::Raw {
                bytes,
                max_offset,
                virtual_offset: offset,
            } => {
                if *offset >= *max_offset {
                    return None;
                };
                match Instr::try_read(bytes, ENDIANESS) {
                    Ok((instr, size)) => {
                        *offset += u32::from(instr.virtual_size());
                        *bytes = &bytes[size..];
                        Some(Ok(instr))
                    }
                    Err(err) => Some(Err(err)),
                }
            }
            FunctionBodyIter::Code(code) => Some(Ok(code.next()?.clone())),
        }
    }
}

impl iter::FusedIterator for FunctionBodyIter<'_> {}

#[derive(Debug, Clone)]
/// An iterator over a sequence of instructions.
pub struct CodeIter<'a, L> {
    virtual_offset: u32,
    instructions: &'a [Instr<L>],
}

impl<'a, L> CodeIter<'a, L> {
    #[inline]
    /// Creates a new instance.
    pub fn new(instructions: &'a [Instr<L>]) -> Self {
        Self {
            virtual_offset: 0,
            instructions,
        }
    }

    #[inline]
    /// Gets the current virtual offset.
    pub fn virtual_offset(&self) -> u32 {
        self.virtual_offset
    }

    /// Returns an iterator yielding virtual offsets and instructions.
    pub fn with_offsets(mut self) -> impl Iterator<Item = (u32, &'a Instr<L>)> {
        iter::from_fn(move || Some((self.virtual_offset(), self.next()?)))
    }

    #[inline]
    /// Returns the instructions as a slice.
    pub fn as_slice(&self) -> &'a [Instr<L>] {
        self.instructions
    }
}

impl<'a, L> Iterator for CodeIter<'a, L> {
    type Item = &'a Instr<L>;

    fn next(&mut self) -> Option<Self::Item> {
        let (instr, rest) = self.instructions.split_first()?;
        self.instructions = rest;
        self.virtual_offset += u32::from(instr.virtual_size());
        Some(instr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// Represents a property attached to a definition.
pub struct Property<'i> {
    #[byte(ctx = Prefixed(ctx))]
    name: Str<'i>,
    #[byte(ctx = Prefixed(ctx))]
    value: Str<'i>,
}

impl<'i> Property<'i> {
    #[inline]
    /// Creates a new instance.
    pub fn new(name: impl Into<Str<'i>>, value: impl Into<Str<'i>>) -> Self {
        Self {
            name: name.into(),
            value: value.into(),
        }
    }

    #[inline]
    /// Gets the name index of this definition.
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    /// Gets the value.
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Converts this definition into an owned version.
    pub fn into_owned(self) -> Property<'static> {
        Property {
            name: self.name.into_owned().into(),
            value: self.value.into_owned().into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryRead, TryWrite, Measure)]
/// A reference to a location in a source file.
pub struct SourceReference {
    file: SourceFileIndex,
    line: u32,
}

impl SourceReference {
    const DEFAULT: Self = Self {
        file: SourceFileIndex::ONE,
        line: 0,
    };

    #[inline]
    /// Creates a new instance.
    pub fn new(file: SourceFileIndex, line: u32) -> Self {
        Self { file, line }
    }

    #[inline]
    /// Gets the source file reference.
    pub fn file(&self) -> SourceFileIndex {
        self.file
    }

    #[inline]
    /// Gets the line number.
    pub fn line(&self) -> u32 {
        self.line
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, TryRead, TryWrite, Measure)]
#[byte(tag_type = u8)]
/// Specifies the visibility of a definition.
pub enum Visibility {
    #[default]
    #[byte(tag = 0x00)]
    Public,
    #[byte(tag = 0x01)]
    Protected,
    #[byte(tag = 0x02)]
    Private,
}

/// A trait for types that can be created from a `u32` pool index.
pub trait IndexType: Copy {
    fn from_u32(value: u32) -> Option<Self>;
}

impl<A> IndexType for PoolIndex<A> {
    #[inline]
    fn from_u32(value: u32) -> Option<Self> {
        Some(PoolIndex::new(value))
    }
}

impl<A> IndexType for NzPoolIndex<A> {
    #[inline]
    fn from_u32(value: u32) -> Option<Self> {
        NzPoolIndex::new(value)
    }
}

/// A trait for types that can be converted into a `Definition`.
pub trait DefinitionIndex<'i>: Into<Definition<'i>> {
    type Index: IndexType;
}

impl DefinitionIndex<'_> for Type {
    type Index = TypeIndex;
}

impl DefinitionIndex<'_> for Class {
    type Index = ClassIndex;
}

impl DefinitionIndex<'_> for EnumMember {
    type Index = EnumValueIndex;
}

impl DefinitionIndex<'_> for Enum {
    type Index = EnumIndex;
}

impl<'i> DefinitionIndex<'i> for Function<'i> {
    type Index = FunctionIndex;
}

impl DefinitionIndex<'_> for Parameter {
    type Index = ParameterIndex;
}

impl DefinitionIndex<'_> for Local {
    type Index = LocalIndex;
}

impl<'i> DefinitionIndex<'i> for Field<'i> {
    type Index = FieldIndex;
}

impl<'i> DefinitionIndex<'i> for SourceFile<'i> {
    type Index = SourceFileIndex;
}

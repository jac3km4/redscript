use crate::bundle::{DefinitionHeader, DefinitionType, PoolIndex};
use crate::bytecode::Code;
use crate::decode::{Decode, DecodeExt};

use core::panic;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::fmt::Display;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub struct Definition {
    pub name: PoolIndex<String>,
    pub parent: PoolIndex<Definition>,
    pub file_offset: u32,
    pub size: u32,
    pub value: DefinitionValue,
}

impl Definition {
    pub const DUMMY: Definition = Definition {
        name: PoolIndex::ZERO,
        parent: PoolIndex::ZERO,
        file_offset: 0,
        size: 0,
        value: DefinitionValue::Type(Type::Prim),
    };

    pub fn decode<I: io::Read + io::Seek>(input: &mut I, header: &DefinitionHeader) -> io::Result<Definition> {
        input.seek(io::SeekFrom::Start(header.offset.into()))?;

        let value = match header.type_ {
            DefinitionType::Type => DefinitionValue::Type(input.decode()?),
            DefinitionType::Class => DefinitionValue::Class(input.decode()?),
            DefinitionType::EnumValue => DefinitionValue::EnumValue(input.decode()?),
            DefinitionType::Enum => DefinitionValue::Enum(input.decode()?),
            DefinitionType::BitField => panic!("Bit field not supported"),
            DefinitionType::Function => DefinitionValue::Function(input.decode()?),
            DefinitionType::Parameter => DefinitionValue::Parameter(input.decode()?),
            DefinitionType::Local => DefinitionValue::Local(input.decode()?),
            DefinitionType::Field => DefinitionValue::Field(input.decode()?),
            DefinitionType::SourceFile => DefinitionValue::SourceFile(input.decode()?),
        };
        let definition = Definition {
            name: header.name,
            parent: header.parent,
            file_offset: header.offset,
            size: header.size,
            value,
        };
        Ok(definition)
    }

    pub fn source(&self) -> Option<&SourceReference> {
        match self.value {
            DefinitionValue::Function(ref fun) => fun.source.as_ref(),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum DefinitionValue {
    Type(Type),
    Class(Class),
    EnumValue(i64),
    Enum(Enum),
    Function(Function),
    Parameter(Parameter),
    Local(Local),
    Field(Field),
    SourceFile(SourceFile),
}

#[derive(Debug)]
pub struct Class {
    pub visibility: Visibility,
    pub flags: ClassFlags,
    pub base: PoolIndex<Definition>,
    pub functions: Vec<PoolIndex<Definition>>,
    pub fields: Vec<PoolIndex<Definition>>,
    pub overrides: Vec<PoolIndex<Definition>>,
}

impl Decode for Class {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let visibility = input.decode()?;
        let flags: ClassFlags = input.decode()?;
        let base = input.decode()?;
        let functions = if flags.has_functions() {
            input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?
        } else {
            vec![]
        };
        let fields = if flags.has_fields() {
            input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?
        } else {
            vec![]
        };
        let overrides = if flags.has_overrides() {
            input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?
        } else {
            vec![]
        };

        let result = Class {
            visibility,
            flags,
            base,
            functions,
            fields,
            overrides,
        };

        Ok(result)
    }
}

#[derive(Debug)]
pub struct Enum {
    pub flags: u8,
    pub size: u8,
    pub members: Vec<PoolIndex<Definition>>,
    pub unk1: bool,
}

impl Decode for Enum {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let flags = input.decode()?;
        let size = input.decode()?;
        let members = input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?;
        let unk1 = input.decode()?;
        let result = Enum {
            flags,
            size,
            members,
            unk1,
        };

        Ok(result)
    }
}

#[derive(Debug)]
pub struct Function {
    pub visibility: Visibility,
    pub flags: FunctionFlags,
    pub source: Option<SourceReference>,
    pub return_type: Option<PoolIndex<Definition>>,
    pub unk1: bool,
    pub base_method: Option<PoolIndex<Definition>>,
    pub parameters: Vec<PoolIndex<Definition>>,
    pub locals: Vec<PoolIndex<Definition>>,
    pub operator: u32,
    pub cast: u8,
    pub code: Code,
}

impl Decode for Function {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let visibility = input.decode()?;
        let flags: FunctionFlags = input.decode()?;
        let source = if flags.is_native() { None } else { Some(input.decode()?) };
        let return_type = if flags.is_void() { None } else { Some(input.decode()?) };
        let unk1 = if flags.is_void() { false } else { input.decode()? };
        let base_method = if flags.has_base_method() {
            Some(input.decode()?)
        } else {
            None
        };
        let parameters = if flags.has_parameters() {
            input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?
        } else {
            vec![]
        };
        let locals = if flags.has_locals() {
            input.decode_vec_prefixed::<u32, PoolIndex<Definition>>()?
        } else {
            vec![]
        };
        let unk3 = if flags.is_operator_overload() {
            input.decode()?
        } else {
            0u32
        };
        let unk4 = if flags.is_cast() { input.decode()? } else { 0u8 };
        let code = if flags.has_body() { input.decode()? } else { Code::EMPTY };

        let result = Function {
            visibility,
            flags,
            source,
            return_type,
            unk1,
            base_method,
            parameters,
            locals,
            operator: unk3,
            cast: unk4,
            code,
        };

        Ok(result)
    }
}

#[derive(Debug)]
pub struct Field {
    pub visibility: Visibility,
    pub type_: PoolIndex<Definition>,
    pub flags: FieldFlags,
    pub hint: Option<String>,
    pub attributes: Vec<Property>,
    pub defaults: Vec<Property>,
}

impl Decode for Field {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let visibility = input.decode()?;
        let type_ = input.decode()?;
        let flags: FieldFlags = input.decode()?;
        let hint = if flags.has_hint() {
            Some(input.decode_str_prefixed::<u16>()?)
        } else {
            None
        };
        let attributes = input.decode_vec_prefixed::<u32, Property>()?;
        let defaults = input.decode_vec_prefixed::<u32, Property>()?;
        let result = Field {
            visibility,
            type_,
            flags,
            hint,
            attributes,
            defaults,
        };

        Ok(result)
    }
}

#[derive(Debug)]
pub enum Type {
    Prim,
    Class,
    Handle(PoolIndex<Definition>),
    WeakHandle(PoolIndex<Definition>),
    Array(PoolIndex<Definition>),
    StaticArray(PoolIndex<Definition>, u32),
    Unk1(PoolIndex<Definition>),
}

impl Decode for Type {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let tag: u8 = input.decode()?;
        match tag {
            0 => Ok(Type::Prim),
            1 => Ok(Type::Class),
            2 => Ok(Type::Handle(input.decode()?)),
            3 => Ok(Type::WeakHandle(input.decode()?)),
            4 => Ok(Type::Array(input.decode()?)),
            5 => Ok(Type::StaticArray(input.decode()?, input.decode()?)),
            6 => Ok(Type::Unk1(input.decode()?)),
            _ => panic!("Unknown Type enum value {}", tag),
        }
    }
}

#[derive(Debug)]
pub struct Local {
    pub type_: PoolIndex<Definition>,
    pub flags: LocalFlags,
}

impl Decode for Local {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let type_ = input.decode()?;
        let flags = input.decode()?;
        Ok(Local { type_, flags })
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub type_: PoolIndex<Definition>,
    pub flags: ParameterFlags,
}

impl Decode for Parameter {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let type_ = input.decode()?;
        let flags = input.decode()?;
        Ok(Parameter { type_, flags })
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub id: u32,
    pub path_hash: u64,
    pub path: PathBuf,
}

impl Decode for SourceFile {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let id = input.decode()?;
        let path_hash = input.decode()?;
        let raw_path = input.decode_str_prefixed::<u16>()?;
        let path = PathBuf::from(raw_path.replace("\\", "/"));
        let result = SourceFile { id, path_hash, path };
        Ok(result)
    }
}

#[derive(Debug)]
pub struct FieldFlags(u16);

impl FieldFlags {
    pub fn is_native(&self) -> bool {
        self.0 & (1 << 0) != 0
    }

    pub fn has_hint(&self) -> bool {
        self.0 & (1 << 5) != 0
    }
}

impl Decode for FieldFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(FieldFlags(input.decode()?))
    }
}

#[derive(Debug)]
pub struct LocalFlags(u8);

impl Decode for LocalFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(LocalFlags(input.decode()?))
    }
}

#[derive(Debug)]
pub struct ParameterFlags(u8);

impl Decode for ParameterFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(ParameterFlags(input.decode()?))
    }
}

#[derive(Debug)]
pub struct ClassFlags(u16);

impl ClassFlags {
    pub fn is_abstract(&self) -> bool {
        self.0 & (1 << 1) != 0
    }

    pub fn is_value_type(&self) -> bool {
        self.0 & (1 << 3) != 0
    }

    pub fn has_functions(&self) -> bool {
        self.0 & (1 << 4) != 0
    }

    pub fn has_fields(&self) -> bool {
        self.0 & (1 << 5) != 0
    }

    pub fn has_overrides(&self) -> bool {
        self.0 & (1 << 8) != 0
    }
}

impl Decode for ClassFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(ClassFlags(input.decode()?))
    }
}

#[derive(Debug, FromPrimitive)]
pub struct FunctionFlags(u32);

impl FunctionFlags {
    pub fn is_static(&self) -> bool {
        self.0 & (1 << 0) != 0
    }

    pub fn is_timer(&self) -> bool {
        self.0 & (1 << 2) != 0
    }

    pub fn is_native(&self) -> bool {
        self.0 & (1 << 4) != 0
    }

    pub fn is_callback(&self) -> bool {
        self.0 & (1 << 5) != 0
    }

    pub fn is_operator_overload(&self) -> bool {
        self.0 & (1 << 6) != 0
    }

    pub fn is_void(&self) -> bool {
        self.0 & (1 << 7) == 0
    }

    pub fn has_base_method(&self) -> bool {
        self.0 & (1 << 8) != 0
    }

    pub fn has_parameters(&self) -> bool {
        self.0 & (1 << 9) != 0
    }

    pub fn has_locals(&self) -> bool {
        self.0 & (1 << 10) != 0
    }

    pub fn has_body(&self) -> bool {
        self.0 & (1 << 11) != 0
    }

    pub fn is_cast(&self) -> bool {
        self.0 & (1 << 12) != 0
    }

    pub fn is_safe(&self) -> bool {
        self.0 & (1 << 13) != 0
    }

    pub fn is_getter(&self) -> bool {
        self.0 & (1 << 18) != 0
    }
}

impl Decode for FunctionFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(FunctionFlags(input.decode()?))
    }
}

#[derive(Debug, FromPrimitive)]
pub enum Visibility {
    Public = 0,
    Protected = 1,
    Private = 2,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Visibility::Public => "public",
            Visibility::Protected => "protected",
            Visibility::Private => "private",
        };
        f.write_str(res)
    }
}

impl Decode for Visibility {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let res = FromPrimitive::from_u8(input.decode()?);
        Ok(res.expect("Invalid Visibility enum value"))
    }
}

#[derive(Debug)]
pub struct SourceReference {
    pub file: PoolIndex<Definition>,
    pub line: u32,
}

impl Decode for SourceReference {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let file = input.decode()?;
        let line = input.decode()?;
        let result = SourceReference { file, line };
        Ok(result)
    }
}

#[derive(Debug)]
pub struct Property {
    pub name: String,
    pub value: String,
}

impl Decode for Property {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let name = input.decode_str_prefixed::<u16>()?;
        let value = input.decode_str_prefixed::<u16>()?;
        Ok(Property { name, value })
    }
}

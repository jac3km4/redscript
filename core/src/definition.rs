use std::fmt::Display;
use std::io;
use std::path::PathBuf;

use modular_bitfield::prelude::*;

use crate::bundle::{ConstantPool, DefinitionHeader, DefinitionType, PoolIndex};
use crate::bytecode::{Code, Offset};
use crate::decode::{Decode, DecodeExt};
use crate::encode::{Encode, EncodeExt};

#[derive(Debug)]
pub struct Definition {
    pub name: PoolIndex<String>,
    pub parent: PoolIndex<Definition>,
    pub unk1: u8,
    pub unk2: u8,
    pub unk3: u8,
    pub value: AnyDefinition,
}

impl Definition {
    pub const DEFAULT: Definition = Definition {
        name: PoolIndex::UNDEFINED,
        parent: PoolIndex::UNDEFINED,
        unk1: 0,
        unk2: 0,
        unk3: 0,
        value: AnyDefinition::Type(Type::Prim),
    };

    pub fn decode<I: io::Read + io::Seek>(input: &mut I, header: &DefinitionHeader) -> io::Result<Definition> {
        input.seek(io::SeekFrom::Start(header.offset.into()))?;

        let value = match header.type_ {
            DefinitionType::Type => AnyDefinition::Type(input.decode()?),
            DefinitionType::Class => AnyDefinition::Class(input.decode()?),
            DefinitionType::EnumValue => AnyDefinition::EnumValue(input.decode()?),
            DefinitionType::Enum => AnyDefinition::Enum(input.decode()?),
            DefinitionType::BitField => panic!("Bit field not supported"),
            DefinitionType::Function => AnyDefinition::Function(input.decode()?),
            DefinitionType::Parameter => AnyDefinition::Parameter(input.decode()?),
            DefinitionType::Local => AnyDefinition::Local(input.decode()?),
            DefinitionType::Field => AnyDefinition::Field(input.decode()?),
            DefinitionType::SourceFile => AnyDefinition::SourceFile(input.decode()?),
        };
        let definition = Definition {
            name: header.name,
            parent: header.parent,
            unk1: header.unk1,
            unk2: header.unk2,
            unk3: header.unk3,
            value,
        };
        Ok(definition)
    }

    pub fn source(&self) -> Option<&SourceReference> {
        match self.value {
            AnyDefinition::Function(ref fun) => fun.source.as_ref(),
            _ => None,
        }
    }

    pub fn first_line(&self, pool: &ConstantPool) -> Option<u32> {
        match self.value {
            AnyDefinition::Function(ref fun) => fun.source.as_ref().map(|source| source.line),
            AnyDefinition::Class(ref class) => class
                .functions
                .iter()
                .filter_map(|idx| {
                    pool.function(*idx)
                        .ok()
                        .and_then(|fun| fun.source.as_ref())
                        .map(|source| source.line)
                })
                .min(),
            _ => None,
        }
    }

    fn default(
        name: PoolIndex<String>,
        parent: PoolIndex<Definition>,
        value: AnyDefinition,
        unk2: u8,
        unk3: u8,
    ) -> Definition {
        Definition {
            name,
            parent,
            unk1: 0,
            unk2,
            unk3,
            value,
        }
    }

    pub fn local(name: PoolIndex<String>, parent: PoolIndex<Function>, local: Local) -> Definition {
        Definition::default(name, parent.cast(), AnyDefinition::Local(local), 7, 60)
    }

    pub fn param(name: PoolIndex<String>, parent: PoolIndex<Function>, param: Parameter) -> Definition {
        Definition::default(name, parent.cast(), AnyDefinition::Parameter(param), 7, 60)
    }

    pub fn class(name: PoolIndex<String>, class: Class) -> Definition {
        Definition::default(name, PoolIndex::UNDEFINED, AnyDefinition::Class(class), 12, 60)
    }

    pub fn type_(name: PoolIndex<String>, type_: Type) -> Definition {
        Definition::default(name, PoolIndex::UNDEFINED, AnyDefinition::Type(type_), 12, 60)
    }

    pub fn function(name: PoolIndex<String>, parent: PoolIndex<Class>, fun: Function) -> Definition {
        Definition::default(name, parent.cast(), AnyDefinition::Function(fun), 12, 60)
    }

    pub fn field(name: PoolIndex<String>, parent: PoolIndex<Class>, field: Field) -> Definition {
        Definition::default(name, parent.cast(), AnyDefinition::Field(field), 12, 60)
    }

    pub fn enum_(name: PoolIndex<String>, enum_: Enum) -> Definition {
        Definition::default(name, PoolIndex::UNDEFINED, AnyDefinition::Enum(enum_), 82, 224)
    }

    pub fn enum_value(name: PoolIndex<String>, parent: PoolIndex<Enum>, value: i64) -> Definition {
        Definition::default(name, parent.cast(), AnyDefinition::EnumValue(value), 82, 219)
    }
}

#[derive(Debug)]
pub enum AnyDefinition {
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

impl AnyDefinition {
    pub fn type_(&self) -> DefinitionType {
        match self {
            AnyDefinition::Type(_) => DefinitionType::Type,
            AnyDefinition::Class(_) => DefinitionType::Class,
            AnyDefinition::EnumValue(_) => DefinitionType::EnumValue,
            AnyDefinition::Enum(_) => DefinitionType::Enum,
            AnyDefinition::Function(_) => DefinitionType::Function,
            AnyDefinition::Parameter(_) => DefinitionType::Parameter,
            AnyDefinition::Local(_) => DefinitionType::Local,
            AnyDefinition::Field(_) => DefinitionType::Field,
            AnyDefinition::SourceFile(_) => DefinitionType::SourceFile,
        }
    }
}

impl Encode for AnyDefinition {
    fn encode<O: io::Write>(output: &mut O, def: &Self) -> io::Result<()> {
        match &def {
            AnyDefinition::Type(type_) => output.encode(type_),
            AnyDefinition::Class(class) => output.encode(class),
            AnyDefinition::EnumValue(value) => output.encode(value),
            AnyDefinition::Enum(enum_) => output.encode(enum_),
            AnyDefinition::Function(fun) => output.encode(fun),
            AnyDefinition::Parameter(param) => output.encode(param),
            AnyDefinition::Local(local) => output.encode(local),
            AnyDefinition::Field(field) => output.encode(field),
            AnyDefinition::SourceFile(file) => output.encode(file),
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub visibility: Visibility,
    pub flags: ClassFlags,
    pub base: PoolIndex<Class>,
    pub functions: Vec<PoolIndex<Function>>,
    pub fields: Vec<PoolIndex<Field>>,
    pub overrides: Vec<PoolIndex<Field>>,
}

impl Decode for Class {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let visibility = input.decode()?;
        let flags: ClassFlags = input.decode()?;
        let base = input.decode()?;
        let functions = if flags.has_functions() {
            input.decode_vec_prefixed::<u32, PoolIndex<Function>>()?
        } else {
            vec![]
        };
        let fields = if flags.has_fields() {
            input.decode_vec_prefixed::<u32, PoolIndex<Field>>()?
        } else {
            vec![]
        };
        let overrides = if flags.has_overrides() {
            input.decode_vec_prefixed::<u32, PoolIndex<Field>>()?
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

impl Encode for Class {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        let flags = value
            .flags
            .with_has_functions(!value.functions.is_empty())
            .with_has_fields(!value.fields.is_empty())
            .with_has_overrides(!value.overrides.is_empty());

        output.encode(&value.visibility)?;
        output.encode(&flags)?;
        output.encode(&value.base)?;
        if flags.has_functions() {
            output.encode_slice_prefixed::<u32, PoolIndex<Function>>(&value.functions)?;
        }
        if flags.has_fields() {
            output.encode_slice_prefixed::<u32, PoolIndex<Field>>(&value.fields)?;
        }
        if flags.has_overrides() {
            output.encode_slice_prefixed::<u32, PoolIndex<Field>>(&value.overrides)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Enum {
    pub flags: u8,
    pub size: u8,
    pub members: Vec<PoolIndex<i64>>,
    pub unk1: bool,
}

impl Decode for Enum {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let flags = input.decode()?;
        let size = input.decode()?;
        let members = input.decode_vec_prefixed::<u32, PoolIndex<i64>>()?;
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

impl Encode for Enum {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.flags)?;
        output.encode(&value.size)?;
        output.encode_slice_prefixed::<u32, PoolIndex<i64>>(&value.members)?;
        output.encode(&value.unk1)
    }
}

#[derive(Debug)]
pub struct Function {
    pub visibility: Visibility,
    pub flags: FunctionFlags,
    pub source: Option<SourceReference>,
    pub return_type: Option<PoolIndex<Type>>,
    pub unk1: bool,
    pub base_method: Option<PoolIndex<Function>>,
    pub parameters: Vec<PoolIndex<Parameter>>,
    pub locals: Vec<PoolIndex<Local>>,
    pub operator: Option<u32>,
    pub cast: u8,
    pub code: Code<Offset>,
}

impl Decode for Function {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let visibility = input.decode()?;
        let flags: FunctionFlags = input.decode()?;
        let source = if flags.is_native() { None } else { Some(input.decode()?) };
        let return_type = if flags.has_return_value() {
            Some(input.decode()?)
        } else {
            None
        };
        let unk1 = if flags.has_return_value() {
            input.decode()?
        } else {
            false
        };
        let base_method = if flags.has_base_method() {
            Some(input.decode()?)
        } else {
            None
        };
        let parameters = if flags.has_parameters() {
            input.decode_vec_prefixed::<u32, PoolIndex<Parameter>>()?
        } else {
            vec![]
        };
        let locals = if flags.has_locals() {
            input.decode_vec_prefixed::<u32, PoolIndex<Local>>()?
        } else {
            vec![]
        };
        let operator = if flags.is_operator_overload() {
            Some(input.decode()?)
        } else {
            None
        };
        let cast = if flags.is_cast() { input.decode()? } else { 0u8 };
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
            operator,
            cast,
            code,
        };

        Ok(result)
    }
}

impl Encode for Function {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        let flags = value
            .flags
            .with_has_return_value(value.return_type.is_some())
            .with_has_base_method(value.base_method.is_some())
            .with_has_parameters(!value.parameters.is_empty())
            .with_has_locals(!value.locals.is_empty())
            .with_is_operator_overload(value.operator.is_some())
            .with_has_body(!value.code.is_empty());

        output.encode(&value.visibility)?;
        output.encode(&flags)?;
        if let Some(ref source) = value.source {
            output.encode(source)?;
        }
        if let Some(ref type_) = value.return_type {
            output.encode(type_)?;
            output.encode(&value.unk1)?;
        }
        if let Some(ref method) = value.base_method {
            output.encode(method)?;
        }
        if flags.has_parameters() {
            output.encode_slice_prefixed::<u32, PoolIndex<Parameter>>(&value.parameters)?;
        }
        if flags.has_locals() {
            output.encode_slice_prefixed::<u32, PoolIndex<Local>>(&value.locals)?;
        }
        if let Some(ref operator) = value.operator {
            output.encode(operator)?;
        }
        if flags.is_cast() {
            output.encode(&value.cast)?;
        }
        if flags.has_body() {
            output.encode(&value.code)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Field {
    pub visibility: Visibility,
    pub type_: PoolIndex<Type>,
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

impl Encode for Field {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        let flags = value.flags.with_has_hint(value.hint.is_some());

        output.encode(&value.visibility)?;
        output.encode(&value.type_)?;
        output.encode(&flags)?;
        if let Some(ref hint) = value.hint {
            output.encode_str_prefixed::<u16>(hint)?;
        }
        output.encode_slice_prefixed::<u32, Property>(&value.attributes)?;
        output.encode_slice_prefixed::<u32, Property>(&value.defaults)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Prim,
    Class,
    Ref(PoolIndex<Type>),
    WeakRef(PoolIndex<Type>),
    Array(PoolIndex<Type>),
    StaticArray(PoolIndex<Type>, u32),
    ScriptRef(PoolIndex<Type>),
}

impl Decode for Type {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let tag: u8 = input.decode()?;
        match tag {
            0 => Ok(Type::Prim),
            1 => Ok(Type::Class),
            2 => Ok(Type::Ref(input.decode()?)),
            3 => Ok(Type::WeakRef(input.decode()?)),
            4 => Ok(Type::Array(input.decode()?)),
            5 => Ok(Type::StaticArray(input.decode()?, input.decode()?)),
            6 => Ok(Type::ScriptRef(input.decode()?)),
            _ => panic!("Unknown Type enum value {}", tag),
        }
    }
}

impl Encode for Type {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        match value {
            Type::Prim => output.encode(&0u8),
            Type::Class => output.encode(&1u8),
            Type::Ref(inner) => {
                output.encode(&2u8)?;
                output.encode(inner)
            }
            Type::WeakRef(inner) => {
                output.encode(&3u8)?;
                output.encode(inner)
            }
            Type::Array(inner) => {
                output.encode(&4u8)?;
                output.encode(inner)
            }
            Type::StaticArray(inner, size) => {
                output.encode(&5u8)?;
                output.encode(inner)?;
                output.encode(size)
            }
            Type::ScriptRef(inner) => {
                output.encode(&6u8)?;
                output.encode(inner)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub type_: PoolIndex<Type>,
    pub flags: LocalFlags,
}

impl Local {
    pub fn new(type_: PoolIndex<Type>, flags: LocalFlags) -> Local {
        Local { type_, flags }
    }
}

impl Decode for Local {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let type_ = input.decode()?;
        let flags = input.decode()?;
        Ok(Local { type_, flags })
    }
}

impl Encode for Local {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.type_)?;
        output.encode(&value.flags)
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub type_: PoolIndex<Type>,
    pub flags: ParameterFlags,
}

impl Decode for Parameter {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let type_ = input.decode()?;
        let flags = input.decode()?;
        Ok(Parameter { type_, flags })
    }
}

impl Encode for Parameter {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.type_)?;
        output.encode(&value.flags)
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

impl Encode for SourceFile {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.id)?;
        output.encode(&value.path_hash)?;
        output.encode_str_prefixed::<u16>(&value.path.to_str().unwrap().replace("/", "\\"))
    }
}

#[bitfield(bits = 16)]
#[derive(Debug, Clone, Copy)]
pub struct FieldFlags {
    pub is_native: bool,
    pub is_edit: bool,
    pub is_inline: bool,
    pub is_const: bool,
    pub is_rep: bool,
    pub has_hint: bool,
    pub is_inst_edit: bool,
    pub has_default: bool,
    pub is_persistent: bool,
    pub bit9: bool,
    pub is_mutable: bool,
    #[skip]
    pub remainder: B5,
}

impl Decode for FieldFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(FieldFlags::from_bytes(input.decode()?))
    }
}

impl Encode for FieldFlags {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(&FieldFlags::into_bytes(*value))
    }
}

#[bitfield(bits = 8)]
#[derive(Debug, Clone, Copy)]
pub struct LocalFlags {
    pub is_const: bool,
    #[skip]
    pub remainder: B7,
}

impl Decode for LocalFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(LocalFlags::from_bytes(input.decode()?))
    }
}

impl Encode for LocalFlags {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(&LocalFlags::into_bytes(*value))
    }
}

#[bitfield(bits = 8)]
#[derive(Debug, Clone, Copy)]
pub struct ParameterFlags {
    pub is_optional: bool,
    pub is_out: bool,
    pub is_short_circuit: bool,
    pub bit3: bool,
    #[skip]
    pub remainder: B4,
}

impl Decode for ParameterFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(ParameterFlags::from_bytes(input.decode()?))
    }
}

impl Encode for ParameterFlags {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(&ParameterFlags::into_bytes(*value))
    }
}

#[bitfield(bits = 16)]
#[derive(Debug, Clone, Copy)]
pub struct ClassFlags {
    pub is_native: bool,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_struct: bool,
    pub has_functions: bool,
    pub has_fields: bool,
    pub bit6: bool,
    pub bit7: bool,
    pub has_overrides: bool,
    #[skip]
    pub remainder: B7,
}

impl Decode for ClassFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(ClassFlags::from_bytes(input.decode()?))
    }
}

impl Encode for ClassFlags {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(&ClassFlags::into_bytes(*value))
    }
}

#[bitfield(bits = 32)]
#[derive(Debug, Clone, Copy)]
pub struct FunctionFlags {
    pub is_static: bool,
    pub is_exec: bool,
    pub is_timer: bool,
    pub is_final: bool,
    pub is_native: bool,
    pub is_callback: bool,
    pub is_operator_overload: bool,
    pub has_return_value: bool,
    pub has_base_method: bool,
    pub has_parameters: bool,
    pub has_locals: bool,
    pub has_body: bool,
    pub is_cast: bool,
    pub is_safe: bool,
    #[skip]
    pub padding: B4,
    pub is_const: bool,
    pub bit19: bool,
    pub bit20: bool,
    pub bit21: bool,
    #[skip]
    pub remainder: B10,
}

impl Decode for FunctionFlags {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(FunctionFlags::from_bytes(input.decode()?))
    }
}

impl Encode for FunctionFlags {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(&FunctionFlags::into_bytes(*value))
    }
}

#[derive(BitfieldSpecifier)]
#[bits = 8]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
        Ok(Visibility::from_bytes(input.decode()?).expect("Invalid Visibility enum value"))
    }
}

impl Encode for Visibility {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&Visibility::into_bytes(*value).unwrap())
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

impl Encode for SourceReference {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.file)?;
        output.encode(&value.line)
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

impl Encode for Property {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode_str_prefixed::<u16>(&value.name)?;
        output.encode_str_prefixed::<u16>(&value.value)
    }
}

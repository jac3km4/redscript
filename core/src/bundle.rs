use std::collections::hash_map::{self, HashMap};
use std::hash::Hash;
use std::io::Seek;
use std::marker::PhantomData;
use std::{fmt, io};

use itertools::chain;
use modular_bitfield::prelude::*;
use thiserror::Error;

use crate::decode::{Decode, DecodeExt};
use crate::definition::{AnyDefinition, Class, Definition, Enum, Field, Function, Local, Parameter, Type};
use crate::encode::{Encode, EncodeExt};
use crate::io::StreamOffset;
use crate::Ref;

#[derive(Debug)]
pub struct ScriptBundle {
    header: Header,
    pub pool: ConstantPool,
}

impl ScriptBundle {
    pub fn load<I: io::Read + io::Seek>(input: &mut I) -> io::Result<Self> {
        let header: Header = input.decode()?;
        let pool = ConstantPool::decode(input, &header)?;
        let cache = ScriptBundle { header, pool };
        Ok(cache)
    }

    pub fn save<O: io::Write + io::Seek>(&self, output: &mut O) -> io::Result<()> {
        output.seek(io::SeekFrom::Start(Header::SIZE as u64))?;
        let header = self.pool.encode(output, &self.header)?;

        output.seek(io::SeekFrom::Start(0))?;
        output.encode(&header)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Header {
    version: u32,
    flags: u32,
    timestamp: Timestamp,
    unk3: u32,
    hash: u32,
    chunks: u32,
    data: TableHeader,
    names: TableHeader,
    tweakdb_indexes: TableHeader,
    resources: TableHeader,
    strings: TableHeader,
    definitions: TableHeader,
}

impl Header {
    const MAGIC: u32 = 0x5344_4552;
    const SIZE: usize = 104;
    const SUPPORTED_VERSION: u32 = 14;
}

impl Decode for Header {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let magic: u32 = input.decode()?;
        if magic != Header::MAGIC {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid file header"));
        }

        let version: u32 = input.decode()?;
        let flags: u32 = input.decode()?;
        let timestamp: Timestamp = input.decode()?;
        if version != Self::SUPPORTED_VERSION {
            log::warn!(
                "Loading an unsupported version of the script cache (v{version}) built at {timestamp}. \
                 You might be running the wrong version of redscript."
            );
        }
        let unk3: u32 = input.decode()?;
        let hash: u32 = input.decode()?;
        let chunks: u32 = input.decode()?;
        let data: TableHeader = input.decode()?;
        let names: TableHeader = input.decode()?;
        let tweakdb_indexes: TableHeader = input.decode()?;
        let resources: TableHeader = input.decode()?;
        let definitions: TableHeader = input.decode()?;
        let strings: TableHeader = input.decode()?;

        let result = Header {
            version,
            flags,
            timestamp,
            unk3,
            hash,
            chunks,
            data,
            names,
            tweakdb_indexes,
            resources,
            strings,
            definitions,
        };
        Ok(result)
    }
}

impl Encode for Header {
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.encode(&Header::MAGIC)?;
        output.encode(&self.version)?;
        output.encode(&self.flags)?;
        output.encode(&self.timestamp)?;
        output.encode(&self.unk3)?;
        output.encode(&self.hash)?;
        output.encode(&self.chunks)?;
        output.encode(&self.data)?;
        output.encode(&self.names)?;
        output.encode(&self.tweakdb_indexes)?;
        output.encode(&self.resources)?;
        output.encode(&self.definitions)?;
        output.encode(&self.strings)
    }
}

#[derive(Debug, Clone, Default)]
pub struct ConstantPool {
    pub names: Strings<CName>,
    pub tweakdb_ids: Strings<TweakDbId>,
    pub resources: Strings<Resource>,
    pub strings: Strings<String>,
    pub(crate) definitions: Vec<Definition>,
}

impl ConstantPool {
    pub fn decode<I: io::Read + io::Seek>(input: &mut I, header: &Header) -> io::Result<Self> {
        let buffer = input.decode_bytes(header.data.count)?;

        let mut cursor = io::Cursor::new(buffer);

        let names = Strings::decode_from(&mut cursor, &input.decode_vec(header.names.count)?)?;
        let tweakdb_ids = Strings::decode_from(&mut cursor, &input.decode_vec(header.tweakdb_indexes.count)?)?;
        let resources = Strings::decode_from(&mut cursor, &input.decode_vec(header.resources.count)?)?;
        let headers: Vec<DefinitionHeader> = input.decode_vec(header.definitions.count)?;
        let strings = Strings::decode_from(&mut cursor, &input.decode_vec(header.strings.count)?)?;

        let mut definitions = Vec::with_capacity(headers.len());
        definitions.push(Definition::DEFAULT);

        for header in headers.iter().skip(1) {
            let definition = Definition::decode(input, header)?;
            definitions.push(definition);
        }

        let result = ConstantPool {
            names,
            tweakdb_ids,
            resources,
            strings,
            definitions,
        };
        Ok(result)
    }

    pub fn encode<O: io::Write + io::Seek>(&self, output: &mut O, header: &Header) -> io::Result<Header> {
        let mut buffer = io::Cursor::new(Vec::with_capacity(header.data.count as usize));
        let mut dedup_map = HashMap::new();
        for str in chain!(
            &self.names.strings,
            &self.tweakdb_ids.strings,
            &self.resources.strings,
            &self.strings.strings
        ) {
            match dedup_map.entry(str.clone()) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(buffer.stream_position()? as u32);
                    buffer.encode(&str.as_ref())?;
                }
                hash_map::Entry::Occupied(_) => {}
            }
        }

        let position = output.stream_position()? as u32;
        let data = TableHeader::new(buffer.get_ref(), buffer.position() as u32, position);
        output.write_all(buffer.get_ref())?;

        let name_offsets = self.names.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let names = TableHeader::new(&name_offsets, self.names.strings.len() as u32, position);
        output.write_all(&name_offsets)?;

        let tweakdb_offsets = self.tweakdb_ids.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let tweakdb_indexes = TableHeader::new(&tweakdb_offsets, self.tweakdb_ids.strings.len() as u32, position);
        output.write_all(&tweakdb_offsets)?;

        let resource_offsets = self.resources.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let resources = TableHeader::new(&resource_offsets, self.resources.strings.len() as u32, position);
        output.write_all(&resource_offsets)?;

        let def_header_pos = output.stream_position()?;
        let def_header_size = DefinitionHeader::SIZE as u64 * self.definitions.len() as u64;
        output.seek(io::SeekFrom::Current(def_header_size as i64))?;

        let string_offsets = self.strings.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let strings = TableHeader::new(&string_offsets, self.strings.strings.len() as u32, position);
        output.write_all(&string_offsets)?;

        let mut buffer = io::Cursor::new(Vec::with_capacity(def_header_size as usize));
        buffer.encode(&DefinitionHeader::DEFAULT)?;

        let mut offset_output = StreamOffset::new_seekable(output)?;
        for definition in self.definitions.iter().skip(1) {
            let header = DefinitionHeader::encode_definition(&mut offset_output, definition)?;
            buffer.encode(&header)?;
        }
        let output = offset_output.into_inner();
        output.seek(io::SeekFrom::Start(def_header_pos))?;
        output.write_all(buffer.get_ref())?;

        let definitions = TableHeader::new(buffer.get_ref(), self.definitions.len() as u32, def_header_pos as u32);
        let header_for_hash = Header {
            data,
            names,
            tweakdb_indexes,
            resources,
            strings,
            definitions,
            hash: 0xDEAD_BEEF,
            ..header.clone()
        };

        let mut buffer = io::Cursor::new(Vec::with_capacity(Header::SIZE));
        buffer.encode(&header_for_hash)?;

        let header = Header {
            hash: crc32fast::hash(buffer.get_ref()),
            ..header_for_hash
        };
        Ok(header)
    }

    fn definition_by<F: Fn(&AnyDefinition) -> Option<&A>, A>(
        &self,
        index: PoolIndex<A>,
        get: F,
    ) -> Result<&A, PoolError> {
        self.definitions
            .get(index.value as usize)
            .and_then(|def| get(&def.value))
            .ok_or_else(|| PoolError::DefinitionNotFound(index.cast()))
    }

    pub fn definition<A>(&self, index: PoolIndex<A>) -> Result<&Definition, PoolError> {
        self.definitions
            .get(index.value as usize)
            .ok_or_else(|| PoolError::DefinitionNotFound(index.cast()))
    }

    pub fn function(&self, index: PoolIndex<Function>) -> Result<&Function, PoolError> {
        self.definition_by(index, AnyDefinition::as_function)
    }

    pub fn function_mut(&mut self, index: PoolIndex<Function>) -> Result<&mut Function, PoolError> {
        self.definitions
            .get_mut(index.value as usize)
            .and_then(|def| def.value.as_function_mut())
            .ok_or_else(|| PoolError::DefinitionNotFound(index.cast()))
    }

    pub fn field(&self, index: PoolIndex<Field>) -> Result<&Field, PoolError> {
        self.definition_by(index, AnyDefinition::as_field)
    }

    pub fn field_mut(&mut self, index: PoolIndex<Field>) -> Result<&mut Field, PoolError> {
        self.definitions
            .get_mut(index.value as usize)
            .and_then(|def| def.value.as_field_mut())
            .ok_or_else(|| PoolError::DefinitionNotFound(index.cast()))
    }

    pub fn parameter(&self, index: PoolIndex<Parameter>) -> Result<&Parameter, PoolError> {
        self.definition_by(index, AnyDefinition::as_parameter)
    }

    pub fn local(&self, index: PoolIndex<Local>) -> Result<&Local, PoolError> {
        self.definition_by(index, AnyDefinition::as_local)
    }

    pub fn type_(&self, index: PoolIndex<Type>) -> Result<&Type, PoolError> {
        self.definition_by(index, AnyDefinition::as_type)
    }

    pub fn class(&self, index: PoolIndex<Class>) -> Result<&Class, PoolError> {
        self.definition_by(index, AnyDefinition::as_class)
    }

    pub fn class_mut(&mut self, index: PoolIndex<Class>) -> Result<&mut Class, PoolError> {
        self.definitions
            .get_mut(index.value as usize)
            .and_then(|def| def.value.as_class_mut())
            .ok_or_else(|| PoolError::DefinitionNotFound(index.cast()))
    }

    pub fn enum_(&self, index: PoolIndex<Enum>) -> Result<&Enum, PoolError> {
        self.definition_by(index, AnyDefinition::as_enum)
    }

    pub fn enum_value(&self, index: PoolIndex<i64>) -> Result<i64, PoolError> {
        self.definition_by(index, AnyDefinition::as_enum_value).cloned()
    }

    pub fn def_name<A>(&self, index: PoolIndex<A>) -> Result<Ref<str>, PoolError> {
        self.names.get(self.definition(index)?.name)
    }

    pub fn definitions(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions
            .iter()
            .enumerate()
            .skip(1)
            .map(|(index, def)| (PoolIndex::new(index as u32), def))
    }

    pub fn reserve<A>(&mut self) -> PoolIndex<A> {
        self.add_definition(Definition::DEFAULT)
    }

    pub fn put_definition<A>(&mut self, index: PoolIndex<A>, definition: Definition) {
        self.definitions[index.value as usize] = definition;
    }

    pub fn swap_definition<A>(&mut self, lhs: PoolIndex<A>, rhs: PoolIndex<A>) {
        self.definitions.swap(lhs.value as usize, rhs.value as usize);
    }

    pub fn add_definition<A>(&mut self, definition: Definition) -> PoolIndex<A> {
        let position = self.definitions.len();
        self.definitions.push(definition);
        PoolIndex::new(position as u32)
    }

    pub fn stub_definition<A>(&mut self, name_idx: PoolIndex<CName>) -> PoolIndex<A> {
        self.add_definition(Definition::type_(name_idx, Type::Prim))
    }

    pub fn rename<A>(&mut self, index: PoolIndex<A>, name: PoolIndex<CName>) {
        self.definitions[index.value as usize].name = name;
    }

    pub fn roots(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions().filter(|(_, def)| def.parent.is_undefined())
    }
}

#[derive(Debug, Clone)]
pub struct Strings<K> {
    strings: Vec<Ref<str>>,
    mappings: HashMap<Ref<str>, PoolIndex<K>>,
    phantom: PhantomData<K>,
}

impl<K: DefaultString> Strings<K> {
    fn decode_from<I: io::Read + io::Seek>(input: &mut I, offsets: &[u32]) -> io::Result<Strings<K>> {
        let mut strings = Vec::with_capacity(offsets.len());
        let mut mappings = HashMap::new();
        for (idx, offset) in offsets.iter().enumerate() {
            input.seek(io::SeekFrom::Start((*offset).into()))?;
            let str: Ref<str> = Ref::from(input.decode::<String>()?);
            strings.push(str.clone());
            mappings.insert(str, PoolIndex::new(idx as u32));
        }
        let result = Strings {
            strings,
            mappings,
            phantom: PhantomData,
        };
        Ok(result)
    }

    fn encoded_offsets(&self, str_map: &HashMap<Ref<str>, u32>) -> io::Result<Vec<u8>> {
        let mut offsets = io::Cursor::new(Vec::new());
        for string in &self.strings {
            offsets.encode(str_map.get(string).unwrap())?;
        }
        Ok(offsets.into_inner())
    }

    pub fn get(&self, index: PoolIndex<K>) -> Result<Ref<str>, PoolError> {
        match K::DEFAULT {
            Some(default) if index.is_undefined() => Ok(Ref::from(default)),
            _ => self
                .strings
                .get(index.value as usize)
                .cloned()
                .ok_or_else(|| PoolError::StringNotFound(index.cast())),
        }
    }

    pub fn get_index(&self, name: &str) -> Option<PoolIndex<K>> {
        self.mappings.get(name).copied()
    }

    pub fn add(&mut self, str: Ref<str>) -> PoolIndex<K> {
        if K::DEFAULT == Some(&str) {
            PoolIndex::UNDEFINED
        } else {
            let idx = PoolIndex::new(self.strings.len() as u32);
            match self.mappings.entry(str.clone()) {
                hash_map::Entry::Occupied(entry) => *entry.get(),
                hash_map::Entry::Vacant(slot) => {
                    self.strings.push(str);
                    *slot.insert(idx)
                }
            }
        }
    }
}

impl<K> Default for Strings<K> {
    fn default() -> Self {
        Self {
            strings: vec![],
            mappings: HashMap::new(),
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone)]
struct TableHeader {
    offset: u32,
    count: u32,
    hash: u32,
}

impl TableHeader {
    fn new(bytes: &[u8], count: u32, offset: u32) -> TableHeader {
        TableHeader {
            offset,
            count,
            hash: crc32fast::hash(bytes),
        }
    }
}

impl Decode for TableHeader {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let offset = input.decode()?;
        let count = input.decode()?;
        let hash = input.decode()?;
        let result = TableHeader { offset, count, hash };
        Ok(result)
    }
}

impl Encode for TableHeader {
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.encode(&self.offset)?;
        output.encode(&self.count)?;
        output.encode(&self.hash)
    }
}

#[derive(Debug)]
pub struct DefinitionHeader {
    pub name: PoolIndex<CName>,
    pub parent: PoolIndex<Definition>,
    pub offset: u32,
    pub size: u32,
    pub type_: DefinitionType,
    pub unk1: u8,
    pub unk2: u8,
    pub unk3: u8,
}

impl DefinitionHeader {
    const DEFAULT: DefinitionHeader = DefinitionHeader {
        name: PoolIndex::UNDEFINED,
        parent: PoolIndex::UNDEFINED,
        offset: 0,
        size: 0,
        type_: DefinitionType::Type,
        unk1: 0,
        unk2: 0,
        unk3: 0,
    };
    const SIZE: usize = 20;

    fn encode_definition<O: io::Write + io::Seek>(
        output: &mut StreamOffset<O>,
        definition: &Definition,
    ) -> io::Result<DefinitionHeader> {
        let offset = output.offset();
        output.encode(&definition.value)?;
        let size = output.offset() - offset;
        let header = DefinitionHeader {
            name: definition.name,
            parent: definition.parent,
            offset: offset as u32,
            size: size as u32,
            type_: definition.value.type_(),
            unk1: definition.unk1,
            unk2: definition.unk2,
            unk3: definition.unk3,
        };
        Ok(header)
    }
}

impl Decode for DefinitionHeader {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let name = input.decode()?;
        let parent = input.decode()?;
        let offset = input.decode()?;
        let size = input.decode()?;
        let type_ = input.decode()?;
        let unk1 = input.decode()?;
        let unk2 = input.decode()?;
        let unk3 = input.decode()?;
        let result = DefinitionHeader {
            name,
            parent,
            offset,
            size,
            type_,
            unk1,
            unk2,
            unk3,
        };
        Ok(result)
    }
}

impl Encode for DefinitionHeader {
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.encode(&self.name)?;
        output.encode(&self.parent)?;
        output.encode(&self.offset)?;
        output.encode(&self.size)?;
        output.encode(&self.type_)?;
        output.encode(&self.unk1)?;
        output.encode(&self.unk2)?;
        output.encode(&self.unk3)
    }
}

#[derive(BitfieldSpecifier)]
#[bits = 8]
#[derive(Debug, Clone, Copy)]
pub enum DefinitionType {
    Type = 0,
    Class = 1,
    EnumValue = 2,
    Enum = 3,
    BitField = 4,
    Function = 5,
    Parameter = 6,
    Local = 7,
    Field = 8,
    SourceFile = 9,
}

impl Decode for DefinitionType {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(DefinitionType::from_bytes(input.decode()?).expect("Invalid DefinitionType enum value"))
    }
}

impl Encode for DefinitionType {
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.encode(&DefinitionType::into_bytes(*self).unwrap())
    }
}

#[bitfield]
#[derive(Debug, Clone, Copy)]
pub struct Timestamp {
    #[skip]
    padding: B10,
    pub day: B5,
    pub month: B5,
    pub year: B12,
    pub millis: B10,
    pub seconds: B6,
    pub minutes: B6,
    pub hours: B10,
}

impl fmt::Display for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:02}/{:02}/{} {:02}:{:02}:{:02}:{:03}",
            self.day() + 1,
            self.month() + 1,
            self.year(),
            self.hours(),
            self.minutes(),
            self.seconds(),
            self.millis()
        ))
    }
}

impl Encode for Timestamp {
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.write_all(&self.into_bytes())
    }
}

impl Decode for Timestamp {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(Timestamp::from_bytes(input.decode()?))
    }
}

pub struct PoolIndex<A> {
    value: u32,
    phantom: PhantomData<A>,
}

impl<A> PoolIndex<A> {
    pub const DEFAULT_SOURCE: PoolIndex<A> = PoolIndex::new(1);
    pub const UNDEFINED: PoolIndex<A> = PoolIndex::new(0);

    pub const fn new(index: u32) -> PoolIndex<A> {
        PoolIndex {
            value: index,
            phantom: PhantomData,
        }
    }

    pub fn is_undefined(&self) -> bool {
        self.value == 0
    }

    #[inline]
    pub fn cast<B>(&self) -> PoolIndex<B> {
        PoolIndex {
            value: self.value,
            phantom: PhantomData,
        }
    }
}

impl<A> Decode for PoolIndex<A> {
    #[inline]
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let index = input.decode::<u32>()?;
        Ok(PoolIndex {
            value: index,
            phantom: PhantomData,
        })
    }
}

impl<A> Encode for PoolIndex<A> {
    #[inline]
    fn encode<O: io::Write>(&self, output: &mut O) -> io::Result<()> {
        output.encode(&(self.value))
    }
}

impl<A> Clone for PoolIndex<A> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> Copy for PoolIndex<A> {}

impl<A> fmt::Debug for PoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("PoolIndex").field(&self.value).finish()
    }
}

impl<A> PartialEq for PoolIndex<A> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<A> Eq for PoolIndex<A> {}

impl<A> PartialOrd for PoolIndex<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<A> Ord for PoolIndex<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<A> Hash for PoolIndex<A> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<A> fmt::Display for PoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.value))
    }
}

impl<A> From<PoolIndex<A>> for u32 {
    fn from(index: PoolIndex<A>) -> Self {
        index.value
    }
}

pub trait DefaultString {
    const DEFAULT: Option<&'static str>;
}

impl DefaultString for String {
    const DEFAULT: Option<&'static str> = None;
}

#[derive(Debug, Clone)]
pub struct CName;

impl DefaultString for CName {
    const DEFAULT: Option<&'static str> = Some("None");
}

#[derive(Debug, Clone)]
pub struct Resource;

impl DefaultString for Resource {
    const DEFAULT: Option<&'static str> = None;
}

#[derive(Debug, Clone, Default)]
pub struct TweakDbId;

impl DefaultString for TweakDbId {
    const DEFAULT: Option<&'static str> = None;
}

#[derive(Debug, Clone, Error)]
pub enum PoolError {
    #[error("definition not found: {0}")]
    DefinitionNotFound(PoolIndex<Definition>),
    #[error("string not found: {0}")]
    StringNotFound(PoolIndex<String>),
    #[error("unexpected entry: {0}")]
    UnexpectedEntry(&'static str),
}

#[cfg(test)]
mod tests {
    use std::io::{self, Cursor};

    use super::ScriptBundle;

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    #[test]
    fn reload_scripts() -> io::Result<()> {
        let scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut tmp = Cursor::new(Vec::new());
        scripts.save(&mut tmp)?;
        tmp.set_position(0);
        let scripts2 = ScriptBundle::load(&mut tmp)?;
        assert_eq!(scripts.pool.definitions.len(), scripts2.pool.definitions.len());
        Ok(())
    }
}

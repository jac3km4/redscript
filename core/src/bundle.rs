use std::collections::hash_map;
use std::collections::hash_map::{Entry, HashMap};
use std::fmt;
use std::hash::Hash;
use std::io;
use std::io::Seek;
use std::marker::PhantomData;
use std::rc::Rc;

use modular_bitfield::prelude::*;

use crate::decode::{Decode, DecodeExt};
use crate::definition::{Class, Definition, DefinitionValue, Enum, Field, Function, Local, Parameter, Type};
use crate::encode::{Encode, EncodeExt};
use crate::error::Error;
use crate::files::FileIndex;

#[derive(Debug)]
pub struct ScriptBundle {
    pub pool: ConstantPool,
}

impl ScriptBundle {
    pub fn load<I: io::Read + io::Seek>(input: &mut I) -> Result<Self, Error> {
        let pool = ConstantPool::decode(input)?;
        let cache = ScriptBundle { pool };
        Ok(cache)
    }

    pub fn save<O: io::Write + io::Seek>(&self, output: &mut O) -> Result<(), Error> {
        self.pool.encode(output)
    }
}

#[derive(Debug)]
pub struct Header {
    version: u32,
    unk1: u32,
    unk2: u32,
    unk3: u32,
    unk4: u32,
    hash: u32,
    unk5: u32,
    strings: TableHeader,
    names: TableHeader,
    tweakdb_indexes: TableHeader,
    resources: TableHeader,
    definitions: TableHeader,
}

impl Header {
    const MAGIC: u32 = 0x53444552;
    const SIZE: usize = 92;
}

impl Decode for Header {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let magic: u32 = input.decode()?;
        if magic != Header::MAGIC {
            Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid file header"))?
        }

        let version: u32 = input.decode()?;
        let unk1: u32 = input.decode()?;
        let unk2: u32 = input.decode()?;
        let unk3: u32 = input.decode()?;
        let unk4: u32 = input.decode()?;
        let hash: u32 = input.decode()?;
        let unk5: u32 = input.decode()?;
        let strings: TableHeader = input.decode()?;
        let names: TableHeader = input.decode()?;
        let tweakdb_indexes: TableHeader = input.decode()?;
        let resources: TableHeader = input.decode()?;
        let definitions: TableHeader = input.decode()?;

        let result = Header {
            version,
            unk1,
            unk2,
            unk3,
            unk4,
            hash,
            unk5,
            strings,
            names,
            tweakdb_indexes,
            resources,
            definitions,
        };
        Ok(result)
    }
}

impl Encode for Header {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&Header::MAGIC)?;
        output.encode(&value.version)?;
        output.encode(&value.unk1)?;
        output.encode(&value.unk2)?;
        output.encode(&value.unk3)?;
        output.encode(&value.unk4)?;
        output.encode(&value.hash)?;
        output.encode(&value.unk5)?;
        output.encode(&value.strings)?;
        output.encode(&value.names)?;
        output.encode(&value.tweakdb_indexes)?;
        output.encode(&value.resources)?;
        output.encode(&value.definitions)
    }
}

#[derive(Debug)]
pub struct ConstantPool {
    header: Header,
    pub names: Names<String>,
    pub tweakdb_ids: Names<TweakDbId>,
    pub resources: Names<Resource>,
    definitions: Vec<Definition>,
}

impl ConstantPool {
    pub fn decode<I: io::Read + io::Seek>(input: &mut I) -> Result<Self, Error> {
        let header: Header = input.decode()?;
        let buffer = input.decode_bytes(header.strings.count)?;

        let mut cursor = io::Cursor::new(buffer);

        let names = Names::decode_from(&mut cursor, &input.decode_vec(header.names.count)?)?;
        let tweakdb_ids = Names::decode_from(&mut cursor, &input.decode_vec(header.tweakdb_indexes.count)?)?;
        let resources = Names::decode_from(&mut cursor, &input.decode_vec(header.resources.count)?)?;

        input.seek(io::SeekFrom::Start(header.definitions.offset.into()))?;
        let headers: Vec<DefinitionHeader> = input.decode_vec(header.definitions.count)?;

        let mut definitions = Vec::with_capacity(headers.len());
        definitions.push(Definition::DEFAULT);

        for header in headers.iter().skip(1) {
            let definition = Definition::decode(input, header)?;
            definitions.push(definition);
        }

        let result = ConstantPool {
            header,
            names,
            tweakdb_ids,
            resources,
            definitions,
        };
        Ok(result)
    }

    pub fn encode<O: io::Write + io::Seek>(&self, output: &mut O) -> Result<(), Error> {
        let mut buffer = io::Cursor::new(Vec::with_capacity(self.header.strings.count as usize));
        let mut dedup_map = HashMap::new();
        for str in self
            .names
            .strings
            .iter()
            .chain(self.tweakdb_ids.strings.iter())
            .chain(self.resources.strings.iter())
        {
            match dedup_map.entry(str.clone()) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(buffer.stream_position()? as u32);
                    buffer.encode(&str.as_str())?;
                }
                hash_map::Entry::Occupied(_) => {}
            }
        }

        output.seek(io::SeekFrom::Start(Header::SIZE as u64))?;

        let position = output.stream_position()? as u32;
        let strings = TableHeader::new(buffer.get_ref(), buffer.position() as u32, position)?;
        output.write_all(buffer.get_ref())?;

        let name_offsets = self.names.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let names = TableHeader::new(&name_offsets, self.names.strings.len() as u32, position)?;
        output.write_all(&name_offsets)?;

        let tweakdb_offsets = self.tweakdb_ids.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let tweakdb_indexes = TableHeader::new(&tweakdb_offsets, self.tweakdb_ids.strings.len() as u32, position)?;
        output.write_all(&tweakdb_offsets)?;

        let resource_offsets = self.resources.encoded_offsets(&dedup_map)?;
        let position = output.stream_position()? as u32;
        let resources = TableHeader::new(&resource_offsets, self.resources.strings.len() as u32, position)?;
        output.write_all(&resource_offsets)?;

        let def_header_pos = output.stream_position()?;
        let def_header_size = DefinitionHeader::SIZE as u64 * self.definitions.len() as u64;
        output.seek(io::SeekFrom::Current(def_header_size as i64))?;

        let mut buffer = io::Cursor::new(Vec::with_capacity(def_header_size as usize));
        for (idx, definition) in self.definitions.iter().enumerate() {
            if idx == 0 {
                buffer.encode(&DefinitionHeader::DEFAULT)?;
            } else {
                let header = DefinitionHeader::encode_definition(output, definition)?;
                buffer.encode(&header)?;
            }
        }
        output.seek(io::SeekFrom::Start(def_header_pos))?;
        output.write_all(buffer.get_ref())?;

        let definitions = TableHeader::new(buffer.get_ref(), self.definitions.len() as u32, def_header_pos as u32)?;
        let header_for_hash = Header {
            strings,
            names,
            tweakdb_indexes,
            resources,
            definitions,
            hash: 0xDEADBEEF,
            ..self.header
        };

        let mut buffer = io::Cursor::new(Vec::with_capacity(Header::SIZE));
        buffer.encode(&header_for_hash)?;

        let header = Header {
            hash: crc(buffer.get_ref()),
            ..header_for_hash
        };
        output.seek(io::SeekFrom::Start(0))?;
        output.encode(&header)?;
        Ok(())
    }

    pub fn definition<A>(&self, index: PoolIndex<A>) -> Result<&Definition, Error> {
        self.definitions
            .get(index.index)
            .ok_or(Error::PoolError(format!("Definition {} not found", index.index)))
    }

    pub fn function(&self, index: PoolIndex<Function>) -> Result<&Function, Error> {
        if let DefinitionValue::Function(ref fun) = self.definition(index)?.value {
            Ok(fun)
        } else {
            Err(Error::PoolError(format!("{} is not a function", index.index)))
        }
    }

    pub fn function_mut(&mut self, index: PoolIndex<Function>) -> Result<&mut Function, Error> {
        let result = self.definitions.get_mut(index.index).map(|def| &mut def.value);
        if let Some(DefinitionValue::Function(fun)) = result {
            Ok(fun)
        } else {
            Err(Error::PoolError(format!("{} is not a function", index.index)))
        }
    }

    pub fn field(&self, index: PoolIndex<Field>) -> Result<&Field, Error> {
        if let DefinitionValue::Field(ref field) = self.definition(index)?.value {
            Ok(field)
        } else {
            Err(Error::PoolError(format!("{} is not a field", index.index)))
        }
    }

    pub fn parameter(&self, index: PoolIndex<Parameter>) -> Result<&Parameter, Error> {
        if let DefinitionValue::Parameter(ref param) = self.definition(index)?.value {
            Ok(param)
        } else {
            Err(Error::PoolError(format!("{} is not a parameter", index.index)))
        }
    }

    pub fn local(&self, index: PoolIndex<Local>) -> Result<&Local, Error> {
        if let DefinitionValue::Local(ref local) = self.definition(index)?.value {
            Ok(local)
        } else {
            Err(Error::PoolError(format!("{} is not a local", index.index)))
        }
    }

    pub fn type_(&self, index: PoolIndex<Type>) -> Result<&Type, Error> {
        if let DefinitionValue::Type(ref type_) = self.definition(index)?.value {
            Ok(type_)
        } else {
            Err(Error::PoolError(format!("{} is not a type", index.index)))
        }
    }

    pub fn class(&self, index: PoolIndex<Class>) -> Result<&Class, Error> {
        if let DefinitionValue::Class(ref class) = self.definition(index)?.value {
            Ok(class)
        } else {
            Err(Error::PoolError(format!("{} is not a class", index.index)))
        }
    }

    pub fn enum_(&self, index: PoolIndex<Enum>) -> Result<&Enum, Error> {
        if let DefinitionValue::Enum(ref enum_) = self.definition(index)?.value {
            Ok(enum_)
        } else {
            Err(Error::PoolError(format!("{} is not an enum", index.index)))
        }
    }

    pub fn definition_name<A>(&self, index: PoolIndex<A>) -> Result<Rc<String>, Error> {
        self.names.get(self.definition(index)?.name)
    }

    pub fn definitions(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions
            .iter()
            .enumerate()
            .skip(1)
            .map(|(index, def)| (PoolIndex::new(index), def))
    }

    pub fn reserve(&mut self) -> PoolIndex<Definition> {
        self.push_definition(Definition::DEFAULT)
    }

    pub fn put_definition(&mut self, index: PoolIndex<Definition>, definition: Definition) {
        self.definitions[index.index] = definition;
    }

    pub fn push_definition(&mut self, definition: Definition) -> PoolIndex<Definition> {
        let position = self.definitions.len();
        self.definitions.push(definition);
        PoolIndex::new(position)
    }

    pub fn roots(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions().filter(|(_, def)| def.parent.is_undefined())
    }

    pub fn files(&self) -> FileIndex {
        FileIndex::from_pool(self)
    }
}

#[derive(Debug)]
pub struct Names<K> {
    pub strings: Vec<Rc<String>>,
    mappings: HashMap<Rc<String>, PoolIndex<K>>,
    phantom: PhantomData<K>,
}

impl<K> Names<K> {
    fn decode_from<I: io::Read + io::Seek>(input: &mut I, offsets: &[u32]) -> io::Result<Names<K>> {
        let mut strings = Vec::with_capacity(offsets.len());
        let mut mappings = HashMap::new();
        for (idx, offset) in offsets.iter().enumerate() {
            input.seek(io::SeekFrom::Start((*offset).into()))?;
            let str: Rc<String> = Rc::new(input.decode()?);
            strings.push(str.clone());
            mappings.insert(str, PoolIndex::new(idx));
        }
        let result = Names {
            strings,
            mappings,
            phantom: PhantomData,
        };
        Ok(result)
    }

    fn encoded_offsets(&self, str_map: &HashMap<Rc<String>, u32>) -> io::Result<Vec<u8>> {
        let mut offsets = io::Cursor::new(Vec::new());
        for string in &self.strings {
            offsets.encode(str_map.get(string).unwrap())?;
        }
        Ok(offsets.into_inner())
    }

    pub fn get(&self, index: PoolIndex<K>) -> Result<Rc<String>, Error> {
        self.strings
            .get(index.index)
            .ok_or(Error::PoolError(format!("String {} not found", index.index)))
            .cloned()
    }

    pub fn add(&mut self, str: String) -> PoolIndex<K> {
        let idx = PoolIndex::new(self.strings.len());
        let rc = Rc::new(str);
        match self.mappings.entry(rc.clone()) {
            Entry::Occupied(entry) => entry.get().clone(),
            Entry::Vacant(slot) => {
                self.strings.push(rc);
                slot.insert(idx).clone()
            }
        }
    }
}

#[derive(Debug)]
struct TableHeader {
    offset: u32,
    count: u32,
    hash: u32,
}

impl TableHeader {
    fn new(bytes: &[u8], count: u32, offset: u32) -> io::Result<TableHeader> {
        let result = TableHeader {
            offset,
            count,
            hash: crc(bytes),
        };
        Ok(result)
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
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.offset)?;
        output.encode(&value.count)?;
        output.encode(&value.hash)
    }
}

#[derive(Debug)]
pub struct DefinitionHeader {
    pub name: PoolIndex<String>,
    pub parent: PoolIndex<Definition>,
    pub offset: u32,
    pub size: u32,
    pub type_: DefinitionType,
    pub unk1: u8,
    pub unk2: u8,
    pub unk3: u8,
}

impl DefinitionHeader {
    const SIZE: usize = 20;

    const DEFAULT: DefinitionHeader = DefinitionHeader {
        name: PoolIndex::UNDEFINED,
        parent: PoolIndex::UNDEFINED,
        offset: 0,
        size: 0,
        type_: DefinitionType::Type,
        unk1: 0,
        unk2: 213,
        unk3: 222,
    };

    fn encode_definition<O: io::Write + io::Seek>(
        output: &mut O,
        definition: &Definition,
    ) -> io::Result<DefinitionHeader> {
        let offset = output.stream_position()?;
        output.encode(&definition.value)?;
        let size = output.stream_position()?;
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
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.name)?;
        output.encode(&value.parent)?;
        output.encode(&value.offset)?;
        output.encode(&value.size)?;
        output.encode(&value.type_)?;
        output.encode(&value.unk1)?;
        output.encode(&value.unk2)?;
        output.encode(&value.unk3)
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
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&DefinitionType::into_bytes(*value).unwrap())
    }
}

pub struct PoolIndex<A> {
    index: usize,
    phantom: PhantomData<A>,
}

impl<A> PoolIndex<A> {
    fn new(index: usize) -> PoolIndex<A> {
        PoolIndex {
            index,
            phantom: PhantomData,
        }
    }

    pub const UNDEFINED: PoolIndex<A> = PoolIndex {
        index: 0,
        phantom: PhantomData,
    };

    pub const DEFAULT_SOURCE: PoolIndex<A> = PoolIndex {
        index: 1,
        phantom: PhantomData,
    };

    pub fn is_undefined(&self) -> bool {
        self.index == 0
    }

    pub fn cast<B>(&self) -> PoolIndex<B> {
        PoolIndex {
            index: self.index,
            phantom: PhantomData,
        }
    }
}

impl<A> Decode for PoolIndex<A> {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let index = input.decode::<u32>()? as usize;
        Ok(PoolIndex {
            index,
            phantom: PhantomData,
        })
    }
}

impl<A> Encode for PoolIndex<A> {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&(value.index as u32))
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
        f.debug_tuple("PoolIndex").field(&self.index).finish()
    }
}

impl<A> PartialEq for PoolIndex<A> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<A> Eq for PoolIndex<A> {}

impl<A> Hash for PoolIndex<A> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Hash::hash(&self.index, state)
    }
}

#[derive(Debug)]
pub struct Resource(String);

#[derive(Debug)]
pub struct TweakDbId(String);

fn crc(bytes: &[u8]) -> u32 {
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(bytes);
    hasher.finalize()
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::ScriptBundle;
    use crate::error::Error;

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    #[test]
    fn reload_scripts() -> Result<(), Error> {
        let scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut tmp = Cursor::new(Vec::new());
        scripts.save(&mut tmp)?;
        tmp.set_position(0);
        let scripts2 = ScriptBundle::load(&mut tmp)?;
        assert_eq!(scripts.pool.definitions.len(), scripts2.pool.definitions.len());
        Ok(())
    }
}

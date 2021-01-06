use crate::decode::{Decode, DecodeExt};
use crate::definition::Definition;
use crate::error::Error;
use crate::files::FileIndex;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::fmt;
use std::hash::Hash;
use std::io;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug)]
pub struct ScriptBundle {
    magic: u32,
    version: u32,
    unk1: u32,
    unk2: u32,
    unk3: u32,
    unk4: u32,
    hash: u32,
    unk5: u32,
    pool: ConstantPool,
}

impl ScriptBundle {
    pub fn load<I: io::Read + io::Seek>(input: &mut I) -> Result<Self, Error> {
        let magic: u32 = input.decode()?;
        if magic != 0x53444552 {
            Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid file header"))?
        }
        let version: u32 = input.decode()?;
        let unk1: u32 = input.decode()?;

        let unk2: u32 = input.decode()?;
        let unk3: u32 = input.decode()?;
        let unk4: u32 = input.decode()?;
        let hash: u32 = input.decode()?;
        let unk5: u32 = input.decode()?;
        let pool = ConstantPool::load(input)?;

        let cache = ScriptBundle {
            magic,
            version,
            unk1,
            unk2,
            unk3,
            unk4,
            hash,
            unk5,
            pool,
        };
        Ok(cache)
    }

    pub fn pool(&self) -> &ConstantPool {
        &self.pool
    }
}

#[derive(Debug)]
pub struct ConstantPool {
    names: Vec<Rc<String>>,
    tdb_indexes: Vec<Rc<String>>,
    resources: Vec<Rc<String>>,
    definitions: Vec<Definition>,
}

impl ConstantPool {
    pub fn load<I: io::Read + io::Seek>(input: &mut I) -> Result<Self, Error> {
        let strings: TableIndex = input.decode()?;
        let names: TableIndex = input.decode()?;
        let tdb_indexes: TableIndex = input.decode()?;
        let resources: TableIndex = input.decode()?;
        let definitions: TableIndex = input.decode()?;

        let buffer = input.decode_bytes(strings.count)?;
        let mut cursor = io::Cursor::new(buffer);

        let names: Vec<String> = names.decode_from(&mut cursor, &input.decode_vec(names.count)?)?;
        let tdb_indexes: Vec<String> = tdb_indexes.decode_from(&mut cursor, &input.decode_vec(tdb_indexes.count)?)?;
        let resources: Vec<String> = resources.decode_from(&mut cursor, &input.decode_vec(resources.count)?)?;

        input.seek(io::SeekFrom::Start(definitions.offset.into()))?;
        let headers: Vec<DefinitionHeader> = input.decode_vec(definitions.count)?;

        let mut definitions = Vec::with_capacity(headers.len());
        definitions.push(Definition::DUMMY);

        for header in headers.iter().skip(1) {
            match Definition::decode(input, &header) {
                Ok(definition) => definitions.push(definition),
                Err(err) => {
                    println!("Error reading definition at {}: {:?}", header.offset, err);
                    definitions.push(Definition::DUMMY)
                }
            }
        }

        let result = ConstantPool {
            names: names.into_iter().map(|str| Rc::new(str)).collect(),
            tdb_indexes: tdb_indexes.into_iter().map(|str| Rc::new(str)).collect(),
            resources: resources.into_iter().map(|str| Rc::new(str)).collect(),
            definitions,
        };
        Ok(result)
    }

    pub fn definition(&self, index: PoolIndex<Definition>) -> Result<&Definition, Error> {
        self.definitions
            .get(index.index)
            .ok_or(Error::PoolError(format!("Definition {} not found", index.index)))
    }

    pub fn name(&self, index: PoolIndex<String>) -> Result<Rc<String>, Error> {
        self.names
            .get(index.index)
            .ok_or(Error::PoolError(format!("Name {} not found", index.index)))
            .map(|str| str.clone())
    }

    pub fn tweakdb_index(&self, index: PoolIndex<TweakDbIndex>) -> Result<Rc<String>, Error> {
        self.tdb_indexes
            .get(index.index)
            .ok_or(Error::PoolError(format!("TweakDB index {} not found", index.index)))
            .map(|str| str.clone())
    }

    pub fn resource(&self, index: PoolIndex<Resource>) -> Result<Rc<String>, Error> {
        self.resources
            .get(index.index)
            .ok_or(Error::PoolError(format!("TweakDB index {} not found", index.index)))
            .map(|str| str.clone())
    }

    pub fn definition_name(&self, index: PoolIndex<Definition>) -> Result<Rc<String>, Error> {
        self.name(self.definition(index)?.name)
    }

    pub fn definitions(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions
            .iter()
            .enumerate()
            .map(|(index, def)| (PoolIndex::new(index), def))
    }

    pub fn roots(&self) -> impl Iterator<Item = (PoolIndex<Definition>, &Definition)> {
        self.definitions().filter(|(_, def)| def.parent.index == 0)
    }

    pub fn files(&self) -> FileIndex {
        FileIndex::from_pool(self)
    }
}

#[derive(Debug)]
struct TableIndex {
    offset: u32,
    count: u32,
    hash: u32,
}

impl TableIndex {
    fn decode_from<I: io::Read + io::Seek, A: Decode>(&self, input: &mut I, offsets: &[u32]) -> io::Result<Vec<A>> {
        let mut vec = Vec::with_capacity(offsets.len());
        for offset in offsets {
            input.seek(io::SeekFrom::Start((*offset).into()))?;
            vec.push(input.decode()?);
        }
        Ok(vec)
    }
}

impl Decode for TableIndex {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let offset = input.decode()?;
        let count = input.decode()?;
        let hash = input.decode()?;
        let result = TableIndex { offset, count, hash };
        Ok(result)
    }
}

#[derive(Debug)]
pub struct DefinitionHeader {
    pub name: PoolIndex<String>,
    pub parent: PoolIndex<Definition>,
    pub offset: u32,
    pub size: u32,
    pub type_: DefinitionType,
    unk1: u8,
    unk2: u8,
    unk3: u8,
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

#[derive(Debug, FromPrimitive)]
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
        let res = FromPrimitive::from_u8(input.decode()?);
        Ok(res.expect("Invalid DefinitionType enum value"))
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

    pub const ZERO: PoolIndex<A> = PoolIndex {
        index: 0,
        phantom: PhantomData,
    };

    pub fn is_root(&self) -> bool {
        self.index == 0
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
pub struct TweakDbIndex(String);

use std::marker::PhantomData;
use std::{fmt, iter, mem, ops};

use bitfield_struct::bitfield;
use byte::ctx::{Delimiter, Endianess, LittleEndian};
use byte::{BytesExt, Measure, ToBytesExt, TryRead, TryWrite};
use indexmap::map::RawEntryApiV1;
use indexmap::{IndexMap, IndexSet};

use crate::definition::{
    Class, Definition, DefinitionHeader, DefinitionIndex, Enum, EnumMember, Field, Function,
    IndexedDefinition, Local, Parameter, SourceFile, Type,
};
use crate::index::{
    self, CNameIndex, ClassIndex, EnumIndex, EnumValueIndex, FieldIndex, FunctionIndex, LocalIndex,
    NzPoolIndex, ParameterIndex, PoolIndex, ResourceIndex, SourceFileIndex, StringIndex,
    TweakDbIndex, TypeIndex,
};
use crate::{ENDIANESS, Str, util};

#[derive(Debug)]
pub struct BundleReader<'i> {
    header: Header,
    contents: &'i [u8],
}

impl<'i> BundleReader<'i> {
    pub fn new(bytes: &'i [u8]) -> byte::Result<Self> {
        let header: Header = bytes.read_at(0, ENDIANESS)?;
        if header.magic != Header::MAGIC {
            return Err(byte::Error::BadInput {
                err: "invalid magic number",
            });
        };
        if header.version != Header::SUPPORTED_VERSION {
            return Err(byte::Error::BadInput {
                err: "unsupported version",
            });
        };
        Ok(BundleReader {
            header,
            contents: bytes,
        })
    }

    pub fn cnames(&self) -> ItemReader<'_, 'i, &'i str> {
        ItemReader::new(self, &self.header.cnames)
    }

    pub fn tweakdb_ids(&self) -> ItemReader<'_, 'i, &'i str> {
        ItemReader::new(self, &self.header.tweakdb_ids)
    }

    pub fn resources(&self) -> ItemReader<'_, 'i, &'i str> {
        ItemReader::new(self, &self.header.resources)
    }

    pub fn strings(&self) -> ItemReader<'_, 'i, &'i str> {
        ItemReader::new(self, &self.header.strings)
    }

    pub fn definitions(&self) -> ItemReader<'_, 'i, Definition<'i>> {
        ItemReader::new(self, &self.header.definitions)
    }
}

#[derive(Debug, TryRead, TryWrite, Measure)]
pub struct Header {
    magic: [u8; 4],
    version: u32,
    flags: u32,
    timestamp: Timestamp,
    build: u32,
    crc: u32,
    segments: u32,
    string_data: TableHeader,
    cnames: TableHeader,
    tweakdb_ids: TableHeader,
    resources: TableHeader,
    definitions: TableHeader,
    strings: TableHeader,
}

impl Header {
    const MAGIC: [u8; 4] = *b"REDS";
    const SIZE: u32 = 104;
    const SUPPORTED_VERSION: u32 = 14;
}

#[derive(Debug, Clone, Copy, TryRead, TryWrite, Measure)]
struct TableHeader {
    offset: u32,
    count: u32,
    hash: u32,
}

impl TableHeader {
    fn new(offset: u32, count: u32, bytes: &[u8]) -> Self {
        TableHeader {
            offset,
            count,
            hash: crc32fast::hash(bytes),
        }
    }
}

#[derive(Debug)]
pub struct ScriptBundle<'i> {
    cnames: StringPool<'i, index::types::CName>,
    tdb_ids: StringPool<'i, index::types::TweakDbId>,
    resources: StringPool<'i, index::types::Resource>,
    strings: StringPool<'i, index::types::String>,
    definitions: Vec<Definition<'i>>,
}

impl<'i> ScriptBundle<'i> {
    pub fn from_bytes(bytes: &'i [u8]) -> byte::Result<Self> {
        let reader = BundleReader::new(bytes)?;
        Self::from_reader(&reader)
    }

    pub fn from_reader(reader: &BundleReader<'i>) -> byte::Result<Self> {
        Ok(Self {
            cnames: reader.cnames().into_iter().collect::<Result<_, _>>()?,
            tdb_ids: reader.tweakdb_ids().into_iter().collect::<Result<_, _>>()?,
            resources: reader.resources().into_iter().collect::<Result<_, _>>()?,
            strings: reader.strings().into_iter().collect::<Result<_, _>>()?,
            definitions: iter::once(Ok(Definition::UNDEFINED))
                .chain(reader.definitions().into_iter().skip(1))
                .collect::<Result<_, _>>()?,
        })
    }

    pub fn into_writeable(self) -> WriteableBundle<'i> {
        let mut string_data = StringData::with_capacity(
            self.cnames.len() + self.tdb_ids.len() + self.resources.len() + self.strings.len(),
        );
        let it = self
            .cnames
            .strings
            .iter()
            .chain(&self.tdb_ids.strings)
            .chain(&self.resources.strings)
            .chain(&self.strings.strings)
            .cloned();
        string_data.extend(it);

        WriteableBundle {
            bundle: self,
            string_data,
        }
    }

    pub fn into_owned(self) -> ScriptBundle<'static> {
        ScriptBundle {
            cnames: self.cnames.into_owned(),
            tdb_ids: self.tdb_ids.into_owned(),
            resources: self.resources.into_owned(),
            strings: self.strings.into_owned(),
            definitions: self
                .definitions
                .into_iter()
                .map(Definition::into_owned)
                .collect(),
        }
    }

    #[inline]
    pub fn cnames_mut(&mut self) -> &mut StringPool<'i, index::types::CName> {
        &mut self.cnames
    }

    #[inline]
    pub fn tdb_ids_mut(&mut self) -> &mut StringPool<'i, index::types::TweakDbId> {
        &mut self.tdb_ids
    }

    #[inline]
    pub fn resources_mut(&mut self) -> &mut StringPool<'i, index::types::Resource> {
        &mut self.resources
    }

    #[inline]
    pub fn strings_mut(&mut self) -> &mut StringPool<'i, index::types::String> {
        &mut self.strings
    }

    #[inline]
    pub fn get_item<I>(&self, index: I) -> Option<&I::Output>
    where
        I: PoolItemIndex<'i>,
    {
        index.get(self)
    }

    #[inline]
    pub fn get_item_mut<I>(&mut self, index: I) -> Option<&mut I::Output>
    where
        I: PoolItemIndexMut<'i>,
    {
        index.get_mut(self)
    }

    #[inline]
    pub fn definitions(&self) -> impl ExactSizeIterator<Item = &Definition<'i>> {
        self.definitions.iter().skip(1)
    }

    #[inline]
    pub fn definitions_mut(&mut self) -> impl ExactSizeIterator<Item = &mut Definition<'i>> {
        self.definitions.iter_mut().skip(1)
    }

    pub fn indexed_definitions(&self) -> impl ExactSizeIterator<Item = IndexedDefinition<'_, 'i>> {
        self.definitions
            .iter()
            .enumerate()
            .skip(1)
            .map(|(index, def)| match def {
                Definition::Type(val) => {
                    IndexedDefinition::Type(TypeIndex::new(index as _).unwrap(), val)
                }
                Definition::Class(val) => {
                    IndexedDefinition::Class(ClassIndex::new(index as _).unwrap(), val)
                }
                Definition::EnumMember(val) => {
                    IndexedDefinition::EnumMember(EnumValueIndex::new(index as _).unwrap(), val)
                }
                Definition::Enum(val) => {
                    IndexedDefinition::Enum(EnumIndex::new(index as _).unwrap(), val)
                }
                Definition::Bitfield => IndexedDefinition::Bitfield,
                Definition::Function(val) => {
                    IndexedDefinition::Function(FunctionIndex::new(index as _).unwrap(), val)
                }
                Definition::Parameter(val) => {
                    IndexedDefinition::Parameter(ParameterIndex::new(index as _).unwrap(), val)
                }
                Definition::Local(val) => {
                    IndexedDefinition::Local(LocalIndex::new(index as _).unwrap(), val)
                }
                Definition::Field(val) => {
                    IndexedDefinition::Field(FieldIndex::new(index as _).unwrap(), val)
                }
                Definition::SourceFile(val) => {
                    IndexedDefinition::SourceFile(SourceFileIndex::new(index as _).unwrap(), val)
                }
            })
    }

    #[inline]
    pub fn define<A>(&mut self, def: A) -> NzPoolIndex<A::Index>
    where
        A: DefinitionIndex<'i>,
    {
        self.define_and_init(def, |_, _, def| def)
    }

    #[inline]
    pub fn define_and_init<A>(
        &mut self,
        def: A,
        init: impl FnOnce(&mut Self, NzPoolIndex<A::Index>, A) -> A,
    ) -> NzPoolIndex<A::Index>
    where
        A: DefinitionIndex<'i>,
    {
        enum Never {}

        match self.define_and_try_init(def, |bundle, index, def| {
            Ok::<_, Never>(init(bundle, index, def))
        }) {
            Ok(index) => index,
        }
    }

    pub fn define_and_try_init<A, E>(
        &mut self,
        def: A,
        init: impl FnOnce(&mut Self, NzPoolIndex<A::Index>, A) -> Result<A, E>,
    ) -> Result<NzPoolIndex<A::Index>, E>
    where
        A: DefinitionIndex<'i>,
    {
        let pos = self.definitions.len();
        let index = u32::try_from(pos).expect("index overflow");
        let index = NzPoolIndex::new(index).expect("definition index set to zero");
        self.definitions.push(Definition::UNDEFINED);
        self.definitions[pos] = init(self, index, def)?.into();
        Ok(index)
    }
}

impl Default for ScriptBundle<'_> {
    fn default() -> Self {
        Self {
            cnames: StringPool::new(),
            tdb_ids: StringPool::new(),
            resources: StringPool::new(),
            strings: StringPool::new(),
            definitions: vec![Definition::UNDEFINED],
        }
    }
}

pub trait PoolItemIndex<'i> {
    type Output: ?Sized;

    fn get<'a>(self, bundle: &'a ScriptBundle<'i>) -> Option<&'a Self::Output>;
}

pub trait PoolItemIndexMut<'i>: PoolItemIndex<'i> {
    fn get_mut<'a>(self, bundle: &'a mut ScriptBundle<'i>) -> Option<&'a mut Self::Output>;
}

macro_rules! impl_string_item {
    ($ty:ty, $name:ident) => {
        impl<'i> PoolItemIndex<'i> for $ty {
            type Output = Str<'i>;

            #[inline]
            fn get<'a>(self, bundle: &'a ScriptBundle<'i>) -> Option<&'a Self::Output> {
                bundle.$name.get(self)
            }
        }
    };
}

impl_string_item!(TweakDbIndex, tdb_ids);
impl_string_item!(ResourceIndex, resources);
impl_string_item!(StringIndex, strings);

impl<'i> PoolItemIndex<'i> for CNameIndex {
    type Output = Str<'i>;

    #[inline]
    fn get<'a>(self, bundle: &'a ScriptBundle<'i>) -> Option<&'a Self::Output> {
        match bundle.cnames.get(self)? {
            str if str.is_empty() => Some(&Str::Borrowed("None")),
            str => Some(str),
        }
    }
}

macro_rules! impl_def_item {
    ($idx:ty, $ty:ident[$($lt:lifetime),*]) => {
        impl<'i> PoolItemIndex<'i> for $idx {
            type Output = $ty<$($lt),*>;

            #[inline]
            fn get<'a>(self, bundle: &'a ScriptBundle<'i>) -> Option<&'a Self::Output> {
                if let Some(Definition::$ty(val)) = bundle.definitions.get(u32::from(self) as usize) {
                    Some(val)
                } else {
                    None
                }
            }
        }

        impl<'i> PoolItemIndexMut<'i> for $idx {
            #[inline]
            fn get_mut<'a>(self, bundle: &'a mut ScriptBundle<'i>) -> Option<&'a mut Self::Output> {
                if let Some(Definition::$ty(val)) = bundle.definitions.get_mut(u32::from(self) as usize) {
                    Some(val)
                } else {
                    None
                }
            }
        }
    };
}

impl_def_item!(TypeIndex, Type[]);
impl_def_item!(ClassIndex, Class[]);
impl_def_item!(EnumValueIndex, EnumMember[]);
impl_def_item!(EnumIndex, Enum[]);
impl_def_item!(FunctionIndex, Function['i]);
impl_def_item!(ParameterIndex, Parameter[]);
impl_def_item!(LocalIndex, Local[]);
impl_def_item!(FieldIndex, Field['i]);
impl_def_item!(SourceFileIndex, SourceFile['i]);

impl<'i, I> ops::Index<I> for ScriptBundle<'i>
where
    I: PoolItemIndex<'i> + fmt::Display + Copy,
{
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        match I::get(index, self) {
            Some(val) => val,
            None => panic!(
                "unresolved {} index: {index}",
                std::any::type_name::<I::Output>()
            ),
        }
    }
}

impl<'i, I> ops::IndexMut<I> for ScriptBundle<'i>
where
    I: PoolItemIndexMut<'i> + fmt::Display + Copy,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        match I::get_mut(index, self) {
            Some(val) => val,
            None => panic!(
                "unresolved {} index: {index}",
                std::any::type_name::<I::Output>()
            ),
        }
    }
}

#[derive(Debug, Default)]
pub struct StringPool<'i, A> {
    strings: IndexSet<Str<'i>, foldhash::fast::RandomState>,
    phantom: PhantomData<PoolIndex<A>>,
}

impl<'i, A> StringPool<'i, A> {
    fn new() -> Self {
        StringPool {
            strings: IndexSet::default(),
            phantom: PhantomData,
        }
    }

    #[inline]
    pub fn add(&mut self, string: impl Into<Str<'i>>) -> PoolIndex<A> {
        let (index, _) = self.strings.insert_full(string.into());
        PoolIndex::new(index as _)
    }

    #[inline]
    pub fn get(&self, index: PoolIndex<A>) -> Option<&Str<'i>> {
        self.strings.get_index(u32::from(index) as _)
    }

    #[inline]
    pub fn get_index(&self, str: &str) -> Option<PoolIndex<A>> {
        self.strings
            .get_full(str)
            .map(|(index, _)| PoolIndex::new(index as _))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    pub fn into_owned(self) -> StringPool<'static, A> {
        StringPool {
            strings: self
                .strings
                .into_iter()
                .map(|s| s.into_owned().into())
                .collect(),
            phantom: PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.strings.clear();
    }

    fn write<Ctx>(
        &self,
        offset: &mut usize,
        bytes: &mut [u8],
        index: &StringData<'i>,
        ctx: Ctx,
    ) -> byte::Result<TableHeader>
    where
        Ctx: Endianess,
    {
        let pos = *offset;
        for string in &self.strings {
            let pos = index.dedup.get(string).expect("should contain all strings");
            bytes.write(offset, pos, ctx)?;
        }
        Ok(TableHeader::new(
            pos as _,
            self.strings.len() as _,
            &bytes[pos..*offset],
        ))
    }
}

impl<'i, Index> FromIterator<&'i str> for StringPool<'i, Index> {
    fn from_iter<T: IntoIterator<Item = &'i str>>(iter: T) -> Self {
        let mut pool = StringPool::new();
        pool.strings.extend(iter.into_iter().map(Str::Borrowed));
        pool
    }
}

#[derive(Debug)]
pub struct WriteableBundle<'i> {
    bundle: ScriptBundle<'i>,
    string_data: StringData<'i>,
}

impl WriteableBundle<'_> {
    #[cfg(feature = "mmap")]
    pub fn save(&self, path: impl AsRef<std::path::Path>) -> Result<(), SaveError> {
        let (mut out, _) = vmap::MapMut::with_options()
            .create(true)
            .truncate(true)
            .resize(self.measure(()))
            .open(path)
            .map_err(SaveError::Mmap)?;
        self.try_write(&mut out, ENDIANESS)
            .map_err(SaveError::Encoding)?;
        Ok(())
    }

    pub fn to_bytes(&self) -> byte::Result<Vec<u8>> {
        let mut bytes = vec![0; self.measure(())];
        self.try_write(&mut bytes, ENDIANESS)?;
        Ok(bytes)
    }
}

impl<Ctx: Endianess> TryWrite<Ctx> for WriteableBundle<'_> {
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> byte::Result<usize> {
        let offset = &mut 0;
        // skip the header
        *offset += Header::SIZE as usize;

        let string_data_start = *offset;
        for str in self.string_data.dedup.keys() {
            bytes.write(offset, str.as_ref(), Delimiter(0))?;
        }

        let string_data = TableHeader::new(
            Header::SIZE as _,
            self.string_data.byte_size as _,
            &bytes[string_data_start..*offset],
        );

        let cnames = self
            .bundle
            .cnames
            .write(offset, bytes, &self.string_data, ctx)?;
        let tweakdb_ids = self
            .bundle
            .tdb_ids
            .write(offset, bytes, &self.string_data, ctx)?;
        let resources = self
            .bundle
            .resources
            .write(offset, bytes, &self.string_data, ctx)?;

        let headers_start = *offset;
        // skip definition headers
        *offset += self.bundle.definitions.len() * Definition::HEADER_SIZE as usize;

        let strings = self
            .bundle
            .strings
            .write(offset, bytes, &self.string_data, ctx)?;

        let mut headers_offset = headers_start;
        bytes.write(&mut headers_offset, &DefinitionHeader::default(), ctx)?;
        for def in self.bundle.definitions() {
            let pos = *offset;
            bytes.write(offset, def, ctx)?;
            let size = *offset - pos;

            let header = DefinitionHeader::from_defintion(def, size as _, pos as _);
            bytes.write(&mut headers_offset, &header, ctx)?;
        }

        let definitions = TableHeader::new(
            headers_start as _,
            self.bundle.definitions.len() as _,
            &bytes[headers_start..headers_offset],
        );

        let header_for_hash = Header {
            magic: Header::MAGIC,
            version: Header::SUPPORTED_VERSION,
            flags: 0,
            timestamp: Timestamp::new(),
            build: 0,
            crc: 0xDEAD_BEEF,
            segments: 7,
            string_data,
            cnames,
            tweakdb_ids,
            resources,
            definitions,
            strings,
        };
        let header = Header {
            crc: crc32fast::hash(&header_for_hash.to_bytes(ctx)?),
            ..header_for_hash
        };
        bytes.write_at(0, &header, ctx)?;

        Ok(*offset)
    }
}

impl<Ctx: Copy> Measure<Ctx> for WriteableBundle<'_> {
    fn measure(&self, ctx: Ctx) -> usize {
        Header::SIZE as usize
            + self.string_data.byte_size
            + self.bundle.cnames.len() * mem::size_of::<u32>()
            + self.bundle.tdb_ids.len() * mem::size_of::<u32>()
            + self.bundle.resources.len() * mem::size_of::<u32>()
            + self.bundle.definitions.len() * Definition::HEADER_SIZE as usize
            + self.bundle.strings.len() * mem::size_of::<u32>()
            + self
                .bundle
                .definitions
                .iter()
                .map(|def| def.measure(ctx))
                .sum::<usize>()
    }
}

#[derive(Debug, Default)]
struct StringData<'i> {
    dedup: IndexMap<Str<'i>, u32, foldhash::fast::RandomState>,
    byte_size: usize,
}

impl StringData<'_> {
    fn with_capacity(capacity: usize) -> Self {
        StringData {
            dedup: IndexMap::with_capacity_and_hasher(
                capacity,
                foldhash::fast::RandomState::default(),
            ),
            byte_size: 0,
        }
    }
}

impl<'i> Extend<Str<'i>> for StringData<'i> {
    fn extend<T: IntoIterator<Item = Str<'i>>>(&mut self, iter: T) {
        for string in iter {
            if let indexmap::map::raw_entry_v1::RawEntryMut::Vacant(entry) =
                self.dedup.raw_entry_mut_v1().from_key(&string)
            {
                let len = string.len();
                entry.insert(string, self.byte_size as _);
                self.byte_size += len + 1;
            }
        }
    }
}

#[bitfield(u64)]
struct Timestamp {
    #[bits(10)]
    __: u16,
    #[bits(5)]
    day: u8,
    #[bits(5)]
    month: u8,
    #[bits(12)]
    year: u16,
    #[bits(10)]
    millis: u16,
    #[bits(6)]
    seconds: u8,
    #[bits(6)]
    minutes: u8,
    #[bits(6)]
    hours: u8,
    #[bits(4)]
    __: u8,
}

util::impl_bitfield_read_write!(Timestamp);

#[derive(Debug)]
pub struct ItemReader<'r, 'i, Item> {
    parent: &'r BundleReader<'i>,
    offset: u32,
    count: u32,
    phantom: PhantomData<Item>,
}

impl<'r, 'i, Item> ItemReader<'r, 'i, Item> {
    fn new(reader: &'r BundleReader<'i>, table: &TableHeader) -> Self {
        ItemReader {
            parent: reader,
            offset: table.offset,
            count: table.count,
            phantom: PhantomData,
        }
    }

    pub fn get(&self, index: impl Into<u32>) -> byte::Result<Item>
    where
        Item: BundleItem<'i>,
    {
        let header_pos = self.offset + index.into() * Item::HEADER_SIZE;
        let header: Item::Header = self.parent.contents.read_at(header_pos as _, ENDIANESS)?;
        let pos = Item::pos(&self.parent.header, &header);
        self.parent.contents.read_at(pos as _, Item::ctx(&header))
    }
}

impl<'r, 'i, Item> IntoIterator for ItemReader<'r, 'i, Item>
where
    Item: BundleItem<'i>,
{
    type IntoIter = ItemIter<'r, 'i, Item>;
    type Item = byte::Result<Item>;

    fn into_iter(self) -> Self::IntoIter {
        ItemIter {
            reader: self,
            index: 0,
        }
    }
}

#[derive(Debug)]
pub struct ItemIter<'r, 'i, Item> {
    reader: ItemReader<'r, 'i, Item>,
    index: u32,
}

impl<'i, Item> Iterator for ItemIter<'_, 'i, Item>
where
    Item: BundleItem<'i>,
{
    type Item = byte::Result<Item>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.reader.count {
            let result = self.reader.get(self.index);
            self.index += 1;
            Some(result)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.reader.count - self.index) as usize;
        (remaining, Some(remaining))
    }
}

impl<'i, Item> iter::ExactSizeIterator for ItemIter<'_, 'i, Item> where Item: BundleItem<'i> {}

impl<'i, Item> iter::FusedIterator for ItemIter<'_, 'i, Item> where Item: BundleItem<'i> {}

pub trait BundleItem<'i>: TryRead<'i, Self::Ctx> {
    type Ctx: Copy;
    type Header: TryRead<'i, LittleEndian>;

    const HEADER_SIZE: u32;

    fn pos(parent: &Header, header: &Self::Header) -> u32;
    fn ctx(header: &Self::Header) -> Self::Ctx;
}

impl<'i> BundleItem<'i> for &'i str {
    type Ctx = Delimiter;
    type Header = u32;

    const HEADER_SIZE: u32 = 4;

    #[inline]
    fn pos(parent: &Header, header: &Self::Header) -> u32 {
        parent.string_data.offset + *header
    }

    #[inline]
    fn ctx(_header: &Self::Header) -> Self::Ctx {
        Delimiter(0)
    }
}

impl<'i> BundleItem<'i> for Definition<'i> {
    type Ctx = (LittleEndian, DefinitionHeader);
    type Header = DefinitionHeader;

    const HEADER_SIZE: u32 = 20;

    #[inline]
    fn pos(_parent: &Header, header: &Self::Header) -> u32 {
        header.offset()
    }

    #[inline]
    fn ctx(header: &Self::Header) -> Self::Ctx {
        (byte::LE, *header)
    }
}

#[cfg(feature = "mmap")]
#[derive(Debug)]
pub enum SaveError {
    Mmap(vmap::Error),
    Encoding(byte::Error),
}

#[cfg(feature = "mmap")]
impl fmt::Display for SaveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SaveError::Mmap(err) => write!(f, "mmap error: {err}"),
            SaveError::Encoding(err) => write!(f, "encoding error: {err}"),
        }
    }
}

#[cfg(feature = "mmap")]
impl std::error::Error for SaveError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SaveError::Mmap(err) => Some(err),
            SaveError::Encoding(err) => Some(err),
        }
    }
}

use std::hash::Hash;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::{fmt, mem};

use byte::ctx::Endianess;
use byte::{Measure, TryRead, TryWrite};

pub type CNameIndex = PoolIndex<types::CName>;
pub type TweakDbIndex = PoolIndex<types::TweakDbId>;
pub type ResourceIndex = PoolIndex<types::Resource>;
pub type StringIndex = PoolIndex<types::String>;

pub type TypeIndex = NzPoolIndex<types::Type>;
pub type ClassIndex = NzPoolIndex<types::Class>;
pub type EnumValueIndex = NzPoolIndex<types::EnumValue>;
pub type EnumIndex = NzPoolIndex<types::Enum>;
pub type FunctionIndex = NzPoolIndex<types::Function>;
pub type ParameterIndex = NzPoolIndex<types::Parameter>;
pub type LocalIndex = NzPoolIndex<types::Local>;
pub type FieldIndex = NzPoolIndex<types::Field>;
pub type SourceFileIndex = NzPoolIndex<types::SourceFile>;

#[repr(transparent)]
pub struct PoolIndex<A>(u32, PhantomData<A>);

impl<A> PoolIndex<A> {
    pub const UNDEFINED: Self = PoolIndex(0, PhantomData);

    #[inline]
    pub(crate) const fn new(index: u32) -> Self {
        PoolIndex(index, PhantomData)
    }
}

impl<A> fmt::Debug for PoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("PoolIndex").field(&self.0).finish()
    }
}

impl<A> Clone for PoolIndex<A> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> Copy for PoolIndex<A> {}

impl<A> PartialEq for PoolIndex<A> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<A> Eq for PoolIndex<A> {}

impl<A> PartialOrd for PoolIndex<A> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A> Ord for PoolIndex<A> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<A> Hash for PoolIndex<A> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[cfg(feature = "identity-hash")]
impl<A> identity_hash::IdentityHashable for PoolIndex<A> {}

impl<A> fmt::Display for PoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<A> From<PoolIndex<A>> for u32 {
    #[inline]
    fn from(index: PoolIndex<A>) -> u32 {
        index.0
    }
}

impl<A, Ctx: Endianess> TryRead<'_, Ctx> for PoolIndex<A> {
    #[inline]
    fn try_read(bytes: &[u8], ctx: Ctx) -> byte::Result<(Self, usize)> {
        let (index, size) = TryRead::try_read(bytes, ctx)?;
        Ok((PoolIndex::new(index), size))
    }
}

impl<A, Ctx: Endianess> TryWrite<Ctx> for PoolIndex<A> {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> byte::Result<usize> {
        self.0.try_write(bytes, ctx)
    }
}

impl<A, Ctx> Measure<Ctx> for PoolIndex<A> {
    #[inline]
    fn measure(&self, ctx: Ctx) -> usize {
        self.0.measure(ctx)
    }
}

impl Default for CNameIndex {
    #[inline]
    fn default() -> Self {
        Self(Default::default(), PhantomData)
    }
}

#[repr(transparent)]
pub struct NzPoolIndex<A>(NonZeroU32, PhantomData<A>);

impl<A> NzPoolIndex<A> {
    pub(crate) const ONE: Self = Self(NonZeroU32::MIN, PhantomData);

    #[inline]
    pub(crate) const fn new(index: u32) -> Option<Self> {
        match NonZeroU32::new(index) {
            Some(index) => Some(NzPoolIndex(index, PhantomData)),
            None => None,
        }
    }
}

impl<A> fmt::Debug for NzPoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("DefIndex").field(&self.0).finish()
    }
}

impl<A> Clone for NzPoolIndex<A> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> Copy for NzPoolIndex<A> {}

impl<A> PartialEq for NzPoolIndex<A> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<A> Eq for NzPoolIndex<A> {}

impl<A> PartialOrd for NzPoolIndex<A> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<A> Ord for NzPoolIndex<A> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<A> Hash for NzPoolIndex<A> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.get().hash(state);
    }
}

#[cfg(feature = "identity-hash")]
impl<A> identity_hash::IdentityHashable for NzPoolIndex<A> {}

impl<A> Default for NzPoolIndex<A> {
    #[inline]
    fn default() -> Self {
        Self(NonZeroU32::MIN, PhantomData)
    }
}

impl<A> fmt::Display for NzPoolIndex<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<A> From<NzPoolIndex<A>> for u32 {
    #[inline]
    fn from(index: NzPoolIndex<A>) -> u32 {
        index.0.get()
    }
}

impl<A, Ctx: Endianess> TryRead<'_, Ctx> for NzPoolIndex<A> {
    #[inline]
    fn try_read(bytes: &[u8], ctx: Ctx) -> byte::Result<(Self, usize)> {
        let (index, size) = TryRead::try_read(bytes, ctx)?;
        let result = NzPoolIndex::new(index).ok_or(byte::Error::BadInput {
            err: "definition index set to zero",
        })?;
        Ok((result, size))
    }
}

impl<A, Ctx: Endianess> TryWrite<Ctx> for NzPoolIndex<A> {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> byte::Result<usize> {
        self.0.get().try_write(bytes, ctx)
    }
}

impl<A, Ctx> Measure<Ctx> for NzPoolIndex<A> {
    #[inline]
    fn measure(&self, _: Ctx) -> usize {
        mem::size_of::<u32>()
    }
}

pub mod types {
    #[derive(Debug, Default)]
    pub struct CName;
    #[derive(Debug, Default)]
    pub struct TweakDbId;
    #[derive(Debug, Default)]
    pub struct Resource;
    #[derive(Debug, Default)]
    pub struct String;

    #[derive(Debug, Default)]
    pub struct Type;
    #[derive(Debug, Default)]
    pub struct Class;
    #[derive(Debug, Default)]
    pub struct EnumValue;
    #[derive(Debug, Default)]
    pub struct Enum;
    #[derive(Debug, Default)]
    pub struct Function;
    #[derive(Debug, Default)]
    pub struct Parameter;
    #[derive(Debug, Default)]
    pub struct Local;
    #[derive(Debug, Default)]
    pub struct Field;
    #[derive(Debug, Default)]
    pub struct SourceFile;
}

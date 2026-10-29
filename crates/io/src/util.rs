use std::mem;

use byte::ctx::{Endianess, Len};
use byte::{BytesExt, Measure, TryRead, TryWrite};

use crate::Str;
use crate::index::NzPoolIndex;

pub struct Prefixed<Ctx>(pub Ctx);

impl<'a, A, Ctx> TryRead<'a, Prefixed<Ctx>> for Vec<A>
where
    A: TryRead<'a, Ctx>,
    Ctx: Endianess,
{
    fn try_read(bytes: &'a [u8], Prefixed(ctx): Prefixed<Ctx>) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        let len: u32 = bytes.read(offset, ctx)?;
        let result = bytes
            .read_iter(offset, ctx)
            .take(len as _)
            .collect::<Result<Vec<_>, _>>()?;
        Ok((result, *offset))
    }
}

impl<A, Ctx> TryWrite<Prefixed<Ctx>> for Vec<A>
where
    A: TryWrite<Ctx>,
    Ctx: Endianess,
{
    fn try_write(&self, bytes: &mut [u8], Prefixed(ctx): Prefixed<Ctx>) -> byte::Result<usize> {
        let offset = &mut 0;
        bytes.write(offset, &(self.len() as u32), ctx)?;
        for item in self {
            bytes.write(offset, item, ctx)?;
        }
        Ok(*offset)
    }
}

impl<A, Ctx> Measure<Prefixed<Ctx>> for Vec<A>
where
    A: Measure<Ctx>,
    Ctx: Copy,
{
    fn measure(&self, Prefixed(ctx): Prefixed<Ctx>) -> usize {
        let len = mem::size_of::<u32>();
        let items: usize = self.iter().map(|item| item.measure(ctx)).sum();
        len + items
    }
}

impl<'a, Ctx> TryRead<'a, Prefixed<Ctx>> for Str<'a>
where
    Ctx: Endianess,
{
    fn try_read(bytes: &'a [u8], Prefixed(ctx): Prefixed<Ctx>) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        let len: u16 = bytes.read(offset, ctx)?;
        let result: &'a str = bytes.read(offset, Len(len.into()))?;
        Ok((Str::Borrowed(result), *offset))
    }
}

impl<Ctx> TryWrite<Prefixed<Ctx>> for Str<'_>
where
    Ctx: Endianess,
{
    fn try_write(&self, bytes: &mut [u8], Prefixed(ctx): Prefixed<Ctx>) -> byte::Result<usize> {
        let offset = &mut 0;
        let len = u16::try_from(self.len()).map_err(|_| byte::Error::BadInput {
            err: "prefixed string exceeds u16 byte length",
        })?;
        bytes.write(offset, &len, ctx)?;
        bytes.write(offset, self.as_ref(), Len(self.len()))?;
        Ok(*offset)
    }
}

impl<Ctx> Measure<Prefixed<Ctx>> for Str<'_> {
    #[inline]
    fn measure(&self, _: Prefixed<Ctx>) -> usize {
        mem::size_of::<u16>() + self.len()
    }
}

pub struct FlagDependent<Ctx>(pub Ctx);

impl<'a, A, Ctx> TryRead<'a, FlagDependent<Ctx>> for Option<A>
where
    A: TryRead<'a, Ctx>,
{
    #[inline]
    fn try_read(
        bytes: &'a [u8],
        FlagDependent(ctx): FlagDependent<Ctx>,
    ) -> byte::Result<(Self, usize)> {
        let (value, size) = A::try_read(bytes, ctx)?;
        Ok((Some(value), size))
    }
}

impl<A, Ctx> TryWrite<FlagDependent<Ctx>> for Option<A>
where
    A: TryWrite<Ctx>,
{
    #[inline]
    fn try_write(
        &self,
        bytes: &mut [u8],
        FlagDependent(ctx): FlagDependent<Ctx>,
    ) -> byte::Result<usize> {
        match self {
            Some(value) => value.try_write(bytes, ctx),
            None => Ok(0),
        }
    }
}

impl<A, Ctx> Measure<FlagDependent<Ctx>> for Option<A>
where
    A: Measure<Ctx>,
{
    #[inline]
    fn measure(&self, FlagDependent(ctx): FlagDependent<Ctx>) -> usize {
        match self {
            Some(value) => value.measure(ctx),
            None => 0,
        }
    }
}

pub struct OptionalIndex<Ctx>(pub Ctx);

impl<A, Ctx: Endianess> TryRead<'_, OptionalIndex<Ctx>> for Option<NzPoolIndex<A>> {
    #[inline]
    fn try_read(
        bytes: &[u8],
        OptionalIndex(ctx): OptionalIndex<Ctx>,
    ) -> byte::Result<(Self, usize)> {
        let (index, size) = TryRead::try_read(bytes, ctx)?;
        Ok((NzPoolIndex::new(index), size))
    }
}

impl<A, Ctx: Endianess> TryWrite<OptionalIndex<Ctx>> for Option<NzPoolIndex<A>> {
    #[inline]
    fn try_write(
        &self,
        bytes: &mut [u8],
        OptionalIndex(ctx): OptionalIndex<Ctx>,
    ) -> byte::Result<usize> {
        match self {
            &Some(i) => u32::from(i).try_write(bytes, ctx),
            None => 0u32.try_write(bytes, ctx),
        }
    }
}

impl<A, Ctx> Measure<OptionalIndex<Ctx>> for Option<NzPoolIndex<A>> {
    #[inline]
    fn measure(&self, _: OptionalIndex<Ctx>) -> usize {
        mem::size_of::<u32>()
    }
}

macro_rules! impl_bitfield_read_write {
    ($ty:ty) => {
        impl<Ctx: Endianess> TryRead<'_, Ctx> for $ty {
            #[inline]
            fn try_read(bytes: &[u8], ctx: Ctx) -> byte::Result<($ty, usize)> {
                let (value, size) = TryRead::try_read(bytes, ctx)?;
                Ok((<$ty>::from_bits(value), size))
            }
        }

        impl<Ctx: Endianess> TryWrite<Ctx> for $ty {
            #[inline]
            fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> byte::Result<usize> {
                self.into_bits().try_write(bytes, ctx)
            }
        }

        impl<Ctx> Measure<Ctx> for $ty {
            #[inline]
            fn measure(&self, _ctx: Ctx) -> usize {
                ::core::mem::size_of::<$ty>()
            }
        }
    };
}

pub(crate) use impl_bitfield_read_write;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefixed_string_length_boundaries() {
        for string in ["a".repeat(u16::MAX as usize), "é".repeat(32767)] {
            let string = Str::Owned(string);
            let mut bytes = vec![0; string.measure(Prefixed(byte::LE))];
            let size = string.try_write(&mut bytes, Prefixed(byte::LE)).unwrap();
            assert_eq!(size, bytes.len());
            let (decoded, consumed) = Str::try_read(&bytes, Prefixed(byte::LE)).unwrap();
            assert_eq!(decoded, string);
            assert_eq!(consumed, size);
        }
    }

    #[test]
    fn reject_oversized_prefixed_strings_before_writing() {
        for string in ["a".repeat(65536), "é".repeat(32768)] {
            let string = Str::Owned(string);
            let mut bytes = vec![0xAB; string.measure(Prefixed(byte::LE))];
            assert!(string.try_write(&mut bytes, Prefixed(byte::LE)).is_err());
            assert!(bytes.iter().all(|&byte| byte == 0xAB));
        }
    }
}

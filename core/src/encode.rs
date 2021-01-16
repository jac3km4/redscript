use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::io;

use byteorder::{LittleEndian, WriteBytesExt};

pub trait Encode {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()>;
}

impl Encode for i64 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_i64::<LittleEndian>(*value)
    }
}

impl Encode for i32 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_i32::<LittleEndian>(*value)
    }
}

impl Encode for i16 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_i16::<LittleEndian>(*value)
    }
}

impl Encode for i8 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_i8(*value)
    }
}

impl Encode for u64 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_u64::<LittleEndian>(*value)
    }
}

impl Encode for u32 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_u32::<LittleEndian>(*value)
    }
}

impl Encode for u16 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_u16::<LittleEndian>(*value)
    }
}

impl Encode for u8 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_u8(*value)
    }
}

impl Encode for bool {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_u8(if *value { 1 } else { 0 })
    }
}

impl Encode for f64 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_f64::<LittleEndian>(*value)
    }
}

impl Encode for f32 {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_f32::<LittleEndian>(*value)
    }
}

impl Encode for &str {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(value.as_bytes())?;
        output.write_u8(0)
    }
}

impl<const N: usize> Encode for [u8; N] {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.write_all(value)
    }
}

pub trait EncodeExt: io::Write + Sized {
    fn encode<A: Encode>(&mut self, value: &A) -> io::Result<()> {
        Encode::encode(self, value)
    }

    fn encode_slice<A: Encode>(&mut self, value: &[A]) -> io::Result<()> {
        for elem in value {
            self.encode(elem)?
        }
        Ok(())
    }

    fn encode_slice_prefixed<S: Encode + TryFrom<usize>, A: Encode>(&mut self, value: &[A]) -> io::Result<()>
    where
        S::Error: Debug,
    {
        self.encode::<S>(&value.len().try_into().expect("Size overflow"))?;
        self.encode_slice(value)
    }

    fn encode_str(&mut self, value: &str) -> io::Result<()> {
        self.write_all(value.as_bytes())
    }

    fn encode_str_prefixed<S: Encode + TryFrom<usize>>(&mut self, value: &str) -> io::Result<()>
    where
        S::Error: Debug,
    {
        self.encode_slice_prefixed::<S, u8>(value.as_bytes())
    }
}

impl<O: io::Write> EncodeExt for O {}

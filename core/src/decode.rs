use std::io;

use byteorder::{LittleEndian, ReadBytesExt};

pub trait Decode: Sized {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self>;
}

impl Decode for i64 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_i64::<LittleEndian>()
    }
}

impl Decode for i32 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_i32::<LittleEndian>()
    }
}

impl Decode for i16 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_i16::<LittleEndian>()
    }
}

impl Decode for i8 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_i8()
    }
}

impl Decode for u64 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_u64::<LittleEndian>()
    }
}

impl Decode for u32 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_u32::<LittleEndian>()
    }
}

impl Decode for u16 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_u16::<LittleEndian>()
    }
}

impl Decode for u8 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_u8()
    }
}

impl Decode for bool {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(input.read_u8()? != 0)
    }
}

impl Decode for f64 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_f64::<LittleEndian>()
    }
}

impl Decode for f32 {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        input.read_f32::<LittleEndian>()
    }
}

impl Decode for String {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let mut str = String::new();
        let mut c: u8;
        loop {
            c = input.read_u8()?;
            if c == 0 {
                break;
            }
            str.push(c.into());
        }
        Ok(str)
    }
}

impl<const N: usize> Decode for [u8; N] {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let mut buf: [u8; N] = [0; N];
        input.read_exact(&mut buf)?;
        Ok(buf)
    }
}

pub trait DecodeExt: io::Read + Sized {
    fn decode<A: Decode>(&mut self) -> io::Result<A> {
        Decode::decode(self)
    }

    fn decode_vec<S: Into<u32>, A: Decode>(&mut self, count: S) -> io::Result<Vec<A>> {
        let size = count.into() as usize;
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            vec.push(self.decode()?);
        }
        Ok(vec)
    }

    fn decode_vec_prefixed<S: Decode + Into<u32>, A: Decode>(&mut self) -> io::Result<Vec<A>> {
        let size: S = self.decode()?;
        self.decode_vec(size)
    }

    fn decode_bytes<S: Into<u32>>(&mut self, count: S) -> io::Result<Vec<u8>> {
        let size = count.into() as usize;
        let mut vec = Vec::with_capacity(size);
        unsafe { vec.set_len(size) }
        self.read_exact(&mut vec)?;
        Ok(vec)
    }

    fn decode_str<S: Into<u32>>(&mut self, count: S) -> io::Result<String> {
        String::from_utf8(self.decode_bytes(count)?).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
    }

    fn decode_str_prefixed<S: Decode + Into<u32>>(&mut self) -> io::Result<String> {
        let size: S = self.decode()?;
        self.decode_str(size)
    }
}

impl<I: io::Read> DecodeExt for I {}

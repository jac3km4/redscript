use std::fs::File;
use std::io;
use std::time::SystemTime;

use byteorder::*;

#[derive(Debug, PartialEq, Eq)]
pub struct CompileTimestamp {
    nanos: u128,
}

impl CompileTimestamp {
    pub fn read<R: io::Read + io::Seek>(input: &mut R) -> io::Result<Self> {
        input.seek(io::SeekFrom::Start(0))?;
        let nanos = input.read_u128::<LittleEndian>()?;
        Ok(Self { nanos })
    }

    pub fn write<W: io::Write + io::Seek>(&self, output: &mut W) -> io::Result<()> {
        output.seek(io::SeekFrom::Start(0))?;
        output.write_u128::<LittleEndian>(self.nanos)?;
        Ok(())
    }

    pub fn of_cache_file(file: &File) -> io::Result<Self> {
        let nanos = file
            .metadata()?
            .modified()?
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        Ok(Self { nanos })
    }
}

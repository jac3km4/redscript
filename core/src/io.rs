use std::io;

/// Wrapper for [`io::Read`] and [`io::Write`] types with the capability
/// of keeping the offset in the stream.
///
/// The main use case for this is to avoid expensive seek calls.
#[derive(Debug)]
pub struct StreamOffset<S> {
    inner: S,
    offset: usize,
}

impl<S> StreamOffset<S> {
    pub fn new_zeroed(inner: S) -> Self {
        Self { inner, offset: 0 }
    }

    #[inline]
    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn into_inner(self) -> S {
        self.inner
    }
}

impl<S: io::Seek> StreamOffset<S> {
    pub fn new_seekable(mut inner: S) -> io::Result<Self> {
        let offset = inner.stream_position()? as usize;
        Ok(Self { inner, offset })
    }
}

impl<R: io::Read> io::Read for StreamOffset<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let read = self.inner.read(buf)?;
        self.offset += read;
        Ok(read)
    }
}

impl<W: io::Write> io::Write for StreamOffset<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let written = self.inner.write(buf)?;
        self.offset += written;
        Ok(written)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

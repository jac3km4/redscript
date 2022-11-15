use std::any::TypeId;
use std::ffi::c_void;
use std::ops::Range;

use hashbrown::HashMap;

pub trait Shatter: Sized {
    // splits a cut into two adjacent cuts
    fn split_at(self, index: usize) -> (Self, Self);

    // splits a cut into three adjacent cuts, the middle one being of the given range
    fn split_range(self, range: Range<usize>) -> (Self, Self, Self) {
        let (left, right) = self.split_at(range.start);
        let (middle, right) = right.split_at(range.end - range.start);
        (left, middle, right)
    }

    // fuzes two adjacent cuts into one
    fn fuze(self, other: Self) -> Option<Self>;
}

impl Shatter for Range<usize> {
    fn split_at(self, index: usize) -> (Self, Self) {
        let start = self.start;
        let end = self.end;
        (start..index, index..end)
    }

    fn fuze(self, other: Self) -> Option<Self> {
        if self.end == other.start {
            Some(self.start..other.end)
        } else {
            None
        }
    }
}
#[derive(Debug)]
pub struct Cut<'a, T>(Range<usize>, &'a Slab<T>);

impl<'a, T> Cut<'a, T> {
    pub fn len(&self) -> usize {
        self.0.end - self.0.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_slice(&self) -> &'a [T] {
        unsafe { self.1.slice_unchecked(self.0.clone()) }
    }

    pub fn as_slice_mut(&mut self) -> &'a mut [T] {
        unsafe { self.1.slice_unchecked_mut(self.0.clone()) }
    }

    pub fn slab(&self) -> &'a Slab<T> {
        self.1
    }

    pub fn slab_mut(&mut self) -> &'a mut Slab<T> {
        unsafe { self.1.as_mut() }
    }
}

impl<'a, T> AsRef<Range<usize>> for Cut<'a, T> {
    fn as_ref(&self) -> &Range<usize> {
        &self.0
    }
}

impl<'a, T> From<Cut<'a, T>> for Range<usize> {
    fn from(cut: Cut<'a, T>) -> Self {
        cut.0
    }
}

impl<'a, T> From<Cut<'a, T>> for &'a [T] {
    fn from(value: Cut<'a, T>) -> Self {
        value.as_slice()
    }
}

impl<'a, T> Shatter for Cut<'a, T> {
    fn split_at(self, at: usize) -> (Self, Self) {
        let (left, right) = self.0.split_at(at);
        (Cut(left, self.1), Cut(right, self.1))
    }

    fn fuze(self, other: Self) -> Option<Self> {
        if self.0.end == other.0.start {
            Some(Cut(self.0.start..other.0.end, self.1))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Cut1<'a, T>(usize, &'a Slab<T>);

impl<'a, T> Cut1<'a, T> {
    pub fn as_t(&self) -> &'a T {
        unsafe { self.1.get_unchecked(self.0) }
    }

    pub fn as_t_mut(&mut self) -> &'a mut T {
        unsafe { self.1.get_unchecked_mut(self.0) }
    }

    pub fn slab(&self) -> &'a Slab<T> {
        self.1
    }

    pub fn slab_mut(&mut self) -> &'a mut Slab<T> {
        unsafe { self.1.as_mut() }
    }
}

impl<'a, T> From<Cut1<'a, T>> for Cut<'a, T> {
    fn from(value: Cut1<'a, T>) -> Self {
        Cut(value.0..value.0 + 1, value.1)
    }
}

#[derive(Debug)]
pub struct Slab<T> {
    data: Vec<T>,
    start: usize,
}

impl<T> Default for Slab<T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            start: Default::default(),
        }
    }
}

#[allow(clippy::mut_from_ref, clippy::cast_ref_to_mut)]
impl<T> Slab<T> {
    #[inline]
    unsafe fn slice_unchecked(&self, range: Range<usize>) -> &[T] {
        std::slice::from_raw_parts(self.data.as_ptr().add(range.start), range.end - range.start)
    }
    #[inline]
    unsafe fn slice_unchecked_mut(&self, range: Range<usize>) -> &mut [T] {
        std::slice::from_raw_parts_mut(self.data.as_ptr().cast_mut().add(range.start), range.end - range.start)
    }

    #[inline]
    unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.data.get_unchecked(index)
    }
    #[inline]
    unsafe fn get_unchecked_mut(&self, index: usize) -> &mut T {
        self.data.as_ptr().cast_mut().add(index).as_mut().unwrap()
    }

    #[inline]
    unsafe fn as_mut(&self) -> &mut Self {
        &mut *(self as *const _ as *mut _)
    }

    pub fn cut(&mut self, len: usize) -> Result<Cut<T>, Cut<T>> {
        let start = self.start;
        let end = start + len;
        let available = self.data.len() - start;
        if available >= len {
            self.start = end;
            Ok(Cut(start..end, self))
        } else {
            Err(Cut(start..self.data.len(), self))
        }
    }

    pub fn cut1(&mut self) -> Option<Cut1<T>> {
        let start = self.start;
        let end = start + 1;
        let available = self.data.len() - start;
        if available >= 1 {
            self.start = end;
            Some(Cut1(start, self))
        } else {
            None
        }
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.data.len() - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn reserve(&mut self, additional: usize) {
        if let Some(additional) = additional.checked_sub(self.len()) {
            self.data.reserve(additional);
        }
    }
}

trait IntoSlab {
    fn type_id(&self) -> TypeId;

    fn slab_ptr(&self) -> *const c_void {
        self as *const _ as *const c_void
    }
}

impl<U: 'static> IntoSlab for Slab<U> {
    fn type_id(&self) -> TypeId {
        TypeId::of::<U>()
    }
}

pub struct Hunk {
    slabs: HashMap<TypeId, Box<dyn IntoSlab>>,
}

impl Hunk {
    fn get_slab<T: 'static>(&mut self) -> &mut Slab<T> {
        let type_id = TypeId::of::<T>();
        let slab = self
            .slabs
            .entry(type_id)
            .or_insert_with(|| Box::new(Slab::<T>::default()));
        unsafe { &mut *(slab.slab_ptr() as *mut Slab<T>) }
    }
}

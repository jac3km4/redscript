use std::borrow::Borrow;
use std::hash::Hash;

use crate::IndexMap;

#[derive(Debug, Clone)]
pub enum ScopedMap<'a, K, V> {
    Cons(IndexMap<K, V>, &'a Self),
    Tail(IndexMap<K, V>),
}

impl<'a, K, V> ScopedMap<'a, K, V>
where
    K: Eq + Hash,
{
    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        Some(self.get_with_depth(key)?.0)
    }

    pub fn get_with_depth<Q>(&self, key: &Q) -> Option<(&V, u16)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.scope_iter()
            .zip(0u16..)
            .find_map(|(map, depth)| Some((map.get(key)?, depth)))
    }

    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.top_mut().insert(k, v)
    }

    #[inline]
    pub fn introduce_scope(&'a self) -> Self {
        self.push_scope(IndexMap::default())
    }

    #[inline]
    pub fn push_scope(&'a self, map: IndexMap<K, V>) -> Self {
        Self::Cons(map, self)
    }

    #[inline]
    pub fn pop_scope(self) -> IndexMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    #[inline]
    pub fn top(&self) -> &IndexMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    #[inline]
    pub fn top_mut(&mut self) -> &mut IndexMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    pub fn len(&self) -> usize {
        self.scope_iter().map(IndexMap::len).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.scope_iter().flat_map(IndexMap::iter)
    }

    #[inline]
    pub fn scope_iter(&self) -> ScopeIter<'_, 'a, K, V> {
        ScopeIter { map: Some(self) }
    }

    pub fn is_top_level(&self) -> bool {
        matches!(self, ScopedMap::Tail(_))
    }
}

impl<K, V> Default for ScopedMap<'_, K, V> {
    fn default() -> Self {
        Self::Tail(IndexMap::default())
    }
}

impl<K: Eq + Hash, V> Extend<(K, V)> for ScopedMap<'_, K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        self.top_mut().extend(iter);
    }
}

impl<K: Eq + Hash, V> FromIterator<(K, V)> for ScopedMap<'_, K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self::Tail(iter.into_iter().collect())
    }
}

impl<K, V> From<IndexMap<K, V>> for ScopedMap<'_, K, V> {
    fn from(map: IndexMap<K, V>) -> Self {
        Self::Tail(map)
    }
}

#[derive(Debug, Clone)]
pub struct ScopeIter<'a, 'b, K, V> {
    map: Option<&'a ScopedMap<'b, K, V>>,
}

impl<'a, K, V> Iterator for ScopeIter<'a, '_, K, V> {
    type Item = &'a IndexMap<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.map? {
            ScopedMap::Cons(head, tail) => {
                self.map = Some(tail);
                Some(head)
            }
            ScopedMap::Tail(map) => {
                self.map = None;
                Some(map)
            }
        }
    }
}

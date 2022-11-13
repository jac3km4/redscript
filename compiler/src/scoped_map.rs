use std::borrow::Borrow;
use std::hash::Hash;

use hashbrown::HashMap;

/// Abstraction for an allocation-less stack-based list of scopes.
/// New elements are added at the top, lookups iterate downward until a match is found.
#[derive(Debug)]
pub enum ScopedMap<'a, K, V> {
    Cons(HashMap<K, V>, &'a Self),
    Tail(HashMap<K, V>),
}

impl<'a, K, V> ScopedMap<'a, K, V>
where
    K: Eq + Hash,
{
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.scope_iter().find_map(|map| map.get(key))
    }

    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.top_mut().insert(k, v)
    }

    #[inline]
    pub fn introduce_scope(&'a self) -> Self {
        self.push_scope(HashMap::default())
    }

    #[inline]
    pub fn push_scope(&'a self, map: HashMap<K, V>) -> Self {
        Self::Cons(map, self)
    }

    #[inline]
    pub fn pop_scope(self) -> HashMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    #[inline]
    pub fn top(&self) -> &HashMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    #[inline]
    pub fn top_mut(&mut self) -> &mut HashMap<K, V> {
        match self {
            Self::Cons(map, _) | Self::Tail(map) => map,
        }
    }

    pub fn len(&self) -> usize {
        self.scope_iter().map(HashMap::len).sum()
    }

    #[inline]
    pub fn scope_iter(&self) -> ScopeIter<'_, 'a, K, V> {
        ScopeIter { map: Some(self) }
    }

    #[inline]
    pub fn is_top_level(&self) -> bool {
        matches!(self, ScopedMap::Tail(_))
    }
}

impl<'a, K, V> Default for ScopedMap<'a, K, V> {
    #[inline]
    fn default() -> Self {
        Self::Tail(HashMap::default())
    }
}

impl<'a, K: Eq + Hash, V> Extend<(K, V)> for ScopedMap<'a, K, V> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        self.top_mut().extend(iter);
    }
}

impl<'a, K: Eq + Hash, V> FromIterator<(K, V)> for ScopedMap<'a, K, V> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self::Tail(iter.into_iter().collect())
    }
}

#[derive(Debug)]
pub struct ScopeIter<'a, 'b, K, V> {
    map: Option<&'a ScopedMap<'b, K, V>>,
}

impl<'a, 'b, K, V> Iterator for ScopeIter<'a, 'b, K, V> {
    type Item = &'a HashMap<K, V>;

    #[inline]
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

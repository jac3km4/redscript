use std::borrow::Cow;
use std::hash::Hash;
use std::{fmt, ptr};

use identity_hash::IdentityHashable;

use crate::Shared;

#[cfg(not(feature = "sync"))]
use std::cell::Cell;

#[cfg(not(feature = "sync"))]
use crate::FrozenIndexSet;

#[derive(Clone, Copy)]
pub struct TypeId<'id>(TypeIdRepr<'id>);

impl fmt::Debug for TypeId<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct StaticArrayDebug(u32);
        impl fmt::Debug for StaticArrayDebug {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "StaticArray({})", self.0)
            }
        }

        match self.0 {
            TypeIdRepr::Named(name) => f.debug_tuple("TypeId").field(&name).finish(),
            TypeIdRepr::StaticArray(size) => f
                .debug_tuple("TypeId")
                .field(&StaticArrayDebug(size))
                .finish(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum TypeIdRepr<'id> {
    Named(&'id str),
    StaticArray(u32),
}

impl TypeId<'_> {
    #[inline]
    pub const fn static_array(size: u32) -> Self {
        Self(TypeIdRepr::StaticArray(size))
    }

    pub fn is_fn(&self) -> bool {
        predef::FN_BY_ARITY.contains(self)
    }

    #[inline]
    pub const fn is_static_array(&self) -> bool {
        matches!(self.0, TypeIdRepr::StaticArray(_))
    }

    pub fn fn_with_arity(arity: usize) -> Option<Self> {
        predef::FN_BY_ARITY.get(arity).copied()
    }

    #[inline]
    pub fn array_with_size(size: usize) -> Option<Self> {
        u32::try_from(size).ok().map(Self::static_array)
    }

    #[inline]
    pub const fn static_array_size(&self) -> Option<u32> {
        match self.0 {
            TypeIdRepr::StaticArray(size) => Some(size),
            TypeIdRepr::Named(_) => None,
        }
    }
}

impl<'id> TypeId<'id> {
    #[inline]
    pub const fn as_name(&self) -> Option<&'id str> {
        match self.0 {
            TypeIdRepr::Named(name) => Some(name),
            TypeIdRepr::StaticArray(_) => None,
        }
    }

    #[inline]
    pub fn as_str(&self) -> Cow<'id, str> {
        match self.0 {
            TypeIdRepr::Named(name) => Cow::Borrowed(name),
            TypeIdRepr::StaticArray(size) => Cow::Owned(format!("array{size}")),
        }
    }

    pub fn simple_name(&self) -> Cow<'id, str> {
        match self.0 {
            TypeIdRepr::Named(name) => {
                Cow::Borrowed(name.rsplit_once('.').map_or(name, |(_, name)| name))
            }
            TypeIdRepr::StaticArray(size) => Cow::Owned(format!("array{size}")),
        }
    }
}

impl PartialEq for TypeId<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (TypeIdRepr::Named(a), TypeIdRepr::Named(b)) => ptr::eq(a.as_ptr(), b.as_ptr()),
            (TypeIdRepr::StaticArray(a), TypeIdRepr::StaticArray(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for TypeId<'_> {}

impl Hash for TypeId<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self.0 {
            TypeIdRepr::Named(name) => ptr::hash(name.as_ptr(), state),
            TypeIdRepr::StaticArray(size) => {
                // Single write_usize satisfies IdentityHashable's IdentityHasher<usize> contract,
                // with bit 0 set so synthetic array hashes do not trivially collide with pointers.
                state.write_usize(
                    (u64::from(size).wrapping_mul(0x9e37_79b9_7f4a_7c15) as usize) | 1,
                );
            }
        }
    }
}

impl IdentityHashable for TypeId<'_> {}

impl fmt::Display for TypeId<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            TypeIdRepr::Named(name) => f.write_str(name),
            TypeIdRepr::StaticArray(size) => write!(f, "array{size}"),
        }
    }
}

/// Looks up a predefined name's index without rescanning `predef::ALL_TYPES`.
///
/// This runs on every interning and lookup, so the mapping is precomputed once
/// and shared by both feature configurations. Predefined names are never copied:
/// they keep the exact `&'static str` addresses used by [`predef`], so pointer
/// identity is preserved against `Symbols::with_default_types`.
fn predef_index(name: &str) -> Option<TypeIndex> {
    static PREDEF_INDEX: std::sync::LazyLock<hashbrown::HashMap<&'static str, TypeIndex>> =
        std::sync::LazyLock::new(|| {
            predef::ALL_TYPES
                .iter()
                .enumerate()
                .filter_map(|(index, id)| Some((id.as_name()?, TypeIndex(index))))
                .collect()
        });
    PREDEF_INDEX.get(name).copied()
}

#[cfg(not(feature = "sync"))]
pub struct TypeInterner {
    parent: Option<Shared<TypeInterner>>,
    /// Number of indices reserved by `predef` and the parent chain.
    base_len: usize,
    names: FrozenIndexSet<Cow<'static, str>>,
    /// Number of names interned into `names`; `FrozenIndexSet` has no `len`.
    count: Cell<usize>,
    /// Set when this interner is forked. Interning a *new* name afterwards would
    /// assign it the child's first local index, aliasing the two; the flag turns
    /// that silent corruption into a loud failure in every build.
    forked: Cell<bool>,
}

#[cfg(not(feature = "sync"))]
impl TypeInterner {
    pub fn intern(&self, name: impl AsRef<str> + Into<String>) -> TypeId<'_> {
        let key = name.as_ref();
        if let Some(idx) = predef_index(key) {
            return predef::ALL_TYPES[idx.0];
        }
        if let Some(parent) = &self.parent
            && let Some(id) = parent.get(key)
        {
            return id;
        }
        if let Some(str) = self.names.get(key) {
            TypeId(TypeIdRepr::Named(str))
        } else {
            assert!(
                !self.forked.get(),
                "cannot intern new type `{key}` into a TypeInterner after forking"
            );
            let interned = self.names.insert(Cow::Owned(name.into()));
            self.count.set(self.count.get() + 1);
            TypeId(TypeIdRepr::Named(interned))
        }
    }

    /// Looks up a canonical type ID without interning a new name.
    pub fn get(&self, name: &str) -> Option<TypeId<'_>> {
        if let Some(idx) = predef_index(name) {
            return Some(predef::ALL_TYPES[idx.0]);
        }
        if let Some(parent) = &self.parent
            && let Some(id) = parent.get(name)
        {
            return Some(id);
        }
        self.names.get(name).map(|s| TypeId(TypeIdRepr::Named(s)))
    }

    pub fn get_index_of(&self, name: impl AsRef<str>) -> Option<TypeIndex> {
        let key = name.as_ref();
        if let Some(idx) = predef_index(key) {
            return Some(idx);
        }
        if let Some(parent) = &self.parent
            && let Some(idx) = parent.get_index_of(key)
        {
            return Some(idx);
        }
        self.names
            .get_index_of(key)
            .map(|index| TypeIndex(self.base_len + index))
    }

    pub fn get_index(&self, index: TypeIndex) -> Option<TypeId<'_>> {
        if index.0 < predef::ALL_TYPES.len() {
            return Some(predef::ALL_TYPES[index.0]);
        }
        if index.0 < self.base_len {
            return self.parent.as_ref()?.get_index(index);
        }
        self.names
            .get_index(index.0 - self.base_len)
            .map(|s| TypeId(TypeIdRepr::Named(s)))
    }

    /// Creates a child interner that shares every name interned in `parent`
    /// (returning the exact same [`TypeId`] pointers) while isolating names
    /// interned afterwards. The parent must outlive the child and must not be
    /// interned into after forking, so a shared handle is required rather than
    /// an owned value. Interning a *new* name into the parent after forking
    /// would alias the child's first local index; this panics instead.
    pub fn fork(parent: &Shared<Self>) -> Self {
        parent.forked.set(true);
        Self {
            parent: Some(Shared::clone(parent)),
            base_len: predef::ALL_TYPES.len() + parent.local_len(),
            names: FrozenIndexSet::default(),
            count: Cell::new(0),
            forked: Cell::new(false),
        }
    }

    fn local_len(&self) -> usize {
        self.parent.as_ref().map_or(0, |parent| parent.local_len()) + self.count.get()
    }
}

#[cfg(not(feature = "sync"))]
impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            parent: None,
            base_len: predef::ALL_TYPES.len(),
            names: FrozenIndexSet::default(),
            count: Cell::new(0),
            forked: Cell::new(false),
        }
    }
}

/// Thread-safe interner preserving pointer identity: names live in stable,
/// append-only storage and a read/write index provides de-duplication and
/// ordered lookups. `elsa::sync` has no `FrozenIndexSet`, so the index is a
/// plain `RwLock<HashMap>` and interned names are stored in both places.
///
/// Predefined names are never copied: they keep the exact `&'static str`
/// addresses used by [`predef`], so pointer identity is preserved against
/// `Symbols::with_default_types`. Their indices occupy the same `0..len`
/// prefix as the default `FrozenIndexSet`.
#[cfg(feature = "sync")]
pub struct TypeInterner {
    parent: Option<Shared<TypeInterner>>,
    /// Number of indices reserved by `predef` and the parent chain.
    base_len: usize,
    names: elsa::sync::FrozenVec<Box<str>>,
    index: std::sync::RwLock<hashbrown::HashMap<Box<str>, TypeIndex>>,
    /// Set when this interner is forked. Interning a *new* name afterwards would
    /// assign it the child's first local index, aliasing the two; the flag turns
    /// that silent corruption into a loud failure in every build.
    forked: std::sync::atomic::AtomicBool,
}

#[cfg(feature = "sync")]
impl TypeInterner {
    /// Maps an index to its interned name, walking the parent chain for indices
    /// reserved before this interner was forked.
    #[inline]
    fn named(&self, index: TypeIndex) -> TypeId<'_> {
        if index.0 < predef::ALL_TYPES.len() {
            return predef::ALL_TYPES[index.0];
        }
        if index.0 < self.base_len {
            return self
                .parent
                .as_ref()
                .and_then(|parent| parent.get_index(index))
                .expect("parent index");
        }
        TypeId(TypeIdRepr::Named(
            self.names
                .get(index.0 - self.base_len)
                .expect("interned name"),
        ))
    }

    pub fn intern(&self, name: impl AsRef<str> + Into<String>) -> TypeId<'_> {
        let key = name.as_ref();
        if let Some(idx) = predef_index(key) {
            return predef::ALL_TYPES[idx.0];
        }
        if let Some(parent) = &self.parent
            && let Some(id) = parent.get(key)
        {
            return id;
        }
        // Fast path: the common case is looking up an already-interned name, so
        // try a shared read lock before taking the exclusive write lock.
        {
            let index = self
                .index
                .read()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(&idx) = index.get(key) {
                return self.named(idx);
            }
        }
        // Hold the write lock across the whole check-then-insert: interning the
        // same name twice would produce two addresses and break pointer identity.
        let mut index = self
            .index
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(&idx) = index.get(key) {
            return self.named(idx);
        }
        assert!(
            !self.forked.load(std::sync::atomic::Ordering::Acquire),
            "cannot intern new type `{key}` into a TypeInterner after forking"
        );
        let name: Box<str> = name.into().into_boxed_str();
        let local = self.names.push_get_index(name.clone());
        let idx = TypeIndex(self.base_len + local);
        index.insert(name, idx);
        self.named(idx)
    }

    /// Looks up a canonical type ID without interning a new name.
    pub fn get(&self, name: &str) -> Option<TypeId<'_>> {
        if let Some(idx) = predef_index(name) {
            return Some(predef::ALL_TYPES[idx.0]);
        }
        if let Some(parent) = &self.parent
            && let Some(id) = parent.get(name)
        {
            return Some(id);
        }
        let index = self
            .index
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let idx = index.get(name)?;
        Some(self.named(*idx))
    }

    pub fn get_index_of(&self, name: impl AsRef<str>) -> Option<TypeIndex> {
        let key = name.as_ref();
        if let Some(idx) = predef_index(key) {
            return Some(idx);
        }
        if let Some(parent) = &self.parent
            && let Some(idx) = parent.get_index_of(key)
        {
            return Some(idx);
        }
        self.index
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(key)
            .copied()
    }

    pub fn get_index(&self, index: TypeIndex) -> Option<TypeId<'_>> {
        if index.0 < predef::ALL_TYPES.len() {
            return Some(predef::ALL_TYPES[index.0]);
        }
        if index.0 < self.base_len {
            return self.parent.as_ref()?.get_index(index);
        }
        self.names
            .get(index.0 - self.base_len)
            .map(|s| TypeId(TypeIdRepr::Named(s)))
    }

    /// Creates a child interner that shares every name interned in `parent`
    /// (returning the exact same [`TypeId`] pointers) while isolating names
    /// interned afterwards. The parent must outlive the child and must not be
    /// interned into after forking, so a shared handle is required rather than
    /// an owned value. Interning a *new* name into the parent after forking
    /// would alias the child's first local index; this panics instead.
    ///
    /// Holding the parent's index write lock across the freeze and the local
    /// length read serializes this with [`Self::intern`]: an in-flight intern
    /// either finishes before `base_len` is computed or observes the frozen
    /// flag and panics without extending `names`.
    pub fn fork(parent: &Shared<Self>) -> Self {
        let guard = parent
            .index
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        parent
            .forked
            .store(true, std::sync::atomic::Ordering::Release);
        let base_len = predef::ALL_TYPES.len() + parent.local_len();
        drop(guard);
        Self {
            parent: Some(Shared::clone(parent)),
            base_len,
            names: elsa::sync::FrozenVec::new(),
            index: std::sync::RwLock::new(hashbrown::HashMap::new()),
            forked: std::sync::atomic::AtomicBool::new(false),
        }
    }

    fn local_len(&self) -> usize {
        self.parent.as_ref().map_or(0, |parent| parent.local_len()) + self.names.len()
    }
}

#[cfg(feature = "sync")]
impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            parent: None,
            base_len: predef::ALL_TYPES.len(),
            names: elsa::sync::FrozenVec::new(),
            index: std::sync::RwLock::new(hashbrown::HashMap::new()),
            forked: std::sync::atomic::AtomicBool::new(false),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeIndex(usize);

pub mod predef {
    use super::*;

    macro_rules! types {
        ($($id:ident => $name:literal),*) => {
            $(pub static $id: TypeId<'static> = TypeId(TypeIdRepr::Named($name));)*

            pub(super) static ALL_TYPES: &[TypeId<'static>] = &[$($id),*];
        };
    }

    types! {
        STRING => "String",
        CNAME => "CName",
        RES_REF => "ResRef",
        TWEAK_DB_ID => "TweakDBID",
        NODE_REF => "NodeRef",
        CRUID => "CRUID",
        LOCALIZATION_STRING => "LocalizationString",
        RED_RESOURCE_REFERENCE_SCRIPT_TOKEN => "redResourceReferenceScriptToken",
        FLOAT => "Float",
        DOUBLE => "Double",
        INT8 => "Int8",
        INT16 => "Int16",
        INT32 => "Int32",
        INT64 => "Int64",
        UINT8 => "Uint8",
        UINT16 => "Uint16",
        UINT32 => "Uint32",
        UINT64 => "Uint64",
        BOOL => "Bool",
        VARIANT => "Variant",
        ISCRIPTABLE => "IScriptable",
        ISERIALIZABLE => "ISerializable",

        VOID => "Void",
        NOTHING => "Nothing",

        REF => "ref",
        WREF => "wref",
        SCRIPT_REF => "script_ref",
        ARRAY => "array",

        FUNCTION0 => "Function0",
        FUNCTION1 => "Function1",
        FUNCTION2 => "Function2",
        FUNCTION3 => "Function3",
        FUNCTION4 => "Function4",
        FUNCTION5 => "Function5",
        FUNCTION6 => "Function6",
        FUNCTION7 => "Function7",
        FUNCTION8 => "Function8"
    }

    pub(super) static FN_BY_ARITY: &[TypeId<'static>] = &[
        predef::FUNCTION0,
        predef::FUNCTION1,
        predef::FUNCTION2,
        predef::FUNCTION3,
        predef::FUNCTION4,
        predef::FUNCTION5,
        predef::FUNCTION6,
        predef::FUNCTION7,
        predef::FUNCTION8,
    ];
}

#[cfg(test)]
mod tests {
    use hashbrown::HashMap;
    use identity_hash::BuildIdentityHasher;

    use super::*;

    #[test]
    fn static_array_type_ids_preserve_size_identity_and_hashing() {
        let interner = TypeInterner::default();
        let named_array1 = interner.intern("array1");
        let a0 = TypeId::static_array(0);
        let a1 = TypeId::static_array(1);
        let a17 = TypeId::static_array(17);
        let a_max = TypeId::static_array(u32::MAX);

        assert_ne!(a1, named_array1);
        assert_ne!(a0, a1);
        assert_ne!(a17, a_max);
        assert_eq!(TypeId::array_with_size(17), Some(a17));
        assert_eq!(TypeId::array_with_size(u32::MAX as usize), Some(a_max));
        #[cfg(target_pointer_width = "64")]
        assert_eq!(TypeId::array_with_size(u32::MAX as usize + 1), None);

        assert!(a17.is_static_array());
        assert_eq!(a17.static_array_size(), Some(17));
        assert_eq!(a_max.static_array_size(), Some(u32::MAX));
        assert_eq!(a17.as_name(), None);
        assert_eq!(a17.as_str(), "array17");
        assert_eq!(a17.simple_name(), "array17");
        assert_eq!(a17.to_string(), "array17");

        assert!(!named_array1.is_static_array());
        assert_eq!(named_array1.static_array_size(), None);
        assert_eq!(named_array1.as_name(), Some("array1"));

        let mut map: HashMap<TypeId<'_>, u32, BuildIdentityHasher<usize>> = HashMap::default();
        map.insert(a0, 0);
        map.insert(a1, 1);
        map.insert(a17, 17);
        map.insert(a_max, u32::MAX);
        map.insert(named_array1, 99);
        assert_eq!(map.get(&TypeId::static_array(0)), Some(&0));
        assert_eq!(map.get(&TypeId::static_array(1)), Some(&1));
        assert_eq!(map.get(&TypeId::static_array(17)), Some(&17));
        assert_eq!(map.get(&TypeId::static_array(u32::MAX)), Some(&u32::MAX));
        assert_eq!(map.get(&named_array1), Some(&99));
    }

    #[test]
    fn predefined_names_round_trip_with_their_indices() {
        let interner = TypeInterner::default();
        for (index, id) in predef::ALL_TYPES.iter().enumerate() {
            let name = id.as_name().expect("predefined id has a name");
            // Interning keeps the shared predefined handle instead of allocating.
            assert_eq!(interner.intern(name), *id);
            assert_eq!(interner.get(name), Some(*id));
            assert_eq!(interner.get_index_of(name), Some(TypeIndex(index)));
            assert_eq!(interner.get_index(TypeIndex(index)), Some(*id));
        }
    }

    #[test]
    fn forked_interners_share_base_names_and_isolate_locals() {
        let base = Shared::new(TypeInterner::default());
        let shared = base.intern("SharedName");

        let child_a = TypeInterner::fork(&base);
        let child_b = TypeInterner::fork(&base);

        // Predefined and base names keep their identity across forks.
        assert_eq!(child_a.intern("SharedName"), shared);
        assert_eq!(child_b.get("SharedName"), Some(shared));
        assert_eq!(child_a.intern("String"), predef::STRING);
        assert_eq!(
            child_a.get_index_of("SharedName"),
            base.get_index_of("SharedName")
        );
        assert_eq!(child_a.get_index_of("String"), base.get_index_of("String"));

        // Names interned in a child do not leak into its parent or siblings.
        let a_local = child_a.intern("OnlyInA");
        assert!(child_b.get("OnlyInA").is_none());
        assert!(base.get("OnlyInA").is_none());
        assert_eq!(child_a.get("OnlyInA"), Some(a_local));

        // Index lookups agree across `predef`, parent and child entries.
        let a_index = child_a.get_index_of("OnlyInA").unwrap();
        assert_eq!(child_a.get_index(a_index), Some(a_local));
        assert!(a_index.0 >= predef::ALL_TYPES.len());

        let b_local = child_b.intern("OnlyInB");
        assert_ne!(a_local, b_local);
        assert!(child_a.get("OnlyInB").is_none());
        assert_eq!(
            child_b.get_index(child_b.get_index_of("OnlyInB").unwrap()),
            Some(b_local)
        );
    }

    #[test]
    fn nested_forks_resolve_parent_entries() {
        let base = Shared::new(TypeInterner::default());
        let child = Shared::new(TypeInterner::fork(&base));
        let mid = child.intern("MidLevel");
        let grandchild = TypeInterner::fork(&child);

        assert_eq!(grandchild.get("MidLevel"), Some(mid));
        assert_eq!(grandchild.intern("MidLevel"), mid);
        assert_eq!(grandchild.get("String"), Some(predef::STRING));

        let mid_index = grandchild.get_index_of("MidLevel").unwrap();
        assert_eq!(child.get_index_of("MidLevel"), Some(mid_index));
        assert_eq!(grandchild.get_index(mid_index), Some(mid));

        let leaf = grandchild.intern("LeafOnly");
        assert_eq!(grandchild.get("LeafOnly"), Some(leaf));
        assert!(child.get("LeafOnly").is_none());
        assert_eq!(
            grandchild.get_index_of("LeafOnly").map(|i| i.0),
            Some(mid_index.0 + 1)
        );
    }

    /// Interning a new name into a forked parent would assign it the child's
    /// first local index, so every build rejects it; re-interning an existing
    /// name stays valid because it resolves to the same index.
    #[test]
    #[should_panic(expected = "after forking")]
    fn interning_a_new_name_after_forking_panics() {
        let base = Shared::new(TypeInterner::default());
        let existing = base.intern("Existing");
        let _child = TypeInterner::fork(&base);

        assert_eq!(base.intern("Existing"), existing);
        assert_eq!(base.get("Existing"), Some(existing));

        base.intern("BrandNew");
    }
}

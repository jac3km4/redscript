use std::borrow::Cow;
use std::hash::Hash;
use std::{fmt, ptr};

use identity_hash::IdentityHashable;

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

pub struct TypeInterner {
    names: FrozenIndexSet<Cow<'static, str>>,
}

impl TypeInterner {
    pub fn intern(&self, name: impl AsRef<str> + Into<String>) -> TypeId<'_> {
        match self.names.get(name.as_ref()) {
            Some(str) => TypeId(TypeIdRepr::Named(str)),
            None => TypeId(TypeIdRepr::Named(
                self.names.insert(Cow::Owned(name.into())),
            )),
        }
    }

    /// Looks up a canonical type ID without interning a new name.
    pub fn get(&self, name: &str) -> Option<TypeId<'_>> {
        self.names.get(name).map(|s| TypeId(TypeIdRepr::Named(s)))
    }

    pub fn get_index_of(&self, name: impl AsRef<str>) -> Option<TypeIndex> {
        self.names.get_index_of(name.as_ref()).map(TypeIndex)
    }

    pub fn get_index(&self, index: TypeIndex) -> Option<TypeId<'_>> {
        self.names
            .get_index(index.0)
            .map(|s| TypeId(TypeIdRepr::Named(s)))
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            names: predef::ALL_TYPES
                .iter()
                .filter_map(|id| id.as_name().map(Cow::Borrowed))
                .collect(),
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
}

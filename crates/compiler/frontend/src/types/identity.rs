use std::borrow::Cow;
use std::hash::Hash;
use std::{fmt, ptr};

use identity_hash::IdentityHashable;

use crate::FrozenIndexSet;

#[derive(Debug, Clone, Copy)]
pub struct TypeId<'id>(&'id str);

impl TypeId<'_> {
    pub fn is_fn(&self) -> bool {
        predef::FN_BY_ARITY.contains(self)
    }

    pub fn is_static_array(&self) -> bool {
        predef::STATIC_ARRAY_TYPES.contains(self)
    }

    pub fn fn_with_arity(arity: usize) -> Option<Self> {
        predef::FN_BY_ARITY.get(arity).copied()
    }

    pub fn array_with_size(size: usize) -> Option<Self> {
        match size {
            0 => Some(predef::ARRAY0),
            1 => Some(predef::ARRAY1),
            2 => Some(predef::ARRAY2),
            3 => Some(predef::ARRAY3),
            4 => Some(predef::ARRAY4),
            5 => Some(predef::ARRAY5),
            6 => Some(predef::ARRAY6),
            7 => Some(predef::ARRAY7),
            8 => Some(predef::ARRAY8),
            9 => Some(predef::ARRAY9),
            10 => Some(predef::ARRAY10),
            11 => Some(predef::ARRAY11),
            12 => Some(predef::ARRAY12),
            13 => Some(predef::ARRAY13),
            14 => Some(predef::ARRAY14),
            15 => Some(predef::ARRAY15),
            16 => Some(predef::ARRAY16),
            // special cases needed to load the full game cache
            30 => Some(predef::ARRAY30),
            32 => Some(predef::ARRAY32),
            45 => Some(predef::ARRAY45),
            64 => Some(predef::ARRAY64),
            100 => Some(predef::ARRAY100),
            128 => Some(predef::ARRAY128),
            4096 => Some(predef::ARRAY4096),
            _ => None,
        }
    }

    pub fn static_array_size(&self) -> Option<u16> {
        match *self {
            id if id == predef::ARRAY0 => Some(0),
            id if id == predef::ARRAY1 => Some(1),
            id if id == predef::ARRAY2 => Some(2),
            id if id == predef::ARRAY3 => Some(3),
            id if id == predef::ARRAY4 => Some(4),
            id if id == predef::ARRAY5 => Some(5),
            id if id == predef::ARRAY6 => Some(6),
            id if id == predef::ARRAY7 => Some(7),
            id if id == predef::ARRAY8 => Some(8),
            id if id == predef::ARRAY9 => Some(9),
            id if id == predef::ARRAY10 => Some(10),
            id if id == predef::ARRAY11 => Some(11),
            id if id == predef::ARRAY12 => Some(12),
            id if id == predef::ARRAY13 => Some(13),
            id if id == predef::ARRAY14 => Some(14),
            id if id == predef::ARRAY15 => Some(15),
            id if id == predef::ARRAY16 => Some(16),
            id if id == predef::ARRAY30 => Some(30),
            id if id == predef::ARRAY32 => Some(32),
            id if id == predef::ARRAY45 => Some(45),
            id if id == predef::ARRAY64 => Some(64),
            id if id == predef::ARRAY100 => Some(100),
            id if id == predef::ARRAY128 => Some(128),
            id if id == predef::ARRAY4096 => Some(4096),
            _ => None,
        }
    }
}

impl<'id> TypeId<'id> {
    #[inline]
    pub fn as_str(&self) -> &'id str {
        self.0
    }

    pub fn simple_name(&self) -> &'id str {
        self.0.rsplit_once('.').map_or(self.0, |(_, name)| name)
    }
}

impl PartialEq for TypeId<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl Eq for TypeId<'_> {}

impl Hash for TypeId<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0.as_ptr(), state);
    }
}

impl IdentityHashable for TypeId<'_> {}

impl AsRef<str> for TypeId<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl fmt::Display for TypeId<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

pub struct TypeInterner {
    names: FrozenIndexSet<Cow<'static, str>>,
}

impl TypeInterner {
    pub fn intern(&self, name: impl AsRef<str> + Into<String>) -> TypeId<'_> {
        match self.names.get(name.as_ref()) {
            Some(str) => TypeId(str),
            None => TypeId(self.names.insert(Cow::Owned(name.into()))),
        }
    }

    pub fn get_index_of(&self, name: impl AsRef<str>) -> Option<TypeIndex> {
        self.names.get_index_of(name.as_ref()).map(TypeIndex)
    }

    pub fn get_index(&self, index: TypeIndex) -> Option<TypeId<'_>> {
        self.names.get_index(index.0).map(TypeId)
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            names: predef::ALL_TYPES
                .iter()
                .map(|id| Cow::Borrowed(id.0))
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
            $(pub static $id: TypeId<'static> = TypeId($name);)*

            pub(super) static ALL_TYPES: &[TypeId<'static>] = &[$($id),*];
        };
    }

    types! {
        STRING => "String",
        CNAME => "CName",
        RES_REF => "ResRef",
        TWEAK_DB_ID => "TweakDBID",
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

        VOID => "Void",
        NOTHING => "Nothing",

        REF => "ref",
        WREF => "wref",
        SCRIPT_REF => "script_ref",
        ARRAY => "array",

        // aliases for static arrays with specific sizes
        ARRAY0 => "array0",
        ARRAY1 => "array1",
        ARRAY2 => "array2",
        ARRAY3 => "array3",
        ARRAY4 => "array4",
        ARRAY5 => "array5",
        ARRAY6 => "array6",
        ARRAY7 => "array7",
        ARRAY8 => "array8",
        ARRAY9 => "array9",
        ARRAY10 => "array10",
        ARRAY11 => "array11",
        ARRAY12 => "array12",
        ARRAY13 => "array13",
        ARRAY14 => "array14",
        ARRAY15 => "array15",
        ARRAY16 => "array16",
        // special cases needed to load the full game cache,
        // these won't be needed if something like const generics is added
        ARRAY30 => "array30",
        ARRAY32 => "array32",
        ARRAY45 => "array45",
        ARRAY64 => "array64",
        ARRAY100 => "array100",
        ARRAY128 => "array128",
        ARRAY4096 => "array4096",

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

    pub(crate) static STATIC_ARRAY_TYPES: &[TypeId<'static>] = &[
        predef::ARRAY0,
        predef::ARRAY1,
        predef::ARRAY2,
        predef::ARRAY3,
        predef::ARRAY4,
        predef::ARRAY5,
        predef::ARRAY6,
        predef::ARRAY7,
        predef::ARRAY8,
        predef::ARRAY9,
        predef::ARRAY10,
        predef::ARRAY11,
        predef::ARRAY12,
        predef::ARRAY13,
        predef::ARRAY14,
        predef::ARRAY15,
        predef::ARRAY16,
        predef::ARRAY30,
        predef::ARRAY32,
        predef::ARRAY45,
        predef::ARRAY64,
        predef::ARRAY100,
        predef::ARRAY128,
        predef::ARRAY4096,
    ];
}

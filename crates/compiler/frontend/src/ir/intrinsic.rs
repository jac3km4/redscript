#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intrinsic {
    Equals,
    NotEquals,
    ArrayClear,
    ArraySize,
    ArrayResize,
    ArrayFindFirst,
    ArrayFindLast,
    ArrayContains,
    ArrayCount,
    ArrayPush,
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArrayGrow,
    ArrayErase,
    ArrayLast,
    ArraySort,
    ArraySortByPredicate,
    ToString,
    EnumInt,
    IntEnum,
    LongIntEnum,
    ToVariant,
    FromVariant,
    VariantIsDefined,
    VariantIsRef,
    VariantIsArray,
    VariantTypeName,
    AsRef,
    Deref,
    RefToWeakRef,
    WeakRefToRef,
    IsDefined,
    NameOf,
}

impl Intrinsic {
    pub fn name(self) -> &'static str {
        match self {
            Self::Equals => "Equals",
            Self::NotEquals => "NotEquals",
            Self::ArrayClear => "ArrayClear",
            Self::ArraySize => "ArraySize",
            Self::ArrayResize => "ArrayResize",
            Self::ArrayFindFirst => "ArrayFindFirst",
            Self::ArrayFindLast => "ArrayFindLast",
            Self::ArrayContains => "ArrayContains",
            Self::ArrayCount => "ArrayCount",
            Self::ArrayPush => "ArrayPush",
            Self::ArrayPop => "ArrayPop",
            Self::ArrayInsert => "ArrayInsert",
            Self::ArrayRemove => "ArrayRemove",
            Self::ArrayGrow => "ArrayGrow",
            Self::ArrayErase => "ArrayErase",
            Self::ArrayLast => "ArrayLast",
            Self::ArraySort => "ArraySort",
            Self::ArraySortByPredicate => "ArraySortByPredicate",
            Self::ToString => "ToString",
            Self::EnumInt => "EnumInt",
            Self::IntEnum => "IntEnum",
            Self::LongIntEnum => "LongIntEnum",
            Self::ToVariant => "ToVariant",
            Self::FromVariant => "FromVariant",
            Self::VariantIsDefined => "VariantIsDefined",
            Self::VariantIsRef => "VariantIsRef",
            Self::VariantIsArray => "VariantIsArray",
            Self::VariantTypeName => "VariantTypeName",
            Self::AsRef => "AsRef",
            Self::Deref => "Deref",
            Self::RefToWeakRef => "RefToWeakRef",
            Self::WeakRefToRef => "WeakRefToRef",
            Self::IsDefined => "IsDefined",
            Self::NameOf => "NameOf",
        }
    }
}

impl From<Intrinsic> for &'static str {
    fn from(value: Intrinsic) -> Self {
        value.name()
    }
}

impl<'a> TryFrom<&'a str> for Intrinsic {
    type Error = UnknownIntrinsic<'a>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let res = match value {
            "Equals" => Self::Equals,
            "NotEquals" => Self::NotEquals,
            "ArrayClear" => Self::ArrayClear,
            "ArraySize" => Self::ArraySize,
            "ArrayResize" => Self::ArrayResize,
            "ArrayFindFirst" => Self::ArrayFindFirst,
            "ArrayFindLast" => Self::ArrayFindLast,
            "ArrayContains" => Self::ArrayContains,
            "ArrayCount" => Self::ArrayCount,
            "ArrayPush" => Self::ArrayPush,
            "ArrayPop" => Self::ArrayPop,
            "ArrayInsert" => Self::ArrayInsert,
            "ArrayRemove" => Self::ArrayRemove,
            "ArrayGrow" => Self::ArrayGrow,
            "ArrayErase" => Self::ArrayErase,
            "ArrayLast" => Self::ArrayLast,
            "ArraySort" => Self::ArraySort,
            "ArraySortByPredicate" => Self::ArraySortByPredicate,
            "ToString" => Self::ToString,
            "EnumInt" => Self::EnumInt,
            "IntEnum" => Self::IntEnum,
            "LongIntEnum" => Self::LongIntEnum,
            "ToVariant" => Self::ToVariant,
            "FromVariant" => Self::FromVariant,
            "VariantIsDefined" => Self::VariantIsDefined,
            "VariantIsRef" => Self::VariantIsRef,
            "VariantIsArray" => Self::VariantIsArray,
            "VariantTypeName" => Self::VariantTypeName,
            "AsRef" => Self::AsRef,
            "Deref" => Self::Deref,
            "RefToWeakRef" => Self::RefToWeakRef,
            "WeakRefToRef" => Self::WeakRefToRef,
            "IsDefined" => Self::IsDefined,
            "NameOf" => Self::NameOf,
            _ => return Err(UnknownIntrinsic(value)),
        };
        Ok(res)
    }
}

#[derive(Debug)]
pub struct UnknownIntrinsic<'a>(pub &'a str);

impl<'a> From<UnknownIntrinsic<'a>> for &'a str {
    fn from(UnknownIntrinsic(str): UnknownIntrinsic<'a>) -> Self {
        str
    }
}

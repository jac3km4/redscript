use std::{io, usize};

use crate::bundle::{PoolIndex, Resource, TweakDbId};
use crate::decode::{Decode, DecodeExt};
use crate::definition::{Class, Enum, Field, Function, Local, Parameter, Type};
use crate::encode::{Encode, EncodeExt};
use crate::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Instr<Loc> {
    Nop,
    Null,
    I32One,
    I32Zero,
    I8Const(i8),
    I16Const(i16),
    I32Const(i32),
    I64Const(i64),
    U8Const(u8),
    U16Const(u16),
    U32Const(u32),
    U64Const(u64),
    F32Const(f32),
    F64Const(f64),
    NameConst(PoolIndex<String>),
    EnumConst(PoolIndex<Enum>, PoolIndex<i64>),
    StringConst(String),
    TweakDbIdConst(PoolIndex<TweakDbId>),
    ResourceConst(PoolIndex<Resource>),
    TrueConst,
    FalseConst,
    Breakpoint(u16, u32, u16, u16, u8, u64),
    Assign,
    Target(Loc),
    Local(PoolIndex<Local>),
    Param(PoolIndex<Parameter>),
    ObjectField(PoolIndex<Field>),
    ExternalVar,
    Switch(PoolIndex<Type>, Loc),
    SwitchLabel(Loc, Loc),
    SwitchDefault,
    Jump(Loc),
    JumpIfFalse(Loc),
    Skip(Loc),
    Conditional(Loc, Loc),
    Construct(u8, PoolIndex<Class>),
    InvokeStatic(Loc, u16, PoolIndex<Function>),
    InvokeVirtual(Loc, u16, PoolIndex<String>),
    ParamEnd,
    Return,
    StructField(PoolIndex<Field>),
    Context(Loc),
    Equals(PoolIndex<Type>),
    NotEquals(PoolIndex<Type>),
    New(PoolIndex<Class>),
    Delete,
    This,
    StartProfiling(Vec<u8>, u8),
    ArrayClear(PoolIndex<Type>),
    ArraySize(PoolIndex<Type>),
    ArrayResize(PoolIndex<Type>),
    ArrayFindFirst(PoolIndex<Type>),
    ArrayFindFirstFast(PoolIndex<Type>),
    ArrayFindLast(PoolIndex<Type>),
    ArrayFindLastFast(PoolIndex<Type>),
    ArrayContains(PoolIndex<Type>),
    ArrayContainsFast(PoolIndex<Type>),
    ArrayCount(PoolIndex<Type>),
    ArrayCountFast(PoolIndex<Type>),
    ArrayPush(PoolIndex<Type>),
    ArrayPop(PoolIndex<Type>),
    ArrayInsert(PoolIndex<Type>),
    ArrayRemove(PoolIndex<Type>),
    ArrayRemoveFast(PoolIndex<Type>),
    ArrayGrow(PoolIndex<Type>),
    ArrayErase(PoolIndex<Type>),
    ArrayEraseFast(PoolIndex<Type>),
    ArrayLast(PoolIndex<Type>),
    ArrayElement(PoolIndex<Type>),
    StaticArraySize(PoolIndex<Type>),
    StaticArrayFindFirst(PoolIndex<Type>),
    StaticArrayFindFirstFast(PoolIndex<Type>),
    StaticArrayFindLast(PoolIndex<Type>),
    StaticArrayFindLastFast(PoolIndex<Type>),
    StaticArrayContains(PoolIndex<Type>),
    StaticArrayContainsFast(PoolIndex<Type>),
    StaticArrayCount(PoolIndex<Type>),
    StaticArrayCountFast(PoolIndex<Type>),
    StaticArrayLast(PoolIndex<Type>),
    StaticArrayElement(PoolIndex<Type>),
    RefToBool,
    WeakRefToBool,
    EnumToI32(PoolIndex<Type>, u8),
    I32ToEnum(PoolIndex<Type>, u8),
    DynamicCast(PoolIndex<Class>, u8),
    ToString(PoolIndex<Type>),
    ToVariant(PoolIndex<Type>),
    FromVariant(PoolIndex<Type>),
    VariantIsValid,
    VariantIsRef,
    VariantIsArray,
    VatiantToCName,
    VariantToString,
    WeakRefToRef,
    RefToWeakRef,
    WeakRefNull,
    AsRef(PoolIndex<Type>),
    Deref(PoolIndex<Type>),
}

impl<L> Instr<L> {
    pub fn size(&self) -> u16 {
        let op_size = match self {
            Instr::Nop | Instr::Null | Instr::I32One | Instr::I32Zero | Instr::TrueConst | Instr::FalseConst => 0,
            Instr::I8Const(_) => 1,
            Instr::I16Const(_) => 2,
            Instr::I32Const(_) => 4,
            Instr::I64Const(_) => 8,
            Instr::U8Const(_) => 1,
            Instr::U16Const(_) => 2,
            Instr::U32Const(_) => 4,
            Instr::U64Const(_) => 8,
            Instr::F32Const(_) => 4,
            Instr::F64Const(_) => 8,
            Instr::NameConst(_) => 8,
            Instr::EnumConst(_, _) => 16,
            Instr::StringConst(bytes) => 4 + bytes.len() as u16,
            Instr::TweakDbIdConst(_) => 8,
            Instr::ResourceConst(_) => 8,
            Instr::Breakpoint(_, _, _, _, _, _) => 19,
            Instr::Assign => 0,
            Instr::Target(_) => return 0, // not present in bytecode
            Instr::Local(_) => 8,
            Instr::Param(_) => 8,
            Instr::ObjectField(_) => 8,
            Instr::ExternalVar => 0,
            Instr::Switch(_, _) => 10,
            Instr::SwitchLabel(_, _) => 4,
            Instr::SwitchDefault => 0,
            Instr::Jump(_) => 2,
            Instr::JumpIfFalse(_) => 2,
            Instr::Skip(_) => 2,
            Instr::Conditional(_, _) => 4,
            Instr::Construct(_, _) => 9,
            Instr::InvokeStatic(_, _, _) | Instr::InvokeVirtual(_, _, _) => 12,
            Instr::ParamEnd => 0,
            Instr::Return => 0,
            Instr::StructField(_) => 8,
            Instr::Context(_) => 2,
            Instr::Equals(_) => 8,
            Instr::NotEquals(_) => 8,
            Instr::New(_) => 8,
            Instr::Delete => 0,
            Instr::This => 0,
            Instr::StartProfiling(bytes, _) => 5 + bytes.len() as u16,
            Instr::ArrayClear(_) => 8,
            Instr::ArraySize(_) => 8,
            Instr::ArrayResize(_) => 8,
            Instr::ArrayFindFirst(_) => 8,
            Instr::ArrayFindFirstFast(_) => 8,
            Instr::ArrayFindLast(_) => 8,
            Instr::ArrayFindLastFast(_) => 8,
            Instr::ArrayContains(_) => 8,
            Instr::ArrayContainsFast(_) => 8,
            Instr::ArrayCount(_) => 8,
            Instr::ArrayCountFast(_) => 8,
            Instr::ArrayPush(_) => 8,
            Instr::ArrayPop(_) => 8,
            Instr::ArrayInsert(_) => 8,
            Instr::ArrayRemove(_) => 8,
            Instr::ArrayRemoveFast(_) => 8,
            Instr::ArrayGrow(_) => 8,
            Instr::ArrayErase(_) => 8,
            Instr::ArrayEraseFast(_) => 8,
            Instr::ArrayLast(_) => 8,
            Instr::ArrayElement(_) => 8,
            Instr::StaticArraySize(_) => 8,
            Instr::StaticArrayFindFirst(_) => 8,
            Instr::StaticArrayFindFirstFast(_) => 8,
            Instr::StaticArrayFindLast(_) => 8,
            Instr::StaticArrayFindLastFast(_) => 8,
            Instr::StaticArrayContains(_) => 8,
            Instr::StaticArrayContainsFast(_) => 8,
            Instr::StaticArrayCount(_) => 8,
            Instr::StaticArrayCountFast(_) => 8,
            Instr::StaticArrayLast(_) => 8,
            Instr::StaticArrayElement(_) => 8,
            Instr::RefToBool => 0,
            Instr::WeakRefToBool => 0,
            Instr::EnumToI32(_, _) => 9,
            Instr::I32ToEnum(_, _) => 9,
            Instr::DynamicCast(_, _) => 9,
            Instr::ToString(_) => 8,
            Instr::ToVariant(_) => 8,
            Instr::FromVariant(_) => 8,
            Instr::VariantIsValid => 0,
            Instr::VariantIsRef => 0,
            Instr::VariantIsArray => 0,
            Instr::VatiantToCName => 0,
            Instr::VariantToString => 0,
            Instr::WeakRefToRef => 0,
            Instr::RefToWeakRef => 0,
            Instr::WeakRefNull => 0,
            Instr::AsRef(_) => 8,
            Instr::Deref(_) => 8,
        };
        1 + op_size
    }
}

impl Instr<Label> {
    pub fn resolved(self, location: Location, targets: &[Location]) -> Instr<Offset> {
        match self {
            Instr::Nop => Instr::Nop,
            Instr::Null => Instr::Null,
            Instr::I32One => Instr::I32One,
            Instr::I32Zero => Instr::I32Zero,
            Instr::I8Const(val) => Instr::I8Const(val),
            Instr::I16Const(val) => Instr::I16Const(val),
            Instr::I32Const(val) => Instr::I32Const(val),
            Instr::I64Const(val) => Instr::I64Const(val),
            Instr::U8Const(val) => Instr::U8Const(val),
            Instr::U16Const(val) => Instr::U16Const(val),
            Instr::U32Const(val) => Instr::U32Const(val),
            Instr::U64Const(val) => Instr::U64Const(val),
            Instr::F32Const(val) => Instr::F32Const(val),
            Instr::F64Const(val) => Instr::F64Const(val),
            Instr::NameConst(idx) => Instr::NameConst(idx),
            Instr::EnumConst(enum_, member) => Instr::EnumConst(enum_, member),
            Instr::StringConst(val) => Instr::StringConst(val),
            Instr::TweakDbIdConst(idx) => Instr::TweakDbIdConst(idx),
            Instr::ResourceConst(idx) => Instr::ResourceConst(idx),
            Instr::TrueConst => Instr::TrueConst,
            Instr::FalseConst => Instr::FalseConst,
            Instr::Breakpoint(_0, _1, _2, _3, _4, _5) => Instr::Breakpoint(_0, _1, _2, _3, _4, _5),
            Instr::Assign => Instr::Assign,
            Instr::Target(label) => Instr::Target(targets[label.index].relative(location)),
            Instr::Local(idx) => Instr::Local(idx),
            Instr::Param(idx) => Instr::Param(idx),
            Instr::ObjectField(idx) => Instr::ObjectField(idx),
            Instr::ExternalVar => Instr::ExternalVar,
            Instr::Switch(idx, label) => Instr::Switch(idx, targets[label.index].relative(location)),
            Instr::SwitchLabel(first_case, exit) => Instr::SwitchLabel(
                targets[first_case.index].relative(location),
                targets[exit.index].relative(location),
            ),
            Instr::SwitchDefault => Instr::SwitchDefault,
            Instr::Jump(label) => Instr::Jump(targets[label.index].relative(location)),
            Instr::JumpIfFalse(label) => Instr::JumpIfFalse(targets[label.index].relative(location)),
            Instr::Skip(label) => Instr::Skip(targets[label.index].relative(location)),
            Instr::Conditional(false_, exit) => Instr::Conditional(
                targets[false_.index].relative(location),
                targets[exit.index].relative(location),
            ),
            Instr::Construct(args, idx) => Instr::Construct(args, idx),
            Instr::InvokeStatic(label, line, idx) => {
                Instr::InvokeStatic(targets[label.index].relative(location), line, idx)
            }
            Instr::InvokeVirtual(label, line, idx) => {
                Instr::InvokeVirtual(targets[label.index].relative(location), line, idx)
            }
            Instr::ParamEnd => Instr::ParamEnd,
            Instr::Return => Instr::Return,
            Instr::StructField(idx) => Instr::StructField(idx),
            Instr::Context(label) => Instr::Context(targets[label.index].relative(location)),
            Instr::Equals(idx) => Instr::Equals(idx),
            Instr::NotEquals(idx) => Instr::NotEquals(idx),
            Instr::New(idx) => Instr::New(idx),
            Instr::Delete => Instr::Delete,
            Instr::This => Instr::This,
            Instr::StartProfiling(_0, _1) => Instr::StartProfiling(_0, _1),
            Instr::ArrayClear(idx) => Instr::ArrayClear(idx),
            Instr::ArraySize(idx) => Instr::ArraySize(idx),
            Instr::ArrayResize(idx) => Instr::ArrayResize(idx),
            Instr::ArrayFindFirst(idx) => Instr::ArrayFindFirst(idx),
            Instr::ArrayFindFirstFast(idx) => Instr::ArrayFindFirstFast(idx),
            Instr::ArrayFindLast(idx) => Instr::ArrayFindLast(idx),
            Instr::ArrayFindLastFast(idx) => Instr::ArrayFindLastFast(idx),
            Instr::ArrayContains(idx) => Instr::ArrayContains(idx),
            Instr::ArrayContainsFast(idx) => Instr::ArrayContainsFast(idx),
            Instr::ArrayCount(idx) => Instr::ArrayCount(idx),
            Instr::ArrayCountFast(idx) => Instr::ArrayCountFast(idx),
            Instr::ArrayPush(idx) => Instr::ArrayPush(idx),
            Instr::ArrayPop(idx) => Instr::ArrayPop(idx),
            Instr::ArrayInsert(idx) => Instr::ArrayInsert(idx),
            Instr::ArrayRemove(idx) => Instr::ArrayRemove(idx),
            Instr::ArrayRemoveFast(idx) => Instr::ArrayRemoveFast(idx),
            Instr::ArrayGrow(idx) => Instr::ArrayGrow(idx),
            Instr::ArrayErase(idx) => Instr::ArrayErase(idx),
            Instr::ArrayEraseFast(idx) => Instr::ArrayEraseFast(idx),
            Instr::ArrayLast(idx) => Instr::ArrayLast(idx),
            Instr::ArrayElement(idx) => Instr::ArrayElement(idx),
            Instr::StaticArraySize(idx) => Instr::StaticArraySize(idx),
            Instr::StaticArrayFindFirst(idx) => Instr::StaticArrayFindFirst(idx),
            Instr::StaticArrayFindFirstFast(idx) => Instr::StaticArrayFindFirstFast(idx),
            Instr::StaticArrayFindLast(idx) => Instr::StaticArrayFindLast(idx),
            Instr::StaticArrayFindLastFast(idx) => Instr::StaticArrayFindLastFast(idx),
            Instr::StaticArrayContains(idx) => Instr::StaticArrayContains(idx),
            Instr::StaticArrayContainsFast(idx) => Instr::StaticArrayContainsFast(idx),
            Instr::StaticArrayCount(idx) => Instr::StaticArrayCount(idx),
            Instr::StaticArrayCountFast(idx) => Instr::StaticArrayCountFast(idx),
            Instr::StaticArrayLast(idx) => Instr::StaticArrayLast(idx),
            Instr::StaticArrayElement(idx) => Instr::StaticArrayElement(idx),
            Instr::RefToBool => Instr::RefToBool,
            Instr::WeakRefToBool => Instr::WeakRefToBool,
            Instr::EnumToI32(idx, size) => Instr::EnumToI32(idx, size),
            Instr::I32ToEnum(idx, size) => Instr::I32ToEnum(idx, size),
            Instr::DynamicCast(idx, size) => Instr::DynamicCast(idx, size),
            Instr::ToString(idx) => Instr::ToString(idx),
            Instr::ToVariant(idx) => Instr::ToVariant(idx),
            Instr::FromVariant(idx) => Instr::FromVariant(idx),
            Instr::VariantIsValid => Instr::VariantIsValid,
            Instr::VariantIsRef => Instr::VariantIsRef,
            Instr::VariantIsArray => Instr::VariantIsArray,
            Instr::VatiantToCName => Instr::VatiantToCName,
            Instr::VariantToString => Instr::VariantToString,
            Instr::WeakRefToRef => Instr::WeakRefToRef,
            Instr::RefToWeakRef => Instr::RefToWeakRef,
            Instr::WeakRefNull => Instr::WeakRefNull,
            Instr::AsRef(idx) => Instr::AsRef(idx),
            Instr::Deref(idx) => Instr::Deref(idx),
        }
    }
}

impl Decode for Instr<Offset> {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let code: u8 = input.decode()?;
        match code {
            0 => Ok(Instr::Nop),
            1 => Ok(Instr::Null),
            2 => Ok(Instr::I32One),
            3 => Ok(Instr::I32Zero),
            4 => Ok(Instr::I8Const(input.decode()?)),
            5 => Ok(Instr::I16Const(input.decode()?)),
            6 => Ok(Instr::I32Const(input.decode()?)),
            7 => Ok(Instr::I64Const(input.decode()?)),
            8 => Ok(Instr::U8Const(input.decode()?)),
            9 => Ok(Instr::U16Const(input.decode()?)),
            10 => Ok(Instr::U32Const(input.decode()?)),
            11 => Ok(Instr::U64Const(input.decode()?)),
            12 => Ok(Instr::F32Const(input.decode()?)),
            13 => Ok(Instr::F64Const(input.decode()?)),
            14 => Ok(Instr::NameConst(input.decode()?)),
            15 => Ok(Instr::EnumConst(input.decode()?, input.decode()?)),
            16 => Ok(Instr::StringConst(input.decode_str_prefixed::<u32>()?)),
            17 => Ok(Instr::TweakDbIdConst(input.decode()?)),
            18 => Ok(Instr::ResourceConst(input.decode()?)),
            19 => Ok(Instr::TrueConst),
            20 => Ok(Instr::FalseConst),
            21 => Ok(Instr::Breakpoint(
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
            )),
            22 => Ok(Instr::Assign),
            23 => Ok(Instr::Target(Offset::new(0))),
            24 => Ok(Instr::Local(input.decode()?)),
            25 => Ok(Instr::Param(input.decode()?)),
            26 => Ok(Instr::ObjectField(input.decode()?)),
            27 => Ok(Instr::ExternalVar),
            28 => Ok(Instr::Switch(input.decode()?, Offset::new(input.decode::<i16>()? + 11))),
            29 => Ok(Instr::SwitchLabel(
                Offset::new(input.decode::<i16>()? + 3),
                Offset::new(input.decode::<i16>()? + 5),
            )),
            30 => Ok(Instr::SwitchDefault),
            31 => Ok(Instr::Jump(Offset::new(input.decode::<i16>()? + 3))),
            32 => Ok(Instr::JumpIfFalse(Offset::new(input.decode::<i16>()? + 3))),
            33 => Ok(Instr::Skip(Offset::new(input.decode::<i16>()? + 3))),
            34 => Ok(Instr::Conditional(
                Offset::new(input.decode::<i16>()? + 3),
                Offset::new(input.decode::<i16>()? + 5),
            )),
            35 => Ok(Instr::Construct(input.decode()?, input.decode()?)),
            36 => Ok(Instr::InvokeStatic(
                Offset::new(input.decode::<i16>()? + 3),
                input.decode()?,
                input.decode()?,
            )),
            37 => Ok(Instr::InvokeVirtual(
                Offset::new(input.decode::<i16>()? + 3),
                input.decode()?,
                input.decode()?,
            )),
            38 => Ok(Instr::ParamEnd),
            39 => Ok(Instr::Return),
            40 => Ok(Instr::StructField(input.decode()?)),
            41 => Ok(Instr::Context(Offset::new(input.decode::<i16>()? + 3))),
            42 => Ok(Instr::Equals(input.decode()?)),
            43 => Ok(Instr::NotEquals(input.decode()?)),
            44 => Ok(Instr::New(input.decode()?)),
            45 => Ok(Instr::Delete),
            46 => Ok(Instr::This),
            47 => Ok(Instr::StartProfiling(
                input.decode_vec_prefixed::<u32, u8>()?,
                input.decode()?,
            )),
            48 => Ok(Instr::ArrayClear(input.decode()?)),
            49 => Ok(Instr::ArraySize(input.decode()?)),
            50 => Ok(Instr::ArrayResize(input.decode()?)),
            51 => Ok(Instr::ArrayFindFirst(input.decode()?)),
            52 => Ok(Instr::ArrayFindFirstFast(input.decode()?)),
            53 => Ok(Instr::ArrayFindLast(input.decode()?)),
            54 => Ok(Instr::ArrayFindLastFast(input.decode()?)),
            55 => Ok(Instr::ArrayContains(input.decode()?)),
            56 => Ok(Instr::ArrayContainsFast(input.decode()?)),
            57 => Ok(Instr::ArrayCount(input.decode()?)),
            58 => Ok(Instr::ArrayCountFast(input.decode()?)),
            59 => Ok(Instr::ArrayPush(input.decode()?)),
            60 => Ok(Instr::ArrayPop(input.decode()?)),
            61 => Ok(Instr::ArrayInsert(input.decode()?)),
            62 => Ok(Instr::ArrayRemove(input.decode()?)),
            63 => Ok(Instr::ArrayRemoveFast(input.decode()?)),
            64 => Ok(Instr::ArrayGrow(input.decode()?)),
            65 => Ok(Instr::ArrayErase(input.decode()?)),
            66 => Ok(Instr::ArrayEraseFast(input.decode()?)),
            67 => Ok(Instr::ArrayLast(input.decode()?)),
            68 => Ok(Instr::ArrayElement(input.decode()?)),
            69 => Ok(Instr::StaticArraySize(input.decode()?)),
            70 => Ok(Instr::StaticArrayFindFirst(input.decode()?)),
            71 => Ok(Instr::StaticArrayFindFirstFast(input.decode()?)),
            72 => Ok(Instr::StaticArrayFindLast(input.decode()?)),
            73 => Ok(Instr::StaticArrayFindLastFast(input.decode()?)),
            74 => Ok(Instr::StaticArrayContains(input.decode()?)),
            75 => Ok(Instr::StaticArrayContainsFast(input.decode()?)),
            76 => Ok(Instr::StaticArrayCount(input.decode()?)),
            77 => Ok(Instr::StaticArrayCountFast(input.decode()?)),
            78 => Ok(Instr::StaticArrayLast(input.decode()?)),
            79 => Ok(Instr::StaticArrayElement(input.decode()?)),
            80 => Ok(Instr::RefToBool),
            81 => Ok(Instr::WeakRefToBool),
            82 => Ok(Instr::EnumToI32(input.decode()?, input.decode()?)),
            83 => Ok(Instr::I32ToEnum(input.decode()?, input.decode()?)),
            84 => Ok(Instr::DynamicCast(input.decode()?, input.decode()?)),
            85 => Ok(Instr::ToString(input.decode()?)),
            86 => Ok(Instr::ToVariant(input.decode()?)),
            87 => Ok(Instr::FromVariant(input.decode()?)),
            88 => Ok(Instr::VariantIsValid),
            89 => Ok(Instr::VariantIsRef),
            90 => Ok(Instr::VariantIsArray),
            91 => Ok(Instr::VatiantToCName),
            92 => Ok(Instr::VariantToString),
            93 => Ok(Instr::WeakRefToRef),
            94 => Ok(Instr::RefToWeakRef),
            95 => Ok(Instr::WeakRefNull),
            96 => Ok(Instr::AsRef(input.decode()?)),
            97 => Ok(Instr::Deref(input.decode()?)),
            other => {
                let msg = format!("Invalid instruction code: {}", other);
                Err(io::Error::new(io::ErrorKind::InvalidData, msg))
            }
        }
    }
}

impl Encode for Instr<Offset> {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        match value {
            Instr::Nop => {
                output.encode(&0u8)?;
            }
            Instr::Null => {
                output.encode(&1u8)?;
            }
            Instr::I32One => {
                output.encode(&2u8)?;
            }
            Instr::I32Zero => {
                output.encode(&3u8)?;
            }
            Instr::I8Const(val) => {
                output.encode(&4u8)?;
                output.encode(val)?;
            }
            Instr::I16Const(val) => {
                output.encode(&5u8)?;
                output.encode(val)?;
            }
            Instr::I32Const(val) => {
                output.encode(&6u8)?;
                output.encode(val)?;
            }
            Instr::I64Const(val) => {
                output.encode(&7u8)?;
                output.encode(val)?;
            }
            Instr::U8Const(val) => {
                output.encode(&8u8)?;
                output.encode(val)?;
            }
            Instr::U16Const(val) => {
                output.encode(&9u8)?;
                output.encode(val)?;
            }
            Instr::U32Const(val) => {
                output.encode(&10u8)?;
                output.encode(val)?;
            }
            Instr::U64Const(val) => {
                output.encode(&11u8)?;
                output.encode(val)?;
            }
            Instr::F32Const(val) => {
                output.encode(&12u8)?;
                output.encode(val)?;
            }
            Instr::F64Const(val) => {
                output.encode(&13u8)?;
                output.encode(val)?;
            }
            Instr::NameConst(val) => {
                output.encode(&14u8)?;
                output.encode(val)?;
            }
            Instr::EnumConst(enum_, member) => {
                output.encode(&15u8)?;
                output.encode(enum_)?;
                output.encode(member)?;
            }
            Instr::StringConst(str) => {
                output.encode(&16u8)?;
                output.encode_str_prefixed::<u32>(str)?;
            }
            Instr::TweakDbIdConst(idx) => {
                output.encode(&17u8)?;
                output.encode(idx)?;
            }
            Instr::ResourceConst(idx) => {
                output.encode(&18u8)?;
                output.encode(idx)?;
            }
            Instr::TrueConst => {
                output.encode(&19u8)?;
            }
            Instr::FalseConst => {
                output.encode(&20u8)?;
            }
            Instr::Breakpoint(unk1, unk2, unk3, unk4, unk5, unk6) => {
                output.encode(&21u8)?;
                output.encode(unk1)?;
                output.encode(unk2)?;
                output.encode(unk3)?;
                output.encode(unk4)?;
                output.encode(unk5)?;
                output.encode(unk6)?;
            }
            Instr::Assign => {
                output.encode(&22u8)?;
            }
            Instr::Target(_) => {
                output.encode(&23u8)?;
            }
            Instr::Local(idx) => {
                output.encode(&24u8)?;
                output.encode(idx)?;
            }
            Instr::Param(idx) => {
                output.encode(&25u8)?;
                output.encode(idx)?;
            }
            Instr::ObjectField(idx) => {
                output.encode(&26u8)?;
                output.encode(idx)?;
            }
            Instr::ExternalVar => {
                output.encode(&27u8)?;
            }
            Instr::Switch(idx, offset) => {
                output.encode(&28u8)?;
                output.encode(idx)?;
                output.encode(&Offset::new(offset.value - 11))?;
            }
            Instr::SwitchLabel(start, exit) => {
                output.encode(&29u8)?;
                output.encode(&Offset::new(start.value - 3))?;
                output.encode(&Offset::new(exit.value - 5))?;
            }
            Instr::SwitchDefault => {
                output.encode(&30u8)?;
            }
            Instr::Jump(offset) => {
                output.encode(&31u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
            }
            Instr::JumpIfFalse(offset) => {
                output.encode(&32u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
            }
            Instr::Skip(offset) => {
                output.encode(&33u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
            }
            Instr::Conditional(if_true, if_false) => {
                output.encode(&34u8)?;
                output.encode(&Offset::new(if_true.value - 3))?;
                output.encode(&Offset::new(if_false.value - 5))?;
            }
            Instr::Construct(n_params, idx) => {
                output.encode(&35u8)?;
                output.encode(n_params)?;
                output.encode(idx)?;
            }
            Instr::InvokeStatic(offset, line, idx) => {
                output.encode(&36u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
                output.encode(line)?;
                output.encode(idx)?;
            }
            Instr::InvokeVirtual(offset, line, idx) => {
                output.encode(&37u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
                output.encode(line)?;
                output.encode(idx)?;
            }
            Instr::ParamEnd => {
                output.encode(&38u8)?;
            }
            Instr::Return => {
                output.encode(&39u8)?;
            }
            Instr::StructField(idx) => {
                output.encode(&40u8)?;
                output.encode(idx)?;
            }
            Instr::Context(offset) => {
                output.encode(&41u8)?;
                output.encode(&Offset::new(offset.value - 3))?;
            }
            Instr::Equals(idx) => {
                output.encode(&42u8)?;
                output.encode(idx)?;
            }
            Instr::NotEquals(idx) => {
                output.encode(&43u8)?;
                output.encode(idx)?;
            }
            Instr::New(idx) => {
                output.encode(&44u8)?;
                output.encode(idx)?;
            }
            Instr::Delete => {
                output.encode(&45u8)?;
            }
            Instr::This => {
                output.encode(&46u8)?;
            }
            Instr::StartProfiling(vec, byte) => {
                output.encode(&47u8)?;
                output.encode_slice_prefixed::<u32, u8>(vec)?;
                output.encode(byte)?;
            }
            Instr::ArrayClear(idx) => {
                output.encode(&48u8)?;
                output.encode(idx)?;
            }
            Instr::ArraySize(idx) => {
                output.encode(&49u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayResize(idx) => {
                output.encode(&50u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayFindFirst(idx) => {
                output.encode(&51u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayFindFirstFast(idx) => {
                output.encode(&52u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayFindLast(idx) => {
                output.encode(&53u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayFindLastFast(idx) => {
                output.encode(&54u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayContains(idx) => {
                output.encode(&55u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayContainsFast(idx) => {
                output.encode(&56u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayCount(idx) => {
                output.encode(&57u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayCountFast(idx) => {
                output.encode(&58u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayPush(idx) => {
                output.encode(&59u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayPop(idx) => {
                output.encode(&60u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayInsert(idx) => {
                output.encode(&61u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayRemove(idx) => {
                output.encode(&62u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayRemoveFast(idx) => {
                output.encode(&63u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayGrow(idx) => {
                output.encode(&64u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayErase(idx) => {
                output.encode(&65u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayEraseFast(idx) => {
                output.encode(&66u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayLast(idx) => {
                output.encode(&67u8)?;
                output.encode(idx)?;
            }
            Instr::ArrayElement(idx) => {
                output.encode(&68u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArraySize(idx) => {
                output.encode(&69u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayFindFirst(idx) => {
                output.encode(&70u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayFindFirstFast(idx) => {
                output.encode(&71u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayFindLast(idx) => {
                output.encode(&72u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayFindLastFast(idx) => {
                output.encode(&73u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayContains(idx) => {
                output.encode(&74u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayContainsFast(idx) => {
                output.encode(&75u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayCount(idx) => {
                output.encode(&76u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayCountFast(idx) => {
                output.encode(&77u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayLast(idx) => {
                output.encode(&78u8)?;
                output.encode(idx)?;
            }
            Instr::StaticArrayElement(idx) => {
                output.encode(&79u8)?;
                output.encode(idx)?;
            }
            Instr::RefToBool => {
                output.encode(&80u8)?;
            }
            Instr::WeakRefToBool => {
                output.encode(&81u8)?;
            }
            Instr::EnumToI32(idx, byte) => {
                output.encode(&82u8)?;
                output.encode(idx)?;
                output.encode(byte)?;
            }
            Instr::I32ToEnum(idx, byte) => {
                output.encode(&83u8)?;
                output.encode(idx)?;
                output.encode(byte)?;
            }
            Instr::DynamicCast(idx, byte) => {
                output.encode(&84u8)?;
                output.encode(idx)?;
                output.encode(byte)?;
            }
            Instr::ToString(idx) => {
                output.encode(&85u8)?;
                output.encode(idx)?;
            }
            Instr::ToVariant(idx) => {
                output.encode(&86u8)?;
                output.encode(idx)?;
            }
            Instr::FromVariant(idx) => {
                output.encode(&87u8)?;
                output.encode(idx)?;
            }
            Instr::VariantIsValid => {
                output.encode(&88u8)?;
            }
            Instr::VariantIsRef => {
                output.encode(&89u8)?;
            }
            Instr::VariantIsArray => {
                output.encode(&90u8)?;
            }
            Instr::VatiantToCName => {
                output.encode(&91u8)?;
            }
            Instr::VariantToString => {
                output.encode(&92u8)?;
            }
            Instr::WeakRefToRef => {
                output.encode(&93u8)?;
            }
            Instr::RefToWeakRef => {
                output.encode(&94u8)?;
            }
            Instr::WeakRefNull => {
                output.encode(&95u8)?;
            }
            Instr::AsRef(idx) => {
                output.encode(&96u8)?;
                output.encode(idx)?;
            }
            Instr::Deref(idx) => {
                output.encode(&97u8)?;
                output.encode(idx)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub value: u16,
}

impl Location {
    pub const MAX: Location = Location { value: std::u16::MAX };

    pub fn new(value: u16) -> Location {
        Location { value }
    }

    pub fn relative(&self, base: Location) -> Offset {
        Offset::new(self.value as i16 - base.value as i16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset {
    pub value: i16,
}

impl Offset {
    pub fn new(value: i16) -> Offset {
        Offset { value }
    }

    pub fn absolute(&self, position: Location) -> Location {
        Location::new((position.value as i32 + self.value as i32) as u16)
    }
}

impl Decode for Offset {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        Ok(Offset::new(input.decode()?))
    }
}

impl Encode for Offset {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        output.encode(&value.value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
    pub index: usize,
}

#[derive(Debug, PartialEq)]
pub struct Code<L>(pub Vec<Instr<L>>);

impl<L: Clone> Code<L> {
    pub const EMPTY: Self = Code(vec![]);

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn cursor(&self) -> CodeCursor<L> {
        CodeCursor::new(self)
    }
}

impl Decode for Code<Offset> {
    fn decode<I: io::Read>(input: &mut I) -> io::Result<Self> {
        let max_offset: u32 = input.decode()?;
        let mut offset = 0;
        let mut code = Vec::new();
        while offset < max_offset {
            let instr: Instr<Offset> = input.decode()?;
            offset += instr.size() as u32;
            code.push(instr);
        }
        Ok(Code(code))
    }
}

impl Encode for Code<Offset> {
    fn encode<O: io::Write>(output: &mut O, value: &Self) -> io::Result<()> {
        let mut buffer = io::Cursor::new(Vec::new());
        let mut size = 0u32;
        for instr in &value.0 {
            buffer.encode(instr)?;
            size += instr.size() as u32;
        }
        output.encode(&size)?;
        output.write_all(buffer.get_ref())
    }
}

pub struct CodeCursor<'a, L> {
    code: &'a [Instr<L>],
    position: Location,
    index: u16,
}

impl<'a, L: Clone> CodeCursor<'a, L> {
    pub fn new(code: &'a Code<L>) -> Self {
        CodeCursor {
            code: &code.0,
            position: Location { value: 0 },
            index: 0,
        }
    }

    pub fn pop(&mut self) -> Result<Instr<L>, Error> {
        let instr = self
            .code
            .get(self.index as usize)
            .ok_or_else(|| Error::eof(format!("Attempted to read past eof: {}", self.position.value)))?;
        self.index += 1;
        self.position = Location::new(self.position.value + instr.size());
        Ok(instr.clone())
    }

    pub fn peek(&self) -> Option<Instr<L>> {
        self.code.get(self.index as usize).cloned()
    }

    pub fn goto(&mut self, position: Location) -> Result<(), Error> {
        self.reset();
        while self.position < position {
            self.pop()?;
        }
        assert_eq!(self.position, position);
        Ok(())
    }

    pub fn seek(&mut self, offset: Offset) -> Result<(), Error> {
        self.goto(offset.absolute(self.position))
    }

    pub fn reset(&mut self) {
        self.index = 0;
        self.position = Location { value: 0 };
    }

    pub fn pos(&self) -> Location {
        self.position
    }
}

impl<'a, L: Clone> Iterator for CodeCursor<'a, L> {
    type Item = (Location, Instr<L>);

    fn next(&mut self) -> Option<Self::Item> {
        let position = self.pos();
        self.pop().ok().map(|instr| (position, instr))
    }
}

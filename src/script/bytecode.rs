use crate::decode::{Decode, DecodeExt};
use crate::error::Error;
use crate::script::bundle::{PoolIndex, Resource, TweakDbIndex};
use crate::script::definition::Definition;

use std::io;

#[derive(Debug)]
pub enum Instr {
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
    EnumConst(PoolIndex<Definition>, PoolIndex<Definition>),
    StringConst(Vec<u8>),
    TweakDBIdConst(PoolIndex<TweakDbIndex>),
    ResourceConst(PoolIndex<Resource>),
    TrueConst,
    FalseConst,
    Unk1(u16, u32, u16, u16, u8, u64),
    Assign,
    Target,
    Local(PoolIndex<Definition>),
    Param(PoolIndex<Definition>),
    ObjectField(PoolIndex<Definition>),
    Unk2,
    Switch(PoolIndex<Definition>, i16),
    SwitchLabel(i16, i16),
    SwitchDefault,
    Jump(i16),
    JumpIfFalse(i16),
    Skip(i16),
    Conditional(i16, i16),
    Construct(u8, PoolIndex<Definition>),
    InvokeStatic(i16, u16, PoolIndex<Definition>),
    InvokeVirtual(i16, u16, PoolIndex<String>),
    ParamEnd,
    Return,
    StructField(PoolIndex<Definition>),
    Context(i16),
    Equals(PoolIndex<Definition>),
    NotEquals(PoolIndex<Definition>),
    New(PoolIndex<Definition>),
    Delete,
    This,
    Unk3(Vec<u8>, u8),
    ArrayClear(PoolIndex<Definition>),
    ArraySize(PoolIndex<Definition>),
    ArrayResize(PoolIndex<Definition>),
    ArrayFindFirst(PoolIndex<Definition>),
    ArrayFindFirstFast(PoolIndex<Definition>),
    ArrayFindLast(PoolIndex<Definition>),
    ArrayFindLastFast(PoolIndex<Definition>),
    ArrayContains(PoolIndex<Definition>),
    ArrayContainsFast(PoolIndex<Definition>),
    Unk4(PoolIndex<Definition>),
    Unk5(PoolIndex<Definition>),
    ArrayPush(PoolIndex<Definition>),
    ArrayPop(PoolIndex<Definition>),
    ArrayInsert(PoolIndex<Definition>),
    ArrayRemove(PoolIndex<Definition>),
    ArrayRemoveFast(PoolIndex<Definition>),
    ArrayGrow(PoolIndex<Definition>),
    ArrayErase(PoolIndex<Definition>),
    ArrayEraseFast(PoolIndex<Definition>),
    ArrayLast(PoolIndex<Definition>),
    ArrayElement(PoolIndex<Definition>),
    StaticArraySize(PoolIndex<Definition>),
    StaticArrayFindFirst(PoolIndex<Definition>),
    StaticArrayFindFirstFast(PoolIndex<Definition>),
    StaticArrayFindLast(PoolIndex<Definition>),
    StaticArrayFindLastFast(PoolIndex<Definition>),
    StaticArrayContains(PoolIndex<Definition>),
    StaticArrayContainsFast(PoolIndex<Definition>),
    Unk6(PoolIndex<Definition>),
    Unk7(PoolIndex<Definition>),
    StaticArrayLast(PoolIndex<Definition>),
    StaticArrayElement(PoolIndex<Definition>),
    HandleToBool,
    WeakHandleToBool,
    EnumToI32(PoolIndex<Definition>, u8),
    I32ToEnum(PoolIndex<Definition>, u8),
    DynamicCast(PoolIndex<Definition>, u8),
    ToString(PoolIndex<Definition>),
    ToVariant(PoolIndex<Definition>),
    FromVariant(PoolIndex<Definition>),
    VariantIsValid,
    VariantIsHandle,
    VariantIsArray,
    Unk8,
    VariantToString,
    WeakHandleToHandle,
    HandleToWeakHandle,
    WeakHandleNull,
    ToScriptRef(PoolIndex<Definition>),
    FromScriptRef(PoolIndex<Definition>),
    Unk9,
}

impl Instr {
    pub fn size(&self) -> u16 {
        match self {
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
            Instr::TweakDBIdConst(_) => 8,
            Instr::ResourceConst(_) => 8,
            Instr::Unk1(_, _, _, _, _, _) => 19,
            Instr::Assign => 0,
            Instr::Target => 0,
            Instr::Local(_) => 8,
            Instr::Param(_) => 8,
            Instr::ObjectField(_) => 8,
            Instr::Unk2 => 0,
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
            Instr::Unk3(bytes, _) => 5 + bytes.len() as u16,
            Instr::ArrayClear(_) => 8,
            Instr::ArraySize(_) => 8,
            Instr::ArrayResize(_) => 8,
            Instr::ArrayFindFirst(_) => 8,
            Instr::ArrayFindFirstFast(_) => 8,
            Instr::ArrayFindLast(_) => 8,
            Instr::ArrayFindLastFast(_) => 8,
            Instr::ArrayContains(_) => 8,
            Instr::ArrayContainsFast(_) => 8,
            Instr::Unk4(_) => 8,
            Instr::Unk5(_) => 8,
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
            Instr::Unk6(_) => 8,
            Instr::Unk7(_) => 8,
            Instr::StaticArrayLast(_) => 8,
            Instr::StaticArrayElement(_) => 8,
            Instr::HandleToBool => 0,
            Instr::WeakHandleToBool => 0,
            Instr::EnumToI32(_, _) => 9,
            Instr::I32ToEnum(_, _) => 9,
            Instr::DynamicCast(_, _) => 9,
            Instr::ToString(_) => 8,
            Instr::ToVariant(_) => 8,
            Instr::FromVariant(_) => 8,
            Instr::VariantIsValid => 0,
            Instr::VariantIsHandle => 0,
            Instr::VariantIsArray => 0,
            Instr::Unk8 => 0,
            Instr::VariantToString => 0,
            Instr::WeakHandleToHandle => 0,
            Instr::HandleToWeakHandle => 0,
            Instr::WeakHandleNull => 0,
            Instr::ToScriptRef(_) => 8,
            Instr::FromScriptRef(_) => 8,
            Instr::Unk9 => 0,
        }
    }
}

impl Decode for Instr {
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
            16 => Ok(Instr::StringConst(input.decode_vec_prefixed::<u32, u8>()?)),
            17 => Ok(Instr::TweakDBIdConst(input.decode()?)),
            18 => Ok(Instr::ResourceConst(input.decode()?)),
            19 => Ok(Instr::TrueConst),
            20 => Ok(Instr::FalseConst),
            21 => Ok(Instr::Unk1(
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
                input.decode()?,
            )),
            22 => Ok(Instr::Assign),
            23 => Ok(Instr::Target),
            24 => Ok(Instr::Local(input.decode()?)),
            25 => Ok(Instr::Param(input.decode()?)),
            26 => Ok(Instr::ObjectField(input.decode()?)),
            27 => Ok(Instr::Unk2),
            28 => Ok(Instr::Switch(input.decode()?, input.decode::<i16>()? + 9)),
            29 => Ok(Instr::SwitchLabel(
                input.decode::<i16>()? + 3,
                input.decode::<i16>()? + 5,
            )),
            30 => Ok(Instr::SwitchDefault),
            31 => Ok(Instr::Jump(input.decode::<i16>()? + 3)),
            32 => Ok(Instr::JumpIfFalse(input.decode::<i16>()? + 3)),
            33 => Ok(Instr::Skip(input.decode::<i16>()? + 3)),
            34 => Ok(Instr::Conditional(
                input.decode::<i16>()? + 3,
                input.decode::<i16>()? + 5,
            )),
            35 => Ok(Instr::Construct(input.decode()?, input.decode()?)),
            36 => Ok(Instr::InvokeStatic(input.decode()?, input.decode()?, input.decode()?)),
            37 => Ok(Instr::InvokeVirtual(input.decode()?, input.decode()?, input.decode()?)),
            38 => Ok(Instr::ParamEnd),
            39 => Ok(Instr::Return),
            40 => Ok(Instr::StructField(input.decode()?)),
            41 => Ok(Instr::Context(input.decode::<i16>()? + 3)),
            42 => Ok(Instr::Equals(input.decode()?)),
            43 => Ok(Instr::NotEquals(input.decode()?)),
            44 => Ok(Instr::New(input.decode()?)),
            45 => Ok(Instr::Delete),
            46 => Ok(Instr::This),
            47 => Ok(Instr::Unk3(input.decode_vec_prefixed::<u32, u8>()?, input.decode()?)),
            48 => Ok(Instr::ArrayClear(input.decode()?)),
            49 => Ok(Instr::ArraySize(input.decode()?)),
            50 => Ok(Instr::ArrayResize(input.decode()?)),
            51 => Ok(Instr::ArrayFindFirst(input.decode()?)),
            52 => Ok(Instr::ArrayFindFirstFast(input.decode()?)),
            53 => Ok(Instr::ArrayFindLast(input.decode()?)),
            54 => Ok(Instr::ArrayFindLastFast(input.decode()?)),
            55 => Ok(Instr::ArrayContains(input.decode()?)),
            56 => Ok(Instr::ArrayContainsFast(input.decode()?)),
            57 => Ok(Instr::Unk4(input.decode()?)),
            58 => Ok(Instr::Unk5(input.decode()?)),
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
            76 => Ok(Instr::Unk6(input.decode()?)),
            77 => Ok(Instr::Unk7(input.decode()?)),
            78 => Ok(Instr::StaticArrayLast(input.decode()?)),
            79 => Ok(Instr::StaticArrayElement(input.decode()?)),
            80 => Ok(Instr::HandleToBool),
            81 => Ok(Instr::WeakHandleToBool),
            82 => Ok(Instr::EnumToI32(input.decode()?, input.decode()?)),
            83 => Ok(Instr::I32ToEnum(input.decode()?, input.decode()?)),
            84 => Ok(Instr::DynamicCast(input.decode()?, input.decode()?)),
            85 => Ok(Instr::ToString(input.decode()?)),
            86 => Ok(Instr::ToVariant(input.decode()?)),
            87 => Ok(Instr::FromVariant(input.decode()?)),
            88 => Ok(Instr::VariantIsValid),
            89 => Ok(Instr::VariantIsHandle),
            90 => Ok(Instr::VariantIsArray),
            91 => Ok(Instr::Unk8),
            92 => Ok(Instr::VariantToString),
            93 => Ok(Instr::WeakHandleToHandle),
            94 => Ok(Instr::HandleToWeakHandle),
            95 => Ok(Instr::WeakHandleNull),
            96 => Ok(Instr::ToScriptRef(input.decode()?)),
            97 => Ok(Instr::FromScriptRef(input.decode()?)),
            98 => Ok(Instr::Unk9),
            other => {
                let msg = format!("Invalid instruction code: {}", other);
                Err(io::Error::new(io::ErrorKind::InvalidData, msg))
            }
        }
    }
}

pub struct BytecodeReader<'a> {
    bytecode: io::Cursor<&'a [u8]>,
    length: usize,
    code_offset: u16,
}

impl<'a> BytecodeReader<'a> {
    pub fn new(body: &'a [u8]) -> BytecodeReader<'a> {
        BytecodeReader {
            bytecode: io::Cursor::new(body),
            length: body.len(),
            code_offset: 0,
        }
    }

    pub fn pop(&mut self) -> Result<Instr, Error> {
        if self.bytecode.position() >= self.length as u64 {
            let msg = format!("Attempted to read at past eof: {}", self.bytecode.position());
            Err(Error::eof(msg))
        } else {
            let instr = self.bytecode.decode::<Instr>()?;
            self.code_offset += 1 + instr.size();
            Ok(instr)
        }
    }

    pub fn peek(&mut self) -> Option<Instr> {
        let old_offset = self.code_offset;
        let old_cursor = self.bytecode.position();
        let instr = self.pop().ok();
        self.code_offset = old_offset;
        self.bytecode.set_position(old_cursor);
        instr
    }

    pub fn set_position(&mut self, position: u16) -> Result<(), Error> {
        self.reset();
        while self.code_offset < position {
            self.pop()?;
        }
        assert_eq!(self.code_offset, position);
        Ok(())
    }

    pub fn seek(&mut self, offset: i32) -> Result<(), Error> {
        let target = (self.code_offset as i32 + offset) as u16;
        self.set_position(target)
    }

    pub fn reset(&mut self) {
        self.code_offset = 0;
        self.bytecode.set_position(0);
    }

    pub fn position(&mut self) -> u16 {
        self.code_offset
    }
}

impl<'a> Iterator for BytecodeReader<'a> {
    type Item = (u16, Instr);

    fn next(&mut self) -> Option<Self::Item> {
        let position = self.position();
        self.pop().ok().map(|instr| (position, instr))
    }
}

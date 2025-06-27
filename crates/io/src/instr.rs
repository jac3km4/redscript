use std::fmt;
use std::ops::{Add, Sub};

use byte::{Measure, TryRead, TryWrite};

use crate::util::Prefixed;
use crate::{
    CNameIndex, ClassIndex, EnumIndex, EnumValueIndex, FieldIndex, FunctionIndex, LocalIndex,
    ParameterIndex, ResourceIndex, StringIndex, TweakDbIndex, TypeIndex,
};

#[derive(Debug, Clone, PartialEq, TryRead, TryWrite, Measure)]
#[byte(tag_type = u8)]
pub enum Instr<Loc = Offset> {
    #[byte(tag = 0x00)]
    Nop,
    #[byte(tag = 0x01)]
    Null,
    #[byte(tag = 0x02)]
    I32One,
    #[byte(tag = 0x03)]
    I32Zero,
    #[byte(tag = 0x04)]
    I8Const(i8),
    #[byte(tag = 0x05)]
    I16Const(i16),
    #[byte(tag = 0x06)]
    I32Const(i32),
    #[byte(tag = 0x07)]
    I64Const(i64),
    #[byte(tag = 0x08)]
    U8Const(u8),
    #[byte(tag = 0x09)]
    U16Const(u16),
    #[byte(tag = 0x0A)]
    U32Const(u32),
    #[byte(tag = 0x0B)]
    U64Const(u64),
    #[byte(tag = 0x0C)]
    F32Const(f32),
    #[byte(tag = 0x0D)]
    F64Const(f64),
    #[byte(tag = 0x0E)]
    CNameConst(CNameIndex),
    #[byte(tag = 0x0F)]
    EnumConst {
        enum_: EnumIndex,
        value: EnumValueIndex,
    },
    #[byte(tag = 0x10)]
    StringConst(StringIndex),
    #[byte(tag = 0x11)]
    TweakDbIdConst(TweakDbIndex),
    #[byte(tag = 0x12)]
    ResourceConst(ResourceIndex),
    #[byte(tag = 0x13)]
    TrueConst,
    #[byte(tag = 0x14)]
    FalseConst,
    #[byte(tag = 0x15)]
    Breakpoint(Box<Breakpoint>),
    #[byte(tag = 0x16)]
    Assign,
    #[byte(tag = 0x17)]
    Target(Loc),
    #[byte(tag = 0x18)]
    Local(LocalIndex),
    #[byte(tag = 0x19)]
    Param(ParameterIndex),
    #[byte(tag = 0x1A)]
    ObjectField(FieldIndex),
    #[byte(tag = 0x1B)]
    ExternalVar,
    #[byte(tag = 0x1C)]
    Switch(Switch<Loc>),
    #[byte(tag = 0x1D)]
    SwitchLabel(SwitchLabel<Loc>),
    #[byte(tag = 0x1E)]
    SwitchDefault,
    #[byte(tag = 0x1F)]
    Jump(Jump<Loc>),
    #[byte(tag = 0x20)]
    JumpIfFalse(Jump<Loc>),
    #[byte(tag = 0x21)]
    Skip(Jump<Loc>),
    #[byte(tag = 0x22)]
    Conditional(Conditional<Loc>),
    #[byte(tag = 0x23)]
    Construct { arg_count: u8, class: ClassIndex },
    #[byte(tag = 0x24)]
    InvokeStatic {
        exit: Jump<Loc>,
        line: u16,
        function: FunctionIndex,
        flags: InvokeFlags,
    },
    #[byte(tag = 0x25)]
    InvokeVirtual {
        exit: Jump<Loc>,
        line: u16,
        function: CNameIndex,
        flags: InvokeFlags,
    },
    #[byte(tag = 0x26)]
    ParamEnd,
    #[byte(tag = 0x27)]
    Return,
    #[byte(tag = 0x28)]
    StructField(FieldIndex),
    #[byte(tag = 0x29)]
    Context(Jump<Loc>),
    #[byte(tag = 0x2A)]
    Equals(TypeIndex),
    #[byte(tag = 0x2B)]
    RefStringEqualsString(TypeIndex),
    #[byte(tag = 0x2C)]
    StringEqualsRefString(TypeIndex),
    #[byte(tag = 0x2D)]
    NotEquals(TypeIndex),
    #[byte(tag = 0x2E)]
    RefStringNotEqualsString(TypeIndex),
    #[byte(tag = 0x2F)]
    StringNotEqualsRefString(TypeIndex),
    #[byte(tag = 0x30)]
    New(ClassIndex),
    #[byte(tag = 0x31)]
    Delete,
    #[byte(tag = 0x32)]
    This,
    #[byte(tag = 0x33)]
    Profile(Box<Profile>),
    #[byte(tag = 0x34)]
    ArrayClear(TypeIndex),
    #[byte(tag = 0x35)]
    ArraySize(TypeIndex),
    #[byte(tag = 0x36)]
    ArrayResize(TypeIndex),
    #[byte(tag = 0x37)]
    ArrayFindFirst(TypeIndex),
    #[byte(tag = 0x38)]
    ArrayFindFirstFast(TypeIndex),
    #[byte(tag = 0x39)]
    ArrayFindLast(TypeIndex),
    #[byte(tag = 0x3A)]
    ArrayFindLastFast(TypeIndex),
    #[byte(tag = 0x3B)]
    ArrayContains(TypeIndex),
    #[byte(tag = 0x3C)]
    ArrayContainsFast(TypeIndex),
    #[byte(tag = 0x3D)]
    ArrayCount(TypeIndex),
    #[byte(tag = 0x3E)]
    ArrayCountFast(TypeIndex),
    #[byte(tag = 0x3F)]
    ArrayPush(TypeIndex),
    #[byte(tag = 0x40)]
    ArrayPop(TypeIndex),
    #[byte(tag = 0x41)]
    ArrayInsert(TypeIndex),
    #[byte(tag = 0x42)]
    ArrayRemove(TypeIndex),
    #[byte(tag = 0x43)]
    ArrayRemoveFast(TypeIndex),
    #[byte(tag = 0x44)]
    ArrayGrow(TypeIndex),
    #[byte(tag = 0x45)]
    ArrayErase(TypeIndex),
    #[byte(tag = 0x46)]
    ArrayEraseFast(TypeIndex),
    #[byte(tag = 0x47)]
    ArrayLast(TypeIndex),
    #[byte(tag = 0x48)]
    ArrayElement(TypeIndex),
    #[byte(tag = 0x49)]
    ArraySort(TypeIndex),
    #[byte(tag = 0x4A)]
    ArraySortByPredicate(TypeIndex),
    #[byte(tag = 0x4B)]
    StaticArraySize(TypeIndex),
    #[byte(tag = 0x4C)]
    StaticArrayFindFirst(TypeIndex),
    #[byte(tag = 0x4D)]
    StaticArrayFindFirstFast(TypeIndex),
    #[byte(tag = 0x4E)]
    StaticArrayFindLast(TypeIndex),
    #[byte(tag = 0x4F)]
    StaticArrayFindLastFast(TypeIndex),
    #[byte(tag = 0x50)]
    StaticArrayContains(TypeIndex),
    #[byte(tag = 0x51)]
    StaticArrayContainsFast(TypeIndex),
    #[byte(tag = 0x52)]
    StaticArrayCount(TypeIndex),
    #[byte(tag = 0x53)]
    StaticArrayCountFast(TypeIndex),
    #[byte(tag = 0x54)]
    StaticArrayLast(TypeIndex),
    #[byte(tag = 0x55)]
    StaticArrayElement(TypeIndex),
    #[byte(tag = 0x56)]
    RefToBool,
    #[byte(tag = 0x57)]
    WeakRefToBool,
    #[byte(tag = 0x58)]
    EnumToI32 { enum_type: TypeIndex, size: u8 },
    #[byte(tag = 0x59)]
    I32ToEnum { enum_type: TypeIndex, size: u8 },
    #[byte(tag = 0x5A)]
    DynamicCast { class: ClassIndex, is_weak: bool },
    #[byte(tag = 0x5B)]
    ToString(TypeIndex),
    #[byte(tag = 0x5C)]
    ToVariant(TypeIndex),
    #[byte(tag = 0x5D)]
    FromVariant(TypeIndex),
    #[byte(tag = 0x5E)]
    VariantIsDefined,
    #[byte(tag = 0x5F)]
    VariantIsRef,
    #[byte(tag = 0x60)]
    VariantIsArray,
    #[byte(tag = 0x61)]
    VariantTypeName,
    #[byte(tag = 0x62)]
    VariantToString,
    #[byte(tag = 0x63)]
    WeakRefToRef,
    #[byte(tag = 0x64)]
    RefToWeakRef,
    #[byte(tag = 0x65)]
    WeakRefNull,
    #[byte(tag = 0x66)]
    AsRef(TypeIndex),
    #[byte(tag = 0x67)]
    Deref(TypeIndex),
}

impl<L> Instr<L> {
    pub fn virtual_size(&self) -> u16 {
        let op_size = match self {
            Instr::Breakpoint(_) => 19,
            Instr::EnumConst { .. } => 16,
            Instr::InvokeStatic { .. } | Instr::InvokeVirtual { .. } => 14,
            Instr::Switch { .. } => 10,
            Instr::Construct { .. }
            | Instr::EnumToI32 { .. }
            | Instr::I32ToEnum { .. }
            | Instr::DynamicCast { .. } => 9,
            Instr::I64Const(_)
            | Instr::U64Const(_)
            | Instr::F64Const(_)
            | Instr::CNameConst(_)
            | Instr::TweakDbIdConst(_)
            | Instr::ResourceConst(_)
            | Instr::Local(_)
            | Instr::Param(_)
            | Instr::ObjectField(_)
            | Instr::StructField(_)
            | Instr::Equals(_)
            | Instr::RefStringEqualsString(_)
            | Instr::StringEqualsRefString(_)
            | Instr::NotEquals(_)
            | Instr::RefStringNotEqualsString(_)
            | Instr::StringNotEqualsRefString(_)
            | Instr::New(_)
            | Instr::ToString(_)
            | Instr::ToVariant(_)
            | Instr::FromVariant(_)
            | Instr::AsRef(_)
            | Instr::Deref(_)
            | Instr::ArrayClear(_)
            | Instr::ArraySize(_)
            | Instr::ArrayResize(_)
            | Instr::ArrayFindFirst(_)
            | Instr::ArrayFindFirstFast(_)
            | Instr::ArrayFindLast(_)
            | Instr::ArrayFindLastFast(_)
            | Instr::ArrayContains(_)
            | Instr::ArrayContainsFast(_)
            | Instr::ArrayCount(_)
            | Instr::ArrayCountFast(_)
            | Instr::ArrayPush(_)
            | Instr::ArrayPop(_)
            | Instr::ArrayInsert(_)
            | Instr::ArrayRemove(_)
            | Instr::ArrayRemoveFast(_)
            | Instr::ArrayGrow(_)
            | Instr::ArrayErase(_)
            | Instr::ArrayEraseFast(_)
            | Instr::ArrayLast(_)
            | Instr::ArrayElement(_)
            | Instr::ArraySort(_)
            | Instr::ArraySortByPredicate(_)
            | Instr::StaticArraySize(_)
            | Instr::StaticArrayFindFirst(_)
            | Instr::StaticArrayFindFirstFast(_)
            | Instr::StaticArrayFindLast(_)
            | Instr::StaticArrayFindLastFast(_)
            | Instr::StaticArrayContains(_)
            | Instr::StaticArrayContainsFast(_)
            | Instr::StaticArrayCount(_)
            | Instr::StaticArrayCountFast(_)
            | Instr::StaticArrayLast(_)
            | Instr::StaticArrayElement(_) => 8,
            Instr::I32Const(_)
            | Instr::U32Const(_)
            | Instr::F32Const(_)
            | Instr::StringConst(_)
            | Instr::SwitchLabel { .. }
            | Instr::Conditional { .. } => 4,
            Instr::I16Const(_)
            | Instr::U16Const(_)
            | Instr::Jump(_)
            | Instr::JumpIfFalse(_)
            | Instr::Skip(_)
            | Instr::Context(_) => 2,
            Instr::I8Const(_) | Instr::U8Const(_) => 1,
            Instr::Nop
            | Instr::Null
            | Instr::I32One
            | Instr::I32Zero
            | Instr::TrueConst
            | Instr::FalseConst
            | Instr::Assign
            | Instr::ExternalVar
            | Instr::SwitchDefault
            | Instr::ParamEnd
            | Instr::Return
            | Instr::Delete
            | Instr::This
            | Instr::RefToBool
            | Instr::WeakRefToBool
            | Instr::VariantIsDefined
            | Instr::VariantIsRef
            | Instr::VariantIsArray
            | Instr::VariantTypeName
            | Instr::VariantToString
            | Instr::WeakRefToRef
            | Instr::RefToWeakRef
            | Instr::WeakRefNull => 0,
            // variable size
            Instr::Profile(instr) => 5 + instr.function.len() as u16,
            // not present in bytecode
            Instr::Target(_) => return 0,
        };
        1 + op_size
    }

    pub fn map_labels(self, f: impl Fn(L) -> Option<Offset>) -> Option<Instr> {
        let res = match self {
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
            Instr::CNameConst(idx) => Instr::CNameConst(idx),
            Instr::EnumConst { enum_, value } => Instr::EnumConst { enum_, value },
            Instr::StringConst(idx) => Instr::StringConst(idx),
            Instr::TweakDbIdConst(idx) => Instr::TweakDbIdConst(idx),
            Instr::ResourceConst(idx) => Instr::ResourceConst(idx),
            Instr::TrueConst => Instr::TrueConst,
            Instr::FalseConst => Instr::FalseConst,
            Instr::Breakpoint(bp) => Instr::Breakpoint(bp),
            Instr::Assign => Instr::Assign,
            Instr::Target(label) => Instr::Target(f(label)?),
            Instr::Local(idx) => Instr::Local(idx),
            Instr::Param(idx) => Instr::Param(idx),
            Instr::ObjectField(idx) => Instr::ObjectField(idx),
            Instr::ExternalVar => Instr::ExternalVar,
            Instr::Switch(instr) => Instr::Switch(Switch::new_with_offset(
                instr.expr_type,
                f(instr.first_case)?,
            )),
            Instr::SwitchLabel(instr) => Instr::SwitchLabel(SwitchLabel::new_with_offset(
                f(instr.next_case)?,
                f(instr.body)?,
            )),
            Instr::SwitchDefault => Instr::SwitchDefault,
            Instr::Jump(label) => Instr::Jump(Jump::new_with_offset(f(label.target)?)),
            Instr::JumpIfFalse(label) => {
                Instr::JumpIfFalse(Jump::new_with_offset(f(label.target)?))
            }
            Instr::Skip(label) => Instr::Skip(Jump::new_with_offset(f(label.target)?)),
            Instr::Conditional(instr) => Instr::Conditional(Conditional::new_with_offset(
                f(instr.false_label)?,
                f(instr.exit)?,
            )),
            Instr::Construct { arg_count, class } => Instr::Construct { arg_count, class },
            Instr::InvokeStatic {
                exit,
                line,
                function,
                flags,
            } => Instr::InvokeStatic {
                exit: Jump::new_with_offset(f(exit.target)?),
                line,
                function,
                flags,
            },
            Instr::InvokeVirtual {
                exit,
                line,
                function,
                flags,
            } => Instr::InvokeVirtual {
                exit: Jump::new_with_offset(f(exit.target)?),
                line,
                function,
                flags,
            },
            Instr::ParamEnd => Instr::ParamEnd,
            Instr::Return => Instr::Return,
            Instr::StructField(idx) => Instr::StructField(idx),
            Instr::Context(label) => Instr::Context(Jump::new_with_offset(f(label.target)?)),
            Instr::Equals(idx) => Instr::Equals(idx),
            Instr::RefStringEqualsString(idx) => Instr::RefStringEqualsString(idx),
            Instr::StringEqualsRefString(idx) => Instr::StringEqualsRefString(idx),
            Instr::NotEquals(idx) => Instr::NotEquals(idx),
            Instr::RefStringNotEqualsString(idx) => Instr::RefStringNotEqualsString(idx),
            Instr::StringNotEqualsRefString(idx) => Instr::StringNotEqualsRefString(idx),
            Instr::New(idx) => Instr::New(idx),
            Instr::Delete => Instr::Delete,
            Instr::This => Instr::This,
            Instr::Profile(instr) => Instr::Profile(instr),
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
            Instr::ArraySort(idx) => Instr::ArraySort(idx),
            Instr::ArraySortByPredicate(idx) => Instr::ArraySortByPredicate(idx),
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
            Instr::EnumToI32 { enum_type, size } => Instr::EnumToI32 { enum_type, size },
            Instr::I32ToEnum { enum_type, size } => Instr::I32ToEnum { enum_type, size },
            Instr::DynamicCast {
                class,
                is_weak: flags,
            } => Instr::DynamicCast {
                class,
                is_weak: flags,
            },
            Instr::ToString(idx) => Instr::ToString(idx),
            Instr::ToVariant(idx) => Instr::ToVariant(idx),
            Instr::FromVariant(idx) => Instr::FromVariant(idx),
            Instr::VariantIsDefined => Instr::VariantIsDefined,
            Instr::VariantIsRef => Instr::VariantIsRef,
            Instr::VariantIsArray => Instr::VariantIsArray,
            Instr::VariantTypeName => Instr::VariantTypeName,
            Instr::VariantToString => Instr::VariantToString,
            Instr::WeakRefToRef => Instr::WeakRefToRef,
            Instr::RefToWeakRef => Instr::RefToWeakRef,
            Instr::WeakRefNull => Instr::WeakRefNull,
            Instr::AsRef(idx) => Instr::AsRef(idx),
            Instr::Deref(idx) => Instr::Deref(idx),
        };
        Some(res)
    }
}

impl<L: fmt::Display> fmt::Display for Instr<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Nop => write!(f, "nop"),
            Instr::Null => write!(f, "ref.null"),
            Instr::I32One => write!(f, "i32.one"),
            Instr::I32Zero => write!(f, "i32.zero"),
            Instr::I8Const(val) => write!(f, "i8.const {val}"),
            Instr::I16Const(val) => write!(f, "i16.const {val}"),
            Instr::I32Const(val) => write!(f, "i32.const {val}"),
            Instr::I64Const(val) => write!(f, "i64.const {val}"),
            Instr::U8Const(val) => write!(f, "u8.const {val}"),
            Instr::U16Const(val) => write!(f, "u16.const {val}"),
            Instr::U32Const(val) => write!(f, "u32.const {val}"),
            Instr::U64Const(val) => write!(f, "u64.const {val}"),
            Instr::F32Const(val) => write!(f, "f32.const {val}"),
            Instr::F64Const(val) => write!(f, "f64.const {val}"),
            Instr::CNameConst(idx) => write!(f, "cname.const {idx}"),
            Instr::EnumConst { enum_, value } => write!(f, "enum.const {enum_} {value}"),
            Instr::StringConst(idx) => write!(f, "string.const {idx}"),
            Instr::TweakDbIdConst(idx) => write!(f, "tweakdb.const {idx}"),
            Instr::ResourceConst(idx) => write!(f, "resource.const {idx}"),
            Instr::TrueConst => write!(f, "true.const"),
            Instr::FalseConst => write!(f, "false.const"),
            Instr::Breakpoint(bp) => write!(f, "breakpoint {}", bp.line),
            Instr::Assign => write!(f, "assign"),
            Instr::Target(label) => write!(f, "target {label}"),
            Instr::Local(idx) => write!(f, "local {idx}"),
            Instr::Param(idx) => write!(f, "param {idx}"),
            Instr::ObjectField(idx) => write!(f, "object.field {idx}"),
            Instr::ExternalVar => write!(f, "external"),
            Instr::Switch(instr) => write!(f, "switch {}", instr.expr_type),
            Instr::SwitchLabel(instr) => {
                write!(f, "switch.label {} {}", instr.next_case, instr.body)
            }
            Instr::SwitchDefault => write!(f, "switch.default"),
            Instr::Jump(label) => write!(f, "jump {}", label.target),
            Instr::JumpIfFalse(label) => write!(f, "jump.if_not {}", label.target),
            Instr::Skip(label) => write!(f, "skip {}", label.target),
            Instr::Conditional(instr) => write!(f, "cond {} {}", instr.false_label, instr.exit),
            Instr::Construct { arg_count, class } => {
                write!(f, "struct.new {arg_count} {class}")
            }
            Instr::InvokeStatic {
                exit,
                line,
                function,
                flags,
            } => write!(
                f,
                "invoke.static j{} l{} f{} {}",
                exit.target, line, function, flags
            ),
            Instr::InvokeVirtual {
                exit,
                line,
                function,
                flags,
            } => write!(
                f,
                "invoke.virtual j{} l{} f{} {}",
                exit.target, line, function, flags
            ),
            Instr::ParamEnd => write!(f, "param.end"),
            Instr::Return => write!(f, "return"),
            Instr::StructField(idx) => write!(f, "struct.field {idx}"),
            Instr::Context(label) => write!(f, "ctx {}", label.target),
            Instr::Equals(idx) => write!(f, "eq {idx}"),
            Instr::RefStringEqualsString(idx) => write!(f, "refstr.eq {idx}"),
            Instr::StringEqualsRefString(idx) => write!(f, "str.eq {idx}"),
            Instr::NotEquals(idx) => write!(f, "neq {idx}"),
            Instr::RefStringNotEqualsString(idx) => {
                write!(f, "refstr.neq {idx}")
            }
            Instr::StringNotEqualsRefString(idx) => {
                write!(f, "str.neq {idx}")
            }
            Instr::New(idx) => write!(f, "object.new {idx}"),
            Instr::Delete => write!(f, "object.delete"),
            Instr::This => write!(f, "this"),
            Instr::Profile(_) => write!(f, "profile"),
            Instr::ArrayClear(idx) => write!(f, "array.clear {idx}"),
            Instr::ArraySize(idx) => write!(f, "array.size {idx}"),
            Instr::ArrayResize(idx) => write!(f, "array.resize {idx}"),
            Instr::ArrayFindFirst(idx) => write!(f, "array.find_first {idx}"),
            Instr::ArrayFindFirstFast(idx) => write!(f, "array.find_first_fast {idx}"),
            Instr::ArrayFindLast(idx) => write!(f, "array.find_last {idx}"),
            Instr::ArrayFindLastFast(idx) => write!(f, "array.find_last_fast {idx}"),
            Instr::ArrayContains(idx) => write!(f, "array.contains {idx}"),
            Instr::ArrayContainsFast(idx) => write!(f, "array.contains_fast {idx}"),
            Instr::ArrayCount(idx) => write!(f, "array.count {idx}"),
            Instr::ArrayCountFast(idx) => write!(f, "array.count_fast {idx}"),
            Instr::ArrayPush(idx) => write!(f, "array.push {idx}"),
            Instr::ArrayPop(idx) => write!(f, "array.pop {idx}"),
            Instr::ArrayInsert(idx) => write!(f, "array.insert {idx}"),
            Instr::ArrayRemove(idx) => write!(f, "array.remove {idx}"),
            Instr::ArrayRemoveFast(idx) => write!(f, "array.remove_fast {idx}"),
            Instr::ArrayGrow(idx) => write!(f, "array.grow {idx}"),
            Instr::ArrayErase(idx) => write!(f, "array.erase {idx}"),
            Instr::ArrayEraseFast(idx) => write!(f, "array.erase_fast {idx}"),
            Instr::ArrayLast(idx) => write!(f, "array.last {idx}"),
            Instr::ArrayElement(idx) => write!(f, "array.element {idx}"),
            Instr::ArraySort(idx) => write!(f, "array.sort {idx}"),
            Instr::ArraySortByPredicate(idx) => write!(f, "array.sort_by {idx}"),
            Instr::StaticArraySize(idx) => write!(f, "static_array.size {idx}"),
            Instr::StaticArrayFindFirst(idx) => write!(f, "static_array.find_first {idx}"),
            Instr::StaticArrayFindFirstFast(idx) => {
                write!(f, "static_array.find_first_fast {idx}")
            }
            Instr::StaticArrayFindLast(idx) => write!(f, "static_array.find_last {idx}"),
            Instr::StaticArrayFindLastFast(idx) => write!(f, "static_array.find_last_fast {idx}"),
            Instr::StaticArrayContains(idx) => write!(f, "static_array.contains {idx}"),
            Instr::StaticArrayContainsFast(idx) => write!(f, "static_array.contains_fast {idx}"),
            Instr::StaticArrayCount(idx) => write!(f, "static_array.count {idx}"),
            Instr::StaticArrayCountFast(idx) => write!(f, "static_array.count_fast {idx}"),
            Instr::StaticArrayLast(idx) => write!(f, "static_array.last {idx}"),
            Instr::StaticArrayElement(idx) => write!(f, "static_array.element {idx}"),
            Instr::RefToBool => write!(f, "ref.to_bool"),
            Instr::WeakRefToBool => write!(f, "wref_to_bool"),
            Instr::EnumToI32 { enum_type, size } => write!(f, "enum.to_int {enum_type} {size}"),
            Instr::I32ToEnum { enum_type, size } => {
                write!(f, "enum.from_int {enum_type} {size}")
            }
            Instr::DynamicCast { class, is_weak } if *is_weak => {
                write!(f, "wref.dyncast {class}")
            }
            Instr::DynamicCast { class, .. } => write!(f, "ref.dyncast {class}"),
            Instr::ToString(idx) => write!(f, "to_string {idx}"),
            Instr::ToVariant(idx) => write!(f, "variant.new {idx}"),
            Instr::FromVariant(idx) => write!(f, "variant.extract {idx}"),
            Instr::VariantIsDefined => write!(f, "variant.is_defined"),
            Instr::VariantIsRef => write!(f, "variant.is_ref"),
            Instr::VariantIsArray => write!(f, "variant.is_array"),
            Instr::VariantTypeName => write!(f, "variant.type_name"),
            Instr::VariantToString => write!(f, "variant.to_string"),
            Instr::WeakRefToRef => write!(f, "wref.to_ref"),
            Instr::RefToWeakRef => write!(f, "ref.to_wref"),
            Instr::WeakRefNull => write!(f, "wref.null"),
            Instr::AsRef(idx) => write!(f, "as_ref {idx}"),
            Instr::Deref(idx) => write!(f, "deref {idx}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct Jump<Loc> {
    target: Loc,
}

impl<Loc> Jump<Loc> {
    #[inline]
    pub fn new(target: Loc) -> Self {
        Jump { target }
    }
}

impl Jump<Offset> {
    #[inline]
    pub fn new_with_offset(target: Offset) -> Self {
        Jump { target: target - 3 }
    }

    #[inline]
    pub fn target(&self) -> Offset {
        self.target + 3
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct Conditional<Loc> {
    false_label: Loc,
    exit: Loc,
}

impl<Loc> Conditional<Loc> {
    #[inline]
    pub fn new(false_label: Loc, exit: Loc) -> Self {
        Conditional { false_label, exit }
    }
}

impl Conditional<Offset> {
    #[inline]
    pub fn new_with_offset(false_label: Offset, exit: Offset) -> Self {
        Conditional {
            false_label: false_label - 3,
            exit: exit - 5,
        }
    }

    #[inline]
    pub fn false_label(&self) -> Offset {
        self.false_label + 3
    }

    #[inline]
    pub fn exit(&self) -> Offset {
        self.exit + 5
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct Switch<Loc> {
    expr_type: TypeIndex,
    first_case: Loc,
}

impl<Loc> Switch<Loc> {
    #[inline]
    pub fn new(expr_type: TypeIndex, first_case: Loc) -> Self {
        Switch {
            expr_type,
            first_case,
        }
    }
}

impl Switch<Offset> {
    #[inline]
    pub fn new_with_offset(expr_type: TypeIndex, first_case: Offset) -> Self {
        Switch {
            expr_type,
            first_case: first_case - 11,
        }
    }

    #[inline]
    pub fn first_case(&self) -> Offset {
        self.first_case + 11
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct SwitchLabel<Loc> {
    next_case: Loc,
    body: Loc,
}

impl<Loc> SwitchLabel<Loc> {
    #[inline]
    pub fn new(next_case: Loc, body: Loc) -> Self {
        SwitchLabel { next_case, body }
    }
}

impl SwitchLabel<Offset> {
    #[inline]
    pub fn new_with_offset(next_case: Offset, body: Offset) -> Self {
        SwitchLabel {
            next_case: next_case - 3,
            body: body - 5,
        }
    }

    #[inline]
    pub fn next_case(&self) -> Offset {
        self.next_case + 3
    }

    #[inline]
    pub fn body(&self) -> Offset {
        self.body + 5
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct Breakpoint {
    line: u16,
    line_start: u32,
    col: u16,
    length: u16,
    enabled: bool,
    padding: [u8; 8],
}

#[derive(Debug, Clone, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct Profile {
    #[byte(ctx = Prefixed(ctx))]
    function: Vec<u8>,
    enabled: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryRead, TryWrite, Measure)]
pub struct Offset {
    value: i16,
}

impl From<Offset> for i16 {
    #[inline]
    fn from(offset: Offset) -> Self {
        offset.value
    }
}

impl From<Offset> for i32 {
    #[inline]
    fn from(offset: Offset) -> Self {
        offset.value.into()
    }
}

impl From<i16> for Offset {
    #[inline]
    fn from(value: i16) -> Self {
        Offset { value }
    }
}

impl Add<i16> for Offset {
    type Output = Self;

    #[inline]
    fn add(self, rhs: i16) -> Self::Output {
        Offset {
            value: self.value + rhs,
        }
    }
}

impl Sub<i16> for Offset {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: i16) -> Self::Output {
        Offset {
            value: self.value - rhs,
        }
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, TryRead, TryWrite, Measure)]
pub struct InvokeFlags(u16);

impl InvokeFlags {
    #[inline]
    pub fn set_is_rvalue_ref(&mut self, nth: u8) {
        self.0 |= 1 << nth;
    }

    #[inline]
    pub fn is_rvalue_ref(&self, nth: u8) -> bool {
        self.0 & (1 << nth) != 0
    }
}

impl fmt::Display for InvokeFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016b}", self.0)
    }
}

use std::borrow::Cow;

use byte::ctx::LittleEndian;

mod bundle;
mod definition;
mod index;
mod instr;
mod util;

const ENDIANESS: LittleEndian = byte::LE;

#[cfg(feature = "mmap")]
pub use bundle::SaveError;
pub use bundle::{BundleReader, PoolItemIndex, PoolItemIndexMut, ScriptBundle, WriteableBundle};
pub use byte;
pub use definition::{
    Class, ClassFlags, CodeIter, Definition, DefinitionHeader, Enum, EnumMember, Field, FieldFlags,
    Function, FunctionBody, FunctionFlags, IndexedDefinition, Local, LocalFlags, Parameter,
    ParameterFlags, Property, SourceFile, SourceReference, Type, TypeKind, Visibility,
};
pub use index::{
    CNameIndex, ClassIndex, EnumIndex, EnumValueIndex, FieldIndex, FunctionIndex, LocalIndex,
    ParameterIndex, ResourceIndex, SourceFileIndex, StringIndex, TweakDbIndex, TypeIndex,
};
pub use instr::{
    Breakpoint, Conditional, Instr, InvokeFlags, Jump, Offset, Profile, Switch, SwitchLabel,
};

pub type Str<'a> = Cow<'a, str>;

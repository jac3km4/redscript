use crate::{FieldId, FreeFunctionIndex, MethodId, TypeId};

/// Index path through a source module's item arrays (including class/struct members).
///
/// The path is rooted at the top-level module item list, so a member of a class
/// declared at top-level index `2` has the path `[2, member_index]`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeclarationPath(pub Vec<usize>);

/// Resolved identity produced during name resolution / inference.
///
/// Bindings are recorded before lowering collapses duplicates, so consumers can
/// still observe every `@replaceMethod` and `@wrapMethod` declaration even when
/// the lowering tables keep only one body per [`MethodId`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedDeclaration<'ctx> {
    Type(TypeId<'ctx>),
    Field(FieldId<'ctx>),
    Function(FreeFunctionIndex),
    Method(MethodId<'ctx>),
    AddedMethod(MethodId<'ctx>),
    Replacement(MethodId<'ctx>),
    Wrapper(MethodId<'ctx>, usize),
}

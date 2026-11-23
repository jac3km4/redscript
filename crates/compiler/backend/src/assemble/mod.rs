use bon::{bon, builder};
use hashbrown::HashSet;
use redscript_compiler_frontend::ast::Span;
use redscript_compiler_frontend::utils::ScopedMap;
use redscript_compiler_frontend::{CoalesceError, MonoType, Symbols, ir};
use redscript_io::{
    FieldIndex as PoolFieldIndex, FunctionIndex as PoolFunctionIndex, Instr, Local as PoolLocal,
    LocalFlags as PoolLocalFlags, LocalIndex as PoolLocalIndex, Offset,
    ParameterIndex as PoolParameterIndex, ScriptBundle, byte,
};
use slab::Slab;
use thiserror::Error;

use crate::monomorph::Monomorphizer;

mod closure;
mod expr;
mod stmt;

#[builder(finish_fn = build)]
pub fn block_builder<'ctx>(
    function_index: PoolFunctionIndex,
    locals: impl IntoIterator<Item = ir::LocalInfo<'ctx>>,
    #[builder(default = vec![])] captures: Vec<(ir::Local, PoolFieldIndex)>,
    block: &ir::Block<'ctx>,
    symbols: &Symbols<'ctx>,
    type_env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
    bundle: &mut ScriptBundle<'ctx>,
    monomorphizer: &mut Monomorphizer<'ctx>,
) -> Result<Vec<Instr>, AssembleError<'ctx>> {
    let mut assembler = Assembler::builder()
        .function_index(function_index)
        .locals(locals)
        .captures(captures)
        .symbols(symbols)
        .type_env(type_env)
        .bundle(bundle)
        .monomorphizer(monomorphizer)
        .build()?;
    assembler.assemble_block(block, None)?;
    assembler.into_code()
}

struct Assembler<'scope, 'ctx> {
    code: Vec<Instr<Label>>,
    locals: Vec<(ir::Local, PoolLocalIndex)>,
    params: Vec<(ir::Local, PoolParameterIndex)>,
    captures: Vec<(ir::Local, PoolFieldIndex)>,
    pub(super) has_return_value: bool,

    labels: Slab<u32>,
    virtual_offset: u32,

    /// Whether assembly is currently inside a loop body, where an initialization
    /// statement can execute more than once.
    pub(super) in_loop: bool,
    /// Locals already read, written, initialized, or captured before this point.
    pub(super) touched_locals: HashSet<ir::Local>,

    pub(super) symbols: &'scope Symbols<'ctx>,
    pub(super) type_env: &'scope ScopedMap<'scope, &'ctx str, MonoType<'ctx>>,
    pub(super) bundle: &'scope mut ScriptBundle<'ctx>,
    pub(super) monomorph: &'scope mut Monomorphizer<'ctx>,
}

#[bon]
impl<'scope, 'ctx> Assembler<'scope, 'ctx> {
    #[builder]
    fn new(
        function_index: PoolFunctionIndex,
        locals: impl IntoIterator<Item = ir::LocalInfo<'ctx>>,
        mut captures: Vec<(ir::Local, PoolFieldIndex)>,
        symbols: &'scope Symbols<'ctx>,
        type_env: &'scope ScopedMap<'scope, &'ctx str, MonoType<'ctx>>,
        bundle: &'scope mut ScriptBundle<'ctx>,
        monomorphizer: &'scope mut Monomorphizer<'ctx>,
    ) -> Result<Self, AssembleError<'ctx>> {
        let mut local_indices = vec![];
        let mut param_indices = vec![];

        for local in locals {
            match local.id {
                ir::Local::Var(i) => {
                    let name = bundle
                        .cnames_mut()
                        .add(format!("{}${i}", local.name.unwrap_or("var")));
                    let typ = local
                        .typ
                        .coalesce(symbols)
                        .map_err(|e| {
                            AssembleError::Coalesce(
                                e.into(),
                                local.name_span.expect("non-spanned local should coalesce"),
                            )
                        })?
                        .assume_mono(type_env);
                    let typ = monomorphizer.type_(&typ, symbols, bundle);
                    let flags = PoolLocalFlags::default().with_is_const(local.is_const);
                    let idx = bundle.define(PoolLocal::new(name, function_index, typ, flags));
                    local_indices.push((local.id, idx));
                }
                ir::Local::Param(_) => {
                    let idx = bundle[function_index].parameters()[param_indices.len()];
                    param_indices.push((local.id, idx));
                }
                ir::Local::This => {}
            }
        }

        local_indices.sort_by_key(|&(l, _)| l);

        let locals = local_indices
            .iter()
            .map(|&(_, idx)| idx)
            .collect::<Vec<_>>();
        bundle[function_index].set_locals(locals);

        captures.sort_by_key(|&(l, _)| l);

        Ok(Self {
            code: Vec::new(),
            locals: local_indices,
            params: param_indices,
            captures,
            has_return_value: bundle[function_index].return_type().is_some(),

            labels: Slab::new(),
            virtual_offset: 0,

            in_loop: false,
            touched_locals: HashSet::new(),

            symbols,
            bundle,
            monomorph: monomorphizer,
            type_env,
        })
    }

    #[inline]
    pub(super) fn emit(&mut self, instr: Instr<Label>) {
        self.virtual_offset += u32::from(instr.virtual_size());
        self.code.push(instr);
    }

    pub(super) fn new_label(&mut self) -> Label {
        let idx = self
            .labels
            .insert(u32::MAX)
            .try_into()
            .expect("label count should not exceed u16::MAX");
        Label(idx)
    }

    #[inline]
    pub(super) fn mark_label(&mut self, label: Label) {
        self.labels[usize::from(label)] = self.virtual_offset;
    }

    fn into_code(self) -> Result<Vec<Instr>, AssembleError<'ctx>> {
        let mut virtual_offset = 0;
        self.code
            .into_iter()
            .map(|instr| {
                let instr = instr
                    .map_labels(|label| {
                        let loc: i64 = self.labels[usize::from(label)].into();
                        Some(Offset::from(i16::try_from(loc - virtual_offset).ok()?))
                    })
                    .ok_or(AssembleError::LabelTooDistant)?;
                virtual_offset += i64::from(instr.virtual_size());
                Ok(instr)
            })
            .collect()
    }

    pub(super) fn create_local_instr(&self, local: ir::Local) -> Option<Instr<Label>> {
        if let Some(idx) = self.resolve_capture(local) {
            return Some(Instr::ObjectField(idx));
        }
        if matches!(local, ir::Local::This) {
            return Some(Instr::This);
        };
        if let Some(idx) = self.resolve_local(local) {
            return Some(Instr::Local(idx));
        }
        if let Some(idx) = self.resolve_param(local) {
            return Some(Instr::Param(idx));
        }
        None
    }

    pub(super) fn resolve_local(&self, loc: ir::Local) -> Option<PoolLocalIndex> {
        let i = self.locals.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.locals.get(i)?.1)
    }

    pub(super) fn resolve_param(&self, loc: ir::Local) -> Option<PoolParameterIndex> {
        let i = self.params.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.params.get(i)?.1)
    }

    pub(super) fn resolve_capture(&self, loc: ir::Local) -> Option<PoolFieldIndex> {
        let i = self.captures.binary_search_by_key(&loc, |&(l, _)| l).ok()?;
        Some(self.captures.get(i)?.1)
    }

    pub(super) fn mono_type(
        &self,
        typ: &ir::Type<'ctx>,
        span: Span,
    ) -> Result<MonoType<'ctx>, AssembleError<'ctx>> {
        Ok(typ
            .coalesced(self.symbols)
            .map_err(|e| AssembleError::Coalesce(e.into(), span))?
            .assume_mono(self.type_env))
    }

    pub(super) fn mono_type_app(
        &self,
        typ: &ir::TypeApp<'ctx>,
        span: Span,
    ) -> Result<MonoType<'ctx>, AssembleError<'ctx>> {
        Ok(typ
            .coalesced(self.symbols)
            .map_err(|e| AssembleError::Coalesce(e.into(), span))?
            .assume_mono(self.type_env))
    }
}

#[derive(Debug, Error)]
pub enum AssembleError<'ctx> {
    #[error(
        "jump offset exceeds the signed 16-bit bytecode limit; reduce the size of this function"
    )]
    LabelTooDistant,
    #[error("cannot generate code from symbols containing an invalid function signature")]
    InvalidFunctionSignature(Option<Span>),
    #[error("`{0}` cannot be used here; {1}")]
    InvalidControlFlow(&'static str, &'static str, Span),
    #[error("struct construction has {0} arguments; the bytecode limit is 255")]
    TooManyArguments(usize, Span),
    #[error(
        "argument {0} requires a temporary-reference flag; bytecode supports these flags only for the first 16 arguments"
    )]
    TemporaryReferenceArgument(usize, Span),
    #[error("cannot generate code for intrinsic `{0}` with these arguments")]
    InvalidIntrinsic(ir::Intrinsic, Span),
    #[error(
        "function type must be a class with a `Call` method; check the function type definitions"
    )]
    InvalidFunctionClass(Span),
    #[error("{0}")]
    Coalesce(Box<CoalesceError<'ctx>>, Span),
    #[error("decoding error: {0}")]
    Decoding(#[from] byte::Error),
}

impl AssembleError<'_> {
    pub fn code(&self) -> &'static str {
        match self {
            Self::LabelTooDistant => "JUMP_OFFSET_OVERFLOW",
            Self::InvalidFunctionSignature(_) => "INVALID_FUNCTION_SIGNATURE",
            Self::InvalidControlFlow(..) => "INVALID_CONTROL_FLOW",
            Self::TooManyArguments(..) => "TOO_MANY_ARGUMENTS",
            Self::TemporaryReferenceArgument(..) => "TEMPORARY_REFERENCE_ARGUMENT",
            Self::InvalidIntrinsic(..) => "INVALID_INTRINSIC",
            Self::InvalidFunctionClass(_) => "INVALID_FUNCTION_CLASS",
            Self::Coalesce(..) => "COALESCE_ERR",
            Self::Decoding(_) => "DECODING_ERR",
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            Self::InvalidFunctionSignature(span) => *span,
            Self::InvalidControlFlow(_, _, span)
            | Self::TooManyArguments(_, span)
            | Self::TemporaryReferenceArgument(_, span)
            | Self::InvalidIntrinsic(_, span)
            | Self::InvalidFunctionClass(span)
            | Self::Coalesce(_, span) => Some(*span),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Label(u16);

impl From<Label> for usize {
    fn from(label: Label) -> usize {
        usize::from(label.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct BlockSpan {
    pub(super) continue_target: Option<Label>,
    pub(super) break_target: Label,
}

#[cfg(test)]
mod diagnostic_tests {
    use redscript_compiler_frontend::ast::SourceMap;

    use super::*;

    #[test]
    fn backend_errors_explain_source_restrictions() {
        let sources = SourceMap::new();
        let file = sources.push_back("test.reds", "break;");
        let span = Span::new(0, 6, file);
        let error =
            AssembleError::InvalidControlFlow("break", "a loop or switch is required", span);
        assert_eq!(
            error.to_string(),
            "`break` cannot be used here; a loop or switch is required"
        );
        assert_eq!(error.span(), Some(span));
        assert_eq!(error.code(), "INVALID_CONTROL_FLOW");
        let error = AssembleError::TooManyArguments(256, span);
        assert_eq!(
            error.to_string(),
            "struct construction has 256 arguments; the bytecode limit is 255"
        );
        assert_eq!(error.span(), Some(span));
        assert_eq!(error.code(), "TOO_MANY_ARGUMENTS");
        let error = AssembleError::TemporaryReferenceArgument(17, span);
        assert_eq!(
            error.to_string(),
            "argument 17 requires a temporary-reference flag; bytecode supports these flags only for the first 16 arguments"
        );
        assert_eq!(error.span(), Some(span));
        assert_eq!(error.code(), "TEMPORARY_REFERENCE_ARGUMENT");
        assert_eq!(
            AssembleError::LabelTooDistant.to_string(),
            "jump offset exceeds the signed 16-bit bytecode limit; reduce the size of this function"
        );
    }
}

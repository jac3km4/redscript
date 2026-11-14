use std::path::Path;

use redscript_compiler_api::ast::{self, SourceMap, Span};
use redscript_compiler_api::{Symbols, TypeIndex, TypeInterner};

pub struct SccOutput {
    sources: SourceMap,
    interner: TypeInterner,
    refs: Vec<SourceRef>,
}

impl SccOutput {
    pub fn new(sources: SourceMap, interner: TypeInterner, refs: Vec<SourceRef>) -> Self {
        Self {
            sources,
            interner,
            refs,
        }
    }

    #[inline]
    pub fn refs(&self) -> &[SourceRef] {
        &self.refs
    }

    pub fn name<'a>(&'a self, ref_: &'a SourceRef) -> Option<&'a str> {
        match ref_ {
            SourceRef::Type(id, _) => self.interner.get_index(*id)?.as_name(),
            SourceRef::Function(name, _)
            | SourceRef::Method(_, name, _)
            | SourceRef::Field(_, name, _) => Some(name),
        }
    }

    pub fn parent_name<'a>(&'a self, ref_: &SourceRef) -> Option<&'a str> {
        match ref_ {
            SourceRef::Method(id, _, _) | SourceRef::Field(id, _, _) => {
                self.interner.get_index(*id)?.as_name()
            }
            _ => None,
        }
    }

    pub fn path<'a>(&'a self, ref_: &SourceRef) -> Option<&'a Path> {
        let span = ref_.span();
        self.sources.get(span.file).map(ast::File::path)
    }

    pub fn line(&self, ref_: &SourceRef) -> Option<usize> {
        let span = ref_.span();
        self.sources
            .get(span.file)
            .map(|f| f.line_and_offset(span.start).0)
    }
}

#[derive(Debug)]
pub enum SourceRef {
    Type(TypeIndex, Span),
    Function(String, Span),
    Method(TypeIndex, String, Span),
    Field(TypeIndex, String, Span),
}

impl SourceRef {
    pub fn type_(&self) -> SourceRefType {
        match self {
            SourceRef::Type(_, _) => SourceRefType::Class,
            SourceRef::Function(_, _) | SourceRef::Method(_, _, _) => SourceRefType::Function,
            SourceRef::Field(_, _, _) => SourceRefType::Field,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            SourceRef::Type(_, span)
            | SourceRef::Function(_, span)
            | SourceRef::Method(_, _, span)
            | SourceRef::Field(_, _, span) => *span,
        }
    }
}

#[repr(u8)]
pub enum SourceRefType {
    Undefined = 0,
    Class = 1,
    Field = 2,
    Function = 3,
    Enum = 4,
}

pub(super) fn extract_refs(syms: &Symbols<'_>, interner: &TypeInterner) -> Vec<SourceRef> {
    let mut refs = Vec::new();

    for (id, def) in syms.types() {
        let Some(id) = id.as_name().and_then(|name| interner.get_index_of(name)) else {
            continue;
        };
        let Some(agg) = def.schema().as_aggregate() else {
            continue;
        };
        if !agg.flags().is_native() {
            continue;
        }
        let Some(span) = agg.span() else { continue };
        refs.push(SourceRef::Type(id, span));

        for entry in agg.methods().iter() {
            let Some(span) = entry.func().span() else {
                continue;
            };
            refs.push(SourceRef::Method(id, (**entry.name()).into(), span));
        }
        for field in agg.fields().iter() {
            let Some(span) = field.field().span() else {
                continue;
            };
            refs.push(SourceRef::Field(id, field.name().into(), span));
        }
    }

    for entry in syms.free_functions() {
        let Some(span) = entry.func().span() else {
            continue;
        };
        if !entry.func().flags().is_native() {
            continue;
        }
        refs.push(SourceRef::Function(entry.name().to_string(), span));
    }

    refs
}

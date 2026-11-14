use std::borrow::Cow;

use anyhow::Context;
use redscript_compiler_api::ast::{FileId, SourceMap, Span};
use redscript_compiler_api::{Diagnostic, FieldId, LowerError, MethodId, Symbols, TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CodeEdit<'ctx> {
    FixStructNewConstructor(TypeId<'ctx>, Span),
    MakeMethodPublic(MethodId<'ctx>),
    MakeFieldPublic(FieldId<'ctx>),
    MakeTypePublic(TypeId<'ctx>),
}

impl<'ctx> CodeEdit<'ctx> {
    pub fn from_diagnostic(diagnostic: &Diagnostic<'ctx>) -> Option<Self> {
        match diagnostic {
            Diagnostic::LoweringError(error) => match error {
                &LowerError::NewWithConstructible { type_id, span } => {
                    Some(Self::FixStructNewConstructor(type_id, span))
                }
                LowerError::InaccessibleMethod(inaccessible_member, _) => {
                    Some(Self::MakeMethodPublic(*inaccessible_member.id()))
                }
                LowerError::InaccessibleField(inaccessible_member, _) => {
                    Some(Self::MakeFieldPublic(*inaccessible_member.id()))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct TextEdit<'ctx> {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
    pub expected: Cow<'ctx, str>,
    pub content: Cow<'ctx, str>,
}

impl<'ctx> TextEdit<'ctx> {
    pub fn apply(&self, source: &mut String) -> bool {
        if source.get(self.start..self.end) != Some(&self.expected) {
            return false;
        }
        source.replace_range(self.start..self.end, &self.content);
        true
    }

    pub fn apply_many(edits: impl IntoIterator<Item = Self>, source: &mut String) -> usize {
        let mut offset: isize = 0;
        let mut applied: usize = 0;
        for edit in edits {
            let start = (edit.start as isize + offset) as usize;
            let end = (edit.end as isize + offset) as usize;
            if source.get(start..end) != Some(&edit.expected) {
                continue;
            }
            source.replace_range(start..end, &edit.content);
            offset += edit.content.len() as isize - (edit.end as isize - edit.start as isize);
            applied += 1;
        }
        applied
    }

    pub fn from_code_edit(
        edit: &CodeEdit<'ctx>,
        symbols: &Symbols<'ctx>,
        sources: &'ctx SourceMap,
    ) -> anyhow::Result<TextEdit<'ctx>> {
        match edit {
            CodeEdit::FixStructNewConstructor(type_id, span) => {
                fix_struct_new_constructor(*type_id, *span, sources)
            }
            CodeEdit::MakeMethodPublic(method) => {
                let (name, method) = symbols.get_method(*method).context("method not found")?;
                let span = method.span().context("method has no source location")?;
                make_item_public(name, span, sources)
            }
            CodeEdit::MakeFieldPublic(field) => {
                let (name, field) = symbols.get_field(*field).context("field not found")?;
                let span = field.span().context("field has no source location")?;
                make_item_public(name, span, sources)
            }
            CodeEdit::MakeTypePublic(id) => {
                let def = symbols.get_type(*id).context("type not found")?;
                let span = def.span().context("type has no source location")?;
                make_item_public(&id.simple_name(), span, sources)
            }
        }
    }
}

fn fix_struct_new_constructor<'ctx>(
    type_id: TypeId<'ctx>,
    span: Span,
    sources: &'ctx SourceMap,
) -> anyhow::Result<TextEdit<'ctx>> {
    let name = type_id.simple_name();

    let file = sources.get(span.file).context("file not found for span")?;
    let source = file.source();

    let slice = source
        .get(span.start as usize..)
        .context("span out of bounds")?;
    let rem = slice
        .strip_prefix("new")
        .context("missing 'new'")?
        .trim_start()
        .strip_prefix(&*name)
        .context("missing struct name")?;

    let start = span.start as usize;
    let end = start + (slice.len() - rem.len());
    Ok(TextEdit {
        file: span.file,
        start,
        end,
        expected: source[start..end].into(),
        content: name,
    })
}

fn make_item_public<'ctx>(
    name: &str,
    name_span: Span,
    sources: &'ctx SourceMap,
) -> anyhow::Result<TextEdit<'ctx>> {
    let file = sources
        .get(name_span.file)
        .context("file not found for span")?;
    let source = file.source();
    let slice = source
        .get(..name_span.end as usize)
        .context("span out of bounds")?;
    let mut rem = slice.strip_suffix(name).context("missing item name")?;
    loop {
        let trimmed_ws = rem.trim_end_matches(|c: char| c.is_whitespace() && c != '\n');
        let trimmed_ident = trimmed_ws.trim_end_matches(char::is_alphabetic);
        if trimmed_ident.len() == trimmed_ws.len() {
            break;
        }
        rem = trimmed_ident;
    }

    let suffix = slice
        .get(rem.len()..)
        .context("span out of bounds")?
        .trim_start();
    let len = if suffix.starts_with("private") {
        "private".len() + 1
    } else if suffix.starts_with("protected") {
        "protected".len() + 1
    } else if suffix.starts_with("public") {
        "public".len() + 1
    } else {
        0
    };

    let start = rem.len();
    let end = start + len;
    Ok(TextEdit {
        file: name_span.file,
        start,
        end,
        expected: source[start..end].into(),
        content: "public ".into(),
    })
}

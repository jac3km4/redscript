use std::fs;

use anyhow::Context;
use hashbrown::{HashMap, HashSet};
use redscript_code_edit::{CodeEdit, TextEdit};
use redscript_compiler_api::ast::{FileId, SourceMap};
use redscript_compiler_api::{Diagnostic, Enum, LowerError, Symbols};

pub fn migrate_if_needed<'ctx>(
    diagnostics: &[Diagnostic<'ctx>],
    symbols: &Symbols<'ctx>,
    sources: &'ctx SourceMap,
) -> anyhow::Result<bool> {
    let edits = generate_edits(diagnostics, symbols);
    if edits.is_empty() {
        return Ok(false);
    }

    let tfd::OkCancel::Ok = tfd::MessageBox::new(
        "REDscript migration",
        "REDscript has detected scripts that require migration. This means that you either just \
        upgraded REDscript or installed an outdated mod. REDscript will migrate them for you \
        automatically when you click OK.",
    )
    .with_icon(tfd::MessageBoxIcon::Info)
    .run_modal_ok_cancel(tfd::OkCancel::Ok) else {
        return Ok(false);
    };

    let mut edits_by_file = HashMap::<FileId, Vec<TextEdit<'_>>>::new();
    for edit in &edits {
        let Ok(edit) = TextEdit::from_code_edit(edit, symbols, sources) else {
            continue;
        };
        edits_by_file.entry(edit.file).or_default().push(edit);
    }

    for (file_id, mut edits) in edits_by_file {
        edits.sort_by_key(|edit| edit.start);

        let file = sources.get(file_id).unwrap();
        let mut contents = fs::read_to_string(file.path())
            .context("failed to read a script file for migration")?;
        let applied = TextEdit::apply_many(edits, &mut contents);
        fs::write(file.path(), contents).context("failed to write a migrated script file")?;
        log::info!(
            "Applied {applied} migration changes to '{}'",
            file.path().display()
        );
    }

    Ok(true)
}

fn generate_edits<'ctx>(
    diagnostics: &[Diagnostic<'ctx>],
    symbols: &Symbols<'ctx>,
) -> HashSet<CodeEdit<'ctx>> {
    let enum_map = symbols
        .types()
        .filter(|(_, def)| def.schema().as_enum().and_then(Enum::span).is_some())
        .map(|(id, _)| (id.simple_name(), id))
        .collect::<HashMap<_, _>>();

    diagnostics
        .iter()
        .filter_map(|diagnostic| match diagnostic {
            Diagnostic::LoweringError(error) => match error {
                &LowerError::NewWithConstructible { type_id, span } => {
                    Some(CodeEdit::FixStructNewConstructor(type_id, span))
                }
                LowerError::UnresolvedVar(name, _) | LowerError::UnresolvedType(name, _) => {
                    if let Some(&id) = enum_map.get(*name) {
                        Some(CodeEdit::MakeTypePublic(id))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

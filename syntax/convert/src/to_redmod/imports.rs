//! REDmod only has module-wide imports (`@using "A.B"`), `import A.B.X` and `import A.B.{X, Y}`
//! are widened to the whole parent module.
//!
//! REDmod does not accept `@if(..)` on imports. Importing a module that does not exist is not an
//! error there, so the condition is dropped.

use std::collections::HashSet;

use redscript_ast::{Import, Item, Path, SourceModule};

use crate::Cx;

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for (decl, span) in &mut module.items {
        if !matches!(decl.item, Item::Import(_)) {
            continue;
        }
        let before = decl.annotations.len();
        decl.annotations.retain(|(ann, _)| ann.name != "if");
        if decl.annotations.len() != before {
            cx.info(
                *span,
                "conditional-import",
                "REDmod does not allow @if on imports, the import was made unconditional (a \
                 missing module is not an error there)",
            );
        }
    }

    let mut seen: HashSet<Path<'src>> = module
        .items
        .iter()
        .filter_map(|(decl, _)| match &decl.item {
            Item::Import(Import::All(path)) => Some(path.clone()),
            _ => None,
        })
        .collect();

    let items = std::mem::take(&mut module.items);
    let mut kept = Vec::with_capacity(items.len());
    for mut entry in items {
        let (decl, span) = &mut entry;
        let (parent, what) = match &decl.item {
            Item::Import(Import::Exact(path)) => {
                let (_, parent) = path.segments.split_last().unzip();
                let parent = parent.unwrap_or_default();
                (Path::new(parent), path.segments.join("."))
            }
            Item::Import(Import::Select(path, names)) => (
                path.clone(),
                format!("{}.{{{}}}", path.segments.join("."), names.join(", ")),
            ),
            _ => {
                kept.push(entry);
                continue;
            }
        };
        if parent.segments.is_empty() {
            cx.info(
                *span,
                "import-widened",
                format!("REDmod cannot import `{what}`, the import was dropped"),
            );
            continue;
        }
        cx.info(
            *span,
            "import-widened",
            format!(
                "REDmod can only import whole modules, `import {what}` was widened to `{}`",
                parent.segments.join(".")
            ),
        );
        if seen.insert(parent.clone()) {
            decl.item = Item::Import(Import::All(parent));
            kept.push(entry);
        }
    }
    module.items = kept.into_boxed_slice();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn exact_and_select_imports_are_widened() {
        let (out, diags) = to_redmod("import A.B.X\nimport C.D.{E, F}\nclass K {}\n", run);
        let out = squash(&out);
        assert!(out.contains("@using \"A.B\""), "{out}");
        assert!(out.contains("@using \"C.D\""), "{out}");
        assert!(!out.contains("import"), "{out}");
        assert_eq!(codes(&diags), ["import-widened", "import-widened"]);
    }

    #[test]
    fn widened_imports_are_deduplicated() {
        let (out, diags) = to_redmod(
            "import A.B.X\nimport A.B.Y\nimport A.B.{Z}\nimport C.*\nimport C.W\nclass K {}\n",
            run,
        );
        let out = squash(&out);
        assert_eq!(out.matches("@using \"A.B\"").count(), 1, "{out}");
        assert_eq!(out.matches("@using \"C\"").count(), 1, "{out}");
        assert_eq!(diags.len(), 4);
    }

    #[test]
    fn import_conditions_are_dropped() {
        let (out, diags) = to_redmod(
            "@if(ModuleExists(\"A.B\"))
import A.B.*

@if(ModuleExists(\"A.B\"))
func F() {}
",
            run,
        );
        let out = squash(&out);
        assert!(
            out.starts_with("@using \"A.B\" @if(ModuleExists(\"A.B\")) function F()"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["conditional-import"]);
    }

    #[test]
    fn glob_imports_are_untouched() {
        let (out, diags) = to_redmod("import A.B.*\nclass K {}\n", run);
        assert!(squash(&out).contains("@using \"A.B\""), "{out}");
        assert!(diags.is_empty());
    }
}

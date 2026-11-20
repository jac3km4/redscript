use std::collections::{BTreeMap, BTreeSet};

use redscript_compiler_frontend::{Symbols, ir};

use crate::target;

pub(crate) fn reserved_names(symbols: &Symbols<'_>) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for entry in symbols.free_functions() {
        names.extend(entry.name().into_iter().map(str::to_owned));
    }
    for (id, def) in symbols.types() {
        names.extend(id.as_str().split('.').map(str::to_owned));
        if let Some(agg) = def.schema().as_aggregate() {
            for entry in agg.methods().iter() {
                names.insert(entry.name().to_string());
            }
        }
    }
    names
}

pub(super) fn authored_name(name: &str) -> Option<String> {
    if target::ident(name).is_ok() {
        Some(name.to_owned())
    } else {
        // Appending a suffix repairs reserved words without guessing how to
        // transliterate spellings that are not target identifiers at all.
        let candidate = format!("{name}_");
        target::ident(&candidate).ok().map(str::to_owned)
    }
}

pub(super) fn contextual_temporary_name(
    local: &ir::LocalInfo<'_>,
    names: &BTreeMap<ir::Local, String>,
) -> Option<String> {
    use ir::{TemporaryOwner as Owner, TemporaryPurpose as Purpose};

    let (owner, suffix) = match (local.temporary_owner?, local.temporary_purpose?) {
        (Owner::Destination(owner), Purpose::ArrayLiteral) => (owner, "Literal"),
        (Owner::IterationBinding(owner), Purpose::IterationIndex) => (owner, "Index"),
        (Owner::IterationBinding(owner), Purpose::IterationIterable) => (owner, "Iterable"),
        _ => return None,
    };
    let name = format!("{}{suffix}", names.get(&owner)?);
    target::ident(&name).ok().map(str::to_owned)
}

pub(super) fn temporary_name(purpose: ir::TemporaryPurpose) -> &'static str {
    match purpose {
        ir::TemporaryPurpose::ArrayLiteral => "arrayLiteral",
        ir::TemporaryPurpose::IterationIndex => "index",
        ir::TemporaryPurpose::IterationIterable => "iterable",
        ir::TemporaryPurpose::PatternScrutinee => "matchedValue",
        ir::TemporaryPurpose::SwitchScrutinee => "switchValue",
        ir::TemporaryPurpose::LoopEntered => "loopEntered",
    }
}

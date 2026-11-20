//! Import-aware plan for the unqualified names a target module may use.
//!
//! Official REDmod rejects fully qualified global identifiers such as
//! `Dependency.Value()`: a free call must name its symbol unqualified. A type
//! reference must likewise be spellable with a bare simple name. This module
//! records, per emitted source module, which symbols are visible unqualified and
//! under which unique simple name, so references fail closed instead of being
//! emitted qualified or silently bound to a colliding symbol.
//!
//! Scope membership mirrors the community frontend: a symbol is visible when it
//! is declared in the emitted module, when it is a root-scope/global symbol, or
//! when an `ast::Import` brings it in. `@using "M.Name"` imports the whole
//! overload family named `M.Name`, matching the target grammar.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use redscript_community_ast::{self as ast, Item, SourceModule};
use redscript_compiler_frontend::{FreeFunctionIndex, QualifiedName, Symbols, TypeId};

use crate::{Result, TranspileDiagnostic, error};

/// Unqualified names visible in one emitted source module.
///
/// Providers are tracked by their fully qualified spelling. Two overloads of the
/// same free function share one qualified name and therefore one simple name;
/// two distinct modules exporting `Pick` produce distinct qualified names and
/// therefore collide. A type and a free function cannot share a qualified name
/// in the community frontend, so the two namespaces cannot alias here.
pub(crate) struct TargetNamePlan {
    /// Symbols declared by the emitted module, keyed by simple name.
    own: BTreeMap<String, BTreeSet<String>>,
    /// Symbols brought in by `ast::Import`s, keyed by simple name.
    imported: BTreeMap<String, BTreeSet<String>>,
}

impl TargetNamePlan {
    pub(crate) fn new<'ctx>(module: &SourceModule<'ctx>, symbols: &Symbols<'ctx>) -> Self {
        let module_path: Vec<&'ctx str> = module
            .path
            .as_ref()
            .map_or_else(Vec::new, |path| path.segments.to_vec());
        let mut plan = Self {
            own: BTreeMap::new(),
            imported: BTreeMap::new(),
        };
        plan.collect_own(module, &module_path);
        plan.collect_imports(module, symbols);
        plan
    }

    /// Resolve the unqualified target spelling of a type reference.
    pub(crate) fn type_name<'ctx>(&self, id: TypeId<'ctx>) -> Result<String> {
        let full = id.as_str();
        self.resolve(full.as_ref(), id.simple_name().as_ref())
    }

    /// Resolve the unqualified target spelling of a free-function call.
    pub(crate) fn free_function_name<'ctx>(
        &self,
        index: FreeFunctionIndex,
        symbols: &Symbols<'ctx>,
    ) -> Result<String> {
        let (name, _) = symbols
            .get_free_function(index)
            .ok_or_else(|| error("missing resolved function", None))?;
        // Root-scope functions have a single component; `Cow::from` keeps that
        // spelling borrowed instead of formatting the qualified name.
        let full = Cow::from(name);
        let simple = name.as_ref().last().copied().unwrap_or(full.as_ref());
        self.resolve(&full, simple)
    }

    fn collect_own<'ctx>(&mut self, module: &SourceModule<'ctx>, module_path: &[&'ctx str]) {
        for (decl, _) in &module.items {
            let name = match &decl.item {
                Item::Class(class) | Item::Struct(class) => class.name.0,
                Item::Enum(enum_) => enum_.name.0,
                Item::Function(function) => function.name.0,
                Item::Import(_) | Item::Let(_) | Item::Default(_) => continue,
            };
            let full = QualifiedName::from_base_and_name(module_path, name).to_string();
            self.own.entry(name.to_owned()).or_default().insert(full);
        }
    }

    fn collect_imports<'ctx>(&mut self, module: &SourceModule<'ctx>, symbols: &Symbols<'ctx>) {
        for (decl, _) in &module.items {
            let Item::Import(import) = &decl.item else {
                continue;
            };
            match import {
                ast::Import::All(path) => {
                    let target = path.segments.join(".");
                    self.import_module(&target, symbols);
                }
                ast::Import::Exact(path) => {
                    self.insert_import(&path.segments.join("."));
                }
                ast::Import::Select(path, names) => {
                    let base = path.segments.join(".");
                    for name in names {
                        self.insert_import(&format!("{base}.{name}"));
                    }
                }
            }
        }
    }

    /// `import M.*` exposes every exported type and free-function overload family
    /// declared directly in `M` (not in nested modules).
    fn import_module<'ctx>(&mut self, target: &str, symbols: &Symbols<'ctx>) {
        for (id, _) in symbols.types() {
            let full = id.as_str();
            if module_of(&full) == Some(target) {
                self.insert_import(full.as_ref());
            }
        }
        for entry in symbols.free_functions() {
            // Compare the module prefix segment-by-segment instead of formatting
            // every (including baseline/dependency) function name to split it.
            if matches_module(entry.name().as_ref(), target) {
                self.insert_import(&entry.name().to_string());
            }
        }
    }

    fn insert_import(&mut self, full: &str) {
        let simple = full.rsplit_once('.').map_or(full, |(_, name)| name);
        self.imported
            .entry(simple.to_owned())
            .or_default()
            .insert(full.to_owned());
    }

    fn resolve(&self, full: &str, simple: &str) -> Result<String> {
        // Root-scope/global symbols have no module component. Only an import can
        // shadow one; same-level globals are unique in the frontend namespace.
        if !full.contains('.') {
            if let Some(providers) = self.imported.get(simple) {
                let other = providers.iter().next().map_or("", String::as_str);
                return Err(collision_error(full, simple, other));
            }
            return Ok(simple.to_owned());
        }
        let own = self.own.get(simple);
        let imported = self.imported.get(simple);
        if !own.is_some_and(|set| set.contains(full))
            && !imported.is_some_and(|set| set.contains(full))
        {
            return Err(error(
                format!(
                    "cannot emit `{full}` unqualified: it is not declared in this module and no import brings it into scope"
                ),
                None,
            ));
        }
        let other = own
            .into_iter()
            .flatten()
            .chain(imported.into_iter().flatten())
            .find(|provider| provider.as_str() != full);
        if let Some(other) = other {
            return Err(collision_error(full, simple, other));
        }
        Ok(simple.to_owned())
    }
}

fn module_of(full: &str) -> Option<&str> {
    full.rsplit_once('.').map(|(module, _)| module)
}

/// Whether the module prefix of `name` (all but the final component) is exactly
/// `target`, compared segment-by-segment without allocating the joined string.
fn matches_module(name: &[&str], target: &str) -> bool {
    let Some((_, module)) = name.split_last() else {
        return false;
    };
    !module.is_empty() && module.iter().copied().eq(target.split('.'))
}

fn collision_error(full: &str, simple: &str, other: &str) -> TranspileDiagnostic {
    error(
        format!(
            "cannot emit `{full}` unqualified: the name `{simple}` collides with `{other}` in this module's scope"
        ),
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan_from(own: &[(&str, &[&str])], imported: &[(&str, &[&str])]) -> TargetNamePlan {
        let collect = |entries: &[(&str, &[&str])]| {
            entries
                .iter()
                .map(|(simple, providers)| {
                    (
                        (*simple).to_owned(),
                        providers.iter().map(|p| (*p).to_owned()).collect(),
                    )
                })
                .collect()
        };
        TargetNamePlan {
            own: collect(own),
            imported: collect(imported),
        }
    }

    #[test]
    fn module_scoped_symbols_resolve_only_from_own_or_imported_scope() {
        let plan = plan_from(&[("Value", &["M.Value"])], &[]);
        assert_eq!(plan.resolve("M.Value", "Value").unwrap(), "Value");
        // A module-scoped symbol with no import fails closed.
        let error = plan.resolve("N.Value", "Value").unwrap_err();
        assert!(
            error.message.contains("no import brings it into scope"),
            "{error:?}"
        );
        // An exact/select import resolves the family unqualified.
        let plan = plan_from(&[], &[("Pick", &["M.Pick"])]);
        assert_eq!(plan.resolve("M.Pick", "Pick").unwrap(), "Pick");
    }

    #[test]
    fn colliding_simple_names_fail_closed_instead_of_picking_one() {
        let plan = plan_from(&[("Pick", &["M.Pick"])], &[("Pick", &["N.Pick"])]);
        let error = plan.resolve("M.Pick", "Pick").unwrap_err();
        assert!(
            error.message.contains("collides with `N.Pick`"),
            "{error:?}"
        );
        let error = plan.resolve("N.Pick", "Pick").unwrap_err();
        assert!(
            error.message.contains("collides with `M.Pick`"),
            "{error:?}"
        );
        // Overloads of one family share a qualified name and never collide.
        let plan = plan_from(&[], &[("Pick", &["M.Pick"])]);
        assert_eq!(plan.resolve("M.Pick", "Pick").unwrap(), "Pick");
    }

    #[test]
    fn root_symbols_stay_unqualified_unless_an_import_shadows_them() {
        let plan = plan_from(&[], &[]);
        assert_eq!(plan.resolve("Int32", "Int32").unwrap(), "Int32");
        let plan = plan_from(&[], &[("Int32", &["M.Int32"])]);
        let error = plan.resolve("Int32", "Int32").unwrap_err();
        assert!(
            error.message.contains("collides with `M.Int32`"),
            "{error:?}"
        );
    }

    #[test]
    fn module_prefix_matching_is_segment_wise() {
        assert!(matches_module(&["M", "Pick"], "M"));
        assert!(matches_module(&["M", "N", "Pick"], "M.N"));
        assert!(!matches_module(&["M", "N", "Pick"], "M"));
        assert!(!matches_module(&["M", "N", "Pick"], "N"));
        assert!(!matches_module(&["Pick"], "M"));
        assert!(!matches_module(&[], "M"));
    }
}

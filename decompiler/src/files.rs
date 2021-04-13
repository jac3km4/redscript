use std::collections::{HashMap, HashSet};
use std::iter;
use std::path::Path;

use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Definition};

pub struct FileIndex<'a> {
    file_map: HashMap<PoolIndex<Definition>, HashSet<PoolIndex<Definition>>>,
    pool: &'a ConstantPool,
}

impl<'a> FileIndex<'a> {
    pub fn from_pool(pool: &'a ConstantPool) -> FileIndex {
        let mut file_map: HashMap<PoolIndex<Definition>, HashSet<PoolIndex<Definition>>> = HashMap::new();

        for (idx, def) in pool.definitions() {
            if let Some(source) = def.source() {
                let root_idx = if def.parent.is_undefined() { idx } else { def.parent };
                file_map
                    .entry(source.file)
                    .and_modify(|vec| {
                        vec.insert(root_idx);
                    })
                    .or_insert_with(|| iter::once(root_idx).collect());
            }
        }

        FileIndex { file_map, pool }
    }

    pub fn iter(&'a self) -> impl Iterator<Item = FileEntry<'a>> {
        let source_files = self.file_map.iter().filter_map(move |(idx, children)| {
            if let AnyDefinition::SourceFile(ref file) = self.pool.definition(*idx).unwrap().value {
                let mut definitions: Vec<&Definition> = children
                    .iter()
                    .filter_map(|child| self.pool.definition(*child).ok())
                    .collect();
                definitions.sort_by_key(|def| def.first_line(self.pool).unwrap_or(0));

                let entry = FileEntry {
                    path: &file.path,
                    definitions,
                };
                Some(entry)
            } else {
                None
            }
        });

        iter::once(self.orphans()).chain(source_files)
    }

    fn orphans(&'a self) -> FileEntry<'a> {
        let definitions = self
            .pool
            .definitions()
            .filter(|(_, def)| match &def.value {
                AnyDefinition::Class(cls) if cls.functions.is_empty() || cls.flags.is_native() => true,
                AnyDefinition::Enum(_) => true,
                AnyDefinition::Function(fun) if def.parent == PoolIndex::UNDEFINED && fun.flags.is_native() => true,
                _ => false,
            })
            .map(|(_, def)| def)
            .collect();
        FileEntry {
            path: Path::new("orphans.script"),
            definitions,
        }
    }
}

pub struct FileEntry<'a> {
    pub path: &'a Path,
    pub definitions: Vec<&'a Definition>,
}

use std::iter;
use std::path::Path;

use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Definition, SourceFile};

pub struct FileIndex<'a> {
    file_map: HashMap<PoolIndex<SourceFile>, HashSet<PoolIndex<Definition>>>,
    pool: &'a ConstantPool,
}

impl<'a> FileIndex<'a> {
    pub fn from_pool(pool: &'a ConstantPool) -> FileIndex<'a> {
        let mut file_map: HashMap<PoolIndex<SourceFile>, HashSet<PoolIndex<Definition>>> = HashMap::new();

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
                let definitions = children
                    .iter()
                    .filter_map(|child| self.pool.definition(*child).ok())
                    .sorted_by_key(|def| def.first_line(self.pool).unwrap_or(0))
                    .collect();

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
                AnyDefinition::Class(class) => class
                    .functions
                    .iter()
                    .filter_map(|idx| self.pool.function(*idx).ok())
                    .all(|fun| fun.flags.is_native()),
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

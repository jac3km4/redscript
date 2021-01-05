use crate::bundle::{ConstantPool, PoolIndex};
use crate::definition::{Definition, DefinitionValue, SourceFile};

use std::collections::{HashMap, HashSet};
use std::iter::{once, FromIterator};

pub struct FileIndex<'a> {
    file_map: HashMap<PoolIndex<Definition>, HashSet<PoolIndex<Definition>>>,
    pool: &'a ConstantPool,
}

impl<'a> FileIndex<'a> {
    pub fn from_pool(pool: &'a ConstantPool) -> FileIndex {
        let mut file_map: HashMap<PoolIndex<Definition>, HashSet<PoolIndex<Definition>>> = HashMap::new();

        for (idx, def) in pool.definitions() {
            if let Some(source) = def.source() {
                let root_idx = if def.parent.is_root() { idx } else { def.parent };
                file_map
                    .entry(source.file)
                    .and_modify(|vec| {
                        vec.insert(root_idx);
                    })
                    .or_insert(HashSet::from_iter(once(root_idx)));
            }
        }

        FileIndex { file_map, pool }
    }

    pub fn files(&'a self) -> impl Iterator<Item = FileEntry<'a>> {
        self.file_map.iter().filter_map(move |(idx, children)| {
            if let DefinitionValue::SourceFile(ref file) = self.pool.definition(*idx).unwrap().value {
                let mut definitions: Vec<&Definition> = children
                    .iter()
                    .filter_map(|child| self.pool.definition(*child).ok())
                    .collect();
                definitions.sort_by_key(|def| def.source().map(|src| src.line).unwrap_or(0));

                let entry = FileEntry { file, definitions };
                Some(entry)
            } else {
                None
            }
        })
    }
}

pub struct FileEntry<'a> {
    pub file: &'a SourceFile,
    pub definitions: Vec<&'a Definition>,
}

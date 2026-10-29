use redscript_community_ast as ast;
use sequence_trie::SequenceTrie;
use smallvec::smallvec;

use crate::symbols::FreeFunctionIndexes;
use crate::{FreeFunctionIndex, QualifiedName, TypeId};

#[derive(Debug, Default)]
pub struct ModuleMap<'ctx> {
    map: SequenceTrie<&'ctx str, Export<'ctx>, hashbrown::DefaultHashBuilder>,
}

impl<'ctx> ModuleMap<'ctx> {
    pub fn add_type(
        &mut self,
        path: QualifiedName<'ctx>,
        id: TypeId<'ctx>,
        public: bool,
    ) -> Result<(), NameRedefinition> {
        if self.map.get(&path).is_some() {
            return Err(NameRedefinition);
        }
        self.map.insert_owned(&path, Export::Type { id, public });
        Ok(())
    }

    pub fn add_function(
        &mut self,
        path: QualifiedName<'ctx>,
        index: FreeFunctionIndex,
        public: bool,
    ) -> Result<(), NameRedefinition> {
        match self.map.get_mut(&path) {
            None => {
                self.map.insert_owned(
                    &path,
                    Export::FreeFunction {
                        overloads: smallvec![index],
                        public,
                    },
                );
            }
            Some(Export::FreeFunction {
                overloads,
                public: is_public,
            }) => {
                overloads.push(index);
                *is_public = *is_public || public;
            }
            Some(_) => return Err(NameRedefinition),
        };
        Ok(())
    }

    pub fn exports(
        &self,
        path: impl AsRef<[&'ctx str]>,
    ) -> Option<impl Iterator<Item = (&'ctx str, &Export<'ctx>)>> {
        let node = self.map.get_node(path.as_ref())?;
        let it = node
            .children_with_keys()
            .filter_map(|(&k, n)| Some((k, n.value()?)));
        Some(it)
    }

    pub fn exists(&self, path: impl IntoIterator<Item = &'ctx str>) -> bool {
        self.map.get_node(path).is_some()
    }

    pub fn visit_import(
        &self,
        import: &ast::Import<'ctx>,
        mut on_type: impl FnMut(&'ctx str, TypeId<'ctx>),
        mut on_function: impl FnMut(&'ctx str, FreeFunctionIndex),
        mut on_error: impl FnMut(ImportError<'ctx>),
    ) {
        let mut visit_export = |export: &Export<'ctx>, name: &'ctx str| {
            match export {
                Export::Type { id, .. } => on_type(name, *id),
                Export::FreeFunction { overloads, .. } => {
                    overloads.iter().for_each(|&idx| on_function(name, idx));
                }
            };
            if export.is_public() {
                Ok(())
            } else {
                Err(ImportError::Private(name))
            }
        };

        match import {
            ast::Import::Exact(path) => {
                let last = path.as_ref().last().unwrap();
                match self.map.get(path.as_ref()) {
                    Some(export) => visit_export(export, last).unwrap_or_else(on_error),
                    None => on_error(ImportError::NotFound(last)),
                }
            }
            ast::Import::Select(path, names) => {
                match self.map.get_node(path.as_ref()) {
                    Some(node) => names.iter().for_each(|&name| match node.get([name]) {
                        Some(export) => {
                            visit_export(export, name).unwrap_or_else(&mut on_error);
                        }
                        None => on_error(ImportError::NotFound(name)),
                    }),
                    None => on_error(ImportError::NotFound(path.as_ref().last().unwrap())),
                };
            }
            ast::Import::All(path) => match self.exports(path.as_ref()) {
                Some(exports) => {
                    let mut peekable = exports.peekable();
                    if peekable.peek().is_none() {
                        on_error(ImportError::NotFound(path.as_ref().last().unwrap()));
                    } else {
                        peekable.filter(|(_, export)| export.is_public()).for_each(
                            |(name, export)| {
                                visit_export(export, name).unwrap_or_else(&mut on_error);
                            },
                        );
                    }
                }
                None => on_error(ImportError::NotFound(path.as_ref().last().unwrap())),
            },
        };
    }
}

#[derive(Debug)]
pub enum Export<'ctx> {
    FreeFunction {
        overloads: FreeFunctionIndexes,
        public: bool,
    },
    Type {
        id: TypeId<'ctx>,
        public: bool,
    },
}

impl<'ctx> Export<'ctx> {
    pub fn is_public(&self) -> bool {
        match self {
            Export::FreeFunction { public, .. } | Export::Type { public, .. } => *public,
        }
    }
}

#[derive(Debug)]
pub enum ImportError<'ctx> {
    NotFound(&'ctx str),
    Private(&'ctx str),
}

#[derive(Debug)]
pub struct NameRedefinition;

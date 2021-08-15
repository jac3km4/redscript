use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

use crate::bundle::{ConstantPool, PoolIndex};
use crate::bytecode::Instr;
use crate::definition::{AnyDefinition, Class, Function};

pub trait Mapper<A> {
    fn apply(&self, value: A) -> A;
}

pub struct MultiMapper<A> {
    mappings: HashMap<A, A>,
}

impl<A> MultiMapper<A> {
    pub fn new(mappings: HashMap<A, A>) -> Self {
        Self { mappings }
    }
}

impl<A: Eq + Hash + Clone> Mapper<A> for MultiMapper<A> {
    fn apply(&self, value: A) -> A {
        match self.mappings.get(&value) {
            Some(replacement) => replacement.clone(),
            None => value,
        }
    }
}

pub struct NoopMapper<A>(PhantomData<A>);

impl<A> Mapper<A> for NoopMapper<A> {
    #[inline(always)]
    fn apply(&self, value: A) -> A {
        value
    }
}

pub struct PoolMapper<CM, FM> {
    map_class: CM,
    map_function: FM,
}

impl<CM, FM> PoolMapper<CM, FM>
where
    CM: Mapper<PoolIndex<Class>>,
    FM: Mapper<PoolIndex<Function>>,
{
    pub fn map(&self, pool: &mut ConstantPool) {
        for def in &mut pool.definitions {
            match &mut def.value {
                AnyDefinition::Type(_) => {}
                AnyDefinition::Class(class) => {
                    class.base = self.map_class.apply(class.base);
                }
                AnyDefinition::EnumValue(_) => {}
                AnyDefinition::Enum(_) => {}
                AnyDefinition::Function(fun) => {
                    def.parent = self.map_class.apply(def.parent.cast()).cast();
                    for instr in &mut fun.code.0 {
                        self.map_instr(instr);
                    }
                }
                AnyDefinition::Parameter(_) => {
                    def.parent = self.map_function.apply(def.parent.cast()).cast();
                }
                AnyDefinition::Local(_) => {
                    def.parent = self.map_function.apply(def.parent.cast()).cast();
                }
                AnyDefinition::Field(_) => {
                    def.parent = self.map_class.apply(def.parent.cast()).cast();
                }
                AnyDefinition::SourceFile(_) => {}
            }
        }
    }

    fn map_instr<L>(&self, instr: &mut Instr<L>) {
        match instr {
            Instr::Construct(_, idx) => *idx = self.map_class.apply(*idx),
            Instr::InvokeStatic(_, _, idx, _) => *idx = self.map_function.apply(*idx),
            Instr::New(idx) => *idx = self.map_class.apply(*idx),
            Instr::DynamicCast(idx, _) => *idx = self.map_class.apply(*idx),
            _ => {}
        }
    }

    pub fn with_class_mapper<CM2: Mapper<PoolIndex<Class>>>(self, mapper: CM2) -> PoolMapper<CM2, FM> {
        PoolMapper {
            map_class: mapper,
            map_function: self.map_function,
        }
    }

    pub fn with_function_mapper<FM2: Mapper<PoolIndex<Function>>>(self, mapper: FM2) -> PoolMapper<CM, FM2> {
        PoolMapper {
            map_class: self.map_class,
            map_function: mapper,
        }
    }
}

impl Default for PoolMapper<NoopMapper<PoolIndex<Class>>, NoopMapper<PoolIndex<Function>>> {
    fn default() -> Self {
        Self {
            map_class: NoopMapper(PhantomData),
            map_function: NoopMapper(PhantomData),
        }
    }
}

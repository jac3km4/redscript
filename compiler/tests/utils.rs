use std::collections::HashMap;
use std::io::Cursor;

use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
use redscript::bytecode::{Code, Offset};
use redscript::definition::{AnyDefinition, ClassFlags, Definition};
use redscript::error::Error;
use redscript_compiler::parser;
use redscript_compiler::unit::CompilationUnit;

pub const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

pub struct TestContext {
    pool: ConstantPool,
    indexes: HashMap<String, PoolIndex<Definition>>,
}

impl TestContext {
    pub fn compiled(source: &str) -> Result<Self, Error> {
        let pool = compiled(source)?;
        let res = Self {
            pool,
            indexes: HashMap::new(),
        };
        Ok(res)
    }

    pub fn match_index(&mut self, idx: PoolIndex<Definition>, name: &str) {
        match self.indexes.get(name) {
            Some(val) if *val == idx => (),
            Some(val) => assert!(false, "{} is {}, expected {}", name, val, idx),
            None => {
                self.indexes.insert(name.to_owned(), idx);
            }
        }
    }

    pub fn run<F>(&mut self, name: &str, check: F)
    where
        F: Fn(Code<Offset>, &mut TestContext),
    {
        let fun = self
            .pool
            .definitions()
            .find_map(|(_, def)| match &def.value {
                AnyDefinition::Function(fun) => {
                    let fun_name = self.pool.names.get(def.name).ok()?;
                    let fun_name = fun_name.split(";").next()?;
                    if fun_name == name {
                        Some(fun)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .expect("Function not found in the pool");
        println!("{:?}", fun.code);
        check(fun.code.clone(), self);
    }
}

/// macro for matching an instruction using a pattern
#[macro_export]
macro_rules! pat {
    ($id:ident($($args:pat),+)) => {
        |instr: Instr<Offset>, _ctx: &mut TestContext| {
            if !matches!(instr, Instr::$id($($args),+)) {
                panic!("Expected {}({}), got {:?}", stringify!($id), stringify!($($args,)+), instr);
            }
        }
    };
    ($id:ident) => {
        |instr: Instr<Offset>, _ctx: &mut TestContext| {
            if !matches!(instr, Instr::$id) {
                panic!("Expected {}, got {:?}", stringify!($id), instr);
            }
        }
    }
}

/// macro for matching an instruction and memorizing it's arguments by names
#[macro_export]
macro_rules! mem {
    ($id:ident($($args:ident),+)) => {
        |instr: Instr<Offset>, ctx: &mut TestContext| {
            if let Instr::$id($($args),+) = instr {
                $(match_index!($args, ctx);)+
            } else {
                panic!("Expected {}, got {:?}", stringify!($id), instr);
            }
        }
    }
}

#[macro_export]
macro_rules! match_index {
    (__, $ctx:ident) => {};
    ($id:ident, $ctx:ident) => {
        $ctx.match_index($id.cast(), stringify!($id))
    };
}

#[macro_export]
macro_rules! check_code {
    [$($check:expr),*] => {
        |code: Code<Offset>, ctx: &mut TestContext| {
            let mut instrs = code.0.into_iter();
            $($check(instrs.next().expect("Not enough instructions"), ctx);)*
        }
    }
}

pub fn compiled(code: &str) -> Result<ConstantPool, Error> {
    let module = parser::parse_str(code).unwrap();
    let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
    CompilationUnit::new(&mut scripts.pool)?.compile(vec![module])?;

    Ok(scripts.pool)
}

pub fn check_class_flags(pool: &ConstantPool, name: &str, flags: ClassFlags) -> Result<(), Error> {
    let name_index = pool.names.get_index(&String::from(name))?;
    let match_ = pool
        .definitions()
        .filter(|(_, def)| matches!(def.value, AnyDefinition::Class(_)))
        .find(|(_, def)| def.name == name_index)
        .map(|(_, def)| &def.value);

    if let Some(AnyDefinition::Class(ref class)) = match_ {
        assert_eq!(class.flags, flags)
    } else {
        assert!(false, "Class definition {} not found in the pool", name)
    }
    Ok(())
}

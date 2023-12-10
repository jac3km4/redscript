use std::fs;
use std::path::Path;

use hashbrown::HashMap;
use pretty_assertions::assert_eq;
use redscript::ast::{Seq, Variance};
use redscript::Str;
use redscript_compiler::error::CompileError;
use redscript_compiler::parser::parse_stmts;
use redscript_compiler::scoped_map::ScopedMap;
use redscript_compiler::sugar::Desugar;
use redscript_compiler::type_repo::{predef, ClassType, DataType, Parameterized, TypeId, TypeRepo, TypeVar};
use redscript_compiler::typer::{CheckedAst, ErrorReporter, IdAlloc, InferType, LocalInfo, TypeEnv, Typer};
use with_locals::with;

#[with]
pub fn check_file_as_seq(path: impl AsRef<Path>, return_type: InferType<'static>) -> TypecheckResult {
    let contents = fs::read_to_string(Path::new(".").join("tests").join(path)).unwrap();
    let mut body = parse_stmts(&contents).unwrap();
    let mut locals = ScopedMap::default();
    let (repo, types) = test_repo();
    let names = ScopedMap::default();
    let vars = ScopedMap::default();
    let env = TypeEnv::new(&types, &vars);
    let mut id_alloc = IdAlloc::default();
    let mut reporter = ErrorReporter::default();

    let mut typer = Typer::new(&repo, &names, env, return_type, &mut id_alloc, &mut reporter);
    Desugar::run(&mut body);
    let body = typer.typeck_seq(&body, &mut locals);
    TypecheckResult {
        repo,
        errors: reporter.into_errors(),
        body,
        locals: locals.pop_scope(),
    }
}

pub struct TypecheckResult {
    repo: TypeRepo<'static>,
    errors: Vec<CompileError<'static>>,
    body: Seq<CheckedAst<'static>>,
    locals: HashMap<Str, LocalInfo<'static>>,
}

impl TypecheckResult {
    pub fn no_errors(self) -> Self {
        assert_eq!(&self.errors, &[], "compilation failed");
        self
    }

    pub fn local_has_type(mut self, name: &str, expected: &str) -> Self {
        let local = self.locals.remove(&Str::from(name)).expect("local not found");
        let simplified = local.typ.simplify(&self.repo).to_string();
        assert_eq!(simplified, expected, "'{}' has wrong type", name);
        self
    }

    pub fn no_more_locals(self) -> Self {
        assert_eq!(self.locals, HashMap::default(), "leftover locals");
        self
    }
}

fn test_repo() -> (TypeRepo<'static>, ScopedMap<'static, Str, TypeId<'static>>) {
    let mut repo = TypeRepo::new();
    let mut names = ScopedMap::default();

    names.insert("ref".into(), predef::REF);
    names.insert("wref".into(), predef::WREF);

    repo.add_type(predef::ISCRIPTABLE, DataType::Class(ClassType::default()));
    names.insert("IScriptable".into(), predef::ISCRIPTABLE);

    macro_rules! add_function_type {
        ($arity:literal, [$($arg:ident),*]) => {
            let id = TypeId::get_fn_by_name($arity).unwrap();
            repo.add_type(
                TypeId::get_fn_by_name($arity).unwrap(),
                DataType::Class(ClassType {
                    type_vars: [
                        $(TypeVar::unconstrained(stringify!($arg).into(), Variance::Contra),)*
                        TypeVar::unconstrained("R".into(), Variance::Co)
                    ].into(),
                    extends: Some(Parameterized::without_args(predef::ISCRIPTABLE)),
                    ..Default::default()
                }),
            );
            names.insert($arity.into(), id);
        }
    }

    add_function_type!("Function0", []);
    add_function_type!("Function1", [A]);
    add_function_type!("Function2", [A, B]);
    add_function_type!("Function3", [A, B, C]);
    add_function_type!("Function4", [A, B, C, D]);
    add_function_type!("Function5", [A, B, C, D, E]);
    add_function_type!("Function6", [A, B, C, D, E, F]);

    (repo, names)
}

// pub const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

// pub struct TestContext {
//     pool: ConstantPool,
//     indexes: HashMap<String, PoolIndex<Definition>>,
// }

// impl TestContext {
//     // pub fn compiled(sources: Vec<&str>) -> Result<Self, Error> {
//     //     let (pool, diagnostics) = compiled(sources)?;
//     //     assert!(
//     //         !diagnostics.iter().any(Diagnostic::is_fatal),
//     //         "Fatal errors: {:?}",
//     //         diagnostics
//     //     );
//     //     let res = Self {
//     //         pool,
//     //         indexes: HashMap::new(),
//     //     };
//     //     Ok(res)
//     // }

//     pub fn match_index(&mut self, idx: PoolIndex<Definition>, name: &str) {
//         match self.indexes.get(name) {
//             Some(val) if *val == idx => (),
//             Some(val) => panic!("{} is {}, expected {}", name, val, idx),
//             None => {
//                 self.indexes.insert(name.to_owned(), idx);
//             }
//         }
//     }

//     pub fn run<F>(&mut self, name: &str, check: F)
//     where
//         F: Fn(Code<Offset>, &mut Self),
//     {
//         let fun = self
//             .pool
//             .definitions()
//             .find_map(|(_, def)| match &def.value {
//                 AnyDefinition::Function(fun) => {
//                     let fun_name = self.pool.names.get(def.name).ok()?;
//                     (FunctionSignature::from_raw(&fun_name).name() == name).then_some(fun)
//                 }
//                 _ => None,
//             })
//             .expect("Function not found in the pool");
//         check(fun.code.clone(), self);
//     }
// }

// /// macro for matching an instruction using a pattern
// #[macro_export]
// macro_rules! pat {
//     ($id:ident($($args:pat),+)) => {
//         |instr: Instr<Offset>, _ctx: &mut TestContext| {
//             if !matches!(instr, Instr::$id($($args),+)) {
//                 panic!("Expected {}({}), got {:?}", stringify!($id), stringify!($($args,)+), instr);
//             }
//         }
//     };
//     ($id:ident) => {
//         |instr: Instr<Offset>, _ctx: &mut TestContext| {
//             if !matches!(instr, Instr::$id) {
//                 panic!("Expected {}, got {:?}", stringify!($id), instr);
//             }
//         }
//     }
// }

// /// macro for matching an instruction and memorizing it's arguments by names
// #[macro_export]
// macro_rules! mem {
//     ($id:ident($($args:ident),+)) => {
//         |instr: Instr<Offset>, ctx: &mut TestContext| {
//             if let Instr::$id($($args),+) = instr {
//                 $(match_index!($args, ctx);)+
//             } else {
//                 panic!("Expected {}, got {:?}", stringify!($id), instr);
//             }
//         }
//     }
// }

// #[macro_export]
// macro_rules! match_index {
//     (__, $ctx:ident) => {};
//     ($id:ident, $ctx:ident) => {
//         $ctx.match_index($id.cast(), stringify!($id))
//     };
// }

// #[macro_export]
// macro_rules! check_code {
//     [$($check:expr),*] => {
//         |code: Code<Offset>, ctx: &mut TestContext| {
//             let mut instrs = code.0.into_iter();
//             $($check(instrs.next().expect("Not enough instructions"), ctx);)*
//         }
//     }
// }

// // pub fn compiled(sources: Vec<&str>) -> Result<(ConstantPool, Vec<Diagnostic>), Error> {
// //     let modules = sources
// //         .iter()
// //         .map(|source| parser::parse_str(source).unwrap())
// //         .collect();
// //     let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
// //     let res = CompilationUnit::new_with_defaults(&mut scripts.pool)?.compile(modules)?;

// //     Ok((scripts.pool, res))
// // }

// pub fn check_class_flags(pool: &ConstantPool, name: &str, flags: ClassFlags) -> Result<(), Error> {
//     let name_index = pool.names.get_index(&String::from(name)).unwrap();
//     let match_ = pool
//         .definitions()
//         .filter(|(_, def)| matches!(def.value, AnyDefinition::Class(_)))
//         .find(|(_, def)| def.name == name_index)
//         .map(|(_, def)| &def.value);

//     if let Some(AnyDefinition::Class(ref class)) = match_ {
//         assert_eq!(class.flags, flags);
//     } else {
//         panic!("Class definition {} not found in the pool", name)
//     }
//     Ok(())
// }

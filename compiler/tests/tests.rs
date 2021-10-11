use std::io::Cursor;

use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
use redscript::bytecode::{Code, Instr, Offset};
use redscript::definition::{AnyDefinition, ClassFlags};
use redscript::error::Error;
use redscript_compiler::parser;
use redscript_compiler::unit::CompilationUnit;

const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

#[test]
fn compile_simple_class() -> Result<(), Error> {
    let sources = "
        public class A {
            private const let m_field: Int32;

            public func DoStuff(fieldOrNot: Bool) -> Int32 {
                return fieldOrNot ? this.m_field : A.Ten();
            }

            public static func Ten() -> Int32 {
                return 10;
            }
        }";

    check_compilation(sources)
}

#[test]
fn compile_ext_class() -> Result<(), Error> {
    let sources = "
        public class X {
            private const let m_base_field: Int32;

            public func BaseMethod() -> Int32 {
                return this.m_base_field;
            }
        }

        public class Y extends X {
            public func CallBase() -> Int32 {
              return this.BaseMethod();
            }
        }";

    check_compilation(sources)
}

#[test]
fn compile_class_with_forward_ref() -> Result<(), Error> {
    let sources = "
        public class MyTestClass456 {
            public let myOtherTestClass: ref<MyTestClass123>;

            public func DoStuff() -> ref<MyTestClass123> {
                return this.myOtherTestClass;
            }
        }
        
        public class MyTestClass123 {
            public let myTestVar: String;
        }";

    check_compilation(sources)
}

#[test]
fn compile_class_with_shorthand_funcs() -> Result<(), Error> {
    let sources = "
        public class ShorthandTest {
            public func InstanceVal() -> String = ShorthandTest.StaticVal()
            public static func StaticVal() -> String = \"static\"
        }";

    check_compilation(sources)
}

#[test]
fn compile_dynamic_casts() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let b: wref<B> = new B();
            let a: wref<A> = b as A;
        }

        class A {}
        class B extends A {}
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(26)),
        Instr::RefToWeakRef,
        Instr::New(PoolIndex::new(23)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(29)),
        Instr::RefToWeakRef,
        Instr::DynamicCast(PoolIndex::new(22), 0),
        Instr::WeakRefToRef,
        Instr::Local(PoolIndex::new(26)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_base_class_overload() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let b = new B();
            b.Testing(1, 2);
        }

        class A {
            final func Testing(a: Int32) -> Int32 = a
        }
        class B extends A {
            final func Testing(a: Int32, b: Int32) -> Int32 = b
        }
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(31)),
        Instr::New(PoolIndex::new(23)),
        Instr::Context(Offset::new(38)),
        Instr::Local(PoolIndex::new(31)),
        Instr::InvokeStatic(Offset::new(26), 0, PoolIndex::new(26), 0),
        Instr::I32Const(1),
        Instr::I32Const(2),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_class_attributes() -> Result<(), Error> {
    let source = r#"
        public abstract class Base {}

        public final class Derived extends Base {}
    "#;

    let expected_base_flags = ClassFlags::new().with_is_abstract(true);
    let expected_derived_flags = ClassFlags::new().with_is_final(true);

    let pool = check_compilation_pool(source)?;
    check_class_flags(&pool, "Base", expected_base_flags)?;
    check_class_flags(&pool, "Derived", expected_derived_flags)?;
    Ok(())
}

#[test]
fn compile_basic_casts() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let a: Float = Cast(1);
            let b: String = Cast(2);
        }

        func Cast(i: Int32) -> Float = 0.0
        func Cast(i: Int32) -> String = \"\"
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(26)),
        Instr::InvokeStatic(Offset::new(21), 0, PoolIndex::new(22), 0),
        Instr::I32Const(1),
        Instr::ParamEnd,
        Instr::Assign,
        Instr::Local(PoolIndex::new(27)),
        Instr::InvokeStatic(Offset::new(21), 0, PoolIndex::new(23), 0),
        Instr::I32Const(2),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_overloaded_call() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let val = new B();
            TestingTarget(val, val);
        }

        func TestingTarget(x: wref<A>, y: ref<B>) {}
        func TestingTarget(x: wref<A>, y: ref<C>) {}

        class A {}
        class B extends A {}
        class C extends A {}
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(39)),
        Instr::New(PoolIndex::new(25)),
        Instr::InvokeStatic(Offset::new(35), 0, PoolIndex::new(22), 0),
        Instr::RefToWeakRef,
        Instr::Local(PoolIndex::new(39)),
        Instr::Local(PoolIndex::new(39)),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_for_loop() -> Result<(), Error> {
    let sources = "
        func Testing() {
            for i in [0, 1] {
                Log(ToString(i));
            }
        }

        func Log(str: String) {}
        func OperatorAssignAdd(out l: Int32, r: Int32) -> Int32 = 0
        func OperatorLess(l: Int32, r: Int32) -> Bool = true
        ";
    let expected = vec![
        Instr::ArrayPush(PoolIndex::new(31)),
        Instr::Local(PoolIndex::new(32)),
        Instr::I32Const(0),
        Instr::ArrayPush(PoolIndex::new(31)),
        Instr::Local(PoolIndex::new(32)),
        Instr::I32Const(1),
        Instr::Assign,
        Instr::Local(PoolIndex::new(33)),
        Instr::Local(PoolIndex::new(32)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(34)),
        Instr::I32Const(0),
        Instr::JumpIfFalse(Offset::new(150)),
        Instr::InvokeStatic(Offset::new(43), 0, PoolIndex::new(24), 0),
        Instr::Local(PoolIndex::new(34)),
        Instr::ArraySize(PoolIndex::new(31)),
        Instr::Local(PoolIndex::new(33)),
        Instr::ParamEnd,
        Instr::Assign,
        Instr::Local(PoolIndex::new(30)),
        Instr::ArrayElement(PoolIndex::new(31)),
        Instr::Local(PoolIndex::new(33)),
        Instr::Local(PoolIndex::new(34)),
        Instr::InvokeStatic(Offset::new(34), 0, PoolIndex::new(22), 0),
        Instr::ToString(PoolIndex::new(8)),
        Instr::Local(PoolIndex::new(30)),
        Instr::ParamEnd,
        Instr::InvokeStatic(Offset::new(30), 0, PoolIndex::new(23), 0),
        Instr::Local(PoolIndex::new(34)),
        Instr::I32Const(1),
        Instr::ParamEnd,
        Instr::Jump(Offset::new(-147)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_nested_array_literals() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let x = [[1, 2], [3, 4], [5, 6]];
        }
        ";
    let expected = vec![
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(26)),
        Instr::I32Const(1),
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(26)),
        Instr::I32Const(2),
        Instr::ArrayPush(PoolIndex::new(23)),
        Instr::Local(PoolIndex::new(25)),
        Instr::Local(PoolIndex::new(26)),
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(27)),
        Instr::I32Const(3),
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(27)),
        Instr::I32Const(4),
        Instr::ArrayPush(PoolIndex::new(23)),
        Instr::Local(PoolIndex::new(25)),
        Instr::Local(PoolIndex::new(27)),
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(28)),
        Instr::I32Const(5),
        Instr::ArrayPush(PoolIndex::new(22)),
        Instr::Local(PoolIndex::new(28)),
        Instr::I32Const(6),
        Instr::ArrayPush(PoolIndex::new(23)),
        Instr::Local(PoolIndex::new(25)),
        Instr::Local(PoolIndex::new(28)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(24)),
        Instr::Local(PoolIndex::new(25)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_variant_conversions() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let x = ToVariant(new A());
            let y: ref<A> = FromVariant(x);
        }

        class A {}
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(23)),
        Instr::ToVariant(PoolIndex::new(25)),
        Instr::New(PoolIndex::new(22)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(26)),
        Instr::FromVariant(PoolIndex::new(25)),
        Instr::Local(PoolIndex::new(23)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_implicit_conversions() -> Result<(), Error> {
    let sources = r#"
        func Testing() {
            Test1("test");
            let a: wref<A> = new A();
            Test2(a);

            a.method(a.member);
        }

        native func Test1(str: script_ref<String>)
        native func Test2(instance: ref<A>)

        class A {
            let member: Int32;

            func method(x: Int32) {}
        }
        "#;
    let expected = vec![
        Instr::InvokeStatic(Offset::new(30), 0, PoolIndex::new(22), 0),
        Instr::AsRef(PoolIndex::new(7)),
        Instr::StringConst(PoolIndex::new(0)),
        Instr::ParamEnd,
        Instr::Assign,
        Instr::Local(PoolIndex::new(35)),
        Instr::RefToWeakRef,
        Instr::New(PoolIndex::new(24)),
        Instr::InvokeStatic(Offset::new(26), 0, PoolIndex::new(23), 0),
        Instr::WeakRefToRef,
        Instr::Local(PoolIndex::new(35)),
        Instr::ParamEnd,
        Instr::Context(Offset::new(51)),
        Instr::WeakRefToRef,
        Instr::Local(PoolIndex::new(35)),
        Instr::InvokeVirtual(Offset::new(38), 0, PoolIndex::new(30), 0),
        Instr::Context(Offset::new(22)),
        Instr::WeakRefToRef,
        Instr::Local(PoolIndex::new(35)),
        Instr::ObjectField(PoolIndex::new(30)),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_switch_case() -> Result<(), Error> {
    let sources = "
        func Testing(val: Int32) -> Bool {
            switch val % 4 {
                case 1:
                case 2:
                case 3:
                    break;
                default:
                    return true;
            }
            return false;
        }

        func OperatorModulo(l: Int32, r: Int32) -> Int32 = 0
        ";
    let expected = vec![
        Instr::Switch(PoolIndex::new(8), Offset::new(41)),
        Instr::InvokeStatic(Offset::new(30), 0, PoolIndex::new(22), 0),
        Instr::Param(PoolIndex::new(23)),
        Instr::I32Const(4),
        Instr::ParamEnd,
        Instr::SwitchLabel(Offset::new(10), Offset::new(30)),
        Instr::I32Const(1),
        Instr::SwitchLabel(Offset::new(10), Offset::new(20)),
        Instr::I32Const(2),
        Instr::SwitchLabel(Offset::new(13), Offset::new(10)),
        Instr::I32Const(3),
        Instr::Jump(Offset::new(6)),
        Instr::SwitchDefault,
        Instr::Return,
        Instr::TrueConst,
        Instr::Return,
        Instr::FalseConst,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_ternary_op() -> Result<(), Error> {
    let sources = "
        func Testing(val: Int32) -> Bool = val % 2 == 0 ? true : false

        func OperatorModulo(l: Int32, r: Int32) -> Int32 = 0
        func OperatorEqual(l: Int32, r: Int32) -> Bool = false
        ";
    let expected = vec![
        Instr::Return,
        Instr::Conditional(Offset::new(57), Offset::new(58)),
        Instr::InvokeStatic(Offset::new(51), 0, PoolIndex::new(23), 0),
        Instr::InvokeStatic(Offset::new(30), 0, PoolIndex::new(22), 0),
        Instr::Param(PoolIndex::new(24)),
        Instr::I32Const(2),
        Instr::ParamEnd,
        Instr::I32Const(0),
        Instr::ParamEnd,
        Instr::TrueConst,
        Instr::FalseConst,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_if_else() -> Result<(), Error> {
    let sources = "
        func Testing(bool: Bool) -> Int32 {
            if bool {
                return 1;
            } else {
                return 0;
            }
        }
        ";
    let expected = vec![
        Instr::JumpIfFalse(Offset::new(21)),
        Instr::Param(PoolIndex::new(22)),
        Instr::Return,
        Instr::I32Const(1),
        Instr::Jump(Offset::new(9)),
        Instr::Return,
        Instr::I32Const(0),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_method_overload_call() -> Result<(), Error> {
    let sources = "
        class B extends A {
            func Testing() -> Int32 = super.Testing()
        }

        class A {
            func Testing() -> Int32 = 0
        }
        ";
    let expected = vec![
        Instr::Return,
        Instr::Context(Offset::new(20)),
        Instr::This,
        Instr::InvokeStatic(Offset::new(16), 0, PoolIndex::new(24), 0),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_null_wref_assignment() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let a: wref<A> = null;
        }

        class A {}
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(25)),
        Instr::RefToWeakRef,
        Instr::Null,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_number_literals() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let a: Float = 1;
            let b: Float = 2.0;
            let c: Double = 3;
            let d: Double = 4.0;
            let e: Double = 5.0d;
            let f: Int32 = 6;
            let g: Int64 = 7;
            let h: Int64 = 8l;
            let i: Uint32 = 9u;
            let j: Uint64 = 10u;
        }
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(22)),
        Instr::F32Const(1.0),
        Instr::Assign,
        Instr::Local(PoolIndex::new(23)),
        Instr::F32Const(2.0),
        Instr::Assign,
        Instr::Local(PoolIndex::new(24)),
        Instr::F64Const(3.0),
        Instr::Assign,
        Instr::Local(PoolIndex::new(25)),
        Instr::F64Const(4.0),
        Instr::Assign,
        Instr::Local(PoolIndex::new(26)),
        Instr::F64Const(5.0),
        Instr::Assign,
        Instr::Local(PoolIndex::new(27)),
        Instr::I32Const(6),
        Instr::Assign,
        Instr::Local(PoolIndex::new(28)),
        Instr::I64Const(7),
        Instr::Assign,
        Instr::Local(PoolIndex::new(29)),
        Instr::I64Const(8),
        Instr::Assign,
        Instr::Local(PoolIndex::new(30)),
        Instr::U32Const(9),
        Instr::Assign,
        Instr::Local(PoolIndex::new(31)),
        Instr::U64Const(10),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_string_literals() -> Result<(), Error> {
    let sources = r#"
        func Testing() {
            let a: String = "\u{1F4A9}";
            let b: CName = n"back";
            //let c: ResRef = r"base\\gameplay\\gui\\common\\buttonhints.inkwidget";
            let d: TweakDBID = t"MappinIcons.QuestMappin";
        }
        "#;
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(22)),
        Instr::StringConst(PoolIndex::new(0)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(23)),
        Instr::NameConst(PoolIndex::new(25)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(24)),
        Instr::TweakDbIdConst(PoolIndex::new(0)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_is_defined() -> Result<(), Error> {
    let sources = "
        func Testing() {
            let x = new A();
            if IsDefined(x) {}
            let y: wref<A> = new A();
            if IsDefined(y) {}
        }

        class A {}
        ";
    let expected = vec![
        Instr::Assign,
        Instr::Local(PoolIndex::new(25)),
        Instr::New(PoolIndex::new(22)),
        Instr::JumpIfFalse(Offset::new(13)),
        Instr::RefToBool,
        Instr::Local(PoolIndex::new(25)),
        Instr::Assign,
        Instr::Local(PoolIndex::new(27)),
        Instr::RefToWeakRef,
        Instr::New(PoolIndex::new(22)),
        Instr::JumpIfFalse(Offset::new(13)),
        Instr::WeakRefToBool,
        Instr::Local(PoolIndex::new(27)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_mutually_dependent_modules() -> Result<(), Error> {
    let sources1 = parser::parse_str(
        "
        module MyModule.Module1
        import MyModule.Module2.{B, Func2}

        public func Func1() -> Int32 = 2

        class A {
            func Thing() -> Int32 {
                Func2();
                return new B().Thing();
            }
        }",
    )
    .unwrap();

    let sources2 = parser::parse_str(
        "
        module MyModule.Module2
        import MyModule.Module1.*

        public func Func2() -> Int32 = 2

        public class B {
            func Thing() -> Int32 = Func1()
        }",
    )
    .unwrap();

    let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
    CompilationUnit::new(&mut scripts.pool)?.compile(vec![sources1, sources2])?;
    Ok(())
}

#[test]
fn compile_enum() -> Result<(), Error> {
    let sources = r#"
        func Testing(dir: Direction) -> Int32 {
            switch dir {
                case Direction.Left:
                    return EnumInt(dir);
                case Direction.Right:
                    return EnumInt(dir);
            }
        }

        enum Direction {
            Left = 0,
            Right = 1,
        }
        "#;

    let expected = vec![
        Instr::Switch(PoolIndex::new(23), Offset::new(20)),
        Instr::Param(PoolIndex::new(24)),
        Instr::SwitchLabel(Offset::new(42), Offset::new(22)),
        Instr::EnumConst(PoolIndex::new(22), PoolIndex::new(25)),
        Instr::Return,
        Instr::EnumToI32(PoolIndex::new(23), 4),
        Instr::Param(PoolIndex::new(24)),
        Instr::SwitchLabel(Offset::new(42), Offset::new(22)),
        Instr::EnumConst(PoolIndex::new(22), PoolIndex::new(26)),
        Instr::Return,
        Instr::EnumToI32(PoolIndex::new(23), 4),
        Instr::Param(PoolIndex::new(24)),
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

#[test]
fn compile_empty_return() -> Result<(), Error> {
    let sources = r#"
        func Testing(bool: Bool) -> Void {
            if bool {
                return;
            }
            Log("hello");
        }

        func Log(str: String) -> Void {}
        "#;
    let expected = vec![
        Instr::JumpIfFalse(Offset::new(14)),
        Instr::Param(PoolIndex::new(23)),
        Instr::Return,
        Instr::Nop,
        Instr::InvokeStatic(Offset::new(21), 0, PoolIndex::new(22), 0),
        Instr::StringConst(PoolIndex::new(0)),
        Instr::ParamEnd,
        Instr::Nop,
    ];
    check_function_bytecode(sources, expected)
}

fn check_compilation_pool(code: &str) -> Result<ConstantPool, Error> {
    let module = parser::parse_str(code).unwrap();
    let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
    CompilationUnit::new(&mut scripts.pool)?.compile(vec![module])?;

    Ok(scripts.pool)
}

fn check_compilation(code: &str) -> Result<(), Error> {
    check_compilation_pool(code)?;
    Ok(())
}

fn check_function_bytecode(code: &str, instrs: Vec<Instr<Offset>>) -> Result<(), Error> {
    let pool = check_compilation_pool(code)?;
    let match_ = pool
        .definitions()
        .find(|(_, def)| matches!(def.value, AnyDefinition::Function(_)))
        .map(|(_, def)| &def.value);

    if let Some(AnyDefinition::Function(fun)) = match_ {
        assert_eq!(fun.code, Code(instrs))
    } else {
        assert!(false, "No function found in the pool")
    }
    Ok(())
}

fn check_class_flags(pool: &ConstantPool, name: &str, flags: ClassFlags) -> Result<(), Error> {
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

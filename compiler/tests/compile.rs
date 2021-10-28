use std::io::Cursor;

use redscript::bundle::ScriptBundle;
use redscript::definition::ClassFlags;
use redscript::error::Error;
use redscript_compiler::parser;
use redscript_compiler::unit::CompilationUnit;

#[allow(unused)]
mod utils;

use utils::*;

#[test]
fn compile_simple_class() {
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

    compiled(sources).unwrap();
}

#[test]
fn compile_ext_class() {
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

    compiled(sources).unwrap();
}

#[test]
fn compile_class_with_forward_ref() {
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

    compiled(sources).unwrap();
}

#[test]
fn compile_class_with_shorthand_funcs() {
    let sources = "
        public class ShorthandTest {
            public func InstanceVal() -> String = ShorthandTest.StaticVal()
            public static func StaticVal() -> String = \"static\"
        }";

    compiled(sources).unwrap();
}

#[test]
fn compile_class_attributes() -> Result<(), Error> {
    let source = r#"
        public abstract class Base {}

        public final class Derived extends Base {}
    "#;

    let expected_base_flags = ClassFlags::new().with_is_abstract(true);
    let expected_derived_flags = ClassFlags::new().with_is_final(true);

    let pool = compiled(source)?;
    check_class_flags(&pool, "Base", expected_base_flags)?;
    check_class_flags(&pool, "Derived", expected_derived_flags)?;
    Ok(())
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

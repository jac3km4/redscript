use itertools::Itertools;
use redscript::definition::ClassFlags;

#[allow(unused)]
mod utils;

use redscript_compiler::diagnostics::Diagnostic;
use redscript_compiler::error::Cause;
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

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[]));
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

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[]));
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

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_class_with_shorthand_funcs() {
    let sources = "
        public class ShorthandTest {
            public func InstanceVal() -> String = ShorthandTest.StaticVal()
            public static func StaticVal() -> String = \"static\"
        }";

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_class_attributes() {
    let source = r#"
        public abstract class Base {}

        public final class Derived extends Base {}
    "#;

    let expected_base_flags = ClassFlags::new().with_is_abstract(true);
    let expected_derived_flags = ClassFlags::new().with_is_final(true);

    let (pool, _) = compiled(vec![source]).unwrap();
    check_class_flags(&pool, "Base", expected_base_flags).unwrap();
    check_class_flags(&pool, "Derived", expected_derived_flags).unwrap();
}

#[test]
fn compile_mutually_dependent_modules() {
    let sources1 = "
        module MyModule.Module1
        import MyModule.Module2.{B, Func2}

        public func Func1() -> Int32 = 2

        class A {
            func Thing() -> Int32 {
                Func2();
                return new B().Thing();
            }
        }";

    let sources2 = "
        module MyModule.Module2
        import MyModule.Module1.*

        public func Func2() -> Int32 = 2

        public class B {
            func Thing() -> Int32 = Func1()
        }";

    let (_, errs) = compiled(vec![sources1, sources2]).unwrap();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_lub_types() {
    let sources = "
        func Testing() {
            let a: array<ref<A>> = [ new C(), new B(), new A() ];
            let b: array<ref<A>> = [ new C(), new B(), new D() ];
            let c: ref<B> = true ? new B() : new C();
            let d: array<ref<A>> = [ true ? new B() :  new D() ];
            let a: ref<A> = true ? new A() : null;
        }

        class A {}
        class B extends A {}
        class C extends B {}
        class D extends A {}
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    let errs = errs.into_iter().filter(Diagnostic::is_fatal).collect_vec();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_casts() {
    let sources = "
        func Testing() {
            let a = Cast<Float>(1);
            let b: Int32 = Cast(a);
            let c = Cast<Double>(b);
        }

        native func Cast(i: Int32) -> Float;
        native func Cast(i: Int32) -> Double;
        native func Cast(i: Float) -> Int32;
        native func Cast(i: Float) -> Double;
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    let errs = errs.into_iter().filter(Diagnostic::is_fatal).collect_vec();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_intrinsic_null_cases() {
    let sources = "
        func Testing() {
            // Equals(null, new A()); // uncomment this after https://github.com/jac3km4/redscript/issues/69
            Equals(new A(), null);
            IsDefined(null);
        }

        class A {}
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn compile_structs() {
    let sources = "
        func Testing() {
            let a: A = new A(1, 2);
        }

        struct A {
            let x: Int32;
            let y: Int32;
        }
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    let errs = errs.into_iter().filter(Diagnostic::is_fatal).collect_vec();
    assert!(matches!(&errs[..], &[]));
}

#[test]
fn fail_on_invalid_structs() {
    let sources = "
        struct A {
            let x: Int32;
            let y: Int32;

            func Test() {}
        }
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[Diagnostic::CompileError(
        Cause::UnsupportedFeature(_),
        _
    )]));
}

#[test]
fn report_unused_variables() {
    let sources = "
        func Testing() {
            let x = 100;
            let y = x + 120;
            let z = 130;
            let w = y + 240; 
        }

        func OperatorAdd(x: Int32, y: Int32) -> Int32 = 0;
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[
        Diagnostic::UnusedLocal(_),
        Diagnostic::UnusedLocal(_),
    ]));
}

#[test]
fn report_missing_return() {
    let sources = "
        func Testing() -> Int32 {}
    ";

    let (_, errs) = compiled(vec![sources]).unwrap();
    assert!(matches!(&errs[..], &[Diagnostic::MissingReturn(_)]));
}

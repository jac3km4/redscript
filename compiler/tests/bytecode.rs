use redscript::bytecode::{Code, Instr, Offset};

#[allow(unused)]
mod utils;

use utils::*;

#[test]
fn compile_dynamic_casts() {
    let sources = "
        func Testing() {
            let b: wref<B> = new B();
            let a: wref<A> = b as A;
        }

        class A {}
        class B extends A {}
        ";

    let check = check_code![
        pat!(Assign),
        mem!(Local(b)),
        pat!(RefToWeakRef),
        mem!(New(class_b)),
        pat!(Assign),
        mem!(Local(a)),
        pat!(RefToWeakRef),
        mem!(DynamicCast(class_a, __)),
        pat!(WeakRefToRef),
        mem!(Local(b)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_base_class_overload() {
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

    let check = check_code![
        pat!(Assign),
        mem!(Local(b)),
        mem!(New(class)),
        pat!(Context(Offset { value: 38 })),
        mem!(Local(b)),
        pat!(InvokeStatic(Offset { value: 26 }, 0, _, 0)),
        pat!(I32Const(1)),
        pat!(I32Const(2)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_basic_casts() {
    let sources = "
        func Testing() {
            let a: Float = Cast(1);
            let b: String = Cast(2);
        }

        func Cast(i: Int32) -> Float = 0.0
        func Cast(i: Int32) -> String = \"\"
        ";

    let check = check_code![
        pat!(Assign),
        mem!(Local(a)),
        pat!(InvokeStatic(Offset { value: 21 }, 0, _, 0)),
        pat!(I32Const(1)),
        pat!(ParamEnd),
        pat!(Assign),
        mem!(Local(b)),
        pat!(InvokeStatic(Offset { value: 21 }, 0, _, 0)),
        pat!(I32Const(2)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_overloaded_call() {
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

    let check = check_code![
        pat!(Assign),
        mem!(Local(val)),
        mem!(New(class)),
        pat!(InvokeStatic(Offset { value: 35 }, 0, _, 0)),
        pat!(RefToWeakRef),
        mem!(Local(val)),
        mem!(Local(val)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_for_loop() {
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

    let check = check_code![
        mem!(ArrayPush(elem_type)),
        mem!(Local(tmp_array)),
        pat!(I32Const(0)),
        mem!(ArrayPush(elem_type)),
        mem!(Local(tmp_array)),
        pat!(I32Const(1)),
        pat!(Assign),
        mem!(Local(array)),
        mem!(Local(tmp_array)),
        pat!(Assign),
        mem!(Local(counter)),
        pat!(I32Const(0)),
        pat!(JumpIfFalse(Offset { value: 150 })),
        pat!(InvokeStatic(Offset { value: 43 }, 0, _, 0)),
        mem!(Local(counter)),
        mem!(ArraySize(elem_type)),
        mem!(Local(array)),
        pat!(ParamEnd),
        pat!(Assign),
        mem!(Local(i)),
        mem!(ArrayElement(elem_type)),
        mem!(Local(array)),
        mem!(Local(counter)),
        pat!(InvokeStatic(Offset { value: 34 }, 0, _, 0)),
        pat!(ToString(_)),
        mem!(Local(i)),
        pat!(ParamEnd),
        pat!(InvokeStatic(Offset { value: 30 }, 0, _, 0)),
        mem!(Local(counter)),
        pat!(I32Const(1)),
        pat!(ParamEnd),
        pat!(Jump(Offset { value: -147 })),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_nested_array_literals() {
    let sources = "
        func Testing() {
            let x = [[1, 2], [3, 4], [5, 6]];
        }
        ";

    let check = check_code![
        mem!(ArrayPush(int_type)),
        mem!(Local(array1)),
        pat!(I32Const(1)),
        mem!(ArrayPush(int_type)),
        mem!(Local(array1)),
        pat!(I32Const(2)),
        mem!(ArrayPush(int_array_type)),
        mem!(Local(array_of_arrays)),
        mem!(Local(array1)),
        mem!(ArrayPush(int_type)),
        mem!(Local(array2)),
        pat!(I32Const(3)),
        mem!(ArrayPush(int_type)),
        mem!(Local(array2)),
        pat!(I32Const(4)),
        mem!(ArrayPush(int_array_type)),
        mem!(Local(array_of_arrays)),
        mem!(Local(array2)),
        mem!(ArrayPush(int_type)),
        mem!(Local(array3)),
        pat!(I32Const(5)),
        mem!(ArrayPush(int_type)),
        mem!(Local(array3)),
        pat!(I32Const(6)),
        mem!(ArrayPush(int_array_type)),
        mem!(Local(array_of_arrays)),
        mem!(Local(array3)),
        pat!(Assign),
        mem!(Local(x)),
        mem!(Local(array_of_arrays)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_variant_conversions() {
    let sources = "
        func Testing() {
            let x = ToVariant(new A());
            let y: ref<A> = FromVariant(x);
        }

        class A {}
        ";

    let check = check_code![
        pat!(Assign),
        mem!(Local(x)),
        mem!(ToVariant(typ)),
        mem!(New(class)),
        pat!(Assign),
        mem!(Local(y)),
        mem!(FromVariant(typ)),
        mem!(Local(x)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_implicit_conversions() {
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

    let check = check_code![
        pat!(InvokeStatic(Offset { value: 30 }, 0, _, 0)),
        mem!(AsRef(typ)),
        pat!(StringConst(_)),
        pat!(ParamEnd),
        pat!(Assign),
        mem!(Local(a)),
        pat!(RefToWeakRef),
        mem!(New(class)),
        pat!(InvokeStatic(Offset { value: 26 }, 0, _, 0)),
        pat!(WeakRefToRef),
        mem!(Local(a)),
        pat!(ParamEnd),
        pat!(Context(Offset { value: 51 })),
        pat!(WeakRefToRef),
        mem!(Local(a)),
        pat!(InvokeVirtual(Offset { value: 38 }, 0, _, 0)),
        pat!(Context(Offset { value: 22 })),
        pat!(WeakRefToRef),
        mem!(Local(a)),
        mem!(ObjectField(member)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_switch_case() {
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

    let check = check_code![
        pat!(Switch(_, Offset { value: 41 })),
        pat!(InvokeStatic(Offset { value: 30 }, 0, _, 0)),
        mem!(Param(val)),
        pat!(I32Const(4)),
        pat!(ParamEnd),
        pat!(SwitchLabel(Offset { value: 10 }, Offset { value: 30 })),
        pat!(I32Const(1)),
        pat!(SwitchLabel(Offset { value: 10 }, Offset { value: 20 })),
        pat!(I32Const(2)),
        pat!(SwitchLabel(Offset { value: 13 }, Offset { value: 10 })),
        pat!(I32Const(3)),
        pat!(Jump(Offset { value: 6 })),
        pat!(SwitchDefault),
        pat!(Return),
        pat!(TrueConst),
        pat!(Return),
        pat!(FalseConst),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_ternary_op() {
    let sources = "
        func Testing(val: Int32) -> Bool = val % 2 == 0 ? true : false

        func OperatorModulo(l: Int32, r: Int32) -> Int32 = 0
        func OperatorEqual(l: Int32, r: Int32) -> Bool = false
        ";

    let check = check_code![
        pat!(Return),
        pat!(Conditional(Offset { value: 57 }, Offset { value: 58 })),
        pat!(InvokeStatic(Offset { value: 51 }, 0, _, 0)),
        pat!(InvokeStatic(Offset { value: 30 }, 0, _, 0)),
        mem!(Param(val)),
        pat!(I32Const(2)),
        pat!(ParamEnd),
        pat!(I32Const(0)),
        pat!(ParamEnd),
        pat!(TrueConst),
        pat!(FalseConst),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_if_else() {
    let sources = "
        func Testing(bool: Bool) -> Int32 {
            if bool {
                return 1;
            } else {
                return 0;
            }
        }
        ";

    let check = check_code![
        pat!(JumpIfFalse(Offset { value: 21 })),
        mem!(Param(bool)),
        pat!(Return),
        pat!(I32Const(1)),
        pat!(Jump(Offset { value: 9 })),
        pat!(Return),
        pat!(I32Const(0)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_method_overload_call() {
    let sources = "
        class B extends A {
            func Testing() -> Int32 = super.Testing()
        }

        class A {
            func Testing() -> Int32 = 0
        }
        ";

    let check = check_code![
        pat!(Return),
        pat!(Context(Offset { value: 20 })),
        pat!(This),
        pat!(InvokeStatic(Offset { value: 16 }, 0, _, 0)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_null_wref_assignment() {
    let sources = "
        func Testing() {
            let a: wref<A> = null;
        }

        class A {}
        ";

    let check = check_code![pat!(Assign), mem!(Local(a)), pat!(RefToWeakRef), pat!(Null), pat!(Nop)];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[allow(illegal_floating_point_literal_pattern)]
#[test]
fn compile_number_literals() {
    let sources = "
        func Testing() {
            let a: Float = 1;
            a = 2.0;
            let b: Double = 3;
            b = 4.0;
            b = 5.0d;
            let c: Int32 = 6;
            let d: Int64 = 7;
            d = 8l;
            let e: Uint32 = 9u;
            let f: Uint64 = 10u;
        }
        ";

    let check = check_code![
        pat!(Assign),
        mem!(Local(a)),
        pat!(F32Const(1.0)),
        pat!(Assign),
        mem!(Local(a)),
        pat!(F32Const(2.0)),
        pat!(Assign),
        mem!(Local(b)),
        pat!(F64Const(3.0)),
        pat!(Assign),
        mem!(Local(b)),
        pat!(F64Const(4.0)),
        pat!(Assign),
        mem!(Local(b)),
        pat!(F64Const(5.0)),
        pat!(Assign),
        mem!(Local(c)),
        pat!(I32Const(6)),
        pat!(Assign),
        mem!(Local(d)),
        pat!(I64Const(7)),
        pat!(Assign),
        mem!(Local(d)),
        pat!(I64Const(8)),
        pat!(Assign),
        mem!(Local(e)),
        pat!(U32Const(9)),
        pat!(Assign),
        mem!(Local(f)),
        pat!(U64Const(10)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_string_literals() {
    let sources = r#"
        func Testing() {
            let a: String = "\u{1F4A9}";
            let b: CName = n"back";
            //let c: ResRef = r"base\\gameplay\\gui\\common\\buttonhints.inkwidget";
            let d: TweakDBID = t"MappinIcons.QuestMappin";
        }
        "#;

    let check = check_code![
        pat!(Assign),
        mem!(Local(a)),
        pat!(StringConst(_)),
        pat!(Assign),
        mem!(Local(b)),
        pat!(NameConst(_)),
        pat!(Assign),
        mem!(Local(c)),
        pat!(TweakDbIdConst(_)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_is_defined() {
    let sources = "
        func Testing() {
            let x = new A();
            if IsDefined(x) {}
            let y: wref<A> = new A();
            if IsDefined(y) {}
        }

        class A {}
        ";

    let check = check_code![
        pat!(Assign),
        mem!(Local(x)),
        mem!(New(class)),
        pat!(JumpIfFalse(Offset { value: 13 })),
        pat!(RefToBool),
        mem!(Local(x)),
        pat!(Assign),
        mem!(Local(y)),
        pat!(RefToWeakRef),
        mem!(New(class)),
        pat!(JumpIfFalse(Offset { value: 13 })),
        pat!(WeakRefToBool),
        mem!(Local(y)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_enum() {
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

    let check = check_code![
        pat!(Switch(_, Offset { value: 20 })),
        mem!(Param(dir)),
        pat!(SwitchLabel(Offset { value: 42 }, Offset { value: 22 })),
        pat!(EnumConst(_, _)),
        pat!(Return),
        pat!(EnumToI32(_, 4)),
        mem!(Param(dir)),
        pat!(SwitchLabel(Offset { value: 42 }, Offset { value: 22 })),
        pat!(EnumConst(_, _)),
        pat!(Return),
        pat!(EnumToI32(_, 4)),
        mem!(Param(dir)),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_empty_return() {
    let sources = r#"
        func Testing(bool: Bool) -> Void {
            if bool {
                return;
            }
            Log("hello");
        }

        func Log(str: String) -> Void {}
        "#;

    let check = check_code![
        pat!(JumpIfFalse(Offset { value: 14 })),
        mem!(Param(bool)),
        pat!(Return),
        pat!(Nop),
        pat!(InvokeStatic(Offset { value: 21 }, 0, _, 0)),
        pat!(StringConst(_)),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_string_interpolation() {
    let sources = r#"
        func Testing(year: Int32) -> String {
            let birthYear = 1990;
            let name = "John";
            return s"My name is \(name) and I am \(year - birthYear) years old";
        }

        func OperatorAdd(a: ref<Script_RefString>, b: ref<Script_RefString>) -> String
        func OperatorSubtract(a: Int32, b: Int32) -> Int32

        class Script_RefString {}
        "#;

    let check = check_code![
        pat!(Assign),
        mem!(Local(birth_year)),
        pat!(I32Const(1990)),
        pat!(Assign),
        mem!(Local(name)),
        pat!(StringConst(_)),
        pat!(Return),
        pat!(InvokeStatic(Offset { value: 203 }, 0, _, 3)),
        mem!(AsRef(str_type)),
        pat!(InvokeStatic(Offset { value: 87 }, 0, _, 2)),
        mem!(AsRef(str_type)),
        pat!(StringConst(_)),
        mem!(AsRef(str_type)),
        pat!(InvokeStatic(Offset { value: 48 }, 0, _, 0)),
        mem!(AsRef(str_type)),
        mem!(Local(name)),
        mem!(AsRef(str_type)),
        pat!(StringConst(_)),
        pat!(ParamEnd),
        pat!(ParamEnd),
        mem!(AsRef(str_type)),
        pat!(InvokeStatic(Offset { value: 82 }, 0, _, 1)),
        mem!(AsRef(str_type)),
        pat!(ToString(_)),
        pat!(InvokeStatic(Offset { value: 34 }, 0, _, 0)),
        mem!(Param(year)),
        mem!(Local(birth_year)),
        pat!(ParamEnd),
        mem!(AsRef(str_type)),
        pat!(StringConst(_)),
        pat!(ParamEnd),
        pat!(ParamEnd),
        pat!(Nop)
    ];
    TestContext::compiled(vec![sources]).unwrap().run("Testing", check)
}

#[test]
fn compile_conditional_functions() {
    let sources1 = r#"
        module My.Mod

        @if(ModuleExists("Other.Mod"))
        func Testing() -> Int32 {
            return 1;
        }

        @if(!ModuleExists("Other.Mod"))
        func Testing() -> Int32 {
            return 2;
        }
        "#;

    let sources2 = r#"module Other.Mod"#;

    let check = check_code![pat!(Return), pat!(I32Const(1)), pat!(Nop)];
    TestContext::compiled(vec![sources1, sources2])
        .unwrap()
        .run("My.Mod.Testing", check);

    let check = check_code![pat!(Return), pat!(I32Const(2)), pat!(Nop)];
    TestContext::compiled(vec![sources1])
        .unwrap()
        .run("My.Mod.Testing", check)
}

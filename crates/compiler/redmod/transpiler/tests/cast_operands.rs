mod support;

const CASTS: &str =
    "native func Cast(value: Int32) -> Float\nnative func Cast(value: Float) -> Int32";

const SOURCE: &str = r#"
native func OperatorAdd(left: Int32, right: Int32) -> Int32
native func OperatorNeg(value: Int32) -> Int32
native func OperatorAssignAdd(out left: Int32, right: Int32) -> Int32
func CastOperandIdentity(value: Int32) -> Int32 { return value; }
func CastOperandLocal(value: Int32) -> Float { return Cast<Float>(value); }
func CastOperandCall(value: Int32) -> Float { return Cast<Float>(CastOperandIdentity(value)); }
func CastOperandUnary(value: Int32) -> Float { return Cast<Float>(-value); }
func CastOperandSum(left: Int32, right: Int32) -> Float { return Cast<Float>(left + right); }
func CastOperandChoice(flag: Bool, left: Int32, right: Int32) -> Float {
    return Cast<Float>(flag ? left : right);
}
func CastOperandAssign(left: Int32, right: Int32) -> Float { return Cast<Float>(left = right); }
func CastOperandCompound(left: Int32, right: Int32) -> Float { return Cast<Float>(left += right); }
func CastOperandNested(value: Float) -> Float { return Cast<Float>(Cast<Int32>(value)); }
"#;

#[test]
fn cast_operands_are_parenthesized() {
    insta::assert_snapshot!(support::emit_parsed(SOURCE, CASTS));
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn cast_operands_with_official_compiler() {
    redscript_test_support::OfficialCompiler::from_env().compile(
        "cast-operands",
        &support::without_synthetic_operator_imports(&support::emit_parsed(SOURCE, CASTS)),
    );
}

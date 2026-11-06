mod support;

const SOURCE: &str = r#"
native func OperatorAssignAdd(out left: Int32, right: Int32) -> Int32
native func OperatorAssignSubtract(out left: Int32, right: Int32) -> Int32
func TernaryStores(flag: Bool, left: Int32, right: Int32) -> Int32 {
    return flag ? (left = 1) : (right = 2);
}
func TernaryCompound(flag: Bool, left: Int32, right: Int32) -> Int32 {
    return flag ? (left += 3) : (right -= 4);
}
func TernaryNested(flag: Bool, other: Bool, left: Int32, right: Int32) -> Int32 {
    return flag ? (other ? (left = 5) : (right += 6)) : (other ? (left -= 7) : (right = 8));
}
func TernaryCondition(flag: Bool, other: Bool, left: Int32, right: Int32) -> Int32 {
    return (flag ? (other = true) : (other = false)) ? (left = 9) : (right = 10);
}
func TernaryChained(flag: Bool, left: Int32, right: Int32) -> Int32 {
    return flag ? (left = right = 11) : (right += left += 12);
}
"#;

#[test]
fn assignment_branches_keep_grouping() {
    insta::assert_snapshot!(support::emit(SOURCE, ""));
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn assignment_branches_with_official_compiler() {
    support::validate_official("ternary-assignments", &support::emit(SOURCE, ""));
}

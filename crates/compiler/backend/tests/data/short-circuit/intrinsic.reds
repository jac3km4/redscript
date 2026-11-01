@intrinsic(Equals)
func LazyEquals(lhs: Bool, skip rhs: Bool) -> Bool;

func Test() -> Bool {
  return LazyEquals(false, true);
}

enum OverflowingEnum {
  Maximum = 1, // Replaced with i64::MAX by the harness
  Overflow,
}

enum NearMaximumEnum {
  NearMaximum = 2, // Replaced with i64::MAX - 1 by the harness
  Maximum, // OK: implicit increment reaches i64::MAX
}

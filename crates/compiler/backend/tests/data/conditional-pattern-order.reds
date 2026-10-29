func Test(skip: Bool) -> Int32 {
  if skip {
    return 0;
  } else if let [first] = Next([1]) {
    return first;
  } else if let [second] = Next([2]) {
    return second;
  } else {
    return 3;
  }
}

func TestFirstPattern() -> Int32 {
  if let [first] = Next([1]) {
    return first;
  } else if false {
    return 0;
  } else if let [last] = Next([2]) {
    return last;
  }
  return 3;
}

func TestWhile() {
  while let [value] = Next([1]) {
    continue;
  }
}

native func Next(values: [Int32]) -> [Int32]
native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool

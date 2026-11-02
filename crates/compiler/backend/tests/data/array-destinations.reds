native func First() -> Int32;
native func Second() -> Int32;
native func OperatorAssignAdd(out lhs: Int32, rhs: Int32) -> Int32;
native func OperatorLess(lhs: Int32, rhs: Int32) -> Bool;

func Fresh() {
  let values = [First(), Second()];
  const typed: [Int32] = [3];
}

func Repeated(again: Bool) {
  while again {
    let empty: [Int32] = [];
    ArrayPush(empty, 1);
    let filled = [First(), Second()];
    ArrayPush(filled, 3);
  }
}

func Shadowed() {
  let values = [1];
  if true {
    let values = [values[0]];
  }
}

func ExistingAssignment() {
  let values = [1, 2];
  values = [values[1], values[0]];
}

func RetainedAliasWhile(again: Bool) {
  let seed = [1, 2];
  let previous: script_ref<[Int32]> = AsRef(seed);
  while again {
    let values = [previous[1], previous[0]];
    previous = AsRef(values);
  }
}

func RetainedAliasNested(again: Bool) {
  let seed = [1, 2];
  let previous: script_ref<[Int32]> = AsRef(seed);
  while again {
    if again {
      if again {
        let values = [previous[1], previous[0]];
        previous = AsRef(values);
      }
    } else {
      let empty: [Int32] = [];
      ArrayPush(empty, 1);
    }
  }
}

func RetainedAliasFor(again: Bool) {
  let seed = [1, 2];
  let previous: script_ref<[Int32]> = AsRef(seed);
  for item in seed {
    let empty: [Int32] = [];
    ArrayPush(empty, item);
    if again {
      let values = [previous[1], previous[0]];
      previous = AsRef(values);
    }
  }
}

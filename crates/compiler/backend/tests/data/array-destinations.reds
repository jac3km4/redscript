native func First() -> Int32;
native func Second() -> Int32;

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

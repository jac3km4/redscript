func DefaultThenArray() {
  let value: Variant;
  let values: array<Int32>;
  value = ToVariant(7);
}

func DefaultThenReturn() {
  let value: Variant;
  return;
}

func DefaultInLoop(again: Bool) {
  while again {
    let first: Variant;
    let second: Variant;
    first = ToVariant("payload");
    second = ToVariant(7);
  }
}

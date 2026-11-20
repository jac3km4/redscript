func ContinueBackEdge(again: Bool) {
  while again {
    let value: Variant;
    if again {
      continue;
    }
    value = ToVariant(7);
  }
}

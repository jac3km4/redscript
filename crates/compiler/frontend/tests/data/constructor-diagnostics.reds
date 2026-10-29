struct Value {
  let value: Int32;
}

@neverRef()
class NeverRef {
  let value: Int32;
}

@privateConstructor("")
class Private {}

func Test() {
  new Value(42);
  new NeverRef(42);
  new Private();
}

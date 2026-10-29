native func FixedArguments(first: Int32, second: Int32)
native func OptionalArguments(required: Int32, opt second: Int32, opt third: Int32)

func TestFixedArguments() {
  FixedArguments(1);
  FixedArguments(1, 2, 3);
  FixedArguments(1, 2); // OK
}

func TestOptionalArguments() {
  OptionalArguments();
  OptionalArguments(1, 2, 3, 4);
  OptionalArguments(1); // OK: required arguments only
  OptionalArguments(1, 2); // OK: some optional arguments
  OptionalArguments(1, 2, 3); // OK: all arguments
}

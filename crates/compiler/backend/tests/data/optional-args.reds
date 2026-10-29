native func OptionalArgs(opt first: Int32, opt second: Int32)

func Test() {
  OptionalArgs();
  OptionalArgs(1);
  OptionalArgs(1, 2);
}

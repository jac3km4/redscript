func Tick(out n: Int32) -> Int32 { n = 1; return n; }
func Chain(out a: Variant, out b: Int32, out n: Int32) -> Variant {
  return a = b = Tick(n);
}
func Indexed(out values: [Int32], out n: Int32) -> Int32 {
  return values[Tick(n)] = Tick(n);
}
class Holder {
  public let value: Int32;
}
func Receiver(holder: ref<Holder>, out n: Int32) -> ref<Holder> {
  n = 1;
  return holder;
}
func Field(holder: ref<Holder>, out n: Int32) -> Int32 {
  return Receiver(holder, n).value = Tick(n);
}
func Conditional(out a: Int32, flag: Bool) -> Int32 {
  return flag ? (a = 1) : (a = 2);
}
func ReadWeak(value: script_ref<ref<Holder>>) -> wref<Holder> { return value; }
func WrapWeak(value: ref<Holder>) -> script_ref<wref<Holder>> { return value; }
func RefIndex(values: script_ref<[Int32]>) -> Int32 { return values[0]; }

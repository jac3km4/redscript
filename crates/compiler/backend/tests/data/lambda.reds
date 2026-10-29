
func Test() {
  let a = 1;
  let f = (b: Int32) -> (c: Int32) -> a + b + c;
  let g = f(2);
  let result = g(3);
}

native func OperatorAdd(a: Int32, b: Int32) -> Int32

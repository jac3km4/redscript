
native func OperatorAdd(a: Int32, b: Int32) -> Int32
native func OperatorAdd(a: String, b: String) -> String
native func OperatorAssignAdd(out a: Int32, b: Int32) -> Int32
native func OperatorLess(a: Int32, b: Int32) -> Bool
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool

func Tick() -> Bool = true
func Inline(name: Int32) -> Int32 = name
func Empty() -> [Int32] = []

func Test(items: [Int32], enabled: Bool) -> String {
  let result = "";
  for item in items {
    let scratch: [Int32];
    let zero: Int32;
    ArrayPush(scratch, item);
    if enabled && Tick() {
      let name = item;
      result = s"value=\(name)";
      continue;
    } else {
      let name = zero;
      ArrayPush(scratch, name);
    }
  }
  return result;
}

struct Cell {
  public let value: Int32;
}

class Holder {
  public let items: [Cell];
}

native func GetHolder() -> Holder
native func NextIndex() -> Int32
native func NextValue() -> Int32

func Test() {
  GetHolder().items[NextIndex()].value = NextValue();
}

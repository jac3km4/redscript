func EmptyPredecessor(value: Int32) {
  switch value {
    case 0:
    case let bound:
      break;
  }
}

func NonemptyPredecessor(value: Int32) {
  switch value {
    case 0:
      let other = value;
    case let bound:
      break;
  }
}

func ChainedPredecessors(value: Int32) {
  switch value {
    case 0:
    case 1:
    case let bound:
      break;
  }
}

class Base {
  public func IsA(name: CName) -> Bool {}
}

class Derived extends Base {
  public let field: Int32;
}

func DestructuringPredecessor(value: ref<Base>) -> Int32 {
  switch value {
    case null:
    case let Derived { field }:
      return field;
  }
  return 0;
}

native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
native func OperatorEqual(lhs: ref<Base>, rhs: ref<Base>) -> Bool;

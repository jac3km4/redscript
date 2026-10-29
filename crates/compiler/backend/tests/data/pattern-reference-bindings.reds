abstract class Scriptable {
  public func IsA(name: CName) -> Bool {}
}

class Base extends Scriptable {}

class Derived extends Base {
  public let value: Int32;
}

func WeakBinding(input: wref<Base>) -> Derived {
  if let narrowed as Derived = input {
    return narrowed;
  }
  return null;
}

func ScriptBinding(input: script_ref<Base>) -> Derived {
  if let narrowed as Derived = input {
    return narrowed;
  }
  return null;
}

func StrongBinding(input: ref<Base>) -> Derived {
  if let narrowed as Derived = input {
    return narrowed;
  }
  return null;
}

func NestedWeakBinding(input: wref<Base>) -> Int32 {
  if let Derived { value } as Derived = input {
    return value;
  }
  return 0;
}

native func OperatorLogicAnd(lhs: Bool, rhs: Bool) -> Bool

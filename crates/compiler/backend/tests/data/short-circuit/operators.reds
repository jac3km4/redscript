func SideEffect(out state: Bool, value: Bool) -> Bool {
  state = value;
  return state;
}

func TestAnd(lhs: Bool, out state: Bool) -> Bool {
  return lhs && SideEffect(state, true);
}

func TestOr(lhs: Bool, out state: Bool) -> Bool {
  return lhs || SideEffect(state, false);
}

func TestAndOr(lhs: Bool, out state: Bool) -> Bool {
  return lhs && (SideEffect(state, false) || SideEffect(state, true));
}

func TestOrAnd(lhs: Bool, out state: Bool) -> Bool {
  return lhs || (SideEffect(state, true) && SideEffect(state, false));
}

func TestNestedLhs(lhs: Bool, out state: Bool) -> Bool {
  return (lhs && SideEffect(state, true)) || SideEffect(state, false);
}

abstract class Scriptable {
  public func IsA(name: CName) -> Bool {}
}

class Class extends Scriptable {
  public let nested: Class;
}

func TestNested(inst: Class) -> Class {
  if let Class { nested: Class { nested: nested? } } = inst {
    return nested;
  }
  return null;
}

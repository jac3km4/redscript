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

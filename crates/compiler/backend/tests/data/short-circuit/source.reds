native func OperatorLogicAnd(lhs: Bool, skip rhs: Bool) -> Bool;
native func OperatorLogicOr(lhs: Bool, skip rhs: Bool) -> Bool;

func Scripted(lhs: Bool, skip rhs: Bool) -> Bool {
  return rhs;
}

func Effect(out state: Bool, value: Bool) -> Bool {
  state = value;
  return state;
}

func AndFalse(out state: Bool) -> Bool {
  return false && Effect(state, true);
}

func AndTrue(out state: Bool) -> Bool {
  return true && Effect(state, true);
}

func OrFalse(out state: Bool) -> Bool {
  return false || Effect(state, false);
}

func OrTrue(out state: Bool) -> Bool {
  return true || Effect(state, false);
}

func Nested(out state: Bool) -> Bool {
  return Effect(state, true) && (Effect(state, false) || Effect(state, true));
}

func Explicit(out state: Bool) -> Bool {
  return Scripted(Effect(state, true), Effect(state, false));
}

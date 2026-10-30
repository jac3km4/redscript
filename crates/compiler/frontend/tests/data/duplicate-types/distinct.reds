module Second

public class SharedClass {
  public func Value() -> Bool { return true; }
}

public struct SharedStruct {
  public static func Value() -> Bool { return false; }
}

func UseDistinct(value: SharedClass) -> Bool {
  value.Value();
  return SharedStruct.Value();
}

module First

public class SharedClass {
  public func Value() -> Int32 { return 1; }
}

public struct SharedStruct {
  public static func Value() -> Int32 { return 2; }
}

func UseOriginals(value: SharedClass) -> Int32 {
  value.Value();
  return SharedStruct.Value();
}

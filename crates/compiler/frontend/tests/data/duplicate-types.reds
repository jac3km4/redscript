class DuplicateClass {
  public func Value() -> Int32 { return 1; }
}

class DuplicateClass {}

struct DuplicateStruct {
  public static func Value() -> Int32 { return 2; }
}

struct DuplicateStruct {}

class ClassThenStruct {
  public func Value() -> Int32 { return 3; }
}

struct ClassThenStruct {}

struct StructThenClass {
  public static func Value() -> Int32 { return 4; }
}

class StructThenClass {}

class ClassThenEnum {
  public func Value() -> Int32 { return 5; }
}

enum ClassThenEnum { Rejected = 0 }

enum EnumThenClass { Original = 0 }

class EnumThenClass {}

func UseOriginals(value: DuplicateClass) -> Int32 {
  value.Value();
  return DuplicateStruct.Value();
}

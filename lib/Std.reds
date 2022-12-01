module Std

public abstract class Comparable extends IScriptable {
  public func Equals(other: Any) -> Bool = this.Compare(other) == 0;
  public func Compare(other: Any) -> Int32;
}

public abstract class Hashable extends Comparable {
  public func Hash() -> Uint64;
}

public abstract class Primitive extends Hashable {
  public func ToString() -> String;
}

public abstract class Integral extends Primitive {
  public func Increment();
  public func Decrement();
}

public class Tuple<A, B> {
  let first: A;
  let second: B;

  public static func New(a: A, b: B) -> Tuple<A, B> {
    let self = new Tuple();
    self.first = a;
    self.second = b;
    return self;
  }

  public func Flipped() -> Tuple<B, A> {
    let self = new Tuple();
    self.first = this.second;
    self.second = this.first;
    return self;
  }
}

public func LogDbg(str: script_ref<String>) {
  LogChannel(n"DEBUG", str);
}

public func Dump(val: Any) {
  LogDbg(Stringify(val));
}

public func Stringify(val: Any) -> String {
  if IsDefined(val as Primitive) {
    return (val as Primitive).ToString();
  } else if IsDefined(val as BoxedStruct) {
    return ToString((val as BoxedStruct).value);
  } else if IsDefined(val as BoxedVariant) {
    return ToString((val as BoxedVariant).value);
  } else {
    return ToString(val);
  }
}

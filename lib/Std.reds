module Std

abstract class Comparable extends IScriptable {
  public func Equals(other: Any) -> Bool = this.Compare(other) == 0;
  public func Compare(other: Any) -> Int32;
}

abstract class Hashable extends Comparable {
  public func Hash() -> Uint64;
}

abstract class Integral extends Hashable {
  public func Increment();
  public func Decrement();
}

class Tuple<A, B> {
  let first: A;
  let second: B;

  static func New(a: A, b: B) -> Tuple<A, B> {
    let self = new Tuple();
    self.first = a;
    self.second = b;
    return self;
  }

  func Flipped() -> Tuple<B, A> {
    let self = new Tuple();
    self.first = this.second;
    self.second = this.first;
    return self;
  }
}

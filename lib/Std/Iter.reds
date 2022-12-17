module Std.Iter
import Std.Hashable

public abstract class Iterable<A> {
  public func Iter() -> Iter<A>;
}

public abstract class Extendable<A> extends Iterable<A> {
  public func Extend(elem: A);
}

public abstract class Iter<A> {
  public func HasNext() -> Bool;
  public func Next() -> A;

  public static func Repeat(value: A) -> Iter<A> =
    Repeat.New(value)

  public final func Map<B>(f: (A) -> B) -> Iter<B> =
    Mapped.New(this, f)
  
  public final func Take(n: Int32) -> Iter<A> =
    Take.New(this, n)
  
  public final func Filter(pred: (A) -> Bool) -> Iter<A> =
    Filtered.New(this, pred)

  public final func ForEach(f: (A) -> Unit) {
    while this.HasNext() {
      f(this.Next());
    }
  }

  public final func Reduce(f: (A, A) -> A) -> A {
    if !this.HasNext() {
      return null;
    }
    let state = this.Next();
    while this.HasNext() {
      state = f(state, this.Next());
    }
    return state;
  }

  public final func Into<C extends Extendable<A>>(out sink: C) -> C {
    while this.HasNext() {
      sink.Extend(this.Next());
    }
    return sink;
  }
}

public class Range extends Iter<Int32> {
  let current: Int32;
  let max: Int32;

  public static func New(min: Int32, max: Int32) -> Range {
    let self = new Range();
    self.current = min;
    self.max = max;
    return self;
  }

  public func HasNext() -> Bool =
    this.current < this.max

  public func Next() -> Int32 {
    let cur = this.current;
    this.current += 1;
    return cur;
  }
}

class Repeat<A> extends Iter<A> {
  let value: A;

  static func New(val: A) -> Repeat<A> {
    let self = new Repeat();
    self.value = val;
    return self;
  }

  public func HasNext() -> Bool = true
  public func Next() -> A = this.value;
}

class Mapped<A, B> extends Iter<B> {
  let base: Iter<A>;
  let map: (A) -> B;

  static func New(base: Iter<A>, map: (A) -> B) -> Mapped<A, B> {
    let self = new Mapped();
    self.base = base;
    self.map = map;
    return self;
  }

  public func HasNext() -> Bool = this.base.HasNext()
  public func Next() -> B = this.map.Apply(this.base.Next())
}

class Take<A> extends Iter<A> {
  let base: Iter<A>;
  let remaining: Int32;

  static func New(base: Iter<A>, count: Int32) -> Take<A> {
    let self = new Take();
    self.base = base;
    self.remaining = count;
    return self;
  }

  public func HasNext() -> Bool =
    this.base.HasNext() && this.remaining > 0

  public func Next() -> A {
    this.remaining -= 1;
    return this.base.Next();
  }
}

class Filtered<A> extends Iter<A> {
  let base: Iter<A>;
  let predicate: (A) -> Bool;
  let current: A;

  static func New(base: Iter<A>, predicate: (A) -> Bool) -> Filtered<A> {
    let self = new Filtered();
    self.base = base;
    self.predicate = predicate;
    return self;
  }

  public func HasNext() -> Bool {
    while this.base.HasNext() {
      let el = this.base.Next();
      if this.predicate.Apply(el) {
        this.current = el;
        return true;
      }
    }
    return false;
  }

  public func Next() -> A = this.current
}

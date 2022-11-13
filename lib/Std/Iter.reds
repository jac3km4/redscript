module Std.Iter
import Std.{Hashable, Integral}

abstract class Iterable<A> {
  func Iter() -> Iter<A>;
}

abstract class Extendable<A> extends Iterable<A> {
  func Extend(elem: A);
}

abstract class Iter<A> {
  func HasNext() -> Bool;
  func Next() -> A;

  static func Repeat(value: A) -> Iter<A> = Repeat.New(value)

  final func Map<B>(f: (A) -> B) -> Iter<B> = Mapped.New(this, f)
  final func Take(n: Int32) -> Iter<A> = Take.New(this, n)
  final func Filter(pred: (A) -> Bool) -> Iter<A> = Filtered.New(this, pred)

  final func ForEach(f: (A) -> Unit) {
    while this.HasNext() {
      f(this.Next());
    }
  }

  final func Reduce(f: (A, A) -> A) -> A {
    if !this.HasNext() {
      return null;
    }
    let state = this.Next();
    while this.HasNext() {
      state = f(state, this.Next());
    }
    return state;
  }

  final func Into<C extends Extendable<A>>(out sink: C) -> C {
    while this.HasNext() {
      sink.Extend(this.Next());
    }
    return sink;
  }
}

class Range<A extends Integral> extends Iter<A> {
  let current: A;
  let max: A;

  static func New(min: A, max: A) -> Range<A> {
    let self = new Range();
    self.current = min;
    self.max = max;
    // hacky solution, change this once overrides work correctly
    self.current.Decrement();
    return self;
  }

  func HasNext() -> Bool = !this.current.Equals(this.max) 

  func Next() -> A {
    this.current.Increment();
    return this.current;
  }
}

class Repeat<A> extends Iter<A> {
  let value: A;

  static func New(val: A) -> Repeat<A> {
    let self = new Repeat();
    self.value = val;
    return self;
  }

  func HasNext() -> Bool = true
  func Next() -> A = this.value;
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

  func HasNext() -> Bool = this.base.HasNext()
  func Next() -> B = this.map.Apply(this.base.Next())
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

  func HasNext() -> Bool =
    this.base.HasNext() && this.remaining > 0

  func Next() -> A {
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

  func HasNext() -> Bool {
    while this.base.HasNext() {
      let el = this.base.Next();
      if this.predicate.Apply(el) {
        this.current = el;
        return true;
      }
    }
    return false;
  }

  func Next() -> A = this.current
}

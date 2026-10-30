class Animal {}
class Dog extends Animal {}
class Chair {}

class Store<T> {
  public func Take<U extends T>(x: U) -> U { return x; }
  public func Pick<U extends T>(x: U, tag: Int32) -> U { return x; }
  public func Pick<U extends T>(x: U, tag: String) -> U { return x; }
  public func Shadow<T extends Animal, U extends T>(x: U) -> U { return x; }
  public func ShadowPick<T extends Animal, U extends T>(x: U, tag: Int32) -> U { return x; }
  public func ShadowPick<T extends Animal, U extends T>(x: U, tag: String) -> U { return x; }
}
class Derived<V> extends Store<V> {}

func BadExplicit(s: Store<Animal>, c: Chair) { s.Take<Chair>(c); }
func BadInferred(s: Store<Animal>, c: Chair) { s.Take(c); }
func BadInheritedExplicit(s: Derived<Animal>, c: Chair) { s.Take<Chair>(c); }
func BadInheritedInferred(s: Derived<Animal>, c: Chair) { s.Take(c); }
func BadOverloadedExplicit(s: Store<Animal>, c: Chair) { s.Pick<Chair>(c, 1); }
func BadOverloadedInferred(s: Store<Animal>, c: Chair) { s.Pick(c, "tag"); }
func BadInheritedOverloadedExplicit(s: Derived<Animal>, c: Chair) { s.Pick<Chair>(c, "tag"); }
func BadInheritedOverloadedInferred(s: Derived<Animal>, c: Chair) { s.Pick(c, 1); }
func BadShadow(s: Store<Animal>, c: Chair) { s.Shadow<Chair, Chair>(c); }
func BadShadowOverloaded(s: Store<Animal>, c: Chair) { s.ShadowPick<Chair, Chair>(c, 1); }
func TooMany(s: Store<Animal>, d: Dog) { s.Take<Animal, Dog>(d); }

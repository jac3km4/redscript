class Animal {}
class Dog extends Animal {}
class Chair {}

// Redeclaring T is diagnosed, but recovery must use the method's T rather
// than the receiver's Chair when resolving U's bound.
class Store<T> {
  public func Shadow<T extends Animal, U extends T>(x: U) -> U { return x; }
  public func ShadowPick<T extends Animal, U extends T>(x: U, tag: Int32) -> U { return x; }
  public func ShadowPick<T extends Animal, U extends T>(x: U, tag: String) -> U { return x; }
  public func Forward<U extends T, T extends Animal>(x: U) -> U { return x; }
}

func ShadowExplicit(s: Store<Chair>, d: Dog) -> Dog { return s.Shadow<Animal, Dog>(d); }
func ShadowInferred(s: Store<Chair>, d: Dog) -> Dog { return s.Shadow(d); }
func ShadowOverloadedExplicit(s: Store<Chair>, d: Dog) -> Dog { return s.ShadowPick<Animal, Dog>(d, 1); }
func ShadowOverloadedInferred(s: Store<Chair>, d: Dog) -> Dog { return s.ShadowPick(d, "tag"); }
func ForwardExplicit(s: Store<Chair>, d: Dog) -> Dog { return s.Forward<Dog, Animal>(d); }

class Animal {}
class Dog extends Animal {}
class Chair {}

class Store<T> {
  public func Take<U extends T>(x: U) -> U { return x; }
  public func Pick<U extends T>(x: U, tag: Int32) -> U { return x; }
  public func Pick<U extends T>(x: U, tag: String) -> U { return x; }
  public func Forward<U extends V, V extends Animal>(x: U) -> U { return x; }
  public func Read(x: T) -> T { return x; }
}
class Middle<V> extends Store<V> {}
class Derived<W> extends Middle<W> {}

func Explicit(s: Store<Animal>, d: Dog) -> Dog { return s.Take<Dog>(d); }
func Inferred(s: Store<Animal>, d: Dog) -> Dog { return s.Take(d); }
func InheritedExplicit(s: Derived<Animal>, d: Dog) -> Dog { return s.Take<Dog>(d); }
func InheritedInferred(s: Derived<Animal>, d: Dog) -> Dog { return s.Take(d); }
func OverloadedExplicit(s: Store<Animal>, d: Dog) -> Dog { return s.Pick<Dog>(d, 1); }
func OverloadedInferred(s: Store<Animal>, d: Dog) -> Dog { return s.Pick(d, "tag"); }
func InheritedOverloadedExplicit(s: Derived<Animal>, d: Dog) -> Dog { return s.Pick<Dog>(d, "tag"); }
func InheritedOverloadedInferred(s: Derived<Animal>, d: Dog) -> Dog { return s.Pick(d, 1); }

func ForwardExplicit(s: Store<Chair>, d: Dog) -> Dog { return s.Forward<Dog, Animal>(d); }
func ReceiverOnly(s: Store<Animal>, a: Animal) -> Animal { return s.Read(a); }
func RigidReceiver<T extends Animal, U extends T>(s: Store<T>, x: U) -> U { return s.Take(x); }

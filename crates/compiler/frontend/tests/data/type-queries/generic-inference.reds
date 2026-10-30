class Animal {}
class Dog extends Animal {}
class Producer<+T> {}
class Consumer<-T> {}
class Cell<T> {}

func Take<A, B extends A>(a: A, b: B) -> B { return b; }
func TakeReversed<A, B extends A>(b: B, a: A) -> B { return b; }
func Chain<A, B extends A, C extends B>(a: A, b: B, c: C) -> C { return c; }
func ChainReversed<A, B extends A, C extends B>(c: C, b: B, a: A) -> C { return c; }
func TakeDog(a: Animal, d: Dog) -> Dog { return Take(a, d); }
func TakeDogReversed(a: Animal, d: Dog) -> Dog { return TakeReversed(d, a); }
func TransitiveDog(a: Animal, d: Dog) -> Dog { return Chain(a, d, d); }
func TransitiveDogReversed(a: Animal, d: Dog) -> Dog { return ChainReversed(d, d, a); }
func First<A extends Animal>(x: A, y: A) -> A { return x; }
func InferredJoin<T extends Animal>(t: T, d: Dog) -> Animal { return First(t, d); }
func InferredJoinReversed<T extends Animal>(t: T, d: Dog) -> Animal { return First(d, t); }
func JoinRigid<T extends Animal>(t: T, d: Dog) -> Animal { return true ? t : d; }
func JoinRigidReversed<T extends Animal>(t: T, d: Dog) -> Animal { return true ? d : t; }
func JoinIdentity<T extends Animal>(t: T) -> T { return true ? t : t; }
func JoinCovariant<T extends Animal>(t: Producer<T>, d: Producer<Dog>) -> Producer<Animal> { return true ? t : d; }
func JoinCovariantReversed<T extends Animal>(t: Producer<T>, d: Producer<Dog>) -> Producer<Animal> { return true ? d : t; }
func JoinContravariant<T extends Animal>(t: Consumer<T>, a: Consumer<Animal>) -> Consumer<T> { return true ? t : a; }
func JoinContravariantReversed<T extends Animal>(t: Consumer<T>, a: Consumer<Animal>) -> Consumer<T> { return true ? a : t; }
func JoinInvariantIdentity<T>(t: Cell<T>) -> Cell<T> { return true ? t : t; }

class Animal {}
class Dog extends Animal {}

class Producer<+T> {}
class Consumer<-T> {}
class Cell<T> {}

func BadRigid<T extends Animal>(t: T, d: Dog) -> Dog { return true ? t : d; }
func BadRigidReversed<T extends Animal>(t: T, d: Dog) -> Dog { return true ? d : t; }
func BadMeet<T extends Animal>(t: Consumer<T>, d: Consumer<Dog>) -> Consumer<Dog> { return true ? t : d; }
func BadMeetReversed<T extends Animal>(t: Consumer<T>, d: Consumer<Dog>) -> Consumer<Dog> { return true ? d : t; }
func BadInvariant(a: Cell<Animal>, d: Cell<Dog>) -> Cell<Animal> { return true ? a : d; }
func BadInvariantReversed(a: Cell<Animal>, d: Cell<Dog>) -> Cell<Animal> { return true ? d : a; }
func BadApplicationMeet(a: Consumer<Producer<Animal>>, d: Consumer<Producer<Dog>>) -> Consumer<Producer<Animal>> { return true ? a : d; }
func BadApplicationMeetReversed(a: Consumer<Producer<Animal>>, d: Consumer<Producer<Dog>>) -> Consumer<Producer<Animal>> { return true ? d : a; }
func Exact<A, B extends A>(a: A, b: B) -> B { return b; }
func BadDirection(a: Animal, d: Dog) -> Dog { return Exact<Dog, Animal>(d, a); }


func First<A extends Animal>(x: A, y: A) -> A { return x; }
func BadInferredJoin<T extends Animal>(t: T, d: Dog) -> Dog {
  let result = First(t, d);
  return result;
}
func BadInferredJoinReversed<T extends Animal>(t: T, d: Dog) -> Dog {
  let result = First(d, t);
  return result;
}
func BadContravariantJoin<T extends Animal>(t: Consumer<T>, a: Consumer<Animal>) -> Consumer<Animal> {
  return true ? t : a;
}
func BadContravariantJoinReversed<T extends Animal>(t: Consumer<T>, a: Consumer<Animal>) -> Consumer<Animal> {
  return true ? a : t;
}

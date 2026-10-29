
class Animal {}

class Pet extends Animal {}

class List<+A> {}

class Predicate<-A> {}

func BareReturn() -> Int32 {
  return;
}

func Testing() {
  let _: Int32 = "";
  let _: CName = "";
  let _: TweakDBID = "";
  let _: ResRef = "";
  let _: Animal = new Pet(); // OK
  let _: Pet = new Animal();

  // covariance
  let a: List<Animal> = new List<Animal>(); // OK
  let b: List<Animal> = new List<Pet>(); // OK
  let c: List<Pet> = new List<Animal>();

  // contravariance
  let _: Predicate<Animal> = new Predicate<Animal>(); // OK
  let _: Predicate<Animal> = new Predicate<Pet>();
  let _: Predicate<Pet> = new Predicate<Animal>(); // OK
}

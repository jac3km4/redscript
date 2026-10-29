class Base {}

class Derived extends Base {
  public let child: wref<Leaf>;
}

class Leaf {
  public func Method() {}
}

func Test(weak: wref<Base>) {
  (weak as Derived).child.Method();
}

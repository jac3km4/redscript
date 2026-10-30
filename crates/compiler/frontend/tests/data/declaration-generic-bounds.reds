class Parent {}
class Child extends Parent {}
class Other {}
class Box<T> {}
class Producer<+T> {}
class Consumer<-T> {}
class Restricted<T extends Box<Parent>> {}
class Covariant<T extends Producer<Parent>> {}
class Contravariant<T extends Consumer<Child>> {}
class Dependent<T, U extends T> {}
class NestedDependent<T, U extends Producer<T>> {}
class ChildBox extends Box<Child> {}

func InvalidArgument(value: Restricted<Box<Other>>) {}
func InvalidInheritedArgument(value: Restricted<ChildBox>) {}
func InvalidReturn() -> Restricted<Box<Child>> { return null; }
class InvalidField { let value: Restricted<Box<Child>>; }
class InvalidBase extends Restricted<Box<Child>> {}
class InvalidMethod {
  public func Accept(value: Restricted<Box<Other>>) {}
}

func ValidInvariant(value: Restricted<Box<Parent>>) {}
func ValidCovariant(value: Covariant<Producer<Child>>) {}
func ValidContravariant(value: Contravariant<Consumer<Parent>>) {}
func ValidDependent(value: Dependent<Parent, Child>) {}
func InvalidDependent(value: Dependent<Child, Parent>) {}
func ValidNestedDependent(value: NestedDependent<Parent, Producer<Child>>) {}
func InvalidNestedDependent(value: NestedDependent<Child, Producer<Parent>>) {}
func ForwardBound<T extends Parent>(value: Dependent<Parent, T>) {}
func ForwardIdentity<T>(value: Dependent<T, T>) {}

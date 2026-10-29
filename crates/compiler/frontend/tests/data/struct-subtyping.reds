
struct Super {
  let x: Int32;
}

struct Sub extends Super {
  let y: Int32;
}

func AcceptFn(f: (Super) -> Sub) {}
func AcceptSuper(s: Super) {}
func AcceptSub(s: Sub) {}
func AcceptSuperRef(s: script_ref<Super>) {}
func AcceptSubRef(s: script_ref<Sub>) {}

func Test() {
  let subToSuper: (Sub) -> Super;
  let superToSub: (Super) -> Sub;
  let subToSub: (Sub) -> Sub;
  let superToSuper: (Super) -> Super;

  let sub: Sub;
  let sup: Super;

  AcceptFn(subToSuper); // Error
  AcceptFn(superToSub); // OK
  AcceptFn(subToSub); // Error
  AcceptFn(superToSuper); // Error

  AcceptSuper(sup); // OK
  AcceptSuper(sub); // OK
  AcceptSub(sub); // OK
  AcceptSub(sup); // Error

  AcceptSuperRef(sup); // OK
  AcceptSuperRef(sub); // OK
  AcceptSubRef(sub); // OK
  AcceptSubRef(sup); // Error
}

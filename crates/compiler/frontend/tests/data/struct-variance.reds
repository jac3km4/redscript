struct CovariantStruct<+T> {}

struct ContravariantStruct<-T> {}

struct InvariantStruct<T> { // OK
  let value: T;
}

func TestInvariantStruct() {
  let value: InvariantStruct<Int32> = InvariantStruct<Int32>(1); // OK
}

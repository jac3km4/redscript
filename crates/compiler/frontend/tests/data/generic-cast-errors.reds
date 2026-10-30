class State {}
class Other {}
struct Value {}

func Unbounded<T>(value: State) {
  value as T;
}

func PrimitiveBound<T extends Int32>(value: State) {
  value as T;
}

func StructBound<T extends Value>(value: State) {
  value as T;
}

func UnrelatedBound<T extends Other>(value: State) {
  value as T;
}

func UnrelatedSource<T extends Other>(value: T) {
  value as State;
}

func RedundantSource<T extends State>(value: T) {
  value as State;
}

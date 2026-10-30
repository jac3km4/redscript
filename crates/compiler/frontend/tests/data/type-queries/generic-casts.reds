class State {}
class WindowState extends State {}

func BaseToParameter<T extends State>(value: State) -> T {
  return value as T;
}
func DerivedToParameter<T extends State>(value: WindowState) -> T {
  return value as T;
}
func WeakBaseToParameter<T extends State>(value: wref<State>) -> wref<T> {
  return value as T;
}
func TransitiveBound<T extends State, U extends T>(value: State) -> U {
  return value as U;
}
func SiblingParameters<T extends State, U extends State>(value: U) -> T {
  return value as T;
}
func WeakDerivedToParameter<T extends State>(value: wref<WindowState>) -> wref<T> {
  return value as T;
}
func ParameterToDerived<T extends State>(value: T) -> WindowState {
  return value as WindowState;
}
func WeakParameterToDerived<T extends State>(value: wref<T>) -> wref<WindowState> {
  return value as WindowState;
}

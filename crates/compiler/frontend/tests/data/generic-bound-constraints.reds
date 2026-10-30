class State {}
class Producer<+T> {}
class Consumer<-T> {}
class Invariant<T> {}

func Take<U extends State>(value: U) -> U {
  return value;
}

func ForwardExplicit<T extends State>(value: T) -> T {
  return Take<T>(value);
}

func ForwardInferred<T extends State>(value: T) -> T {
  return Take(value);
}

func Upcast<T extends State>(value: T) -> State {
  return value;
}

func InitializeUpcast<T extends State>(value: T) -> State {
  let state: State = value;
  return state;
}

func TransitiveUpcast<T extends State, U extends T>(value: U) -> State {
  return value;
}

func ImplicitDowncast<T extends State>(value: State) -> T {
  return value;
}

func IndependentParameters<T extends State, U extends State>(value: T) -> U {
  return value;
}

func UnboundedUpcast<T>(value: T) -> State {
  return value;
}

func CovariantUpcast<T extends State>(value: Producer<T>) -> Producer<State> {
  return value;
}

func CovariantDowncast<T extends State>(value: Producer<State>) -> Producer<T> {
  return value;
}

func ContravariantUpcast<T extends State>(value: Consumer<State>) -> Consumer<T> {
  return value;
}

func ContravariantDowncast<T extends State>(value: Consumer<T>) -> Consumer<State> {
  return value;
}

func InvariantUpcast<T extends State>(value: Invariant<T>) -> Invariant<State> {
  return value;
}

func InvariantDowncast<T extends State>(value: Invariant<State>) -> Invariant<T> {
  return value;
}

func InvariantIdentity<T extends State>(value: Invariant<T>) -> Invariant<T> {
  return value;
}

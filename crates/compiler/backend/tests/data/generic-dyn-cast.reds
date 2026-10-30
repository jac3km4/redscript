class State {}
class WindowState extends State {}
class TextState extends State {}

func Find<T extends State>(value: State) -> T {
  return value as T;
}

func FindWeak<T extends State>(value: wref<State>) -> wref<T> {
  return value as T;
}

func Forward<T extends State>(value: State) -> T {
  return Find<T>(value);
}

func FindWindow<T extends State>(value: T) -> WindowState {
  return value as WindowState;
}

func FindWeakWindow<T extends State>(value: wref<T>) -> wref<WindowState> {
  return value as WindowState;
}

func Test(value: State, weak: wref<State>) {
  let window = Forward<WindowState>(value);
  let text = Forward<TextState>(value);
  let weakWindow = FindWeak<WindowState>(weak);
  let weakText = FindWeak<TextState>(weak);
  let concreteWindow = FindWindow<State>(value);
  let concreteWeakWindow = FindWeakWindow<State>(weak);
}

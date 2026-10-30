@if(false)
@replaceMethod(NativeTarget)
func Value() -> Int32 { return 0; }

@wrapMethod(NativeTarget)
func Value() -> Int32 { return wrappedMethod(); }

@replaceMethod(NativeTarget)
func Value() -> Int32 { return 1; }

@replaceMethod(NativeTarget)
func Value(value: Int32) -> Int32 { return 2; }

@wrapMethod(NativeTarget)
func Value() -> Int32 { return wrappedMethod(); }

@if(false)
@replaceMethod(NativeTarget)
func Value() -> Int32 { return 3; }

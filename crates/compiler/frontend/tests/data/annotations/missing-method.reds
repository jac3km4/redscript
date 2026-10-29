@wrapMethod(NativeTarget)
func MissingWrapped() {}

@replaceMethod(NativeTarget)
func MissingReplaced() {}

@wrapMethod(NativeTarget)
func Existing(value: Int32) {}

@replaceMethod(NativeTarget)
func Existing(value: Int32) {}

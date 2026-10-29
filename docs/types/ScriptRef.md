A reference to a script value, written as `script_ref<T>`. It allows values such as arrays, strings, and structs to be passed by reference rather than copied. Script references are created implicitly when passing values to functions that accept a `script_ref<T>` parameter. Use `Deref` to access the referenced value.

Unlike `ref<T>` and `wref<T>`, script references refer to script values rather than expressing ownership of class instances.

### Example

```redscript
func ReadValue(value: script_ref<Int32>) -> Int32 {
  return Deref(value);
}

func Example() -> Int32 {
  let number: Int32 = 42;
  return ReadValue(number);
}
```

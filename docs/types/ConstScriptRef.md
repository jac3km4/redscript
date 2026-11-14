A read-only reference to a script value, written as `const_script_ref<T>`. Unlike `script_ref<T>`, it cannot be written through, allowing covariant subtyping across inherited types (including structs).

### Example

```redscript
func ReadValue(value: const_script_ref<Int32>) -> Int32 {
  return Deref(value);
}
```

A strong reference to a class instance, written as `ref<T>`. Creating an instance with `new` produces a strong reference. The instance is kept alive as long as at least one strong reference exists, unlike a weak reference (`wref<T>`).

### Example

```redscript
class Example {}

func CreateExample() -> ref<Example> {
  return new Example();
}
```

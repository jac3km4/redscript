A weak reference to a class instance, written as `wref<T>`. It does not keep the instance alive. When the last strong reference (`ref<T>`) is released, the instance is destroyed and any remaining weak references can no longer access it. Use `IsDefined` to check whether the instance still exists.

### Example

```redscript
class Example {}

func CheckLifetime() {
  let owner: ref<Example> = new Example();
  let observer: wref<Example> = owner;
  IsDefined(observer); // true: owner keeps the instance alive

  owner = null;
  IsDefined(observer); // false: no strong references remain
}
```

Assigning `new Example()` directly to a `wref<Example>` leaves no surviving strong owner, so the instance is destroyed after the assignment.

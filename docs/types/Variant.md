A container for a single value whose type is determined at runtime. Create a variant with `ToVariant(value)` and retrieve its value with `FromVariant<T>(variant)`, where `T` must match the stored type exactly. Use `VariantTypeName(variant)` to inspect the stored type when it is not known.

### Example

```redscript
let number: Int32 = 42;
let variant = ToVariant(number);
let value = FromVariant<Int32>(variant);
```

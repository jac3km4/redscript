A container for a single value whose type is determined at runtime. Create a variant with `ToVariant(value)` and retrieve its value with `FromVariant<T>(variant)`, where `T` must match the stored type exactly. Use `VariantTypeName(variant)` to inspect the stored type when it is not known.

Repeated default-initialized declarations (for example inside loops) have an
unresolved backend reset risk: entry-only VM initialization may leave a previous
payload on later executions. Runtime behavior is unverified. See
[required reset/rejection work and tests](../language-future-work.md#variant-default-initialization-on-repeated-declarations).

### Example

```redscript
let number: Int32 = 42;
let variant = ToVariant(number);
let value = FromVariant<Int32>(variant);
```

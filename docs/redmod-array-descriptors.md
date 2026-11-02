# REDmod dynamic-array Grow and Count

See the [native language guide](redmod-language.md#control-flow-arrays-and-constness)
for the complete supported descriptor list and
[future work](language-future-work.md#analysis-lookup-and-constness) for missing
array/Variant operations.

The native adapter exposes two additional dynamic-array member descriptors:

| Native spelling | Shared intrinsic | Arguments | Result |
| --- | --- | --- | --- |
| `values.Grow(amount)` | `ArrayGrow` | `amount: Int32` | `Void` |
| `values.Count(value)` | `ArrayCount` | `value: T` for `values: array<T>` | `Int32` |

These descriptors resolve the existing intrinsic signatures supplied by the boot
library (or loaded symbols); they do not synthesize declarations or accept an
ordinary function with the same name as an intrinsic. Backend serialization uses
the existing typed `ArrayGrow` and `ArrayCount` instructions.

Both operations require an array **place**. Temporary receivers such as
`MakeArray().Count(value)` remain rejected even though Count only reads. Grow
cannot mutate const parameters or const locals; Count can read either. Existing
script-reference receiver unwrapping and ordinary user-method resolution remain
unchanged.

This is a descriptor-only addition. Fixed-array member operations and
`EraseFast` are not enabled or aliased. Names are case-sensitive, and explicit
community-style `ArrayGrow(values, amount)` / `ArrayCount(values, value)` calls
remain unavailable in native source.

Regression coverage lives in the adapter's `tests/array_descriptors.rs` and
`tests/array_descriptors_backend.rs`. The latter checks instructions after bundle
serialization and decoding; it does not establish game-runtime behavior.

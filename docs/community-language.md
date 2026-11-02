# Community REDscript

Community REDscript is the language compiled by the shared frontend and bytecode
backend. Native REDmod is a separate dialect, not an interchangeable spelling of
this language. This guide summarizes repository-specific semantics; the
[wiki](https://wiki.redmodding.org/redscript) provides introductory language material.
See the [guide index](README.md) for detailed contracts and type references.

## Declarations and types

Functions use `func`, locals use `let`, and const locals use `const` (not
`const let`). Signatures and local types can be inferred where supported by the
community compiler; REDmod source export has a narrower explicit-type contract.
Classes, structs, enums, generic types/functions, lambdas and function values are
community features; accepting them here does not imply native export support.

Community `ref<T>` is a strong object handle and `wref<T>` a weak handle.
`script_ref<T>` models script-reference adaptation. Native `ref<T>` means the latter,
not a strong handle. Wrapping/unwrapping script references composes with other
coercions; assignment and out forwarding use community reference-storage rules
rather than native write-through policy. See
[shared expression semantics](shared-expression-defaults.md).

[Explicit enum storage](enum-storage.md) enforces integer ranges and preserves
width. Omitted storage infers a signed width including zero. Bundles erase
signedness, so decompiled storage is canonical rather than the authored signedness.
The opaque engine primitives NodeRef, CRUID, LocalizationString and
redResourceReferenceScriptToken are not Uint64 aliases; their registration does
not introduce literals, defaults or arbitrary conversions.

## Expressions, calls and mutation

- Assignment is value-producing: its result has the coerced destination type and
  is not a writable place. Evaluate the destination once, before the RHS, with no
  destination readback. Chains use the same rule.
- Positional holes must correspond to optional parameters and emit Nop, not null.
  In community syntax a trailing comma adds no slot: `Call(42,)` has one argument;
  `Call(42,,)` has a value and a hole. Native trailing commas differ.
- Arrays have built-in Int32 indexing with writable-place semantics. Non-array
  indexing resolves `OperatorArray(receiver, key)` as a call, not a place. See
  [switches and operators](control-flow-operators.md).
- [Call lookup and inheritance](lookup-inheritance.md) retain applicable overloads
  across static ancestry and prioritize real methods over equivalent generated
  receiver-first aliases. Specificity is bounded, not a numeric scoring system.
- Source `skip` parameters retain lazy intent through source, symbols and bundles.
  [Array-prefix safety](lazy-evaluation-limitations.md) rejects forms whose current
  lowering would reorder effects or escape a lazy branch. Type-qualified static
  calls can take array literals; runtime member and closure callees remain guarded.
- Marked `implicit func Cast` / `explicit func Cast` declarations preserve intent,
  but do not enable arbitrary user-defined conversions. Ordinary unmarked `Cast`
  functions keep their existing behavior. The bounded marked-provider selection
  described in [conversion semantics](conversion-declarations.md) is native-only.

## Constness and initialization

[Const parameters and locals](const-parameters.md) use strict community rules.
A const local's declaration initializer is permitted once; later assignment is
not, even after an uninitialized declaration. Direct-root referent mutation,
mutable forwarding and selected non-const methods are checked. This is not
transitive alias/effect analysis, const-aware overload ranking, or general const
field/return enforcement.

Aggregate `default field = value;` directives support own and inherited fields in
classes and structs. They are typed metadata, not constructor assignments. The
[default contract](shared-expression-defaults.md#shared-aggregate-defaults)
defines duplicate precedence and supplying-class keys. Constant lowering is
bounded to supported literal/enum forms, not general compile-time evaluation.

Declaration execution and storage allocation are separate. Array construction
keeps explicit execution-site operations for repeated loop declarations; a slot
may have escaped via script references on an earlier iteration. The backend's
[repeated Variant default-initialization risk](language-future-work.md#variant-default-initialization-on-repeated-declarations)
is unresolved: function-entry VM defaults do not establish per-iteration resets.
Runtime behavior has not been verified.

## Diagnostics and tool boundaries

[Invalid signatures](invalid-signature-recovery.md) preserve parameter identity
rather than deleting unknown types or fabricating Void returns. Invalid bodies
are unavailable; valid siblings may still be analyzed. Returned IR or an IDE type
query can include recovery and is not proof that compilation succeeded. Backend
errors must prevent saving output.

Decompilation preserves ordinary initialized const declarations, but rejects const
locals that would require unsafe hoisting (such as expanded array initialization
or an initializer inside an expression) rather than turning initialization into
an illegal later assignment. Detached dispatch for non-final switch defaults is
also explicitly unsupported; declaration-only decompilation remains available.

The [REDmod transpiler](../crates/compiler/redmod/transpiler/README.md) uses typed
community IR and returns diagnostics instead of partial source on unsupported
input. Const locals are explicitly rejected, not silently emitted as mutable.
Custom generics, lambdas/function values, unsafe places/prefixes, unproven default
resets and unresolved declaration/patch ownership have no general fallback.
Callbacks require a non-const Bool result to match native events; community
callback contracts are not automatically rewritten. Official compilation of
emitted source is not runtime validation.

See [future work](language-future-work.md) for unresolved behavior and validation
gates, and [historical evidence](language-investigations.md) for investigation
conclusions that must not be mistaken for current support or measurements.

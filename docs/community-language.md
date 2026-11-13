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

```redscript
// Functions, locals, and const bindings
func Add(a: Int32, b: Int32) -> Int32 {
  let result = a + b;
  const multiplier = 2;
  return result * multiplier;
}

// Explicit enum storage width
enum Choice: Uint8 {
  First = 0,
  Second = 1
}
```

Community `ref<T>` is a strong object handle and `wref<T>` a weak handle.
`script_ref<T>` models script-reference adaptation. Native `ref<T>` means the latter,
not a strong handle. Wrapping/unwrapping script references composes with other
coercions; assignment and out forwarding use community reference-storage rules
rather than native write-through policy. See
[shared expression semantics](language-semantics.md#shared-expression-and-aggregate-default-apis).

[Explicit enum storage](language-semantics.md#explicit-enum-storage) enforces integer ranges and preserves
width. Omitted storage defaults to 4 bytes for Int32-range values and 8 bytes otherwise. Bundles erase
signedness, so decompiled storage is canonical rather than the authored signedness.
The opaque engine primitives NodeRef, CRUID, LocalizationString and
redResourceReferenceScriptToken are not Uint64 aliases; their registration does
not introduce literals, defaults or arbitrary conversions.

## Expressions, calls and mutation

- **Assignment is value-producing:** Its result has the coerced destination type and
  is not a writable place. Evaluate the destination once, before the RHS, with no
  destination readback. Chains use the same rule.
  ```redscript
  let x: Int32;
  let y: Int32;
  x = y = 10; // y = 10 yields 10, then x is assigned 10
  ```
- **Positional holes:** Omitted arguments correspond to optional parameters and emit
  `Nop`, not `null`. A trailing comma adds no slot:
  ```redscript
  Target(42,)   // 1 argument: 42
  Target(42,,)  // 2 arguments: 42 followed by 1 omitted hole
  Target(,)     // 1 argument: omitted hole
  ```
- **Array and overloaded indexing:** Dynamic/static arrays have built-in Int32 indexing
  with writable-place semantics (`arr[0] = 5`). Non-array indexing resolves
  `OperatorArray(receiver, key)` as a function call, not an assignable place. See
  [switches and operators](language-semantics.md#switch-order-and-overloaded-indexing).
- **Call lookup and inheritance:** [Overload selection](language-semantics.md#call-lookup-overloads-and-inheritance)
  retains applicable overloads across static ancestry and prioritizes real methods over
  equivalent generated receiver-first aliases. Specificity is bounded, not a numeric scoring system.
- **Lazy evaluation and array safety:** Source `skip` parameters retain lazy intent.
  [Array-prefix safety](language-semantics.md#lazy-evaluation-limitations) diagnoses forms
  whose statement-prefix array construction could reorder effects or escape a lazy branch:
  ```redscript
  // UNSAFE: array literal inside a lazy branch can evaluate eagerly
  CallWithSkipParam(condition ? [1, 2] : []);

  // SAFE: bind array to a local at its execution site
  let items = [1, 2];
  CallWithSkipParam(items);
  ```
- **Conversions:** Marked `implicit func Cast` / `explicit func Cast` declarations preserve
  intent, but do not enable arbitrary user-defined conversions. Ordinary unmarked `Cast`
  functions keep their existing behavior. Marked provider selection is native-only.

## Constness and initialization

[Const parameters and locals](language-semantics.md#const-parameters-and-locals) use strict community rules:
- A const local's declaration initializer is permitted once; subsequent assignment is rejected,
  even after an uninitialized declaration:
  ```redscript
  const limit: Int32 = 100; // Allowed: initial declaration assignment
  // limit = 200;           // ERROR: cannot assign to const local
  ```
- Direct-root referent mutation, mutable forwarding, and mutating array intrinsics are hard errors.
- Forwarding a const reference or container to an uncertain non-`out` parameter is diagnosed at
  `ErrorAllowedAtRuntime` severity (rejected in default strict mode, tolerated in `--runtime-compatible`).

Aggregate `default field = value;` directives support own and inherited fields in
classes and structs. They are typed metadata, not constructor assignments:
```redscript
class Config {
  default timeout = 30;
  default enabled = true;
}
```
The [default contract](language-semantics.md#shared-aggregate-defaults)
defines duplicate precedence and supplying-class keys. Constant lowering is
bounded to supported literal/enum forms, not general compile-time evaluation.

Declaration execution and storage allocation are separate. Array construction
keeps explicit execution-site operations for repeated loop declarations; a slot
may have escaped via script references on an earlier iteration. The backend's
[repeated Variant default-initialization risk](language-future-work.md#variant-default-initialization-on-repeated-declarations)
is unresolved: function-entry VM defaults do not establish per-iteration resets.
Runtime behavior has not been verified.

## Diagnostics and tool boundaries

CLI `compile` and `lint` are strict by default: their fatal threshold is
`DiagnosticLevel::ErrorAllowedAtRuntime`. `--runtime-compatible` changes it to
`DiagnosticLevel::Error`, matching SCC's fatal threshold. Runtime-tolerated
diagnostics are still reported by these CLI commands; hard errors still fail lint
and prevent compile output. This changes fatality, not semantic policies or a
promise of correct runtime behavior. Access/visibility diagnostics already use
runtime-tolerated severity; this is not a new relaxation of lookup rules.

Compiler API checking/compilation builders also default to the strict threshold;
callers can set `fatal_diagnostic_level` explicitly. Diagnostic display/warning
filters are separate from fatality.

[Invalid signatures](language-semantics.md#invalid-signature-recovery) preserve parameter identity
rather than deleting unknown types or fabricating Void returns. Invalid bodies
are unavailable; valid siblings may still be analyzed. Returned IR or an IDE type
query can include recovery and is not proof that compilation succeeded. Backend
errors must prevent saving output.

Decompilation preserves reconstructable initialized const declarations and
read-only VM-default const locals. If initialization requires hoisting or expanded
writes, it warns with the function and local names and emits mutable `let` only
for the affected local. It reconstructs grouped/backward-linked switch labels and
first/middle default bodies when dispatch and body order are provable; unprovable
layouts still fail. See [const export/decompilation](language-semantics.md#export-and-decompilation)
and [switch semantics](language-semantics.md#switch-order-and-overloaded-indexing).

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

# Const parameters

Const parameter flags are enforced by shared frontend analysis after function
bodies have been lowered and calls resolved. Community source uses the strict
policy by default. Diagnostics are shared frontend errors (`OTHER`, with message
`cannot write through a const parameter`), not REDmod capability errors.

Both policies reject assignment (including compound/value-producing assignment)
to const parameters or their fields, explicit `out` forwarding, and mutating array
intrinsics. Provenance follows inherited fields, script-reference `Deref`/`AsRef`,
strong/weak-reference `WeakRefToRef`/`RefToWeakRef` conversions (explicit or
inserted by coercion), casts, and both arms of conditional expressions. Array reads and ordinary scalar
by-value arguments are allowed. This is parameter enforcement, not new enforcement
of const fields, const returns, or const methods.

Two independent, per-source-file `SemanticPolicy` flags provide the characterized
REDmod exceptions; both are false by default and true in the adapter's `POLICY`:

- `const_param_element_writes`: indexed element places stop const provenance.
  Assigning elements (including fields within them) or passing them as `out` is
  permitted. Replacing the entire array or mutating its size is still rejected.
- `const_param_non_out_forwarding`: non-`out` forwarding is permitted, including
  to non-const reference parameters. It does not relax explicit `out` arguments
  or known mutating array intrinsics.

Strict forwarding checks use resolved non-const parameter types: script/strong/
weak references, arrays and class objects may expose mutable state. Known const
parameters remain legal targets. Function-value calls have no parameter flags;
strict analysis checks their resolved reference/container argument types instead.
User method receivers are not presumed mutating merely because a method is
non-const: receiver effects are not characterized by this pass.

## Analysis bounds

This is a direct-root check, not an interprocedural effect or alias analysis.
Copying a reference into a local or returning it does not propagate constness.
Calls returning references and generic parameters without a concrete mutable bound
can hide provenance or mutation. The pass does not infer effects of arbitrary
intrinsics beyond the known mutating array operations.

Closure bodies are visited, captured const roots remain checked, and closure
parameters are not mistaken for enclosing parameters with the same numeric ID.
Nested closures retain captured roots. The current IR identifies captures by
`Local` only, without defining-scope identity: nested captures with colliding local
IDs can be conservatively treated as const. Lambda syntax does not currently
accept const parameter qualifiers. These limits must not be interpreted as a
transitive immutability guarantee.

# Const parameters and locals

See the [language guide](README.md) for dialect context and
[future work](language-future-work.md#analysis-lookup-and-constness) for broader
constness gaps.

Const parameter flags are enforced by shared frontend analysis after function
bodies have been lowered and calls resolved. Community source uses the strict
semantic policy by default. Diagnostics are shared frontend errors (`OTHER`),
not REDmod capability errors. Proven writes use hard `Error` severity with
`cannot write through a const parameter` (or `const local`). Uncertain non-out
mutable forwarding and function-value forwarding use `ErrorAllowedAtRuntime`
with `forwarding a const ... may allow mutation`: strict mode rejects them,
while CLI `--runtime-compatible` and SCC tolerate them. This does not establish
that the callee is harmless or provide transitive immutability. See the
[mode contract](community-language.md#diagnostics-and-tool-boundaries).

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
analysis checks their resolved reference/container argument types instead. These
non-out checks diagnose possible mutation, not a proven write, and use the
runtime-tolerated severity above. Direct assignment, explicit `out`, known
mutating array intrinsics and the existing non-const local-receiver checks remain
hard errors in both modes. Native semantic policy flags and their exemptions are
unchanged; choosing a fatal threshold does not enable a native policy.
User method receivers are not presumed mutating merely because a method is
non-const: receiver effects are not characterized by this pass.

## Const locals

Community syntax uses `const name: Type;`, `const name = value;`, and
`const name: Type = value;`. The former `const let` syntax is rejected; native
REDmod syntax remains `const var name: Type;`.
The AST qualifier and `ir::LocalInfo::is_const` survive bridging, hoisting and
lowering; constness is not encoded by changing or erasing the value's type.
The shared post-resolution validator distinguishes const parameter roots from
const local roots, and binding assignment from referent mutation.

Strict community defaults allow the declaration initializer exactly once, for
both inferred and explicitly typed const locals. All other binding assignments
are rejected, including the first assignment after an uninitialized declaration;
this is intentional, not definite-assignment analysis. Field/index writes,
mutating array intrinsics and explicit `out` forwarding remain hard errors.
Ordinary forwarding to mutable reference/container parameters and uncertain
function-value forwarding are diagnosed at runtime-tolerated severity and rejected
by the default strict threshold.
Scalar by-value reads and forwarding to const parameters remain legal. Non-const
instance methods on const local referents are rejected; const methods are legal.
Parameter receiver behavior is unchanged.

The independent, per-file `const_local_handle_rebinding` policy defaults to false
and is enabled by REDmod. For non-struct class objects and strong/weak class
handles, it permits repeated root assignment, inline initialization, and ordinary
or `out` forwarding of the handle. It does not permit field/index mutation,
forwarding a field as `out`, mutating array intrinsics, or non-const receiver
methods. Scalars, structs, arrays and script references do not get this exception.
Under this native policy, default storage initialization is not an explicit source
assignment; scalar/struct assignment, even the first assignment or an inline
initializer, is rejected. In particular, historical official compiler probes
record const Int32 inline initialization failing with “Value on the left side is
constant. Unable to assign to it.” This native restriction is deliberately distinct
from community initialization; it is not inferred from ordinary non-const
initializer acceptance.

`LocalInfo::initializer` records the exact declaration-store span during lowering,
including hoisted declarations. Validation matches both local ID and that span,
consumes the exemption once, and uses separate metadata for each closure scope.
The initializer value is still visited: nested binding writes, captured writes,
forwarding and referent mutation receive no initialization exemption. No new IR
expression variant or backend assignment behavior is introduced.

Local provenance follows inherited fields, indexes, dynamic casts, conditional
branches, script-reference wrappers, strong/weak conversions (including inserted
coercions), and closure captures. Captured bindings retain their binding-versus-
referent distinction; nested closure locals use their own metadata.

This is validation of the selected resolved method, not const-aware overload
ranking. Const returns, const fields and arbitrary method effects remain outside
this pass. No runtime serialization or full installed-corpus coverage measurement
is implied by the focused tests.

## Export and decompilation

The REDmod transpiler explicitly rejects const locals rather than dropping their
qualifier or emitting native declarations with different initialization rules.
Decompilation preserves ordinary const declarations whose initializer can be
reconstructed, as well as read-only const locals using their VM-default value.
When a const initializer cannot be preserved at its execution site (including
expanded array writes or initialization inside expressions), it logs a warning
identifying the function and local and emits mutable `let` for that local only.
Other initialized/default const locals keep their qualifier. This is a lossy
source-recovery fallback, not a relaxation of compiler const rules; it avoids an
uninitialized const followed by an illegal assignment. It does not add const-local
support to the transpiler.

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

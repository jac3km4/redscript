# Conversion investigation archive

**Historical evidence, not the current contract or a validation log.** See
[conversion declarations and bounded selection](conversion-declarations.md) for
implemented semantics, [future work](language-future-work.md#conversions-and-native-bindings)
for missing capabilities and the [investigation summary](language-investigations.md)
for corpus/evidence limits.

## Removed broad prototype

A prototype following the `86af12a` investigation broadened static casts, overload
selection and shared coercion too much. It was removed before committing. Its
primitive matrix, identity, converted-overload and enum-width tests were removed
with it and are **not ongoing regression coverage or delivered support**.
Historical discussion is available at `33fa996`; do not recreate the prototype
under a different filename.

Findings retained from that work:

- Existing free-function IR/native binding can emit return-qualified provider
  calls such as `Cast;Int8;Int64`; a primitive conversion is not necessarily its
  own opcode. Provider declaration intent alone does not prove installed ABI.
- Applicability must be read-only. A currently concrete lower bound is not a
  proven final type. Do not coalesce/freeze live variables to select providers.
- Exact applicable overloads precede converted alternatives. Multiple direct
  alternatives do not justify numeric-distance ranking or chaining.
- Enum opcode width follows the integer operand/result, not merely enum storage.
  Both frontend and backend must validate supported storage/width cells and typed
  operands/results. A prototype two-type-argument intrinsic shortcut was discarded;
  the delivered slice uses the existing single-type-argument contract.
- Identity casts interact with ordinary Cast precedence, contextual literals and
  reference representations. Equal-looking weak/strong/script-reference types
  cannot be treated as interchangeable; no general identity fallback was retained.
- Prefix-free conversion wrappers can preserve existing lazy guards. A new
  prefix-producing plan needs expression-local execution before admission.

## Guarded implicit-conversion discovery

An unrestricted implicit fallback in shared coercion was unsafe: with A-to-B and
Pick(B)/Pick(C), the diagnostic-recovery candidate could become successful merely
by reversing declaration order. With A-to-B and A-to-C it could accept the first
alternative instead of preserving ambiguity.

Fixed-pair preflight was insufficient because script-reference dereferencing
could expose a provider afterward. Permission must reach actual application
through recursive coercion, and remain disabled for out arguments and the typed
resolver's recovery-only fallback. This is why the delivered bounded paths do not
claim conversion-aware overload matching.

## Delivered bounded follow-up

The retained slices were `943bf74` (exact native explicit providers), `2f3c5b6`
(bounded explicit enum/Int32 fallbacks) and `1fc6efd` (guarded exact implicit
providers). Selection/fixed-type evidence lives in the focused conversions module;
call lowering applies small hooks. No new IR/opcode, ranking, chaining, general
identity fallback or matcher expansion was introduced.

A historical boot-only corpus comparison recorded 7,999 shared errors before
these slices and 3,315 afterward, with unsupported-conversion diagnostics changing
from 5,275 to 815. These are the previously recorded 2026-11-02 results, **not a
current-HEAD rerun or semantic-accuracy score**. Total errors matched the smaller
experiment, but final diagnostic categories differed; experimental categories must
not be reused as delivered counts. Newly reached code can expose further failures.

Synthetic tests inspect provider targets, evaluation once, Skip ordering, bounded
four-byte enum instructions and failure/output preservation. They do not reconcile
installed provider ABI or prove runtime behavior. The broad prototype never
completed full official/runtime, malformed-metadata or output-preservation
validation; later bounded regression coverage does not retroactively validate it.
Local patches, copied workspaces, census reports and build logs are disposable,
not required artifacts. Useful rules are retained above; detailed obsolete logs
remain in Git history.

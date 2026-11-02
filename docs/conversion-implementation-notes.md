# Deferred conversion implementation findings

A prototype following the investigation in [redmod-parity-next.md](redmod-parity-next.md)
was removed before committing. It broadened `lower_static_cast`, overload selection
and shared coercion in `lower/calls.rs` too much. **Provider dispatch, new opcode
fallbacks and general identity casts are not implemented by this batch.** The
previous conversion registration/fail-closed behavior remains unchanged.

## Findings worth retaining

- Existing free-function call IR and backend native binding can emit direct
  conversion-provider calls with return-qualified names such as
  `Cast;Int8;Int64`. A primitive conversion is not necessarily a standalone opcode.
- Calls, assignments, initializers and returns share coercion machinery. A future
  implementation should reuse one bounded selector/application path rather than
  add per-context conversion exceptions.
- Exact applicable overloads must be considered before provider-converted
  alternatives. Multiple directly converted alternatives remain ambiguous; no
  numeric-distance ranking or conversion chaining is justified by current probes.
- Applicability must be read-only. A variable's current lower bound is not proof
  of a fixed final type. Provider/opcode selection needs concrete evidence or
  properly isolated candidate inference, followed by constraint commitment only
  after unique selection.
- Enum opcode width follows the integer operand/result width, not simply enum
  storage. The prototype encoded generated intrinsic calls with two type arguments
  even though the public intrinsic expects one; this was expedient, not a clean
  shared contract. Prefer explicit typed conversion metadata with validation.
- Both frontend and backend must validate supported enum/storage/width cells and
  operand/result consistency. Backend validation of metadata shape alone is not
  sufficient. Unsupported emission must prevent saving output.
- Conversion wrappers may preserve existing lazy guards if they produce no
  statement prefixes. New prefix-producing plans remain unsupported without
  expression-local execution. See [lazy limitations](lazy-evaluation-limitations.md).
- Native provider declarations alone do not prove installed ABI/binding. Synthetic
  fixtures validate compiler selection and serialization, not engine reconciliation.
  Arbitrary authored scripted conversions must remain fail-closed.

## Identity casts are not automatically an isolated fix

The prototype identity path became coupled to ordinary Cast precedence,
contextual literal typing and when operands are lowered. It reused native operand
handling across dialects and required extra applicability checks to avoid
replacing community Cast overloads. None of that work was retained.

Before implementing a small identity fallback, specify precedence and require
proven same-representation operands. Preserve contextual literal types, inference
constraints, source spans, evaluation exactly once, and ordinary user Cast
behavior. Do not treat equal-looking weak/strong/script-reference types as
interchangeable or resolve fresh variables merely to claim identity.

## Prototype evidence and limits

Before removal, focused tests passed for a generated 242-cell primitive matrix,
provider use in several coercion contexts, exact-overload preference, converted
ambiguity, identity precedence, serialized provider targets, single evaluation,
lazy ordering and selected opcode-width cases. Those tests and the implementation
were removed together; they are not ongoing regression coverage or delivered
support. No full workspace validation, fresh official comparison, full corpus
replay or runtime validation of the prototype was completed. Malformed-metadata
and compiler-API output-preservation tests were not completed.

The retained batch instead delivers opaque engine builtins, native dynamic-array
Grow/Count descriptors, shared indexed subtype applicability and contextual
switch literals, each independently tested. It deliberately does not continue
the broad conversion prototype under a different filename.

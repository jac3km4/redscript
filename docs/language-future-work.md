# Language future work

This tracks unsupported behavior and correctness work, not a promise of official
REDmod or game-runtime parity. Completed features belong in the language guides
and focused semantic contracts, not in this list.

## Variant default initialization on repeated declarations

`crates/compiler/backend/src/assemble.rs` skips `Stmt::InitDefault` when the type
is `Variant`, relying on VM default construction of function-local storage.
Function-entry VM initialization is not a reset when a declaration executes again
on a loop back edge. A repeatedly executed declaration may therefore retain the
previous iteration's payload instead of becoming an empty Variant.

This is a backend risk, not merely a REDmod transpiler limitation. Game-runtime
behavior has **not been verified**. Do not treat successful compilation or a
missing reset instruction as evidence that repeated initialization is correct.
No semantics fix is included in this documentation change.

Required follow-up:

- Establish an evidence-backed backend reset at each declaration execution point,
  or reject unsupported repeated Variant default initialization before saving.
- Add backend instruction/serialization tests for function-entry initialization
  versus loop declarations, nested loops, and continue/back-edge paths. Store a
  payload before the next declaration execution so the regression distinguishes
  a real reset from entry-only initialization.
- If rejection is necessary, test diagnostics and compiler API output preservation;
  do not silently omit the reset or replace Variant with an unrelated value.
- Verify the resulting behavior in the game separately from compiler tests.
  Keep transpiler Variant default resets gated until their target semantics are
  established too.

## Conversions and native bindings

The bounded provider and enum paths are implemented; see
[conversion semantics](language-semantics.md#conversion-declarations-and-bounded-native-selection). Remaining work includes
conversion-aware overload applicability, scripted-provider eligibility and
source/cache reconciliation, wider enum/integer cells, general identity/String
conversions, native literal retyping, and qualified weak conversions. Do not add
numeric-distance ranking, chaining, or a return-only Bool exception without
new evidence. Verify installed provider bindings rather than assuming that a
native source declaration proves an engine ABI.

## Evaluation and initialization

- Expression-local array construction is not implemented. Preserve lazy branches,
  callee/receiver/argument order, switch matcher execution and loop reevaluation
  before removing [prefix safety diagnostics](language-semantics.md#lazy-evaluation-limitations).
  Runtime-compatible mode currently tolerates them and retains potentially
  reordered/eager legacy lowering; this is not a completed correctness fix.
- Native initialized declaration groups and general value sequences need explicit
  evaluation/result rules. Broader prefix/postfix ordering and native float
  matching need independent characterization.
- Carry verified, versioned native-struct completeness through source/dependency
  merging; never mark every imported struct complete. Check every supplied
  constructor argument, including excess arguments during recovery. Characterize
  zero/default construction and omitted field slots separately from partial
  positional construction and emitted default values.
- Broader decompiler switch layouts need provable matcher order, body fallthrough
  and exit boundaries. Grouped/backward-linked shared-body labels and detached
  first/middle defaults are supported; unprovable layouts remain rejected.

## Analysis, lookup and constness

- Implement explicit analysis-only partial declaration harvesting. Preserve
  trustworthy siblings, invalid-signature provenance and unavailable-body status;
  never invent empty/native implementations or emit partial units.
- Separate resolution outcomes from diagnostic wording and record provisional
  recovery bindings. Invalid candidates must not silently become verified dispatch.
- Native cast/grouping disambiguation needs a syntax-based rule: historical probes
  found `-(x) - (y)` misread as a cast to x, while official arithmetic succeeded.
  Retain real `(Type)-value` controls; do not fix it with a type-name whitelist.
- Broaden native candidate tiers only with read-only applicability: applicable
  instance methods retain priority over static/global candidates. Qualified-global
  calls, untyped NULL equality and type/local collision edges need their own rules.
- Extend overload specificity beyond the currently bounded concrete reference
  signatures only with evidence (weak/script references, out, omitted/null slots,
  conversions and differing flags). Generic static hiding already substitutes
  inherited parameter types for hiding; applicability still needs receiver
  substitution as tracked below.
- Const field/return enforcement, transitive alias/effect analysis and const-aware
  overload ranking remain incomplete. Nested closure capture IDs lack defining-scope
  identity. Characterize override out-flag ABI differences before relaxing checks.
- Broader mutable scalar forms, nonstring/nonfield metadata, invalid/inherited hint
  targets, fixed-array operations, EraseFast and additional Variant members need
  precise contracts rather than aliases to superficially similar operations.

### Transactional inference and failure rollback

A recursive constraint operation can mutate earlier or nested variables before a
later step fails. Local validate-before-store does not restore all reachable state,
leaving transactional failure handling as an outstanding correctness goal.

- **Architecture:** Implement an internal undo journal with checkpoints, commit, and
  rollback shared across entire recursive constraint operations. Account for bounds,
  representatives/path compression, directed edges, and ownership-component changes in
  the current solver representation.
- **Speculative callers:**
  - *Try-and-commit:* Retain successful constraints and roll back failures (for compatible
    contextual hints).
  - *Probe:* Always roll back, including on success (for read-only applicability queries).
  - Audit ignored return hints, dynamic-cast alternative checks, and fallback paths that
    discard constraint errors to ensure live variables are never mutated solely to test
    a candidate.
- **Regression criteria:** Regressions must prove unchanged reachable inference after
  failure in nested generic arguments, invariant equality, representative compression,
  edge propagation, and speculative hints. Follow failed operations with subsequent successful
  inference to catch contamination. Maintain existing checks for cyclic bounds without
  panics or leaks.

## Export, conditions and tooling

- Transpilation rejects const locals rather than erasing the qualifier. Supporting
  them requires preserving the community initialization/mutation contract, not
  emitting native scalar const declarations with different rules.
- Conditions need resolved semantic identity, actual inactive bodies and a
  variant-aware export plan. Conversion grouping already includes the Cast return
  destination; broader alias/cross-namespace signature equivalence remains open.
- Broaden export only with execution-site default resets, correct place/reference
  lowering, typed intrinsic mappings, exact/select imports and proven patch ownership.
  Struct/enum/Variant/fixed-array resets, wider literals, declaration flags and
  callback-result calls remain bounded by the transpiler contract.
- Native custom generics/function values, mixed-reference forms, broader event/patch
  adaptation and mod-defined/import-only patch targets require separate work.
- IDE references/rename, completion/member indexing and range formatting must retain
  written provenance and snapshot ownership. Coordinate compiler/IDE dependency pins
  and validate both repositories when changing shared contracts.

## Deferred frontend and REDmod review work

The following work was identified by the review against `1.0.x`. These are
outstanding issues, not supported behavior. Small fixes for omitted arguments,
local receiver shadowing, optional-root cycle preservation and native equality
negation were made separately. Assignments to script-reference destinations remain
accepted with REDmod write-through semantics; preserving community rebinding
remains future work.

### Correctness: instantiated overload applicability and const forwarding

- **Receiver substitution in overload applicability.**
  `crates/compiler/frontend/src/lower/calls.rs` tests candidates using declared
  parameter types before applying the receiver environment. With `Base<T>.Pick(T)`
  and `Child extends Base<Int32>` declaring `Pick(Bool)`, `Child.Pick(true)` reports
  ambiguity against the inherited generic candidate. Static hiding substitution
  does not solve this applicability problem. Instantiate candidate parameters
  before applicability and specificity comparisons without mutating inference.
  Test inherited and child selections, method/free type arguments, declaration
  order, and fresh inference variables.
- **Instantiated const-forwarding checks.**
  `crates/compiler/frontend/src/stages/const_validation.rs` classifies declared
  parameter types. `Take<T>(value: T)` accepts a const array without the forwarding
  diagnostic produced by an equivalent concrete array parameter. Use resolved
  function and receiver type arguments when classifying parameter mutability.
  Cover const parameters and locals, object/array arguments, and scalar/const
  parameter controls. Preserve the file-local native policy exceptions.

### Correctness: target symbol names and literal-only field defaults

- **Import-aware target names.**
  `crates/compiler/redmod/transpiler/src/body.rs` emits fully qualified free calls,
  while `src/target.rs` emits qualified types. Snapshots include `Dependency.Value()`
  and `Models.Value`, but the official import probe rejects qualified calls such
  as `First.Pick(1)`. Introduce a target-name plan tied to resolved symbol identity,
  imports and collisions; merely stripping namespaces can select another symbol.
  Until the representation is verified, unsupported references should fail closed.
  Test same-module and dependency calls/types, conflicting simple names, overloads,
  and exact/wildcard imports through semantic roundtrips and optional official probes.
- **Field-default formatting distinct from expressions.**
  `crates/compiler/redmod/transpiler/src/declarations.rs` special-cases Uint32 but
  otherwise uses expression formatting for literal-only field defaults. Int16,
  Double and Int32 minimum values can become casts or arithmetic expressions.
  Define an exhaustive field-default formatter with verified spellings or typed
  rejection. Cover every constant kind and numeric boundary in fields separately
  from local initializers, including added fields and official parser validation.
- **Community script-reference rebinding.**
  The transpiler emits assignments to `script_ref` destinations unchanged and does
  not diagnose the semantic difference: REDmod writes through the reference,
  whereas community source rebinds it. For example, emitted `a = b` modifies a
  pointee instead of changing which place `a` references. Preserving community
  rebinding requires a proven representation, including alias identity and
  subsequent reads; successful target compilation does not establish equivalence.
  Audit `out` reference forwarding under the same dialect distinction.

### Array guard structure and safe static callees

- Adapter `src/array_guard.rs` and transpiler `src/source.rs` reject member-shaped
  callees with array arguments before resolution, including static `Host.Take([1])`.
  Defer static-versus-runtime classification to resolved validation. Preserve the
  runtime receiver/callee evaluation boundary; syntactic member access alone is
  neither a safe exemption nor proof of observable receiver evaluation.
- Frontend `src/lower.rs` recursively scans an expression subtree on each recursive
  lowering call. Run structural guards once per expression root rather than
  repeatedly scanning descendants, while retaining selected-overload/skip checks,
  closure execution boundaries and diagnostic deduplication. Add a deep-expression
  scaling test and preserve lazy/receiver/prior-operand regression coverage.

### Test maintenance, without losing semantic coverage

- Consolidate frontend lazy-array cases from `lowering_inference.rs`,
  `overload_selection.rs` and `array_guard_compatibility.rs` into a matrix covering
  single/overloaded calls and empty/nonempty/nested literals. Preserve selected
  overload and lazy receiver distinctions. Merge fresh-local ambiguity coverage
  from `indexed_applicability.rs` into `overload_selection.rs`, retaining declaration
  order and crossing-signature cases. Snapshot diagnostic presentation once while
  keeping code/count/severity and retained-IR assertions explicit.
- Replace adapter `control_flow.rs`'s full debug-IR snapshot with a stable projection
  of statement shape, call identities and continue targets. Keep execution-order
  and backend jump tests: snapshots do not replace them. Move the weak default
  hoisting smoke test in `semantics.rs` into the execution-based control-flow suite.
  Use a metadata snapshot for the large manual expectation in `metadata.rs`, keeping
  span assertions explicit.
- Share transpiler emission/target-roundtrip harnesses and repeated overload
  scenario construction, preserving separate conversion and borrow contracts.
  Negative transpiler tests should establish successful frontend analysis before
  asserting the intended export diagnostic. Combine repeated struct-default fixture
  compilation into one emission snapshot plus reset-site and typed-IR assertions.
- Replace raw byte-window opcode scans and function-name prefix matching in
  transpiler `as_ref.rs` with bounded instruction decoding and exact identities,
  reusing the approach in `official.rs` and `field_reads.rs`. Preserve explicit
  version/layout checks and distinguish compiler acceptance from runtime evidence.
- Adapter `CapabilityError::{OperatorDeclaration, EnumStorage, ConstReturn}` appear
  obsolete, but the enum is publicly exported. Review API compatibility before
  removing them and the stale const-return message assertion; const returns already
  have positive semantic coverage.

## Evidence needed for parity claims

Use the optional [semantic corpus survey](../crates/compiler/redmod/adapter/CORPUS.md)
with explicit source/dependency inputs and compiler/report fingerprints. Rerun
unchanged inputs after major slices and classify newly exposed failures. Historical
diagnostic counts are not current results or accuracy percentages.

Measure declaration, clean body/type and binding coverage separately, including
unsupported, dependency-blocked, recovered, failed and unvisited sites. Record
source-role/origin edges, selected declarations/substitutions/dispatch and per-argument
recovery before IR loss. Returned types and exact-span mappings are not proof that
an expression was fully checked. Keep provisional observations separate.

A stratified positive/negative official differential audit must inspect emitted
call/override targets and metadata, not just acceptance. Include high-risk and
unmapped/error strata, disclose denominators and oracle limits, and account for
sampling uncertainty. Official bundle inspection can be partial (external type
placeholders and Profile records); it is not full decoding or runtime validation.
Keep installation-dependent probes optional and reduce findings to authored
portable fixtures; do not commit installed game scripts or require local artifacts.
See [CONTRIBUTING.md](../CONTRIBUTING.md) for code-validation requirements.

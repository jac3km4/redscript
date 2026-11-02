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
[conversion semantics](conversion-declarations.md). Remaining work includes
conversion-aware overload applicability, scripted-provider eligibility and
source/cache reconciliation, wider enum/integer cells, general identity/String
conversions, native literal retyping, and qualified weak conversions. Do not add
numeric-distance ranking, chaining, or a return-only Bool exception without
new evidence. Verify installed provider bindings rather than assuming that a
native source declaration proves an engine ABI.

## Evaluation and initialization

- Expression-local array construction is not implemented. Preserve lazy branches,
  callee/receiver/argument order, switch matcher execution and loop reevaluation
  before removing [prefix safety diagnostics](lazy-evaluation-limitations.md).
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
  inherited parameter types; it is not outstanding work.
- Const field/return enforcement, transitive alias/effect analysis and const-aware
  overload ranking remain incomplete. Nested closure capture IDs lack defining-scope
  identity. Characterize override out-flag ABI differences before relaxing checks.
- Broader mutable scalar forms, nonstring/nonfield metadata, invalid/inherited hint
  targets, fixed-array operations, EraseFast and additional Variant members need
  precise contracts rather than aliases to superficially similar operations.
- Retain transactional inference as a separate hardening goal; review the
  [inference notes](type-inference-hardening.md) against the current solver before
  implementation rather than replaying an old plan verbatim.

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

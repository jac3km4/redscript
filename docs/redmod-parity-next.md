# Next steps toward 90–99% REDmod semantic analysis

Investigation: 2026-11-01, HEAD `86af12a`. This is a proposed workstream, not
implemented support or a semantic-accuracy claim. Production code and installed
scripts were unchanged. See also [previous checkpoints](redmod-semantic-parity.md).

## Reproduced baseline

The current release survey replayed all seven reports byte-for-byte against the
installed `D:/win/Cyberpunk 2077/tools/redmod/scripts`, read-only: 2,112 files,
21,873,807 bytes, embedded boot/default symbols, **no game bundle**.
Reports are disposable under `target/redmod-parity-next-baseline/`.

| Observation | Count |
| --- | ---: |
| Files reaching processing / excluded | 2,112 / 0 |
| Shared errors / warnings | 12,655 / 3,705 |
| Unsupported conversion diagnostics | 4,958 |
| Invalid-signature diagnostics | 4,921 |
| Incomplete native constructor diagnostics, runtime-allowed | 1,008 |
| Unresolved functions, including 728 Cast calls | 805 |
| Type errors / overload ambiguities | 298 / 163 |
| Unresolved types / members / references | 162 / 140 / 73 |
| Arity / const-local / temporary diagnostics | 118 / 5 / 4 |
| Valid / invalid function signature flags | 53,819 / 56 |
| Explicit bodies retained / parsed | 38,824 / 38,861 |
| Exact-span mapped / bridged expression spans | 781,341 / 1,144,329 |
| Returned non-Nothing type / unresolved mapped queries | 778,780 / 2,561 |

The principal opportunity is **dependency completeness and conversion dispatch**,
not further syntax admission. Counts are diagnostic sites, not independent bugs;
removing a blocker can expose new errors.

## 1. Verified dependencies and signature recovery

Four engine primitives are absent from boot/default symbols: `NodeRef`, `CRUID`,
`LocalizationString`, and `redResourceReferenceScriptToken`. Fresh official
pass-through probes and decoded installed bundle records independently establish
primitive identities. They do **not** establish integer aliases, native layouts,
conversions, literals or default values. Do not fabricate class definitions or
alias these types to Uint64.

Two invalid resource-token equality overloads block intrinsic equality fallback
at **4,822 sites in 780 files**. Valid matching overloads still win; the defect is
that equality only falls back on `UnresolvedFunction`, while the invalid sibling
changes that outcome to `InvalidFunctionSignature`. The other 99 invalid calls
actually depend on NodeRef/CRUID signatures.

An isolated survey importing **only the four verified primitive definitions**
from the installed bundle, with unchanged corpus bytes, measured:

| Observation | Boot only | Four verified primitives restored |
| --- | ---: | ---: |
| Errors / warnings | 12,655 / 3,705 | 8,035 / 1,609 |
| Invalid signatures / unavailable explicit bodies | 56 / 37 | 0 / 0 |
| Invalid-signature diagnostics | 4,921 | 0 |
| Unresolved types | 162 | 4 |
| Unsupported conversions | 4,958 | 5,275 |
| Incomplete native constructors | 1,008 | 1,078 |
| Exact-span mapped expressions | 781,341 | 875,101 |

This is a dependency experiment, not a production feature or full bundle-backed
analysis. The net reduction is **4,620 errors**, not the sum of removed original
diagnostics. It also removes 68 original missing member/reference cascades.

Required work:

- Explicit engine-dependency profile/loading with provenance and source/dependency
  reconciliation. Reparsing source aggregate stubs must not silently erase bundle
  completeness/fields. Keep boot-only and dependency-backed results separate.
- Shared resolution outcomes independent of user-facing diagnostic variants;
  preserve invalid candidate provenance and distinguish known-inapplicable from
  potentially relevant invalid overloads. Keep strict compilation fail-closed.
- Equality recovery may retain provisional analysis, but must not claim verified
  dispatch when an unresolved candidate could apply. Do not simply delete broken
  declarations or suppress their errors.

The four remaining unresolved types are parser ambiguity: `-(x) - (y)` is parsed
as a cast to `x`. Official reduced arithmetic succeeds. Fix native cast/grouping
disambiguation with real `(Type)-value` controls, not a name whitelist.

## 2. Shared conversion planning, bounded native eligibility

The 4,958 unsupported conversions comprise **3,126 explicit casts and 1,832
implicit coercions**. Current explicit attribution can report unsupported from a
target declaration without proving that the source matches.

High-volume original-site groups:

- 1,455 inferred enum-to-Int32 explicit casts.
- 1,223 inferred different-primitive explicit casts.
- 1,374 observed EntityID-to-StatsObjectID implicit coercions.
- 362 Bool-to-AIbehaviorConditionOutcomes implicit coercions.
- 728 unresolved Cast calls: 282 enum targets, 219 nested weak-object targets,
  206 String targets, and 21 identity/value/upcast cases.

Explicit source classifications are best-effort: 196 operands remain unknown.
The implicit observation retains unresolved variables at 15 sites. These are not
promised additive fixes.

**Official evidence:** 416 completed conversion probes, including a complete
11-source × 11-target × implicit/explicit **242-case primitive matrix**. It matches
the declared primitive conversion table plus identity exactly. Exact overloads
win; Int8 with Int16 and Int64 converted alternatives is ambiguous in either
order. Do not implement numeric distance ranking, a widening ladder or chaining.

Use shared conversion requests/plans separating read-only applicability,
selection and committing constraints. Plans should distinguish identity/subtype,
reference adaptation, callable provider, and typed intrinsic. Apply the same
machinery to calls, initializers, assignments, returns, defaults and operators;
never lower a source operand repeatedly to try alternatives.

Deliver in bounded slices:

1. Imported primitive providers with explicit/implicit intent and callable binding
   provenance; reproduce all matrix cells and selected emitted call targets.
2. ID/vector providers with exact direct-edge eligibility and explicit-only
   negatives; do not confuse inheritance with conversion dispatch.
3. Native builtins: storage-sensitive enum/integer conversions, bounded ToString,
   object Bool, qualified weak casts, identity and struct upcasts.
4. Source/cache provider reconciliation before activating installed scripted
   conversions. Authored scripted conversions generally reject in official tests,
   while installed Bool/enum conversion accepts; even installed EHotkey implicit
   use rejects. Source declaration intent alone is not sufficient eligibility.

Enum conversion emission needs typed width metadata: existing four-byte EnumInt/
IntEnum paths are insufficient. Official instruction size depends on conversion
operand/result width, not simply enum storage. Some Int16/Uint16-to-enum probes
failed without semantic diagnostics; these remain unclassified, not proven rules.

Retain [lazy safety restrictions](lazy-evaluation-limitations.md). A plan producing
statement prefixes must remain unsupported without expression-local execution.
For IDE-only support ahead of emission, enforce an explicit backend capability
gate; never save silently unsupported output. No game-runtime parity was tested.

## 3. Native constructors and defaults

All 1,008 incomplete-constructor diagnostics supply the script-declared field
count. Existing backend completeness metadata covers **1,005 sites / eight
types**, but the boot/source path does not propagate it. EulerAngles (two sites)
and PhoneCallInformation (one) are outside that registry. Do not mark every
imported struct complete or remove the check globally; carry verified,
versioned metadata through source/dependency merging.

Separate shared semantic work is needed for zero/default struct construction and
explicit omitted field slots. Official probes accept `S()` and a leading omitted
slot, reject partial positional initialization, and reject excess arguments.
There are eight original zero-argument scripted-struct arity errors.

The current incomplete-native path can zip arguments to fields and leave excess
expressions unchecked/dropped. Validate every supplied argument even in recovery;
represent default construction separately from explicit field-slot initialization.
Official acceptance alone did not establish emitted default values.

## 4. Remaining inference, lookup and native surface

The residual audit reconciles 878 diagnostic rows outside the three dominant
groups and unresolved types. Its highest-confidence independent causes are:

| Cause | Original sites | Proposed location |
| --- | ---: | --- |
| Instance lookup commits before applicable static/global candidates | 155 | Native candidate tiers over shared read-only applicability |
| Untyped NULL equality chooses among unrelated authored overloads | 152 | Native source-aware equality routing; typed NULL remains distinct |
| Missing dynamic/fixed-array and Variant members | 100 | Native descriptors plus precise shared IR/backend support |
| Known-type-qualified calls to global functions | 37 | Native qualified-global candidate tier |
| Indexed derived-object evidence rejects applicable base parameters | 32 | Shared inference applicability correction |
| Missing engine-type field/local fallout | 68 | Dependency restoration above, not separate lookup fixes |

Do not implement lookup by catching lowering errors and retrying: that risks
inference mutation and duplicate side effects. Instance precedence holds when an
instance candidate is applicable; globals-first is also wrong.

Builtins include EraseFast (distinct element-order behavior, not Erase), Grow,
Count(value), fixed-array Size/FindFirst, Variant.IsValid and Variant.GetTypeName.
The latter returns CName officially, unlike the shared boot String signature;
do not silently change the community ABI. Fixed arrays need correct static-array
instructions, not just dynamic-array member aliases.

Smaller work: weak subtype specificity, branch coercion for weak/bare conditional
joins, contextual switch literals and ref<String> scrutinees, native nested-weak
normalization, numeric default metadata, two super-static calls, five shallow-
const array-boundary cases and four out-literal cases. Characterize these
independently; do not weaken shared const checks or resurrect the removed special
Int32-to-Bool return hook. General conversion work owns Bool conversions.

**Keep valid rejections:** the optional-tail overload ambiguity also fails
officially. Six static/free calls through value receivers have negative official
analogues; do not broadly admit or discard evaluation of receiver expressions.

## 5. Coverage instrumentation before a percentage claim

Current exact-span mapping is **68.28%**, not semantic accuracy. The 362,988
unmapped spans include 126,291 callee nodes whose enclosing call maps, 3,079
default-directive nodes outside the function walk, 776 spans in unavailable
bodies, and **179,342 still needing causal attribution**. Other spans are
qualifiers/folded operands. Native casts introduce synthetic nonempty Cast-name
spans, contaminating the purported written-expression denominator.

Conversely, **5,588 resolved call spans contain errors**. This is an overlap
observation, not exactly 5,588 proven bad bindings. Authored recovery probes prove
the underlying issue: `Takes(unknown)` can retain a zero-argument call, while
`Takes("wrong")` retains a mistyped argument; both still query as Int32 from the
valid function's return signature. Returned types do not prove full checking.

Implement bounded, opt-in observation hooks rather than a second analyzer:

- Native source identity/role and native→bridge→IR origin edges; exclude synthetic
  nodes from written populations without losing callee/type binding obligations.
- Attempt/result records before IR loss: clean, folded-checked, binding-only,
  default-checked, unsupported, dependency-blocked, recovered, failed and unvisited.
- Per-argument and statement recovery provenance; provisional types/bindings must
  not count as clean. Do not taint unrelated expressions solely by body proximity.
- Separate field-default checking, contextual Null results, body completeness and
  signature obligations. Record selected declaration, substitutions and dispatch.
- A reproducibility manifest: compiler/binary/source/dependency fingerprints,
  report schema, completion marker and sampling seed.

Partial declaration harvesting remains useful for edited/future unsupported files,
but recovers **zero excluded files in this baseline**. It is not the immediate
answer to unmapped spans. Keep partial results analysis-only and explicitly poisoned.

## Recommended implementation sequence

1. **Measurement foundation:** source-role/origin and recovery ledger, with tests
   proving failed arguments cannot masquerade as clean call analysis.
2. **Verified engine primitives and metadata merge:** rerun dependency-backed and
   boot-only checkpoints; restore signatures/bodies without fabricated semantics.
3. **Shared candidate applicability/recovery:** fix indexed subtype evidence and
   separate no-match/invalid-candidate/fallback outcomes without mutating inference.
4. **Shared conversion plans + imported primitive providers:** exact-overload
   preference, direct-edge ambiguity and consistent coercion contexts.
5. **Native ID/vector providers and builtin cast families:** enum widths, String,
   weak/object targets, then separately reconcile installed scripted providers.
6. **Native lookup tiers and NULL equality:** preserve applicable-instance priority,
   qualified-global/static behavior and typed-null controls.
7. **Verified constructor completeness + shared default/slot construction:** check
   all arguments, retain ordering/default provenance and gate incomplete emission.
8. **Remaining builtins and smaller semantic rules:** precise instructions,
   branch/switch/reference handling and narrow native policies where justified.
9. **Fresh full-corpus checkpoint + stratified official differential audit:**
   reclassify newly exposed failures after every major slice, not just removals.

Steps 2, 3 and constructor characterization can proceed independently once the
measurement contract is agreed; conversion selection should reuse applicability
rather than add another inconsistent eligibility relation.

## Acceptance targets

Define separate **clean type coverage** and **clean binding coverage** over frozen
native source populations, including blocked/unsupported/unvisited sites. Publish
useful provisional results separately. Compare types and selected declarations,
not just accept/reject or error totals.

- Initial milestone: at least 90% clean type and binding coverage, plus official
  stratified agreement evidence with a declared denominator.
- Strong milestone: at least 95% for both, with no high-risk stratum hidden by
  abundant simple expressions.
- A 99% claim requires at least 99% clean coverage and defensible lower confidence
  bounds for agreement, with missing dependencies and oracle-unobservable sites
  disclosed rather than removed from the denominator.

Sample across files/bodies, features and baseline states (clean-looking, error,
unmapped). Inspect official selected call/override targets and conversion/default
metadata; use known inclusion weights and cluster-aware uncertainty. Hundreds of
authored feature probes are valuable regressions, not a representative corpus
accuracy sample. Full official bundle decoding still encounters thin definitions/
Profile records; partial raw call/opcode inspection is narrower evidence.

## Evidence and validation

Detailed disposable reports under `target/parity-next/`:
`dependencies.md`, `conversions.md`, `residuals.md`, and `coverage.md`, plus scripts,
per-site inventories and fresh official fixtures/logs. They may include local game
identifiers or copied sources and are not portable committed tests. Preserve
critical conclusions here; reduce future regression fixtures to authored sources.

Validation consisted of deterministic baseline replay, exact report/partition
reconciliation, isolated dependency experiments, existing-release adapter probes,
standalone target-only instrumentation and official reduced compilations. Primitive
matrix checks had zero table mismatches. Some official failures remain explicitly
unclassified; full bytecode/runtime agreement was not established. No production
build/test/Clippy run was needed or claimed for this investigation.

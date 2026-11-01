# REDmod semantic parity investigation

Original investigation checkpoint: 2026-11-01, compiler `fad6745`. The original
investigation made no production changes. The cleanup refresh below supersedes
previous integration measurements for the current working tree. Historical
checkpoints retain their original counts; final history is being arranged
separately, so no final revision is assigned here. No checkpoint claims 90–99% parity.

## Baseline and meaning of parity

The original boot-only snapshot processes 2,046 of 2,112 files (96.9%), excluding 66
for unsupported capabilities. It reports 47,982 shared errors, 1,456 adapter
processing errors and 25,364 warnings. Among eligible files, 1,576 have at least
one shared/adapter error (including runtime-allowed errors). Thus 470 eligible
files have no reported errors, but neither those files nor returned IR establish
semantic correctness. Many files contain only declarations.

The inputs are the installed `tools/redmod/scripts`, embedded boot library and
default symbols, **without a game bundle**. Original reports are disposable under
`target/redmod-semantic-corpus-shared/`. A matched release executable replayed all
five reports byte-for-byte. Corpus copies used for experiments never modified
installed sources.

A credible 90–99% target needs separate denominators:

1. **Declaration coverage:** faithfully represented declarations/signatures, with
   unsupported and poisoned entries counted, not silently dropped.
2. **Body/expression coverage:** source bodies actually checked, and written
   expressions with fully resolved, non-poison types. Synthetic nodes do not inflate
   the denominator. Record unresolved, unsupported and not-visited separately.
3. **Binding/dispatch agreement:** resolved fields, call overloads, casts and override
   slots agree with official authored differential tests. Successful compilation
   alone does not identify the selected overload; inspect emitted targets too.
4. **Diagnostic agreement:** both accepted and rejected authored cases, classified
   separately from absent engine dependencies and recovery cascades.

Do not call the percentage of files entering processing a semantic parity score.
A full-corpus accuracy percentage cannot be inferred from current diagnostics.

## Cleanup refresh and validation (2026-11-01)

The cleaned working tree was rebuilt with stable Rust and surveyed the installed
`D:/win/Cyberpunk 2077/tools/redmod/scripts` read-only, using embedded boot/default
symbols and no game bundle. Reports are in
`target/redmod-semantic-corpus-cleanup/`; replay to
`target/redmod-semantic-corpus-cleanup-replay/` produced **seven byte-identical
reports**. This checkpoint supersedes the previous integration measurement below.

All **2,112 files / 21,873,807 bytes** reach processing, with zero syntax,
capability, validation, missing-module or adapter processing errors. Shared totals
remain **12,655 errors / 3,705 warnings / 0 pedantic**, or 16,360 diagnostics;
1,172 files have errors and 940 have no reported errors, not semantic passes.

| Changed error group | Previous integration | Cleanup refresh |
| --- | ---: | ---: |
| `UNSUPPORTED_NATIVE_CONVERSION` | 4,925 (645 files) | 4,958 (651 files) |
| `UNRESOLVED_FN` | 838 (289 files) | 805 (276 files) |
| Unresolved functions naming `Cast` | 761 | 728 |

The source-aware native cast helper now attributes 33 formerly unresolved calls
as unsupported conversion dispatch; conversion selection/emission is still not
implemented. All other per-code counts and all coverage totals below are
unchanged. Coverage partitions reconcile, including 781,341 expression-query
rows. Coverage, expression queries, file outcomes and summary are byte-identical
to the previous reports; diagnostic reports differ. This is improved attribution,
not proof of additional working conversions or semantic accuracy.

Cleanup includes source-based lazy argument guards and cast fallback operands,
community `const` syntax round-tripping, const-local entry defaults, and removal
of the special Int32-to-Bool event-return coercion. Ordinary matching native
`Cast` resolution remains subject to the same fail-closed conversion rules.
See [lazy evaluation limitations](lazy-evaluation-limitations.md) for source guards,
conservative source-free typed-call rejection and expression-prefix limits.
Const-local default initialization does not permit explicit scalar/struct writes.

Full `CONTRIBUTING.md` validation passed on this working tree:

- Default and all-feature workspace tests: **738 passed, 0 failed, 9 ignored**
  in each configuration, across 117 targets each (previous checkpoint: 728 passed).
- Default and all-feature workspace Clippy, **all targets**, with `-D warnings`.
- `nightly-2026-10-15` formatting check.
- Stable release workspace build and all-feature, no-dependency documentation.
- Nine independent package checks: community AST/parser/formatter,
  frontend/backend, REDmod adapter/transpiler, decompiler and CLI.
- Five release `semantic_corpus` example tests.

Cargo used `--locked -j2 --target-dir target/parity-integration`,
`CARGO_PROFILE_DEV_DEBUG=0`, `CARGO_PROFILE_TEST_DEBUG=0`, and
`CARGO_INCREMENTAL=0`; release commands also used `CARGO_PROFILE_RELEASE_DEBUG=0`.
Only `target/parity-integration/debug` was deleted before rebuilding and again
before the corpus run to recover disk space. Release/docs and official-event
artifacts were preserved. The combined corpus build/test command timed out during
release-test compilation after the executable build succeeded; the separate
release-test retry passed. No source or test fixes were needed during validation.
The existing `chumsky` future-incompatibility advisory remains. These checks do
not establish official-compiler or game-runtime equivalence.

## Previous integration measurement (2026-11-01)

A stable release example built from the dirty tree based on `d46faac` surveyed the
same installed directory read-only: **2,112 files / 21,873,807 bytes**, embedded
boot/default symbols and no game bundle. Reports are in
`target/redmod-semantic-corpus-parity/`; the same executable replayed to
`target/redmod-semantic-corpus-parity-replay/` with **all seven files byte-identical**.
Build used `--release --locked --target-dir target/parity-integration -j2`,
`CARGO_PROFILE_RELEASE_DEBUG=0` and `CARGO_PROFILE_RELEASE_INCREMENTAL=false`.
The five release example tests passed; coverage totals/partitions reconciled.
No concrete measurement bug was found in `adapter/examples/semantic_corpus.rs`,
so instrumentation was left unchanged. This is not workspace validation.

| Counter | Snapshot baseline | Integration working tree |
| --- | ---: | ---: |
| Processing-eligible files | 2,046 | 2,112 |
| Excluded files (capability) | 66 | 0 |
| Syntax / validation / missing-module files | 0 / 0 / 0 | 0 / 0 / 0 |
| Adapter processing errors | 1,456 | 0 |
| Shared errors, including runtime-allowed | 47,982 | 12,655 |
| Shared warnings / pedantic | 25,364 / 0 | 3,705 / 0 |

There are **16,360 total diagnostics**, errors in 1,172 files and 940 files without
reported errors. None of these file counts is a semantic pass count. No baseline
body/query counters exist for a like-for-like coverage delta.

| Coverage observation | Count |
| --- | ---: |
| Available ASTs / declarations excluding imports | 2,112 / 96,126 |
| Function declarations | 53,875 |
| Signature flags valid / invalid / mixed / unavailable | 53,819 / 56 / 0 / 0 |
| Explicit bodies parsed / retained / unavailable | 38,861 / 38,824 / 37 |
| Written expression spans / mapped / unmapped | 1,144,329 / 781,341 / 362,988 |
| Queries resolved / unresolved / failure | 778,780 / 2,561 / 0 |
| Mixed outcomes (subset, worst outcome wins) | 38 |

These measure availability, **not type accuracy**. Signature flags do not prove
valid semantics; retained bodies include recovery IR and do not prove complete
checking. The 37 unavailable bodies occur in files with invalid signatures, but
causes are not assigned by the harness. Prototypes/synthetic bodies are excluded.
Expression counts deduplicate nonempty bridged AST spans, exclude Omitted/Error
placeholders and boot, and include field/default values. Mapping requires exact
spans on non-Nop IR in retained functions; callee names, folding, defaults and
unavailable bodies can remain unmapped. Synthetic nodes reusing source spans are
not reliably separable. A returned non-`Nothing` type is not a correctness or
poison-free guarantee. All 2,561 unresolved sites include `UnknownNullType`, and
38 also have a returned-type observation. Zero query failures applies only to
mapped sites, not the 362,988 unmapped spans.

| Dominant remaining error code | Count |
| --- | ---: |
| `UNSUPPORTED_NATIVE_CONVERSION` | 4,925 |
| `INVALID_FUNCTION_SIGNATURE` | 4,921 |
| `NON_FULLY_DEFINED_CTR` | 1,008 |
| `UNRESOLVED_FN` | 838 |
| `TYPE_ERR` | 298 |
| `MULTIPLE_MATCHING_OVERLOADS` | 163 |
| `UNRESOLVED_TYPE` | 162 |
| `UNRESOLVED_MEMBER` | 140 |
| `INVALID_ARG_COUNT` | 118 |
| `UNRESOLVED_REF` | 73 |
| `OTHER` / `INVALID_TEMP` | 5 / 4 |

Conversion declarations now register, but dispatch deliberately **fails closed**.
Invalid-signature guards dominate recovery cascades: 4,822 rows name `OperatorEqual`;
missing types include `NodeRef` (140) and `CRUID` (15). These are not independent
bug counts or evidence for fabricated engine definitions. Of unresolved functions,
761 name `Cast`. The 1,008 incomplete-native-layout checks principally concern
Vector2/Vector4/inkMargin/Color. Residual type/overload/arity/member errors need
separate authored reductions. Warnings are 3,117 unused locals and 588 unused item
qualifiers; do not suppress semantic errors to make totals smaller.

**Implemented in this measured tree:** signature-poison preservation/guards,
source lazy `skip`, explicit/implicit conversion registration (not dispatch),
const-local checks, event return handling, overload/forwarder and override-slot/
visibility/persistent fixes, Variant extraction ordering, ordered switch defaults,
overloaded indexing and bounded scalar mutable. Detailed handoffs remain the
contract for each feature; their validation is not replaced by this survey.

**Deferred:** partial declaration harvesting is **not implemented**; whole modules
with parse-stage diagnostics are still excluded, even though this input has none.
The coverage observer is not a partial-analysis mode. Conversion selection and
emission, complete dependency/layout modeling, residual semantic fixes and a
stratified official binding/dispatch comparison remain open. No full-corpus
accuracy percentage, full language parity or game-runtime behavior is established.
See [CORPUS.md](../crates/compiler/redmod/adapter/CORPUS.md) for reproduction and
full per-code/file counters. Historical ablations below remain baseline-relative,
not current results or additive predictions.

## Previous integration validation

The committed integration tree at `8667b28` passed the complete contribution
checks after the signature-validation traversal was made deterministic:

- Stable workspace tests, default and all features: **728 passed, 0 failed,
  9 ignored** in each configuration (117 targets each).
- Stable workspace Clippy, all targets, default and all features: passed with
  `-D warnings`.
- `nightly-2026-10-15` formatting check: passed.
- Stable release workspace build and all-feature, no-dependency docs: passed.
- Standalone checks for community AST/parser/formatter, frontend/backend,
  REDmod adapter/transpiler, decompiler and CLI: all nine passed.

Cargo commands used `--locked`, `-j2`, `target/parity-integration`, disabled
incremental compilation, and disabled development/test debug information. Disk
exhaustion on an initial test compilation was resolved by removing only that
validation target's generated debug directory and retrying. Release artifacts
and official probe evidence were preserved. Cargo's existing `chumsky`
future-incompatibility advisory remains. These compiler checks and authored
bytecode fixtures do not establish game-runtime compatibility.

## Dominant measured effects

Independent copied-corpus ablations, all relative to the unchanged baseline:

| Experiment | Shared errors | Unresolved functions | Interpretation |
| --- | ---: | ---: | --- |
| Baseline | 47,982 | 24,572 | Whole-file exclusion, boot only |
| Retain 207 non-lazy math declarations; omit two lazy ones | 36,422 | 10,305 | Demonstrates operator declaration-loss impact |
| Erase `skip` and admit all math declarations | 31,417 | 3,623 | **Unsound**, eager evaluation; never a proposed fix |
| Erase const-local qualifiers | 47,707 | 25,670 | **Unsound**; admitting more bodies exposes more failures |
| Omit imported implicit casts in five files, retain siblings | 38,508 | 23,395 | Availability experiment, not conversion support |

Effects are not additive. The conversion experiment reduces missing `Cast` by
1,801 but adds 2,021 explicit unsupported-conversion errors: better attribution,
not working conversion selection. Reducing diagnostics can also mean analyzing
less code, so track unchanged source sites and newly exposed bodies separately.

## Recommended implementation workstream

### 1. Preserve invalid signatures and contain recovery cascades

`frontend/src/stages/resolution.rs::create_function_env` currently drops parameters
whose types cannot resolve (`filter_map`) and substitutes `Void` for unresolved
explicit returns. Body binding then zips surviving types with original names.
This changes the meaning of the written signature and creates misleading results.

For example, `(id: MissingID, value: Int32)` becomes a one-argument signature and
binds the surviving Int32 type to `id`. This is independently reproduced, not an
assumed implicit-receiver counting bug.

Affected populations, **not promised fix counts**:

- 5,466 of 5,713 arity errors supply exactly one more argument than the reported max.
- All 565 CName/Bool mismatches correlate with shifted leading-ID signatures.
- 127 of 169 overload ambiguities have collapsed-signature evidence.
- 81 of 185 impossible casts have shifted source parameter types.
- Seven of 12 missing-override errors have erased distinguishing parameters.

Preserve slot/name identity with an error/poison type or explicitly invalid
signature state. Do not select poisoned candidates as ordinary overloads, infer
fabricated body bindings, or run normal duplicate/override comparisons against
corrupted shapes. Distinguish omitted returns from failed explicit returns.
Test unknown first/middle/last slots, optional/out flags, unknown returns, overloads,
overrides and cross-file provider restoration. This improves analysis reliability;
it is not support for unknown types.

### 2. Implement source `skip` parameters end to end

A single blocker in `core/math/operators.script` excludes **209 operators**.
21,814 unresolved-function diagnostics name operators. The non-lazy retention
experiment alone removes 14,872 operator reports.

Carry short-circuit intent through source qualifiers, semantic parameters, bundle
loading/emission and calls. The backend already emits `Skip` for short-circuit
bundle parameter flags, but the source bridge rejects it and shared source flags
do not carry it. Preserve RHS laziness, including prefixes and nested expressions;
do not merely remove the bridge rejection. Official probes confirm lazy operator
call bytecode and flag byte `04` for a scripted skipped Bool parameter. General
scripted-skip runtime behavior still needs characterization.

Implementation evidence (2026-11-01, working tree based on `d46faac`):

- Shared AST/parser/formatter now retain `skip`; semantic `ParamFlags` carry
  `is_short_circuit` through source resolution, bundle loading and parameter emission.
  The native bridge maps `skip` rather than excluding the operator provider.
  Decompiler/community and native source emission retain the qualifier too.
- Authored community bytecode snapshot `bytecode__source_short_circuit.snap` covers
  false/true LHS for both Bool operators, nested chains, LHS-before-RHS effects and
  explicit scripted calls. Side-effect calls appear once, inside RHS `Skip` spans.
- Native backend regression serializes/reloads declarations and calls, checks lazy
  flags (including byte `04`) and exact imported names `OperatorLogicAnd;BoolSkipBool;Bool` and
  `OperatorLogicOr;BoolSkipBool;Bool`, then compiles another community caller
  against the reloaded imported and scripted declarations. No `Skip` text is lost
    from mangled params.
- Fail closed: array-literal statement prefixes inside lazy arguments are rejected
  with `UNSAFE_ARRAY_EVALUATION`, including explicit calls and mixed-source calls.
  Overloaded/typed lazy calls conservatively reject pending prefixes. Intrinsics
  carrying skip flags are assembly errors rather than silently eager instructions.

Focused gates passed: backend bytecode tests; frontend `lowering_inference` tests;
community parser and formatter qualifier tests; native bridge/operator/optional-slot
suites and the native lazy backend test; standalone backend/adapter checks; affected
package all-target Clippy (`-D warnings`); pinned nightly format check. See local
`target/parity-investigation/blockers.md` for commands and limits. These are bytecode
and metadata checks, **not game-runtime execution**. Arbitrary scripted skip runtime
semantics, unusual qualifier combinations and non-Bool engine lazy dispatch remain
uncharacterized. The integration survey above now admits the operator module,
but does not establish a parity percentage or that all 209 declarations compile.

### 3. Restore foundational conversion-provider declarations

Nine excluded files contain 127 conversions (52 implicit, 75 explicit), plus
EntityID/PersistentID/StatsObjectID, BlackboardID types, ScriptExecutionContext, AI
outcomes, equipment and vehicle APIs. These are mostly *excluded source providers*,
not necessarily missing engine data. All 882 override-without-base rows cross an
excluded source ancestor chain by textual audit.

Separate registration from selection. Preserve implicit/explicit identity and
source signature even when selection is not implemented; do not make all casts
ordinary eligible free functions. The declared numeric relation is not a universal
widening ladder. Official scripted custom-conversion uses failed in several
corpus-shaped authored fixtures despite declaration acceptance. Imported primitive
conversions, scripted conversions, value/ref/const categories and expected-type
contexts need their own differential matrix before enabling selection.

Also inventory genuinely absent engine types: NodeRef has 129 unresolved-type
reports and no class/struct/enum declaration found in the installed script tree.
Record an explicit dependency mode and model intrinsic engine types from verified
evidence. Do not fabricate definitions or hide remaining source-provider gaps by
changing dependency inputs without labeling the comparison.

### 4. Add native const-local semantics, not single-assignment bindings

The 53 blocked files contain 103 const locals, all without inline initializers.
Official probes permit repeated assignment to const object/weak handles, but
reject field mutation and non-const receiver methods. Ordinary and out forwarding
were accepted in those local-handle fixtures. Scalar/struct assignment is rejected.

This differs from both immutable bindings and the current const-parameter pass.
Represent const referent provenance independently from rebindable handle storage;
cover casts, weak conversions, const methods and captures. Keep strict community
rules and explicit native relaxations where semantics differ. Do not erase const
or simply reuse parameter root-write rejection. Track newly analyzed bodies, not
only diagnostic reduction.

#### Shared implementation checkpoint (Step 4 const locals)

Community syntax now uses `const name`, and the native bridge retains `const var`
instead of excluding its file. Local IR metadata preserves the qualifier through
hoisting/lowering and bundle emission. The shared const validator distinguishes
parameter roots, local referent constness and rebindable handle storage; the
file-local REDmod policy permits repeated object/weak-handle rebinding and ordinary/
out handle forwarding. Strict community defaults retain root-write and forwarding
checks. Scalar/struct explicit assignment (including inline initialization), array
mutation, field writes and selected non-const receiver methods remain rejected.
Const method identity is retained in shared method flags and bundle loading/emission.

Focused tests cover weak conversions, casts, captures, native hoisting, strict/native
policy isolation in both module orders and existing parameter behavior. This is
selected-method validation, not const-aware overload ranking or transitive alias/
effect analysis. The 103-local/53-file baseline above has not been remeasured;
newly admitted corpus bodies and runtime serialization effects remain unmeasured.
See `const-parameters.md` for exact semantics and limits.

### 5. Correct event return contracts

The adapter currently forces events to Void. Official events accept `return true`
and reject bare `return` as Void-to-Bool, indicating an implicit Bool contract.
An integer return also compiles; its exact conversion path needs characterization,
not general numeric truthiness. Empty-body fallthrough behavior needs bytecode
inspection before implementation.

207 current Bool/Void errors occur in event bodies. Another 176 occur in functions
returning a missing AI outcome type and must not be conflated with events.
Update callback signatures, base compatibility, patches, calls and backend metadata
coherently. Existing fixtures asserting Bool event returns are invalid need revision.

### 6. Fix overload sets and generated forwarder identity

Confirmed independent official differences:

- **Specificity:** a child-typed argument chooses the child-parameter overload;
  current shared selection can report ambiguity. Eleven SetData rows are a likely
  affected set. Use a unique undominated applicable signature, with explicit tests
  for crossing preferences, null, optional holes, ref/weak/out/Variant and numeric
  conversions; avoid an unverified scalar scoring scheme.
- **Forwarders:** real instance methods beat generated receiver-first static
  forwarders. Current analysis produces duplicate/ambiguous methods (29 ambiguity
  rows) and five false missing-override reports. Treat forwarders as fallback
  aliases, not authored override declarations; retain genuine missing-override checks.
- **Inherited static overloads:** subclass name buckets must not hide applicable
  differently typed base overloads. Collect candidates across ancestry with
  per-signature hiding, instead of stopping at the first matching name.

Official optional-tail overlap is genuinely ambiguous. Do **not** implement a
blanket exact-arity/fewest-defaults preference or return-type-only overload selection.

#### Shared implementation checkpoint (Step 5 overload work)

The shared resolver now selects a unique undominated applicable reference-parameter
signature, without scalar scores, return hints, or speculative inference mutations.
Ranking is deliberately bounded: equal parameter counts/flags, concrete nongeneric
class parameters with matching strong-reference representation (or bare class
parameters). Every differing parameter must narrow along the declared ancestry;
crossing preferences remain ambiguous. Differing omitted/null slots, out parameters,
weak/script-reference categories, Variant and numeric conversions do not establish
specificity. These are fail-closed boundaries, not claims that official REDmod
rejects every such case. Existing applicability/coercion behavior is unchanged.
Optional-tail overlap and return-only duplicates remain ambiguous/invalid.

Generated static forwarders remain queryable aliases but do not define override
slots or authored override obligations. Real instance methods suppress aliases with
the same effective parameter types, including across ancestry and independent of
declaration order; other alias overloads remain available. Bodyless imported static
declarations also receive aliases. Forwarded receivers are evaluated once, first,
and checked using the original receiver parameter's out/ref rules.

Qualified static calls gather the entire ancestry, with nearer declarations hiding
only identical parameter signatures. Instance methods do not hide base statics;
same-class duplicates are retained, and selected targets still undergo visibility
checks. The inherited-static warning policy has not changed. Generic signature
normalization and category/conversion preferences beyond the probes remain follow-up
work, rather than inferred numeric or default-count rankings.

Regression tests inspect shared and native-source IR targets plus serialized bytecode
for child overload selection, real-method precedence and receiver-first alias calls.
They cover holes/null, out/ref/weak, Variant, declaration order, optional overlap,
return-only duplicates, static ancestry/hiding/access, and genuine override errors.
These tests do not establish runtime or full-corpus semantic parity.

### 7. Correct inherited return and access rules

- **Override return compatibility:** 412 of 414 return mismatch rows have
  source-level covariance evidence (including two weak returns); two widen back
  to an original ancestor slot return. Official probes accept both shapes, but
  reject direct two-level widening and unrelated returns. Validate against a
  canonical original slot contract, not just the nearest override; confirm the
  exact rule with sibling/deep-return probes. Preserve source-visible call types.
- **Persistent fields:** all 148 reports trace to inherited protected persistent
  fields. Official accepts declaring-class and descendant access, including through
  base-typed receivers, but rejects unrelated external access even when written
  public. Admit descendants while retaining ordinary private restrictions and
  external-access checks; do not remove persistent validation wholesale.
- **Override visibility:** 270 reports arise from native-accepted narrowed
  visibility. Make declaration validation source-policy aware while preserving
  call-site visibility and implicit visibility inheritance.

Implementation checkpoint (2026-11-01): shared symbols expose the original
`override_slot` by following immediate predecessor links, and instantiate return
contracts through generic bases. Community declarations enforce nearest-override
covariance; REDmod validates the original slot, including sibling returns and
widened-back root returns confirmed by the official follow-up in
`target/parity-investigation/override-slot-followup/summary.json`. Covariance is
restricted to the same strong/weak class-reference representation; arrays,
structs, script references and representation changes remain exact-only.
Source-visible returns and emitted return metadata remain declaration-specific.
Native-slot mangling follows the root even through multiple scripted overrides.
Per-file policy permits native visibility narrowing and suppresses inherited
static style warnings without changing call-site accessibility. Persistent
access admits declaring-class descendants while retaining external/private checks.

These require native/community mixed-source tests and dispatch/metadata checks,
not only disappearing diagnostics. Out-flag changes on overrides were accepted
in one official direction but have zero baseline reports; characterize ABI and the
reverse direction separately before relaxing that check.

### 8. Select Variant extraction before object casts

104 impossible-cast rows use Variant sources. The class-target fast path currently
tries object downcasting before considering Variant extraction. Existing FromVariant
IR/backend support should be selected instead. Official object, weak and struct
extraction fixtures compile. Preserve target reference representation and single
source evaluation; test nested extraction/downcasts and reject unrelated-object
casts. Fix zero-length diagnostic spans from synthetic Cast callee nodes too.

### 9. Finish bounded remaining capability gaps

- Scalar mutable Float field: one file. Matched official bytes support no-extra-flag
  normalization for the observed instanceeditable/inlined Float form; explicit const
  remains effective. Broader scalar types/targets need evidence.
- Non-last switch default: one file. Actual corpus bodies terminate with break,
  but official also permits fallthrough. Prefer ordered label representation; only
  normalize the restricted break-terminated case if its evaluation equivalence is
  proven. Preserve loop continue targets and one scrutinee evaluation.
- Index operator: one utility file. Official dispatches `operator[]` as OperatorArray
  with an out receiver and String key; its Bool result is not assignable. Add an
  overloaded-call path without changing built-in array place semantics.

### 10. Add explicit partial-analysis and coverage reporting

Coverage observation is implemented in the runner (current counters above).
**Partial declaration harvesting remains deferred and is not implemented.**

Retain supported siblings and trustworthy signatures when a body is unavailable,
but make this an explicit analysis-only contract. Unsupported signatures must not
be silently approximated (especially lazy parameters/conversions), and unavailable
bodies must not become synthetic empty/native implementations. Preserve diagnostic
ownership, incompleteness, source identity and query provenance. Strict compilation
must still reject unsupported input and must never emit artifacts from partial units.

The runner observes expression queries and retained bodies; resolved call/field/
override identity agreement, complete poison-dependency tracking and body-check
completeness remain future measurements. Record nonzero correct query spans and test
IDE navigation independently. Once the preceding changes land, rerun the corpus
with unchanged inputs, inspect residual root causes and establish a stratified
positive/negative official differential suite. That is the gate for a 90–99%
static-analysis target; current data does not justify promising that percentage.

## Noise and correctness are separate

2,244 inherited-static reports are warnings, not semantic failures; native official
fixtures accept the calls. Suppress or reword them through an intentional warning
policy after fixing lookup itself. The 17,076 unused-local and 6,044 unused-qualifier
warnings do not indicate inference failure. Native struct-construction diagnostics
(869) need dependency/layout-aware triage after provider restoration; they should
not be disabled merely to reach a numerical target.

## Evidence and validation

Detailed local reports: `target/parity-investigation/{blockers,overloads,inheritance}.md`.
They retain authored fixtures, official compiler outputs and source-site
classifications. No installed game sources or generated bundles belong in committed
fixtures. Official acceptance/bytecode evidence is not game-runtime validation.

Initial ordinary-target builds encountered stale AST artifacts (18 missing API
errors). A fresh build succeeded with:

```sh
cargo build -p redscript-redmod-adapter --locked --example semantic_corpus --target-dir target/parity-current-build
```

Workspace/all-target cargo check also passed. The newly built survey reproduced
nine authored reductions (signature loss/control, event return, specificity,
forwarder collision, inherited static lookup, optional ambiguity, duplicate return
signatures and covariant return) under `target/parity-investigation/current-reductions/`.
This supersedes the local-binary build caveat in the earlier overload report; its
full-corpus numeric classifications remain checkpoint-based and non-additive.
No production changes, commits, or runtime tests were made by the original
investigation. The current corpus checkpoint adds documentation only; it does not
validate all concurrent implementation/test work.

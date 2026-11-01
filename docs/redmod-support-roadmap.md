# REDmod support roadmap

## Scope and priorities

Native parsing/formatting, partial semantic analysis, mixed-language IDE analysis,
and strict community REDscript → REDmod export exist. Full semantic parity and
runtime compatibility do not.

The first-blocker, native-reference, compatibility, and end-to-end field-default
batch is implemented. The numbered workstream below records its design and
acceptance gates; the status table distinguishes delivered support from deferred
semantics. Accepting more syntax must not silently erase behavior or weaken
community-language checks.

- [Semantic adapter contracts and rejection list](../crates/compiler/redmod/adapter/README.md)
- [Transpiler contracts and limitations](../crates/compiler/redmod/transpiler/README.md)
- [Local parser corpus check](../crates/syntax/redmod/parser/CORPUS.md)
- [Shared expression/default APIs and semantics](shared-expression-defaults.md)
- [Const parameter checks and REDmod relaxations](const-parameters.md)
- [Contribution and validation requirements](../CONTRIBUTING.md)

## Corpus checkpoints

The 2026-10-30 local semantic run analyzed all 2,112 installed files under
`tools/redmod/scripts`, including one capital-S `.Script` file. It used the boot
library and default symbols, **not a game bundle**. Files with parsing, capability,
or pre-resolution validation errors were excluded before shared processing.

| Stage | Result |
| --- | ---: |
| Native syntax diagnostics | 0 |
| Files excluded by bridge capability checks | 1,154 |
| Additional files excluded by unsupported types | 157 |
| Modules reaching semantic processing | 801 |
| Adapter processing errors | 588 |
| Shared errors / warnings | 13,769 / 485 |

Capability checks report only the first blocker in each file. The largest first
blockers were default directives (592 files), `export` (238), `virtual` (133),
`constexpr` (50), enum storage (41), and const locals (33). Another 157 converted
files failed unsupported-type checks, including explicit `ref<T>`.

Of the shared diagnostics, 12,644 concerned unresolved types, references,
functions, or members. Excluded declarations and missing engine definitions cause
substantial cascades. All 588 adapter processing errors were missing override
bases; they are not independently established override bugs. Diagnostic-free
files and returned IR must not be counted as semantic passes.

The original disposable probe omitted unused-local analysis. The repeatable
harness reproduces its 485 warnings plus 964 `UNUSED_LOCAL` warnings (1,449 total).

The historical integrated run at original commit `6b53cb2` on 2026-10-31 used
the same corpus and boot-only inputs, including the unused-local pass. This was
**before the shared-default rewrite**; that revision is preserved on
`backup-feat-redmod`. These historical counts are not current-HEAD results;
see the integration working-tree rerun below:

| Stage | Integrated result |
| --- | ---: |
| Native syntax diagnostics | 0 |
| Files excluded by bridge capability checks | 66 |
| Additional files excluded by validation | 0 |
| Modules reaching semantic processing | 2,046 |
| Adapter processing errors | 1,456 |
| Shared errors / warnings | 47,982 / 25,364 |

More analyzed bodies expose more errors, including cascades from excluded
operator declarations and missing engine definitions. These are processing counts,
not semantic passes or runtime compatibility claims. That historical combined run
completed following the implicit `IScriptable` self-base/ancestor-cycle fix.

See the [repeatable corpus survey](../crates/compiler/redmod/adapter/CORPUS.md)
for commands, instrumentation and residual groups. Final reports are under
`target/redmod-semantic-corpus-final/`; the original probe was under
`target/redmod-corpus-review/`. These are disposable, untracked artifacts, not
portable test dependencies. Reduce findings to authored regression fixtures rather
than committing installed game scripts.

### Cleanup refresh (2026-11-01)

This working-tree refresh **supersedes the previous integration rerun below**;
its historical counts remain for comparison. No final revision is assigned while
history is being arranged. The release survey read the same installed **2,112
files / 21,873,807 bytes** without modification, with boot/default symbols and no
bundle. All files reach processing; exclusions and adapter errors remain zero.

Shared totals remain **12,655 errors / 3,705 warnings / 0 pedantic** (16,360
rows), with errors in 1,172 files and no reported errors in 940, not semantic
passes. Source-aware cast fallback reclassifies 33 errors: unsupported conversions
rise from **4,925 to 4,958** (645 to 651 files), unresolved functions fall from
**838 to 805** (289 to 276 files), and unresolved `Cast` calls fall from **761 to
728**. Other error groups and all coverage counters below are unchanged. This
is diagnostic attribution, not implemented conversion dispatch.

Reports in `target/redmod-semantic-corpus-cleanup/` replayed to
`target/redmod-semantic-corpus-cleanup-replay/` with all seven files byte-identical.
Coverage partitions and 781,341 query rows reconcile. Five release example tests
passed. Full contribution validation passed: default/all-feature workspace tests
(**738 passed, 0 failed, 9 ignored**, 117 targets each; previously 728 passed),
both all-targets Clippy configurations with `-D warnings`, pinned
`nightly-2026-10-15` fmt, release workspace build, all-feature no-dependency docs,
and independent checks of all nine affected packages.

Validation used stable Rust, `--locked -j2`, `target/parity-integration`, DEV/TEST
debug zero and incremental off; release debug was also zero. Only the generated
validation debug directory was removed for space, preserving release/docs and
official-event evidence. A combined corpus build/test command timed out during
test compilation; the release example had built, and the separate test retry
passed. No source/test fixes were required. The existing `chumsky` advisory remains.

The cleanup retains source-aware lazy guards/native cast operands, community
`const` syntax round-tripping and const-local entry defaults, and removes the
special Int32-to-Bool event-return coercion. See
[lazy evaluation limitations](lazy-evaluation-limitations.md) for conservative
source-free typed-call restrictions and unsupported expression-local prefixes.
Conversion dispatch, partial declaration harvesting, complete dependencies/layout
and official binding/runtime equivalence remain open; coverage is not accuracy.

### Previous integration working-tree rerun (2026-11-01)

The dirty integration tree based on `d46faac` was rebuilt as a stable release
`semantic_corpus` example using the existing `target/parity-integration` directory,
`--locked -j2`, debug info zero and incremental compilation off. It read the same
installed **2,112 files / 21,873,807 bytes** without modifications, using embedded
boot/default symbols only and no game bundle.

| Counter | Snapshot baseline | Working-tree rerun |
| --- | ---: | ---: |
| Processing-eligible files | 2,046 | 2,112 |
| Capability-excluded files | 66 | 0 |
| Syntax / validation / missing-module files | 0 / 0 / 0 | 0 / 0 / 0 |
| Adapter processing errors | 1,456 | 0 |
| Shared errors (including runtime-allowed) | 47,982 | 12,655 |
| Shared warnings / pedantic | 25,364 / 0 | 3,705 / 0 |

Errors affect 1,172 files; the other 940 are not semantic passes. Total diagnostics:
16,360. Coverage records 96,126 declarations, 53,875 functions (53,819 valid-signature
flags, 56 invalid, zero mixed/unavailable), and 38,824 retained of 38,861 explicit
bodies (37 unavailable). Of 1,144,329 distinct written expression spans, 781,341
map exactly to visited non-Nop IR and 362,988 do not. Queries resolve at 778,780,
are unresolved at 2,561 and fail at zero mapped spans; 38 have mixed observations.
No baseline body/query coverage was measured.

Dominant residuals: 4,925 fail-closed conversion-dispatch errors, 4,921 invalid-
signature guards (4,822 name `OperatorEqual`), 1,008 incomplete native-struct
constructor errors, 838 unresolved functions (761 `Cast`), 298 type errors and
163 overload ambiguities. Missing types total 162, including NodeRef (140) and
CRUID (15). Other errors: 140 missing members, 118 arity, 73 missing references,
five const-local writes and four invalid temporaries. Warnings are 3,117 unused
locals and 588 unused qualifiers. Missing dependency/layout information and recovery
cascades remain distinct from independently established semantic bugs.

All seven reports under `target/redmod-semantic-corpus-parity/` replayed byte-for-
byte to `target/redmod-semantic-corpus-parity-replay/`. Five release example tests
passed and coverage partitions reconcile. Runner inspection found no concrete
measurement bug, so no instrumentation was changed. This is not a workspace test
certification; concurrent implementation/test work has separate gates.

Implemented in this measured tree: signature-poison guards, source lazy `skip`,
conversion registration, const-local checks, event/overload/override fixes,
Variant extraction ordering, ordered switch defaults, overloaded indexing and
bounded scalar mutable. **Conversion dispatch stays fail closed; partial
declaration harvesting is not implemented.** Coverage reporting does not enable
partial analysis. General conversion selection/emission, complete dependencies/
layout, residual semantic triage and official binding/dispatch comparisons remain
deferred. Historical delivery tables below describe earlier commits, not the
status of these uncommitted integration changes.

Retained bodies include recovery IR; signature flags and returned types do not
establish correctness. Expression counts exclude placeholders/boot but include
field/default values; mapping misses folded/callee/default sites and cannot
separate generated nodes reusing written spans. Unresolved queries here are
UnknownNullType observations; zero failures applies only to mapped spans. Neither
processing eligibility nor query availability is a type-accuracy percentage or
runtime claim. See [CORPUS.md](../crates/compiler/redmod/adapter/CORPUS.md) and the
[semantic investigation](redmod-semantic-parity.md) for full counters and limits.

## Architecture constraints

- Keep native nodes in `redscript-redmod-ast`, grammar in
  `redscript-redmod-parser`, and printing/formatting in
  `redscript-redmod-formatter`. Parsing retains unsupported constructs.
- Keep native mappings, capability checks, and native-to-shared conversion in
  `redscript-redmod-adapter`. Shared frontend extensions must be language-neutral,
  using shared AST/default features for common semantics and narrow per-source
  policies only for dialect differences.
- Aggregate defaults use shared `Item::Default` / `FieldDefault` nodes in both
  languages, not private adapter plans or supplemental frontend inputs. Preserve
  the remaining native loop-fixup plans in the complete `ParsedModule`, including
  cloned IDE snapshots. Bare-AST APIs fail closed for those loop constructs, not
  for shared aggregate defaults.
- Preserve evaluation count/order, lazy evaluation, writable places, and written
  spans. Synthetic provenance must not hide source children from IDE queries.
- Carry resolved semantic identities after binding. Spans, source spellings,
  pointers, and overload positions are not permanent declaration identities.
- Diagnose unsupported semantics explicitly. Do not approximate reference
  wrappers, skipped arguments, mutation rules, or initialization by discarding
  syntax or substituting values.
- Use typed diagnostics, complete diagnostic/output snapshots, and direct
  assertions for identity, provenance, order, flags, and serialized properties.

## Implementation workstream

Keep independent fixes in separate Conventional Commits. Each commit must build
and include its API initializer/call-site updates, regression tests, and relevant
contract documentation. Shared prerequisites may need separate commits before
adapter integration.

### Delivery status (2026-10-31)

| Step | Status and implementation |
| --- | --- |
| 1. Repeatable corpus run | Harness delivered in `6e2cf7c`, including case-insensitive `.script` discovery. IDE discovery is tracked separately in the IDE repository. |
| 2–4. Function modifiers | `e1b8390`: verified analysis-only `export`/`constexpr`; checked virtual/private/bodyless method rules. No engine-exposure or compile-time-evaluation claim. |
| 5. Enum storage | `fdd0c7b`, `27983d4`: both-language syntax, range checks, width preservation and bundle regressions. Signedness is erased in bundles; omitted native storage still differs from the official four-byte default. |
| 6. References and const checks | `be40fa2`: shared compositional script-reference coercion and receiver unwrapping; strict const-parameter checks by default, with explicit REDmod element-write/non-out-forwarding relaxations. Native `ref<T>` maps to script references, not community strong references; ref locals/returns, nested refs and arrays of refs remain rejected. Only reference write-through is dialect-specific. |
| 7. Operators/conversions | `2d16262`: checked operator registration and arity. Explicit conversions are retained/queryable but selection fails closed; implicit conversions and index operators remain unsupported. |
| 8. Omitted slots | `2d16262`: shared signature-checked positional holes emit `Nop`, not null. Community trailing commas add no slot; native trailing commas retain final holes. Lazy `skip` remains separate. |
| 9. Other first blockers | `7f5513f`: shared assignments/chains yield the coerced destination type, not Void, with one place evaluation and no readback; unsafe array-prefix lifting fails closed. `2d0fa87`: profile properties, const returns and Profile IO correction; `0d99929`: verified mutable array fields. Const locals, scalar mutable, and broader metadata remain deferred. |
| 10. Method/type collisions | `9204818`: implicit methods precede types; community lookup unchanged. |
| 11. Native field visibility | `24ef415`: omitted native field visibility is public, with explicit flags unchanged. |
| 12. Imported callbacks | `39b4178`: shared support for bodyless callbacks on native, non-import-only aggregates, without invented bodies or native method flags; other missing-body checks remain. |
| 13. Integration checkpoints | `6e2cf7c` supplies the harness; `d06f200` fixes the self-base/ancestor-cycle hang. Recorded corpus/validation results below are historical, not reruns on rewritten commits. |
| 14. Defaults end to end | `a0a8f48`: shared AST/parser/formatter support, typed own/inherited target resolution and merged serialized properties, with backend round-trip regressions. No private default plans or supplemental-input API. |

Historical validation recorded at original `6b53cb2` (preserved on
`backup-feat-redmod`, before the shared-default rewrite) included independent
workspace/all-target `cargo check` for implementation commits, default/all-feature
workspace tests and all-target strict Clippy, pinned nightly formatting, release
build, generated docs, and a standalone adapter check. This records the original
validation only; it does not certify the rewritten trees or claim a corpus rerun.

### Historical original implementation steps and gates

The following list and step-detail sections retain the original design, including
superseded adapter-plan/supplemental-input proposals. They are historical, not the
current API contract. Use the status table and shared API guide
above for completion, rather than interpreting the original imperative wording as
unfinished work. Characterization may run in parallel, but capabilities must remain
gated until their representation and consumers are ready.

1. **Make the semantic corpus run repeatable.** Add an optional harness with
   explicit corpus/dependency inputs, deterministic discovery, per-stage counts,
   grouped typed diagnostics, and minimal-reproduction links. Include `.Script`
   discovery consistently in the IDE and harness. Keep installed sources out of CI
   and committed fixtures. Record the baseline before changing acceptance rules.

2. **Accept `export` on verified function targets.** Treat it as an analysis-only
   no-op if characterization supports that decision. Cover free functions,
   instance/static methods, overrides, and visibility. Reject unsupported targets;
   do not claim equivalent engine exposure or bytecode. See modifier details below.

3. **Support `virtual`.** Reuse existing non-final dispatch where equivalent, but
   establish private override and bodyless-declaration rules. Check conflicts with
   `final`, `static`, and `override`; do not simply discard virtual intent where it
   changes legality. See modifier details below.

4. **Handle `constexpr` deliberately.** Compare ordinary calls, declaration/body
   restrictions, overrides, and constant-context uses with official fixtures.
   Ignore it for ordinary analysis only where verified equivalent. Diagnose any
   unsupported compile-time evaluation explicitly instead of pretending to fold it.

5. **Support explicit enum storage in both languages.** Add optional community
   syntax alongside native syntax and a shared storage model with unchanged
   omitted-storage behavior. Carry widths/signedness through resolution, bundle
   loading, backend emission, formatting, queries, and export. Verify legal storage
   types and representation; test boundaries, implicit increment overflow, and
   serialize/reload. The existing IO size byte alone does not settle signedness.

6. **Support native `ref<T>`.** This is native REDmod syntax, not a community-only
   feature. Replace the blanket rejection with a mapping that preserves native
   reference/value semantics. Characterize class, value, and nested reference
   forms; parameters/returns/locals/fields; `weak<T>` interaction; null, coercions,
   mutation, and overload identity. Reuse a shared representation only when its
   semantics match, extending language-neutral type/lowering support otherwise.
   Cover IDE type/definition queries, mixed dependencies, and backend round trips.
   Do not preserve the rejection merely because community `ref<T>` differs.

7. **Register operator and conversion declarations.** Add checked signatures and
   native declaration registration using shared resolution where appropriate.
   Validate operator arity/type rules and conversion selection, keeping value
   conversions distinct from object downcasts. This should reduce secondary
   missing-operator/cast diagnostics as more native declarations become available.

8. **Resolve omitted argument slots.** Preserve holes through overload resolution,
   validate each against the actual optional parameter, and retain positional
   identity. Do not substitute `null`. Treat `skip` as a separate lazy-call lowering
   requirement, not an optionality flag.

9. **Address the remaining first-blocker families explicitly.**
   - **Metadata:** inventory nonstring/nonfield forms; support verified targets and
     value types without losing keys, duplicates, order, or provenance.
   - **Assignment values/chains:** implement typed result/place lowering with one
     evaluation of receivers and RHS, including nested expressions and lazy sites.
   - **Const returns:** determine type identity, mutation, and metadata effects
     before mapping them.
   - **Const locals:** defer readonly support for now. Keep an explicit limitation;
     do not silently lower them as mutable in strict processing. Any future partial
     analysis mode must be clearly distinct from target-valid acceptance.
   - **`mutable`:** characterize mutation/editor/serialization effects alongside
     const semantics before deciding whether it can be ignored.
   Rerun capability discovery: the first failure per file hides subsequent forms,
   including initialized groups and other currently documented gaps. A deferred
   category remains a blocker, not a completed capability.

10. **Prefer implicit methods over same-named types.** Fix native lookup ordering
    with shadowing and constructor fallback coverage; leave community lookup
    unchanged. Details below.

11. **Apply native omitted-field visibility defaults.** Establish rules for
    scripted/imported/importonly declarations without weakening explicit visibility.
    Details below.

12. **Support imported event prototypes.** Preserve verified callback/prototype
    flags without synthetic bodies or blanket native-member inference. Details below.

13. **Run a pre-defaults corpus checkpoint.** Compare accepted modules, first
    blockers, and dependency cascades with the baseline. Inventory residual
    unsupported-type diagnostics separately from unknown symbols. Explicit
    `ref<T>` must now be supported for verified forms; custom generics, function
    types, or mixed-reference forms need their own decisions rather than a blanket
    unsupported-type bucket. Run full contribution checks before defaults work.

14. **Implement default directives end to end.** Follow the characterization,
    retention, resolution, emission, and regression stages below. Include own and
    inherited fields, structs, duplicates, and imported defaults. Rerun the corpus
    and contribution checks after integration.

### Step 10 details: implicit method/type name collisions

**Suggested commit:** `fix(redmod): prefer implicit methods over types`

Before `9204818`, constructor lookup preceded implicit-member lookup. Installed sources
call `SignalEvent(...)` as a method even when a same-named type exists; a tiny
self-contained probe reproduces invalid construction, while
`this.SignalEvent(...)` succeeds.

For native inputs, establish lookup precedence as:

1. Local or parameter callable value.
2. Eligible implicit method.
3. Type construction.
4. Free function.

Keep community lookup unchanged. Cover instance/static/inherited methods,
local and hoisted-local shadowing, type collisions, constructor fallback, free
functions, visibility, and explicit-receiver controls. Characterize ambiguous
cases with the official compiler before broadening the rule beyond these cases.
Official probes found one type-versus-local edge where the type wins; the current
local-first behavior is an intentional remaining difference, not full parity.

### Step 11 details: omitted field visibility

**Suggested commit:** `fix(redmod): apply native field visibility defaults`

Before `24ef415`, omitted visibility on imported fields became shared private visibility,
but installed native sources access these fields externally. The mismatch also
reproduces with an isolated imported class and consumer.

First characterize omitted visibility across scripted, `import`, and `importonly`
classes; imported versus scripted fields; structs; and own, inherited, and
unrelated-class access. Explicit public/protected/private visibility is a control.

Implement only the verified rule in the adapter or a narrow resolution policy.
Preserve explicit visibility and implicit-versus-written provenance where relevant.
Do not make every native field public merely to silence diagnostics.

### Step 12 details: imported event prototypes

**Suggested commit:** `fix(redmod): support imported event prototypes`

Before `39b4178`, bodyless protected events in imported classes received `MISSING_BODY`.
An isolated `import class NativeController { protected event OnMounted(); }`
reproduces this without missing dependencies.

Characterize `import`/`importonly` event prototypes, scripted subclass
implementations, and body-bearing declarations. Map prototypes to their verified
resolved flags and callback identity, preserving Void return semantics.

Do not synthesize empty bodies or make all members of imported classes native.
Keep missing-body checks for invalid scripted declarations. Test subclass
compatibility and serialized callback/native/unimplemented flags, not just the
absence of diagnostics.

### Step 2 details: export modifier

**Suggested commit:** `fix(redmod): accept export function modifiers`

`export` is a candidate for an analysis-only no-op, not an unconditional discard.
Compare official fixtures with and without it on free functions, instance/static
methods, overrides, and different visibility. Inspect flags where decoding permits.

If resolution is unaffected, accept it only on verified declaration kinds without
inventing shared flags. Document the distinction between analysis acceptance and
equivalent engine exposure or emitted bytecode. Continue rejecting unverified
modifier targets such as fields.

### Step 3 details: virtual methods

**Suggested commit:** `fix(redmod): support virtual methods`

Ordinary non-final shared methods may already provide the necessary dispatch, but
the corpus also contains private virtual methods and bodyless virtual declarations.
Blindly dropping the modifier can leave override eligibility or body legality wrong.

Characterize ordinary versus virtual methods, private overrides, imported/scripted
bodyless declarations, and conflicts with `final`, `static`, and `override`.
Map onto existing flags where justified; retain explicit virtual intent in adapter
metadata if needed. Revisit the override search's exclusion of private base methods
only when supported by those results.

## Step 14: default directives end to end

**Delivered in `a0a8f48` as shared aggregate defaults.** Both languages parse and
format `default field = value;` as shared `Item::Default` / `FieldDefault` nodes.
The frontend consumes those AST members directly after inheritance resolution,
resolves targets to `FieldId`s, and the backend merges properties on declaring
fields using supplying-class keys. No private default plans, supplemental frontend
inputs, or initialization bytecode are involved. Bare-AST defaults retain their
semantics; native loop fixups still require metadata-aware processing.

Sections A–E below preserve the **historical design gates**, including the
superseded plan/supplemental-input proposal, not instructions for current APIs.
See the [shared guide](shared-expression-defaults.md) for the delivered contract.

Official bounded representation probes established own-field duplicate first-wins
and inherited duplicate last-wins per supplying class. Existing bundle, base and
sibling entries are preserved. The shared frontend deliberately checks types more
strictly than official inherited defaults that accept some invalid text. Contiguous numeric
spellings are retained; parenthesized/comment-separated forms fall back to typed
formatting. Tests cover these distinctions without requiring a game installation.

Defaults are class-qualified field metadata, not constructor assignments. Support
own and inherited targets together; attaching directives only to same-class field
initializers would miss a substantial part of the corpus and risk changing meaning.

A structural scan of the installed corpus found 2,729 directives: approximately
612 inherited-field targets and 274 directives in structs. Observed expressions
were numeric/Bool/String/CName/TweakDBID/ResRef literals and enum members. Negative
floats and duplicate defaults occur. These are source-scan observations, not
verified constant-expression or duplicate-precedence rules.

### A. Characterize acceptance and representation

Use authored official-compiler fixtures to establish:

- Own, inherited, shadowed, and imported-base field targets; multi-level ancestry.
- Struct defaults, directives before field declarations, and invalid targets.
- Duplicate directives and conflicts with inline initializers, including ordering.
- Base/derived/sibling defaults and inheritance without a new override.
- Numeric widths, coercions, signed minima, negative floats, and negative zero.
- Literal/enum support and explicit rejection or support of richer expressions.
- Default-target access rules, independently of ordinary expression visibility.
- Exact property placement, supplying-class keys, ordering, and value encoding.

Official compilation establishes acceptance, decoded definitions establish
representation, and runtime construction establishes behavior. Do not conflate
them. Verify reader compatibility with official output before relying on decoded
results; use a bounded inspection tool if the full reader cannot handle it.

### B. Retain directive plans

Add an adapter-owned default plan to `ParsedModule`, retaining the enclosing
aggregate descriptor, target/expression spans, directive span, source order, and
duplicates. Bind descriptors to accepted declarations and carry resolved identities
thereafter. Do not synthesize fields or overwrite base declaration initializers.

Update parsing/processing APIs and IDE snapshot handling together. Bare-AST callers
must not silently lose defaults.

### C. Resolve and type-check after inheritance resolution

Introduce narrow, language-neutral supplemental field-default inputs to the shared
frontend. Resolve targets after the full inheritance graph is available, while the
correct type/function environment is retained. Keep these identities distinct:

```text
Supplying aggregate: Derived
Declaring field: Base.value
Value: typed constant
```

Reuse hierarchy/type-substitution logic where practical. Lower against the resolved
field type with precise target/expression diagnostics. Start with verified literal
families and enum members, not a general constant evaluator. Existing constant
lowering does not generally fold expressions; negative floats need an explicit,
bounded solution without changing ordinary operator dispatch accidentally.

### D. Extend lowered defaults and backend emission

Represent inherited defaults with resolved `FieldId`s and supplying aggregate
identity; preserve ordering/duplicates until verified precedence is applied.

Finalize properties after declaring fields exist. Merge into the declaring field's
default list without deleting other supplying classes' entries or existing bundle
properties. Coordinate own-field, added-field, and inherited-default emission so
later singleton writes cannot erase prior entries. Account for concrete base
instantiation when shared generic dependencies are involved.

The existing IO representation stores an ordered property list and appears
sufficient; confirm this against official inherited-default output before deciding
that no binary-format change is necessary.

### E. Regression and integration gates

- Adapter/frontend tests for target binding, cross-file/order independence,
  inherited/shadowed fields, structs, duplicates, and every supported value family.
- Serialize/reload tests for declaring-field identity, supplying-class keys, exact
  values, presence flags, preserved imported entries, and sibling isolation.
- Assert no synthetic initialization bytecode is emitted.
- Preserve community inline defaults, added-field defaults, ordinary unary
  operators, and existing bundle data.
- Update the adapter rejection list only as capabilities become implemented.

Split retention/frontend plumbing, checked resolution, and backend emission into
independently buildable commits. Keep end-to-end acceptance gated until required
plans and representations are wired through all consumers.

## Subsequent semantic work

Reprioritize after each corpus checkpoint; first-blocker counts are not independent
coverage gains and cannot simply be added together. The historical pre-rewrite
checkpoint's first blockers were (historical, all now processing-eligible in the
working-tree rerun above; descriptions below retain the original prioritization):

1. Const locals: 53 files; bounded readonly checks now implemented in the working tree.
2. Implicit conversion declarations: 9 files; registration now implemented, selection deferred.
3. Scalar `mutable`: 1 file; bounded Float form now implemented, general scalar rules deferred.
4. Index operator declarations: 1 file; overloaded-call path now implemented.
5. Lazy `skip` parameters: 1 math-operator provider; source lazy lowering now implemented.
6. Native switch label order: 1 file; ordered defaults now implemented.

Initialized groups, broader metadata, explicit conversion dispatch, and unverified
reference forms remain follow-ups too; first-blocker counts do not inventory every
remaining gap.

Additional retained work:

- Fix unsafe prefix lifting at expression execution sites before relaxing adapter
  or exporter guards; cover lazy branches, receiver/argument order, switch matchers,
  and loop reevaluation. Never mutate a copied receiver as a workaround.
- Resolve conditional versions by semantic signatures rather than source spelling.
  Inactive versions need their own actual analysis/bodies.
- Extend event/patch return adaptation, fixed-array operations, contextual function
  values, native float matching, and prefix/postfix ordering with explicit rules.
- Add snapshot-qualified identities/finalized facts where queries or export plans
  need them; exclude failed speculative candidates and test clones/revisions.

## Export, IDE, and release work

- Broaden export only after verified place/reference lowering, default resets, and
  intrinsic mappings. Preserve exact/select imports and plan cross-module
  conditions/patches without guessing ownership from names.
- Check decoded literal values as well as official compilation. Runtime checks
  remain necessary for mutation, wrappers, patches, and repeated initialization.
- Extend IDE completion/member indexing, references/rename, and range formatting
  without losing trivia or written provenance. Preserve snapshot ownership and
  stale-action/edit gates. Measure rebuild cost before incrementalization.
- Publish coordinated compiler/IDE releases with immutable dependency pins and
  matching CI configuration. Validate both repositories and intermediate commits.

## Validation and reporting

For each change, run focused regression tests and independent affected-package
checks. At integration points run the locked default/all-feature workspace tests,
all-targets strict Clippy in both configurations, pinned nightly formatting,
release build, and documentation commands from `CONTRIBUTING.md`. Verify shared
changes do not alter community behavior unintentionally.

Keep installation-dependent corpus and official-tool checks optional and separate
from normal CI. Rerun the corpus after the compatibility/modifier batch and after
default support. Record the revision, discovery rules, dependency/bundle inputs,
stage counts, first blockers, and independent minimal reproductions. More accepted
files can expose more errors; total diagnostics alone do not measure progress.

Earlier local official compiler v1.2.2 checks accepted five authored exports,
including String/CName pool checks, but did not establish game-runtime behavior.
Those historical results do not validate later work. New authored official probes
characterized modifiers, references, operators, omitted slots, enums, native
properties and defaults; bounded binary inspection was used where the existing
full reader could not decode official bundles. These are acceptance/representation
checks, not runtime tests. A bounded release may
document explicit rejections; it must not advertise full language parity.

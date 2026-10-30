# REDmod support roadmap

## Scope and priorities

Native parsing/formatting, partial semantic analysis, mixed-language IDE analysis,
and strict community REDscript → REDmod export exist. Full semantic parity and
runtime compatibility do not.

The next milestone is broader, trustworthy analysis of installed native sources:
fix independently reproduced compatibility bugs, verify modifier semantics, then
implement own and inherited field defaults end to end. Accepting more syntax must
not silently erase behavior or weaken community-language checks.

- [Semantic adapter contracts and rejection list](../crates/compiler/redmod/adapter/README.md)
- [Exporter contracts and limitations](../crates/compiler/redmod/export/README.md)
- [Local parser corpus check](../crates/syntax/redmod/parser/CORPUS.md)
- [Contribution and validation requirements](../CONTRIBUTING.md)

## Corpus baseline

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

The local report and probes were saved under `target/redmod-corpus-review/`.
These are disposable, untracked artifacts, not a portable test dependency.
Reproduce important findings with authored regression fixtures rather than
committing installed game scripts.

## Architecture constraints

- Keep native nodes in `redscript-redmod-ast`, grammar in
  `redscript-redmod-parser`, and printing/formatting in
  `redscript-redmod-formatter`. Parsing retains unsupported constructs.
- Keep native mappings, capability checks, and native-to-shared conversion in
  `redscript-redmod-adapter`. Shared frontend extensions must be language-neutral,
  using narrow per-source policies or supplemental inputs where needed.
- Do not add native-only syntax variants to the community AST. Preserve adapter
  plans in the complete `ParsedModule`, including cloned IDE snapshots. Bare-AST
  APIs must fail closed if extracting the AST would discard required semantics.
- Preserve evaluation count/order, lazy evaluation, writable places, and written
  spans. Synthetic provenance must not hide source children from IDE queries.
- Carry resolved semantic identities after binding. Spans, source spellings,
  pointers, and overload positions are not permanent declaration identities.
- Diagnose unsupported semantics explicitly. Do not approximate reference
  wrappers, skipped arguments, mutation rules, or initialization by discarding
  syntax or substituting values.
- Use typed diagnostics, complete diagnostic/output snapshots, and direct
  assertions for identity, provenance, order, flags, and serialized properties.

## Next implementation batch

Keep independent fixes in separate Conventional Commits. Each commit must build
and include its API initializer/call-site updates, regression tests, and relevant
contract documentation. Shared prerequisites may need separate commits before
adapter integration.

### 1. Implicit method/type name collisions

**Suggested commit:** `fix(redmod): prefer implicit methods over types`

Constructor lookup currently precedes implicit-member lookup. Installed sources
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

### 2. Omitted field visibility

**Suggested commit:** `fix(redmod): apply native field visibility defaults`

Omitted visibility on imported fields currently becomes shared private visibility,
but installed native sources access these fields externally. The mismatch also
reproduces with an isolated imported class and consumer.

First characterize omitted visibility across scripted, `import`, and `importonly`
classes; imported versus scripted fields; structs; and own, inherited, and
unrelated-class access. Explicit public/protected/private visibility is a control.

Implement only the verified rule in the adapter or a narrow resolution policy.
Preserve explicit visibility and implicit-versus-written provenance where relevant.
Do not make every native field public merely to silence diagnostics.

### 3. Imported event prototypes

**Suggested commit:** `fix(redmod): support imported event prototypes`

Bodyless protected events in imported classes currently receive `MISSING_BODY`.
An isolated `import class NativeController { protected event OnMounted(); }`
reproduces this without missing dependencies.

Characterize `import`/`importonly` event prototypes, scripted subclass
implementations, and body-bearing declarations. Map prototypes to their verified
resolved flags and callback identity, preserving Void return semantics.

Do not synthesize empty bodies or make all members of imported classes native.
Keep missing-body checks for invalid scripted declarations. Test subclass
compatibility and serialized callback/native/unimplemented flags, not just the
absence of diagnostics.

### 4. Export modifier

**Suggested commit:** `fix(redmod): accept export function modifiers`

`export` is a candidate for an analysis-only no-op, not an unconditional discard.
Compare official fixtures with and without it on free functions, instance/static
methods, overrides, and different visibility. Inspect flags where decoding permits.

If resolution is unaffected, accept it only on verified declaration kinds without
inventing shared flags. Document the distinction between analysis acceptance and
equivalent engine exposure or emitted bytecode. Continue rejecting unverified
modifier targets such as fields.

### 5. Virtual methods

**Suggested commit:** `fix(redmod): support virtual methods`

Ordinary non-final shared methods may already provide the necessary dispatch, but
the corpus also contains private virtual methods and bodyless virtual declarations.
Blindly dropping the modifier can leave override eligibility or body legality wrong.

Characterize ordinary versus virtual methods, private overrides, imported/scripted
bodyless declarations, and conflicts with `final`, `static`, and `override`.
Map onto existing flags where justified; retain explicit virtual intent in adapter
metadata if needed. Revisit the override search's exclusion of private base methods
only when supported by those results.

Rerun corpus analysis after this compatibility/modifier batch before choosing the
next newly exposed blockers.

## Default directives: end-to-end implementation

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

## Following semantic work

Reprioritize after each corpus checkpoint; first-blocker counts are not independent
coverage gains and cannot simply be added together.

| Capability | Next step |
| --- | --- |
| `constexpr` | Characterize function resolution and compile-time behavior; do not assume it is an ordinary function modifier |
| Explicit enum storage | Verify widths, ranges, type rules, and serialized representation |
| Operator/conversion declarations | Register and type-check native declarations; high leverage for reducing downstream missing-operator/cast diagnostics |
| Explicit `ref<T>` | Define native reference/value semantics; do not map blindly to community strong wrappers |
| `.Script` discovery | Make IDE discovery and dialect selection consistent, with case-variant tests |
| Const locals / `mutable` | Establish mutation and legality rules before mapping or ignoring modifiers |
| Omitted arguments / `skip` | Resolve actual slots and preserve lazy argument execution; optional flags are insufficient |
| Assignment values / initialized groups | Preserve values, places, initializer visibility, and evaluation count/order |

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
Those historical results do not validate the planned work. A bounded release may
document explicit rejections; it must not advertise full language parity.

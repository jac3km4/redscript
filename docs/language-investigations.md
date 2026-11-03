# Historical language investigations

**Archive, not current support or validation.** This reference brings together
corpus investigations and conversion experiments, including rejected designs and
the narrower delivered follow-up. The first part explains what historical corpus
and official observations can establish; the second records conversion-specific
causes, discarded approaches and bounded results.

Use the [guide index](README.md), [shared semantics](language-semantics.md) and
[REDmod semantics](redmod-semantics.md) for current contracts, and
[future work](language-future-work.md) for outstanding work. Original detailed
reports remain in Git history. Historical counts do not certify current HEAD,
semantic accuracy or runtime parity; no new compiler/corpus/runtime measurements
were made for this consolidation.

## Contents

- [Corpus findings and evidence policy](#corpus-findings-and-evidence-policy)
  - [Language investigation summary](#language-investigation-summary)
    - [What the corpus investigations established](#what-the-corpus-investigations-established)
    - [Official evidence worth preserving](#official-evidence-worth-preserving)
    - [Superseded designs and reporting rules](#superseded-designs-and-reporting-rules)
- [Conversion experiments and bounded follow-up](#conversion-experiments-and-bounded-follow-up)
  - [Conversion investigation archive](#conversion-investigation-archive)
    - [Removed broad prototype](#removed-broad-prototype)
    - [Guarded implicit-conversion discovery](#guarded-implicit-conversion-discovery)
    - [Delivered bounded follow-up](#delivered-bounded-follow-up)

## Corpus findings and evidence policy

### Language investigation summary

**Archive, not current support or validation.** This condenses the 2026-10-29
through 2026-11-02 REDmod roadmap, semantic-parity and follow-up investigations.
Use the [language guide](README.md) for current contracts and
[future work](language-future-work.md) for remaining work. Full original logs and
superseded plans remain in Git history; disposable local artifacts are not
portable test dependencies.

#### What the corpus investigations established

The installed corpus had 2,112 script files (21,873,807 bytes), including a
capital-S `.Script` file. Surveys used embedded boot/default symbols and **no game
bundle**, unless explicitly labeled as isolated dependency experiments. Installed
sources were read-only; authored reductions, not copied game scripts, belong in
regression fixtures.

The initial survey admitted only 801 files. Whole-file first blockers included
defaults, export/virtual/constexpr modifiers, enum storage, references and const
locals. Restoring declarations exposed more errors, not fewer: the `fad6745`
investigation baseline admitted 2,046 files and recorded 47,982 shared errors.
These are historical stage/diagnostic counts, not successful semantic passes.

Useful causal findings:

- Losing one math provider excluded 209 operators and caused many unresolved-call
  cascades. Erasing skip reduced diagnostics but was unsound eager evaluation;
  source lazy flags and execution-site guards were required instead.
- Deleting unknown signature parameters shifted names onto surviving types and
  fabricated arity/override behavior. Preserved invalid slots and unavailable
  bodies are safer than plausible-looking corrupted signatures.
- Excluded conversion providers also hid foundational ID and API declarations.
  Registration and dispatch eligibility must be separate.
- Four absent engine primitives (NodeRef, CRUID, LocalizationString and
  redResourceReferenceScriptToken) were independently established by official
  pass-through probes and installed primitive records, not by guessed aliases.
  Restoring only those definitions eliminated invalid-signature cascades but
  exposed additional conversion and constructor diagnostics. They are now shared
  opaque builtins; the experiment did not prove literals, defaults or layouts.
- Constructor source fields do not establish complete engine layout. Most observed
  incomplete-constructor sites correlated with an existing bounded completeness
  registry, but additional types lay outside it. Versioned metadata reconciliation
  is required, not blanket imported-struct acceptance.

Later investigation checkpoints admitted all files. Error disappearance still did
not prove correct binding: a call with failed/mistyped arguments could return the
valid callee signature's type from recovery IR. Exact-span mapping missed callee,
folded, default and unavailable sites, and synthetic spans could contaminate the
written-expression population. File eligibility, retained bodies, non-Nothing
query results and zero failures on mapped spans are not accuracy denominators.
No 90–99% semantic-parity claim was established.

The conversion follow-up at `86af12a` investigated 416 completed official probes,
including an 11-source × 11-target × implicit/explicit primitive matrix (242 cases).
The matrix matched declared primitive edges plus identity; exact overloads won,
while multiple converted alternatives were ambiguous independent of order. It
provided no justification for numeric distance ranking, universal widening or
conversion chaining. See [conversion findings](#conversion-investigation-archive)
for the rejected prototype and delivered bounded slice.

#### Official evidence worth preserving

These were bounded official compiler v1.2.2 observations, not runtime tests:

- [Enums](language-semantics.md#explicit-enum-storage): legal signed/unsigned storage ranges, four-byte omitted
  native storage, erased signedness and rejection of the Int64 minimum spelling.
- [Defaults](language-semantics.md#shared-aggregate-defaults): own-field
  duplicate first-wins and inherited duplicate last-wins per supplying class;
  defaults live on declaring fields as class-qualified properties, not constructor
  assignments. Official inherited defaults accepted some invalid text; shared
  checking intentionally stays stricter.
- [Const locals](language-semantics.md#const-parameters-and-locals): repeated class-handle rebinding and handle
  forwarding were accepted; scalar/struct assignment and non-const referent methods
  were not. Native scalar const inline initialization was rejected, unlike the
  community initializer rule.
- [Events](redmod-semantics.md#redmod-event-return-contract): implicit Bool metadata, legal fallthrough without a
  synthetic false return, and Int32-to-Bool via `Cast;Int32;Bool`, not generic
  truthiness. Some invented native bindings and isolated patch probes failed;
  they did not establish official patch parity.
- [Overloads/inheritance](language-semantics.md#call-lookup-overloads-and-inheritance): child specificity, real-method
  preference over generated aliases, optional-tail ambiguity, original-slot return
  contracts and declaring-class persistent access.
- [Indexing](language-semantics.md#switch-order-and-overloaded-indexing): OperatorArray is an ordinary call-shaped
  result, not a writable array slot. Non-final switch defaults permit fallthrough.
- Mutable Float: the observed `instanceeditable inlined` non-imported class field
  required no extra flag; explicit const stayed set. No universal mutable-scalar
  normalization was established.
- Exported String/CName values preserved raw backslashes and LF; community escape
  spellings were not interchangeable with native literals. Inline scalar/call,
  array-copy and object-reference initializers compiled officially, but that did
  not establish repeated loop initialization or runtime side-effect order.

Some official validation bundles contain external type placeholders or profiling
records that the full reader cannot decode. Bounded record/opcode inspection is
narrower evidence, not a successful full bundle roundtrip.

#### Superseded designs and reporting rules

Private adapter default plans and supplemental frontend default inputs were
superseded by shared `Item::Default` / `FieldDefault` AST members. Native loop
fixups still need complete ParsedModule metadata. Old instructions to force event
Void returns, erase const qualifiers, defer all conversion dispatch, or treat
ordered defaults/indexing as unimplemented are obsolete.

Repeated working-tree checkpoints, build-directory/resource settings, concurrent
edit failures and test-count logs have been removed from durable language docs.
They neither certify current HEAD nor define a user requirement. Reproduction
belongs in the optional [corpus harness](../crates/compiler/redmod/adapter/CORPUS.md);
code validation belongs in [CONTRIBUTING.md](../CONTRIBUTING.md). Fresh claims need
a fresh run with revision/input/dependency fingerprints. No new corpus, official
compiler or runtime measurements were made for this documentation consolidation.

## Conversion experiments and bounded follow-up

### Conversion investigation archive

**Historical evidence, not the current contract or a validation log.** See
[conversion declarations and bounded selection](language-semantics.md#conversion-declarations-and-bounded-native-selection) for
implemented semantics, [future work](language-future-work.md#conversions-and-native-bindings)
for missing capabilities and the [investigation summary](language-investigations.md)
for corpus/evidence limits.

#### Removed broad prototype

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

#### Guarded implicit-conversion discovery

An unrestricted implicit fallback in shared coercion was unsafe: with A-to-B and
Pick(B)/Pick(C), the diagnostic-recovery candidate could become successful merely
by reversing declaration order. With A-to-B and A-to-C it could accept the first
alternative instead of preserving ambiguity.

Fixed-pair preflight was insufficient because script-reference dereferencing
could expose a provider afterward. Permission must reach actual application
through recursive coercion, and remain disabled for out arguments and the typed
resolver's recovery-only fallback. This is why the delivered bounded paths do not
claim conversion-aware overload matching.

#### Delivered bounded follow-up

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

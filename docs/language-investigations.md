# Historical language investigations

**Archive, not current support or validation.** This condenses the 2026-10-29
through 2026-11-02 REDmod roadmap, semantic-parity and follow-up investigations.
Use the [language guide](README.md) for current contracts and
[future work](language-future-work.md) for remaining work. Full original logs and
superseded plans remain in Git history; disposable local artifacts are not
portable test dependencies.

## What the corpus investigations established

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
conversion chaining. See [conversion findings](conversion-implementation-notes.md)
for the rejected prototype and delivered bounded slice.

## Official evidence worth preserving

These were bounded official compiler v1.2.2 observations, not runtime tests:

- [Enums](enum-storage.md): legal signed/unsigned storage ranges, four-byte omitted
  native storage, erased signedness and rejection of the Int64 minimum spelling.
- [Defaults](shared-expression-defaults.md#shared-aggregate-defaults): own-field
  duplicate first-wins and inherited duplicate last-wins per supplying class;
  defaults live on declaring fields as class-qualified properties, not constructor
  assignments. Official inherited defaults accepted some invalid text; shared
  checking intentionally stays stricter.
- [Const locals](const-parameters.md): repeated class-handle rebinding and handle
  forwarding were accepted; scalar/struct assignment and non-const referent methods
  were not. Native scalar const inline initialization was rejected, unlike the
  community initializer rule.
- [Events](redmod-events.md): implicit Bool metadata, legal fallthrough without a
  synthetic false return, and Int32-to-Bool via `Cast;Int32;Bool`, not generic
  truthiness. Some invented native bindings and isolated patch probes failed;
  they did not establish official patch parity.
- [Overloads/inheritance](lookup-inheritance.md): child specificity, real-method
  preference over generated aliases, optional-tail ambiguity, original-slot return
  contracts and declaring-class persistent access.
- [Indexing](control-flow-operators.md): OperatorArray is an ordinary call-shaped
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

## Superseded designs and reporting rules

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

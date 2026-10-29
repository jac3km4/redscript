# REDmod support roadmap

## Current scope

Native parsing/printing, partial semantic analysis, mixed-language IDE analysis,
and strict community REDscript → REDmod export exist. Full semantic parity and
runtime compatibility do not.

- [Semantic adapter contracts and limitations](../crates/compiler/redmod/adapter/README.md)
- [Exporter contracts and limitations](../crates/compiler/redmod/export/README.md)
- [Local parser corpus check](../crates/syntax/redmod/parser/CORPUS.md)

## Architecture conventions

- Keep native nodes in `redscript-redmod-ast`, grammar in `redscript-redmod-parser`, and native
  printing/formatting in `redscript-redmod-formatter`. Parsing must retain unsupported
  constructs rather than skip them or substitute values.
- Keep target mappings, capability checks and native-to-shared conversion in
  `redscript-redmod-adapter`. The shared frontend must remain language-agnostic; use
  narrow, configurable per-source policies when shared lowering needs variation.
- Do not add native-only syntax variants to the community AST. Native loops use
  a compiler-owned plan applied to typed IR; callers must retain the complete
  `ParsedModule`, including when cloning IDE snapshots.
- Diagnose unsupported semantics explicitly. Do not emulate custom generics,
  lambdas, `mixedRef` or class-to-`ref` defaulting in REDmod.
- Preserve source evaluation order, writable places and written spans. Synthetic
  expressions use zero-width provenance without masking written children.
- Use typed errors and module-level visitors. Snapshot complete syntax/output
  and diagnostics; use direct assertions for spans, identity, order and bytecode.
  Tests must use authored fixtures, not copied game scripts.

## Remaining work, in priority order

### 1. Make coordinated releases reproducible

Publish the compiler changes and pin an immutable dependency for the IDE; update
its path-dependency/CI setup together. Split shared changes into independently
buildable commits: qualifiers/hints, parsed-source API, semantic policies,
parser utilities, and generic query/visitor helpers. Keep target-specific work
separate. Run each repository's contribution checks and verify intermediate
shared commits, not only the combined tree.

### 2. Preserve evaluation order and semantic identity

The shared frontend can lift array prefixes across lazy expressions or earlier
operands. Both adapter and exporter currently reject unsafe shapes. Fix this at
the lowering boundary by keeping prerequisites at their execution site; test
lazy branches, call/receiver order, switch matchers and loop reevaluation before
relaxing guards. Never fix mutation by operating on a copied receiver.

Add optional snapshot-qualified source identities/finalized semantic facts when
needed to associate retained declarations with resolved bodies. Spans, pointer
IDs and overload positions are not identities. Exclude failed speculative
candidates and test moves/clones/revisions. Use these facts to support inferred
signatures, competing patches, multiple wrappers and conditional export plans;
inactive variants need their own actual analysis/bodies.

### 3. Extend native semantics deliberately

The adapter README owns the rejection list. Prioritize omitted arguments,
assignment values, initialized groups, default directives and operator/conversion
registration. Establish target behavior with small official fixtures first;
then add checked lowering and IR/backend regressions. Preserve evaluation count
and distinguish optional flags from omitted argument slots.

Resolve conditional versions by resolved signature rather than source spelling.
Extend event compatibility, fixed-array operations and modifier handling only
with explicit target rules. Check prefix/postfix ordering, native float matching
and contextual function values from community dependencies.

Add official ambiguity fixtures for casts versus grouping, type/value name
collisions and comparison/postfix combinations before changing precedence.
Port applicable shared lexer/parser fixes deliberately, without coupling grammars.

### 4. Broaden export coverage safely

The exporter README owns unsupported operations and numeric limits. Add verified
place/reference lowering, default resets and intrinsic mappings before accepting
more inputs. Preserve exact/select imports rather than widening them. Plan
cross-module conditions/patches explicitly; do not guess source ownership from
names. Keep unsupported cases rejected while these prerequisites are missing.

For every new literal spelling, check decoded official constant-pool values as
well as compilation. For patches, wrappers, mutation and repeated initialization,
add runtime checks before making behavioral claims.

### 5. Complete IDE features

Mixed saved/live/speculative analysis, hover, definitions, workspace symbols,
array-member completion and conservative native document indentation exist.
Remaining work:

- Add general keyword/local completion, a fuller member index, references/rename.
- Debounce rebuilds and introduce incremental work only after measuring rebuild
  cost; preserve all-buffer overlays, version invalidation and snapshot ownership.
- Add range/spacing/wrapping formatting with trivia preservation. Current native
  formatting changes indentation only and rejects diagnostic-bearing input.
- Keep edit gates at the actual target file and reject stale resolved actions.
  Client `.script` association/highlighting remains separate from server discovery.

## Validation boundary

The installed corpus parses 2,112 files without syntax diagnostics; this is not
semantic or official-compiler conformance. Five authored exports compiled with
local official compiler v1.2.2, including String/CName pool checks; no game runtime
was tested. Official checks were not rerun after integration with upstream
`ebfe5b8`.

After that integration, both workspaces passed locked default/all-feature tests,
strict Clippy, pinned formatting, release builds and docs. Repeat these checks
after changes; keep optional installed-corpus and official-tool tests separate
from ordinary CI. A bounded release may document explicit rejections, but must
not advertise full language parity.

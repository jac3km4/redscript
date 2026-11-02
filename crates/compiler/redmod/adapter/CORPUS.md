# Optional semantic corpus survey

This example analyzes an explicitly supplied native REDmod source directory. It
is a repeatable diagnostic survey, **not** an acceptance test, official-compiler
comparison, bytecode generator, or runtime compatibility claim. Installed game
sources are neither committed nor required by ordinary tests or CI.

## Run

From the workspace root, using stable Rust (quote paths containing spaces):

```sh
cargo run -p redscript-redmod-adapter --locked --example semantic_corpus -- 'D:/win/Cyberpunk 2077/tools/redmod/scripts'
```

An optional second positional argument selects the output directory:

```sh
cargo run -p redscript-redmod-adapter --locked --example semantic_corpus -- 'D:/win/Cyberpunk 2077/tools/redmod/scripts' target/my-semantic-corpus
```

The default is `target/redmod-semantic-corpus/`, relative to the working directory.
The example overwrites its seven report files but does not clear the directory.
Use a disposable, ignored directory; reports may contain game identifiers and
local paths. No additional packages, installation, environment variables, or game
executables are required. `--help` prints the CLI contract.

**Dependency mode is boot only:** embedded `boot.reds` plus
`Symbols::with_default_types()`. There is no bundle input or additional dependency
loader. This keeps the dependency choice explicit and reproduces the earlier
boot-only probe; do not compare these results to a bundle-backed analysis without
recording that difference. Engine definitions and declarations in excluded files
are absent, so unresolved-symbol diagnostics often cascade.

## Discovery and stages

- Recursively discover every regular file with an ASCII-case-insensitive
  `.script` extension, including `.Script`. Include hidden/ignored files; do not
  follow symlinks or junctions. Sort normalized relative paths by ASCII-lowercase
  spelling, breaking ties by original spelling. The corpus is read unmodified as
  UTF-8. Missing/non-directory/empty corpora and traversal/read errors fail the run.
- Parse with `parse_file_with_metadata`, preserving the complete `ParsedModule`.
  Per-file outcomes distinguish syntax, bridge capability, pre-resolution
  validation, missing module without diagnostics, and processing eligibility.
- Like the IDE, exclude **every native module with parse-stage diagnostics**,
  including converted modules with validation errors. Empty valid modules still
  count; ownership is never inferred from `module.span()`.
- Process all eligible modules together with the parsed boot dependency, not one
  file at a time. Collect adapter processing errors separately from shared typed
  diagnostic codes. Run the IDE's `UnusedLocals` diagnostic pass too.
- Semantic diagnostics are expected and do not make the command fail. I/O, boot
  parsing, or invalid diagnostic spans do. An infrastructure failure can leave
  previous reports in place; only use reports from a successful invocation.

Capability diagnostics expose the **first blocker per file**, not every unsupported
construct. Validation may emit multiple diagnostics per excluded file. Returned
IR, processing eligibility, and diagnostic-free files are not semantic passes.
The harness deliberately does not change acceptance rules or suppress cascades.

## Reports

The summary is printed to stdout; complete reports are written to the output
folder. Ordering is deterministic for unchanged inputs/compiler behavior; file
names are corpus-relative with `/` separators. The root is recorded in the summary.

| File | Contents |
| --- | --- |
| `summary.md` | Dependency mode, discovered files/bytes, mutually exclusive parse-stage file counts, adapter errors, shared errors/warnings/pedantic counts |
| `files.tsv` | Every discovered file, byte count, parse-stage outcome, parse-stage diagnostic count |
| `aggregate.tsv` | Counts and distinct files grouped by stage, typed code, adapter reason and severity; up to three exact file/line/column examples and one message |
| `diagnostics.tsv` | Every diagnostic, including full message, stage, code, reason, severity, file, 1-based line/Unicode-scalar column, and half-open byte offsets |
| `diagnostics.log` | All full messages with editor-friendly `file:line:column` locations and byte spans; no source snippets |
| `coverage.tsv` | Per-file bridged declarations, signature flags, explicit/retained bodies and written/mapped expression counts; unavailable AST totals are `NA` |
| `expression_queries.tsv` | Exact written spans observed in retained IR, conservative query outcome, mixed-outcome flag and query reasons |

TSV cells escape backslashes, tabs, CR and LF. Shared codes are retained rather
than collapsed into the IDE wrapper's `OTHER` code. Adapter reasons retain enum
variant names (including modifier kinds) rather than grouping by message wording.
Shared `ErrorAllowedAtRuntime` contributes to errors; `Pedantic` is counted
separately, not silently omitted.

For repeatability, run twice with separate output directories and compare the
seven files. A parent process may also change the compiler concurrently: record the
revision and working-tree changes externally when using a run as a checkpoint.

## Local checkpoint (2026-10-30)

The full local directory above contained **2,112 files / 21,873,807 bytes**, including
one `.Script`. Two consecutive runs produced byte-identical reports under
`target/redmod-semantic-corpus/` and `target/redmod-semantic-corpus-first/`.
These counts precede the concurrent modifier/reference acceptance changes; they
are the boot-only baseline, not a claim about the final combined working tree.
An intermediate debug-mode run timed out without reports. A later inheritance
cycle fix removed an implicit `IScriptable` self-base and made ancestor traversal
terminate on cycles; the final combined run below completed successfully.

| Outcome | Count |
| --- | ---: |
| Syntax diagnostics / excluded files | 0 / 0 |
| First capability blockers / excluded files | 1,154 / 1,154 |
| Pre-resolution validation diagnostics / excluded files | 436 / 157 |
| Missing modules without diagnostics | 0 |
| Modules reaching processing | 801 |
| Adapter processing errors | 588 |
| Shared errors / warnings / pedantic | 13,769 / 1,449 / 0 |

All validation diagnostics were `UnsupportedType`. All adapter processing errors
were `OverrideWithoutBase` (224 distinct files); absent declarations prevent these
from establishing independent override bugs. The largest first capability
blockers were `DefaultDirective` (592), `Modifier(Export)` (238),
`Modifier(Virtual)` (133), `Modifier(Constexpr)` (50), `EnumStorage` (41), and
`ConstLocal` (33).

Shared unresolved type/reference/function/member diagnostics totalled 12,644.
The previous disposable probe reported 485 warnings without the unused-local
pass; this harness reports the same 485 plus **964 `UNUSED_LOCAL` warnings**.
This difference is instrumentation, not expanded semantic acceptance.

## Historical integrated checkpoint (2026-10-31)

At original commit `6b53cb2`, the same **2,112 files / 21,873,807 bytes** were
surveyed with the same boot-only dependencies and unused-local pass. This was
**before the shared-default rewrite**; the original revision is preserved on
`backup-feat-redmod`. These results have not been rerun for the rewritten history
and must not be presented as current-HEAD counts. The historical command was:

```sh
cargo run --release -p redscript-redmod-adapter --locked --example semantic_corpus -- 'D:/win/Cyberpunk 2077/tools/redmod/scripts' target/redmod-semantic-corpus-final
```

| Outcome | Baseline | Integrated |
| --- | ---: | ---: |
| Syntax-blocked files | 0 | 0 |
| Capability-blocked files | 1,154 | 66 |
| Validation-blocked files | 157 | 0 |
| Modules reaching processing | 801 | 2,046 |
| Adapter processing errors | 588 | 1,456 |
| Shared errors | 13,769 | 47,982 |
| Shared warnings | 1,449 | 25,364 |

That run's remaining first blockers were `ConstLocal` (53 files),
`ConversionDeclaration` (9), and one each of `Modifier(Mutable)`,
`OperatorDeclaration` (index operator), `SkipParameter`, and `SwitchLabelOrder`.
These counts hide later unsupported constructs in the same files.

The largest shared error groups are `UNRESOLVED_FN` (24,572), `UNRESOLVED_REF`
(7,278), `INVALID_ARG_COUNT` (5,713), `UNRESOLVED_TYPE` (5,235), `TYPE_ERR`
(2,633), and `UNRESOLVED_MEMBER` (998). Adapter errors comprise
`OverrideWithoutBase` (882), `OverrideReturnTypeMismatch` (414),
`PersistentFieldAccess` (148), and other errors (12).

More bodies reached analysis in that run, exposing more diagnostics. In particular,
the math-operator file was excluded by `skip`, causing many missing
`OperatorLogicAnd` errors. Do not interpret the totals as independent bugs or
2,046 processing-eligible modules as semantic passes. Reports remain disposable
under `target/redmod-semantic-corpus-final/`; no runtime parity is established.

## Cleanup working-tree refresh (2026-11-01)

This refresh supersedes the previous working-tree checkpoint below; its old
counts remain historical. No final revision is assigned while history is being
arranged. Stable release surveyed the installed directory read-only with the same
boot/default symbols and no game bundle: **2,112 files / 21,873,807 bytes**.

```sh
CARGO_PROFILE_DEV_DEBUG=0 CARGO_PROFILE_TEST_DEBUG=0 CARGO_PROFILE_RELEASE_DEBUG=0 CARGO_INCREMENTAL=0 cargo +stable build --release --locked -p redscript-redmod-adapter --example semantic_corpus --target-dir target/parity-integration -j2
target/parity-integration/release/examples/semantic_corpus.exe 'D:/win/Cyberpunk 2077/tools/redmod/scripts' target/redmod-semantic-corpus-cleanup
target/parity-integration/release/examples/semantic_corpus.exe 'D:/win/Cyberpunk 2077/tools/redmod/scripts' target/redmod-semantic-corpus-cleanup-replay
```

**All seven reports replay byte-for-byte.** Five release example tests passed.
All files reach processing, with zero syntax/capability/validation/missing-module
files and zero adapter errors. Shared totals remain **12,655 errors / 3,705
warnings / 0 pedantic**, with errors in 1,172 files and 940 files without reported
errors (not semantic passes). Total diagnostics remain 16,360.

| Changed shared group | Previous checkpoint | Cleanup refresh |
| --- | ---: | ---: |
| `UNSUPPORTED_NATIVE_CONVERSION` | 4,925 / 645 files | 4,958 / 651 files |
| `UNRESOLVED_FN` | 838 / 289 files | 805 / 276 files |
| Unresolved calls naming `Cast` | 761 | 728 |

Source-aware native cast fallback now reports 33 calls as unsupported conversion
dispatch rather than unresolved functions. No conversion selection/emission is
claimed. All other grouped counts below remain unchanged. `coverage.tsv`,
`expression_queries.tsv`, `files.tsv` and `summary.md` are byte-identical to the
previous checkpoint; diagnostic reports differ. The unchanged coverage totals
below reconcile, including all signature/body/expression/query partitions and
781,341 expression-query rows. Availability still does not establish correctness.

Full contribution validation passed on this working tree: default/all-feature
workspace tests each **738 passed, 0 failed, 9 ignored** across 117 targets
(previously 728 passed); both all-targets Clippy configurations with `-D warnings`;
pinned nightly-2026-10-15 fmt; release workspace build; all-feature no-dependency
docs; and nine separate package checks (community AST/parser/formatter,
frontend/backend, REDmod adapter/transpiler, decompiler and CLI). Cargo used
`--locked -j2`, `target/parity-integration`, DEV/TEST debug zero and incremental
off, with release debug zero. Only that target's generated debug directory was
removed for space; release/docs and official-event artifacts were preserved.
The combined corpus build/test command timed out after the executable built;
the separate release-test retry passed. No source/test fixes were needed. The
existing `chumsky` future-incompatibility advisory remains.

Cleanup includes source-based lazy guards and native cast fallback operands,
community `const` syntax round-tripping, const-local entry defaults, and removal
of the special Int32-to-Bool event-return coercion. Ordinary native `Cast` lookup
is not general conversion support. See
[lazy evaluation limitations](../../../../docs/lazy-evaluation-limitations.md)
for conservative typed-call boundaries and expression-prefix restrictions.
Partial declaration harvesting, complete dependency/layout modeling, general
conversion dispatch and official/runtime comparison remain deferred.

## Previous working-tree checkpoint (2026-11-01)

Stable release example rebuilt from the dirty integration tree based on `d46faac`
(not a committed revision of these changes). Installed sources were read only:
**2,112 files / 21,873,807 UTF-8 bytes**, boot/default symbols only, no game bundle.
The snapshot comparison is the 47,982 shared-error / 1,456 adapter-error /
25,364-warning baseline above; no baseline body/query coverage was recorded.

```sh
CARGO_PROFILE_RELEASE_DEBUG=0 CARGO_PROFILE_RELEASE_INCREMENTAL=false cargo +stable build --release --locked -p redscript-redmod-adapter --example semantic_corpus --target-dir target/parity-integration -j2
target/parity-integration/release/examples/semantic_corpus.exe 'D:/win/Cyberpunk 2077/tools/redmod/scripts' target/redmod-semantic-corpus-parity
```

The same executable replayed to `target/redmod-semantic-corpus-parity-replay/`;
**all seven reports were byte-identical**. Survey executable SHA-256:
`d770e92403bf5981acb8e9041480f114562c3473de978186fad23c4f203df6c5`.
The five release example tests passed.
Coverage TSV sums and signature/body/expression/query partitions were checked.
Inspection of the runner found no concrete measurement bug; no instrumentation
change was needed. This does not certify concurrent test changes or the workspace.

| Outcome | Snapshot baseline | Current working tree |
| --- | ---: | ---: |
| Files reaching processing | 2,046 | 2,112 |
| Capability-excluded files | 66 | 0 |
| Syntax / validation / missing-module files | 0 / 0 / 0 | 0 / 0 / 0 |
| Adapter processing errors | 1,456 | 0 |
| Shared errors (including runtime-allowed) | 47,982 | 12,655 |
| Shared warnings / pedantic | 25,364 / 0 | 3,705 / 0 |

Current errors affect **1,172 files**; 940 files have no reported errors, not 940
semantic passes. There are 16,360 diagnostics total (12,655 errors + 3,705 warnings).
All former first-blocker files now enter processing; this is not an accuracy score.

| Observed coverage | Count |
| --- | ---: |
| Bridged ASTs / declarations excluding imports | 2,112 / 96,126 |
| Function declarations | 53,875 |
| Signature flags valid / invalid / mixed / unavailable | 53,819 / 56 / 0 / 0 |
| Explicit parsed bodies / retained in lowered unit / unavailable | 38,861 / 38,824 / 37 |
| Distinct written expression spans | 1,144,329 |
| Exact-span mapped / unmapped | 781,341 / 362,988 |
| Query resolved / unresolved / failure | 778,780 / 2,561 / 0 |
| Mixed query outcomes (subset of mapped) | 38 |

**Measurement limits:** declarations are bridged AST items, not a faithful-declaration
score. Signature validity is only the compiler flag. Retained bodies include
recovery IR, not necessarily complete or diagnostic-free checking; the 37 unavailable
bodies occur in files with invalid signatures, but the harness does not assign
causes. Prototypes and synthetic bodies are outside the explicit-body denominator.
Written expressions include bodies and field/default values; imports, empty spans,
Omitted/Error placeholders and boot are excluded. Mapping requires an exact written
span on a non-Nop IR expression in a retained function. Unmapped expressions can be
callee names, folded/default expressions or unavailable bodies, not query failures.
Generated nodes reusing written spans cannot reliably be separated. Worst observed
outcome wins (failure, then unresolved, then resolved). Resolved means only a type
other than `Nothing` was returned, not a correct or poison-free type. All 2,561
unresolved sites include `UnknownNullType`; 38 also returned a type on another
observation. Zero query failures says nothing about the 362,988 unmapped sites.

Dominant remaining error groups:

| Shared code | Diagnostics | Files |
| --- | ---: | ---: |
| `UNSUPPORTED_NATIVE_CONVERSION` | 4,925 | 645 |
| `INVALID_FUNCTION_SIGNATURE` | 4,921 | 788 |
| `NON_FULLY_DEFINED_CTR` (runtime-allowed error) | 1,008 | 237 |
| `UNRESOLVED_FN` | 838 | 289 |
| `TYPE_ERR` | 298 | 139 |
| `MULTIPLE_MATCHING_OVERLOADS` | 163 | 78 |
| `UNRESOLVED_TYPE` | 162 | 67 |
| `UNRESOLVED_MEMBER` | 140 | 86 |
| `INVALID_ARG_COUNT` | 118 | 41 |
| `UNRESOLVED_REF` | 73 | 19 |
| `OTHER` / `INVALID_TEMP` | 5 / 4 | 4 / 2 |

Conversion providers now register, but **dispatch remains fail closed**; their
4,925 diagnostics are not implemented conversions. Invalid-signature reports are
recovery/dependency guards, not 4,921 independent signature bugs: 4,822 name
`OperatorEqual`. Missing-type rows are dominated by `NodeRef` (140) and `CRUID`
(15); the boot-only dependency model remains material. Of unresolved functions,
761 name `Cast`. Incomplete native-layout constructor checks principally concern
Vector2/Vector4/inkMargin/Color. Residual type/overload/member/arity failures need
authored reductions rather than blanket suppression. Warnings comprise 3,117
`UNUSED_LOCAL` and 588 `UNUSED_ITEM_QUALIFIERS`.

Implemented in the measured working tree: signature-poison preservation/guards,
source `skip`, conversion registration, const-local checks, event/overload/override
fixes, Variant extraction ordering, ordered switch defaults, overloaded indexing
and the bounded scalar-mutable form. See the [investigation](../../../../docs/redmod-semantic-parity.md)
and linked handoffs for exact contracts. **Partial declaration harvesting is not
implemented**: parse-stage failures still exclude whole modules. The new reports
only observe existing analysis; they do not enable a partial-analysis mode.
Conversion selection, complete dependency/layout modeling, residual semantic
triage and official binding/dispatch comparisons remain deferred. No full-corpus
type-accuracy percentage, full language parity or runtime compatibility is claimed.

## Bounded-conversion checkpoint (2026-11-02)

Two fresh release runs at `afb485e`, including conversion commits `943bf74`,
`2f3c5b6` and `1fc6efd`, produced **byte-identical copies of all seven reports**.
Inputs remain 2,112 files / 21,873,807 bytes, embedded boot/default symbols and no
game bundle. Unrelated in-progress frontend metadata changes were present.
Reports are under `target/redmod-semantic-corpus-bounded-conversions/` and its
`-replay` counterpart.

| Metric | Pre-conversion (`33fa996`) | Current checkpoint |
| --- | ---: | ---: |
| Modules reaching processing | 2,112 | 2,112 |
| Adapter processing errors | 0 | 0 |
| Shared errors | 7,999 | 3,315 |
| Shared warnings | 1,608 | 971 |
| Unsupported native conversions | 5,275 | 815 |
| Valid / invalid signatures | 53,875 / 0 | 53,875 / 0 |
| Explicit bodies retained / unavailable | 38,861 / 0 | 38,861 / 0 |
| Mapped expression spans | 875,426 | 901,495 |
| Resolved / unresolved mapped queries | 872,731 / 2,695 | 898,793 / 2,702 |

The error reduction is 58.56%, **not** an accuracy or parity percentage. Query
availability can include recovery IR; more mapped expressions can expose more
unresolved queries. Remaining errors include incomplete native constructors
(1,078), unsupported conversions (815), unresolved functions (586), type errors
(346), ambiguous overloads (190), arity errors (126), unresolved members (121),
unresolved references (38), invalid temporaries (6), const-write errors (5) and
unresolved types (4).

Direct exact native provider dispatch and bounded explicit enum/Int32 fallbacks
are now implemented; older fail-closed descriptions above are historical.
Converted overload matching, scripted providers, general identity casts and
wider enum conversion cells remain deferred. See
[implementation findings](../../../../docs/conversion-implementation-notes.md) and
[conversion contracts](../../../../docs/conversion-declarations.md).
No fresh official-compiler, installed-binding or runtime comparison was performed.

## Reproduce and reduce

Start from the exact location in `aggregate.tsv`, then inspect all occurrences in
`diagnostics.tsv` or `diagnostics.log`. Keep the boot-only dependency mode in the
reproduction. Check whether the declaration it needs was excluded before calling
an unresolved symbol or override failure an adapter bug.

Replace installed source with a minimal, authored example under
[`tests/data/`](tests/data/), preserving the relevant syntax, dependencies and
source spans. Add complete diagnostic/IR snapshots using
[`tests/fixtures.rs`](tests/fixtures.rs), or direct semantic assertions using
[`tests/semantics.rs`](tests/semantics.rs). Follow
[regression guidance](../../../../CONTRIBUTING.md#regression-tests). Do not copy
installed scripts or generated reports into committed fixtures.

The example itself has game-independent discovery, location/report, filtering,
empty-module and cross-file-resolution tests:

```sh
cargo test -p redscript-redmod-adapter --locked --example semantic_corpus
cargo clippy -p redscript-redmod-adapter --locked --example semantic_corpus -- -D warnings
```

See the [adapter contracts](README.md) and
[roadmap](../../../../docs/redmod-support-roadmap.md) for limitations and follow-up
work. IDE discovery changes are separate from this example.

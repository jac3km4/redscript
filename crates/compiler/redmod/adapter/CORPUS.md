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
The example overwrites its five report files but does not clear the directory.
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

TSV cells escape backslashes, tabs, CR and LF. Shared codes are retained rather
than collapsed into the IDE wrapper's `OTHER` code. Adapter reasons retain enum
variant names (including modifier kinds) rather than grouping by message wording.
Shared `ErrorAllowedAtRuntime` contributes to errors; `Pedantic` is counted
separately, not silently omitted.

For repeatability, run twice with separate output directories and compare the
five files. A parent process may also change the compiler concurrently: record the
revision and working-tree changes externally when using a run as a checkpoint.

## Local checkpoint (2026-10-30)

The full local directory above contained **2,112 files / 21,873,807 bytes**, including
one `.Script`. Two consecutive runs produced byte-identical reports under
`target/redmod-semantic-corpus/` and `target/redmod-semantic-corpus-first/`.
These counts precede the concurrent modifier/reference acceptance changes; they
are the boot-only baseline, not a claim about the final combined working tree.
A later debug-mode run against those concurrent changes exceeded its 120-second
validation timeout before writing reports (`target/redmod-semantic-corpus-current/`);
no updated counts were obtained from that attempt.

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

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
loader. Do not compare these results to bundle-backed analysis without recording
that difference. Engine definitions and declarations in excluded files are
absent, so unresolved-symbol diagnostics often cascade.

## Discovery and stages

- Recursively discover every regular file with an ASCII-case-insensitive
  `.script` extension, including `.Script`. Include hidden/ignored files; do not
  follow symlinks or junctions. Sort normalized relative paths by ASCII-lowercase
  spelling, breaking ties by original spelling. Read unmodified UTF-8 sources.
  Missing/non-directory/empty corpora and traversal/read errors fail the run.
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
The harness does not change acceptance rules or suppress cascades.

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
| `diagnostics.log` | Full messages with editor-friendly `file:line:column` locations and byte spans; no source snippets |
| `coverage.tsv` | Per-file bridged declarations, signature flags, explicit/retained bodies and written/mapped expression counts; unavailable AST totals are `NA` |
| `expression_queries.tsv` | Exact written spans observed in retained IR, conservative query outcome, mixed-outcome flag and query reasons |

TSV cells escape backslashes, tabs, CR and LF. Shared codes are retained rather
than collapsed into the IDE wrapper's `OTHER` code. Adapter reasons retain enum
variant names (including modifier kinds) rather than grouping by message wording.
Shared `ErrorAllowedAtRuntime` contributes to errors; `Pedantic` is counted
separately, not silently omitted.

For repeatability, run twice with separate output directories and compare the
seven files. Record the compiler revision, working-tree changes and source/input
fingerprints externally when using a run as a checkpoint. Historical observations
are summarized in the [investigation archive](../../../../docs/language-investigations.md)
and [conversion findings](../../../../docs/conversion-implementation-notes.md), not
maintained here as competing current baselines.

## Measurement limits

- Bridged declarations and valid-signature flags do not establish faithful types
  or bindings. Retained bodies include recovery IR, not necessarily complete or
  diagnostic-free checking. Prototypes and synthetic bodies are outside the
  explicit-body denominator.
- Written expression spans include bodies and field/default values, excluding
  imports, empty spans, Omitted/Error placeholders and boot. Mapping requires an
  exact written span on a non-Nop IR expression in a retained function. Unmapped
  expressions can be callee names, folded/default expressions or unavailable
  bodies, not query failures. Generated nodes reusing written spans cannot
  reliably be separated.
- Worst observed query outcome wins: failure, then unresolved, then resolved.
  Resolved means a type other than `Nothing` was returned, not a correct or
  poison-free type. Mixed outcomes are recorded separately. Zero query failures
  on mapped spans says nothing about unvisited or unmapped expressions.
- Diagnostic disappearance, restored declarations and query availability are not
  semantic-accuracy percentages. Binding comparisons need independently authored
  official probes and inspection of selected targets; runtime claims need game
  execution. Do not turn recovery observations into permission to emit output.

## Reproduce and reduce

Start from the exact location in `aggregate.tsv`, then inspect all occurrences in
`diagnostics.tsv` or `diagnostics.log`. Keep the boot-only dependency mode in the
reproduction. Check whether a needed declaration was excluded before calling an
unresolved symbol or override failure an adapter bug.

Replace installed source with a minimal, authored example under
[`tests/data/`](tests/data/), preserving relevant syntax, dependencies and source
spans. Add full diagnostic/IR snapshots using [`tests/fixtures.rs`](tests/fixtures.rs),
or direct semantic assertions using [`tests/semantics.rs`](tests/semantics.rs).
Follow [regression guidance](../../../../CONTRIBUTING.md#regression-tests). Do not
copy installed scripts or generated reports into committed fixtures.

The example has game-independent discovery, location/report, filtering,
empty-module and cross-file-resolution tests:

```sh
cargo test -p redscript-redmod-adapter --locked --example semantic_corpus
cargo clippy -p redscript-redmod-adapter --locked --example semantic_corpus -- -D warnings
```

See the [adapter API](README.md), [language guide](../../../../docs/README.md) and
[future work](../../../../docs/language-future-work.md) for current contracts and
limitations. IDE discovery changes are separate from this example.

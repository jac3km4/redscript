# Local parser corpus check

The installed corpus parsed **2,112 `.script` files / 21,873,807 bytes** with no
lexer/parser diagnostics, missing modules, read failures, panics or invalid spans
(2026-10-29). Files were parsed unmodified into native ASTs; no semantic bridge
participated. This is syntax coverage, not semantic or official-compiler parity.

## Run

From the workspace root in `sh`/Git Bash:

```sh
REDMOD_SCRIPTS_DIR='D:/win/Cyberpunk 2077/tools/redmod/scripts' cargo test -p redscript-redmod-parser --locked --test corpus local_scripts_corpus -- --ignored --exact --nocapture
```

Ordinary tests skip this check. Discovery is case-insensitive, skips symlinks and
sorts relative paths. Missing paths and empty corpora fail. The runner checks
robustness, not a fixed clean-file count for every installation.

Reports are overwritten under ignored `target/redmod-parser-corpus/`:

- `summary.md`: counts; `files.tsv`: per-file outcomes.
- `diagnostics.tsv`: messages and byte/line/Unicode-scalar positions.
- `aggregate.tsv`: grouped diagnostics and example paths.

Do not commit installed sources or generated reports. Reports contain no source
snippets but may include identifiers; review before sharing. For each new failure,
reduce it to an authored fixture under `tests/data/` and snapshot the retained AST
or diagnostics. Preserve unsupported syntax and reject its semantics in the
adapter—never obtain zero errors by silently skipping constructs.

Remaining semantic and IDE work is tracked in the
[roadmap](../../../../docs/redmod-support-roadmap.md).

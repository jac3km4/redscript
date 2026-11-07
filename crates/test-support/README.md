# redscript-test-support

Internal, unpublished helpers for opt-in probes against a trusted local official
REDmod compiler. Successful compilation is **not a runtime guarantee**.

## APIs

- `OfficialCompiler::from_env()` reads absolute `REDMOD_EXE` and
  `REDMOD_GAME_ROOT` paths. It never discovers or downloads an installation.
- `compile(name, source)` / `reject(name, source, diagnostic)` run one source.
  `compile_files(name, sources)` / `reject_files(name, sources, diagnostic)` run
  multiple sources through the same acceptance/rejection checks. Inputs must be
  nonempty and are written byte-for-byte: one source uses `authored.script`;
  multiple sources use `authored-0.script`, `authored-1.script`, etc.
- Each invocation has a 60-second timeout and a disposable working directory.
  `unique_directory(name)` atomically reserves an absolute directory under the
  workspace's `target/redmod-official`. Names allow ASCII letters, digits, `-`
  and `_`. Sources, logs, outputs and side effects are always retained there.
- `OfficialOutput` exposes `directory`, `stdout`, `stderr`, `bundle_bytes()` and
  `assert_version(version)`. Rejection requires a completed nonzero exit and the
  expected diagnostic; spawn, timeout and artifact errors are not rejections.
- `OfficialBundle::new(&bytes)` provides bounded **REDS v14** inspection: exact
  or unambiguous prefix definition lookup, definition indices/names, function
  instructions, and literal pools (header offsets 44, 56, 68 and 92). Function
  decoding supports only the official **v1.2.2 14-byte Profile preamble**, stays
  within the named physical record, and retains `Breakpoint` instructions.
  Unsupported layouts, malformed data, missing names and ambiguity panic.
  Keep the bytes owned by the caller; there is no `OfficialOutput::bundle()`.

## Running and rerunning

Local unit tests and linting do not require the official compiler:

```sh
cargo test -p redscript-test-support
cargo clippy -p redscript-test-support --all-targets --no-deps
```

To rerun official probes, set `REDMOD_EXE` to the trusted compiler executable and
`REDMOD_GAME_ROOT` to its game installation root, both absolute paths, then run
from the workspace root (optionally insert a test-name filter before `--`):

```sh
cargo test -p redscript-redmod-transpiler --test official -- --ignored --nocapture
```

Every rerun reserves new evidence directories; inspect the printed directory and
its `redmod-validation.stdout.log` / `redmod-validation.stderr.log` on failure.
The compiler can read the local game installation; these helpers are not a
sandbox for untrusted executables.

# Experimental Cyberpunk 2077 2.5 full-bundle compatibility

Based on redscript v0.5.31 by jac3km4 and contributors, under the original MIT
licence. This is an unofficial compatibility contribution, not an upstream release.

Based on Antrakt92's upstream PR #203, commit
`c18995d329743e1a89774fe5378b388e3205e08d`:
https://github.com/jac3km4/redscript/pull/203 . That commit skips GitHub CI;
its reported local results are not GitHub check results. This fork additionally
fixes Clippy diagnostics from Rust 1.99 and applies nightly rustfmt.
It also includes Antrakt92's zero-capacity C API error-buffer fix from PR #204
(`a77cd07b97744d358d6ee9f0da8fb79056e7e70a`). The broad dependency refresh in
PR #205 is intentionally separate from this compatibility build.

The 2.5 format keeps version 14 but adds header flag 0x2 and a seventh table
header. There is one 12-byte linkage record per definition, including index zero.
The reader follows stored table offsets and validates linkage count, bounds and
CRC. Unchanged bundles preserve their bytes; edited full bundles regenerate zero
linkage rows with matching counts and checksums. Legacy layout is retained.
External patch linkage (row bit 0) needs resolution against a precompiled base;
this patch rejects it explicitly. Other nonzero producer fields are not fully
understood, so this is not a general patch-bundle implementation.

The SCC signature enum uses four-byte storage, accepted by the official 2.5
REDmod compiler. Serialization finishes before output files are opened, so a
serialization failure does not truncate an existing output file. This does not
make filesystem writes atomic after they start.

## Validation

Synthetic tests cover legacy writing, unchanged extended byte preservation,
definition growth, invalid linkage bounds/count/CRC and external-patch rejection.
No new game cache, decompiled game code or proprietary mod source is included.
The tests build extended input from the existing upstream predef fixture.

Local Windows checks for this fork include 6,235 passing workspace tests,
including the libclang-based C API suite, all-target/all-feature Clippy, and
nightly formatting. The Windows SCC executable and DLL are built with the
release features `mmap,popup`. REDCheatMenu2 v1.2.0 is compiled against a copy of
the installed 2.5 cache; the output is read again and compiled again from its
backup. Header and all seven table CRCs are independently checked with Python.
No proprietary game cache or mod source is part of this repository or package.

The PR author reports additional official SCC and in-game checks in PR #203.
Those checks have not been repeated locally for this fork. In-game behavior of
REDCheatMenu2 and compatibility with other frameworks still need verification.

## Local checks

```text
cargo build --locked --workspace
cargo test --locked --workspace
cargo clippy --locked --workspace --all-features --all-targets
cargo +nightly fmt --all -- --check
cargo build --locked --release -p redscript-cli -p scc-lib -p scc --features mmap,popup
```

The bindgen-based scc-lib-tests suite needs libclang. Build the debug DLL without
`popup` before running its API tests so intentional compiler-error cases do not
wait for an interactive message box.

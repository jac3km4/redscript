# REDmod event return contract (step 8)

REDmod events have an implicit `Bool` result, not an inferred result and not
`Void`. Explicit event return annotations remain invalid. This applies to empty
bodies, callback prototypes, inheritance, additions, replacements, wrappers, and
source calls. Bare `return;` is a Void-to-Bool error; fallthrough is legal.

## Fresh official evidence

Fixtures, invocation scripts, stdout/stderr, and successful bundles are isolated
under `target/parity-investigation/step8-events/`. The executable was
`D:/win/Cyberpunk 2077/tools/redmod/bin/redMod.exe`, with its parent as cwd, using
`validate-mod-scripts -compile -root "D:/win/Cyberpunk 2077" -directory <case>`.
The compiler identifies itself as v1.2.2. No installation inputs were modified.

| Fixture | Official result |
| --- | --- |
| Empty event | Success; Bool return metadata, Profile then Nop |
| `return true;` | Success; Return, debug breakpoint, TrueConst, Nop |
| Conditional Bool return and fallthrough | Success; one Return, trailing Nop |
| Semicolon event in a script class | Success; same empty implementation |
| Bare return | Error: unable to convert const void to Bool |
| Int32 literal / parameter return | Success; invokes `Cast;Int32;Bool` |
| Float parameter return | Error: casting to Bool needs to be explicit |
| String return | Error: unable to convert String to Bool |
| Event called from a Bool-returning method | Success |
| Derived event | Success; Bool metadata and base_method link |
| Callback declared on imported IScriptable / derived implementation | Success; Bool metadata and inherited slot link |

`bytecode.txt`, `inherit-bytecode.txt`, and `native-bytecode.txt` retain the
inspection output. A small disposable inspector uses `BundleReader` to read
function records directly: full `ScriptBundle::from_bytes` cannot load external
type placeholders in these official validation bundles. Some profiling records
in inherited bodies decode oddly; the metadata links are readable. This is not
runtime evidence for a particular value returned by fallthrough. In particular,
there is **no emitted default false return**.

The invented native-class probes fail native binding; follow-up root probes use
actual `IScriptable`. Isolated official wrap/replace root probes fail with
“Function 'OnProbe__annot__…' is not returning any values”. These failures do not
establish official patch bytecode parity. Local patch tests instead exercise
loaded Bool callback metadata, return-value wrapper forwarding, replacement, and
addition through serialization.

## Implementation boundaries

- Adapter normalization supplies Bool signatures before shared resolution.
  Semicolon events in script classes become empty implementations; imported
  callback prototypes stay bodyless in the shared frontend.
- Adapter IR finishing appends Nop only to REDmod callback implementations and
  patches. It does not synthesize a return expression or change community
  callback fallthrough.
- Base callback compatibility requires Bool. Bool callback patches are no longer
  rejected merely for being non-Void. Incompatible community callback contracts
  are not rewritten.
- Callback-specific backend emission retains inherited slot metadata. This is a
  shared metadata correction, not a change to community callback body/return
  semantics.
- Return expressions use ordinary shared coercion. Int32 returns in implicit
  Bool events and ordinary Bool-returning functions are currently rejected, even
  when a matching native `Cast` declaration exists. The return-specific policy
  hook and special Cast lowering have been removed; implicit Bool event contracts
  are unchanged. No numeric, Float, String or object truthiness is introduced.
- The official Int32 literal/parameter acceptance and emitted `Cast;Int32;Bool`
  above remain evidence of an **unsupported conversion**, not local conversion
  support. Official probes also accept Int32 returns in ordinary Bool-returning
  functions. Supporting these cases requires general conversion work with
  evidence-backed applicability and overload selection, rather than a
  return-only exception.

## Native batch review follow-up (2026-11-01)

Fresh official v1.2.2 probes are retained under
`target/parity-integration/official-events/17940-1793562698845071300/`, including
authored sources, command lines, stdout/stderr and successful compiled/hash
artifacts. The trusted executable and invocation are the same as above; fixtures
were never installed and installed sources were not modified.

| Fixture | Official result |
| --- | --- |
| Transpiled Bool callback with conditional and parameter returns | Success, zero warnings |
| Script-class `protected event OnReview();` | Success, zero warnings |
| Importonly-class `protected event OnReview();` | Rejected: cannot have scripted properties nor methods |
| Free `function ReviewFreePrototype();` | Success, zero warnings |
| Free `function ReviewFreeVoidPrototype(): Void;` | Success, zero warnings |
| Free `function ReviewFreeValuePrototype(): Bool;` | Rejected: must return a value |

The adapter now keeps importonly semicolon events bodyless so shared missing-body
validation rejects them, rather than inventing forbidden scripted methods. Script
class events still normalize to empty Bool implementations. Free non-native Void
semicolon functions normalize to empty implementations too; value-returning free
prototypes still require bodies. Community source behavior is unchanged.

Transpilation now preserves Bool callback return expressions directly. It rejects
Void, non-Bool and const-return callback contracts with source diagnostics, even
for prototypes, instead of emitting an incompatible REDmod event. A
transpile-to-adapter semantic roundtrip checks Bool signatures and conditional and
parameter return ownership, beyond native parser acceptance.

The detached switch dispatch/body layout used for first/middle defaults is not
yet reconstructed by the decompiler. It now returns an explicit error instead of
silently moving case bodies into default. Compilation/serialization/decompilation
regressions cover both default positions; declaration-only decompilation remains
available. This is a safe rejection, not support for reordered default bodies.

Review validation uses low-debug builds, `CARGO_INCREMENTAL=0`,
`--target-dir target/parity-integration -j2`. The focused `sources`, `transpile`,
`ordered_switch`, `native_declarations`, `events` and `events_backend` suites
passed (50 tests), and the opt-in official contract test passed all six probes.
Full transpiler/decompiler package suites also passed. A broader run initially
hit disk-full linker failures; after removing generated PDBs it reached an
unrelated adapter `invalid_generic_arity_does_not_validate_override_or_body_contracts`
failure (native parser rejects `Int32` before semantic validation). The full
adapter suite passed with only that test skipped. Existing frontend dead-code
warnings remain. Nightly formatting and scoped diff whitespace checks passed;
workspace-wide `git diff --check` reports an unrelated extra EOF blank line in
the CLI source-failure snapshot. Strict all-targets Clippy for transpiler and
decompiler stops at the two existing frontend dead-code warnings; the same
command with `-D warnings -A dead_code` passes.

## Return-conversion removal validation (2026-11-01)

Regression coverage now requires Int32 literal/parameter returns to be rejected
in events and ordinary Bool-returning functions, including with a native Cast
available. Backend-facing tests stop before emission on those diagnostics; valid
Bool event fallthrough, metadata, inheritance and patch roundtrips remain covered.
No frontend policy test referenced the removed hook.

Using `CARGO_PROFILE_DEV_DEBUG=0 CARGO_PROFILE_TEST_DEBUG=0 CARGO_INCREMENTAL=0`
and `--locked --target-dir target/parity-integration -j2`, the focused adapter
`events`, `events_backend`, `fixtures`, `declaration_compatibility` and frontend
`semantic_policy` commands were attempted. Both were blocked before tests ran by
concurrent edits in `frontend/src/lower/calls.rs` (invalid patterns and cascading
builder errors). These are not passing validation results. Workspace nightly
format checking also reported unrelated const/parser formatting changes; those
agent-owned files were not modified for this removal. Scoped nightly rustfmt
checking (`skip_children=true`) passed for all five changed Rust files, and scoped
`git diff --check` passed for those files and this document.

## Earlier validation

Using the isolated `target/step8-events-tests` build directory:

- `cargo test -p redscript-redmod-adapter --target-dir target/step8-events-tests --test events --test events_backend --test fixtures --test declaration_compatibility`: passed.
- `cargo test -p redscript-redmod-adapter --target-dir target/step8-events-tests --test semantics event`: passed.
- Backend `--test bytecode`: all 4 tests passed.
- Frontend `--test semantic_policy`: all 17 tests passed.
- `rustfmt --check` on the changed implementation/test files passed (stable
  rustfmt warns about the repository's nightly-only formatting options).
- `git diff --check`: passed.
- Broader `--test semantics` run: 24 passed, one failure in
  `persistent_access_uses_declaring_class_ownership_not_receiver_spelling`
  (expected one diagnostic, got none). The unrelated ownership path was not
  changed as part of this task.

No staging or commits were performed.

# Bounded conversion implementation and findings

The broad prototype following [redmod-parity-next.md](redmod-parity-next.md) was
removed before committing: it broadened `lower_static_cast`, overload selection
and shared coercion in `lower/calls.rs` too much. A smaller follow-up is now
implemented; the experiment and discarded-prototype findings below are historical.

## Delivered implementation (2026-11-02)

- `943bf74`: exact native explicit providers, using existing free-function calls.
- `2f3c5b6`: bounded enum/Int32 explicit conversions, using existing intrinsics.
- `1fc6efd`: implicit exact native providers, disabled for out arguments and
  recovery-only overload selection, including recursive script-reference coercion.

Selection and fixed-type evidence live in `lower/conversions.rs`; call lowering
has small application hooks. There is no conversion ranking, chaining, matcher
expansion, new IR, new opcode, or general identity fallback. Existing native
consumer/provider policy gates remain. See [the supported contract](conversion-declarations.md).

Two fresh release corpus runs at `afb485e` produced byte-identical reports across
all seven files, using unchanged installed sources and boot-only dependencies:

| Metric | Before (`33fa996`) | Delivered |
| --- | ---: | ---: |
| Shared errors | 7,999 | 3,315 |
| Unsupported native conversions | 5,275 | 815 |
| Shared warnings | 1,608 | 971 |
| Mapped expression spans | 875,426 | 901,495 |
| Resolved / unresolved mapped queries | 872,731 / 2,695 | 898,793 / 2,702 |

This is a net reduction of 4,684 errors (58.56%), not a semantic-accuracy score.
All 2,112 files reach processing; all 53,875 signatures are valid and all 38,861
explicit bodies are retained (including recovery IR). Total errors match the
experiment, but the final unsupported-conversion grouping differs; do not reuse
experimental category counts as delivered counts. Reports are disposable under
`target/redmod-semantic-corpus-bounded-conversions{,-replay}/`.

Validation passed: stable workspace tests (809 default / 814 all-feature tests,
9 ignored each), both all-target strict Clippy variants, pinned nightly formatting,
release workspace build, all-feature documentation, and standalone frontend,
backend, adapter and API checks. An initial concurrent array snapshot mismatch
cleared after the separate array change landed; both test variants were rerun.
The working tree also contained unrelated in-progress changes during validation.
No new official-compiler comparison, installed-provider ABI reconciliation or
runtime validation was performed. Native declarations do not themselves prove ABI.

## Minimal follow-up experiment (2026-11-02)

A new isolated experiment at `33fa996` measured a substantially smaller approach.
That experiment made no production changes, staging or commits. Baseline replay reproduced
all seven reports byte-for-byte; inputs were the unchanged installed corpus,
embedded boot/default symbols, and no game bundle.

| Isolated variant | Errors | Net reduction from 7,999 |
| --- | ---: | ---: |
| Explicit exact native providers only | 6,829 | 1,170 |
| Enum-to-Int32 fallback only | 6,682 | 1,317 |
| Guarded implicit exact providers only | 6,442 | 1,557 |
| Guarded explicit + enum + implicit | 3,783 | 4,216 (52.71%) |
| Guarded combined + Int32-to-default-enum | 3,315 | 4,684 (58.56%) |

The guarded core reduces unsupported conversions from 5,275 to 819. It reuses
ordinary free-function calls for providers and existing single-type-argument
EnumInt/IntEnum intrinsics for bounded four-byte enum conversions. It introduces
no new IR, opcode, conversion ranking, chaining or conversion-aware matcher.

### Experiment's proposed implementation

1. An isolated helper selecting a unique exact unary native provider for proven
   fixed source/target types. Existing native call binding and return-qualified
   Cast naming handle emission. Require explicit/implicit intent, valid signatures
   and conservative qualifier checks; reject scripted/generic/out/const/skip cases.
2. A small explicit-cast hook after existing Variant/object handling, operating on
   the already-lowered operand. Keep existing ordinary Cast precedence and literal
   contextual typing; native literal retyping is deliberately outside this slice.
3. Bounded enum/Int32 explicit fallbacks using existing intrinsics. The tested
   forward guard excludes Int64 storage/out-of-Int32 declared values; the reverse
   guard accepts only default storage with declared values fitting Int32. No other
   width/signedness cells are implied.
4. A coercion hook for fixed incompatible pairs at known targets, after existing
   reference handling, wrapping the operand once without statement prefixes.
5. An explicit permission passed through argument coercion and recursive
   script-reference adaptation: disable new provider conversions when the typed
   resolver selected its diagnostic-recovery fallback rather than a matched
   overload. Do not expand candidate matching in this first implementation.

The full guarded experimental patch was 184 added / four removed lines in
`calls.rs`, including measurement scaffolding; reverse enums added 22 lines.
That is an experiment-size measurement, not a recommended layout: put provider
selection/type-evidence helpers in a focused module and retain only small hooks
in call lowering. Avoid recreating the broad discarded implementation under a
new filename. No work was applied to production `calls.rs`.

### Important safety discovery

An unrestricted implicit fallback in shared coerce is **not safe**. With implicit
A-to-B and overloads Pick(B)/Pick(C), the current diagnostic-recovery candidate
can become successful; reversing declarations changes acceptance. With both
A-to-B and A-to-C it silently accepts the first converted alternative.

A fixed-pair preflight also proved insufficient: script-reference dereferencing
can expose a provider after the check. Permission must reach the actual provider
application through recursive coercion. The guarded experiment rejects both
orders, leaves normal coercion diagnostics intact, and retains exact-overload and
single-candidate positives. It loses only eight corpus improvements relative to
the unsafe experiment. Converted overload selection is still unsupported, rather
than incorrectly claimed as implemented.

### Evidence and remaining limits

The guarded core passed 54 focused tests, one ignored; the reverse-enum extension
passed 56, one ignored. Synthetic serialization checks covered provider targets,
evaluation once, Skip ordering and four-byte enum instructions. No fresh official
compiler or runtime tests, full workspace checks, malformed-metadata/save-failure
campaign, or installed-provider binding reconciliation were performed. Native
flags and declaration provenance do not independently prove an engine ABI;
production must use the existing supported native-binding contract and test
unavailable/invalid bindings rather than silently invent callable functions.

Fixed evidence means monomorphic nullary data, or equal concrete lower and upper
bounds—not a currently concrete lower bound. Selection must not coalesce/freeze
live variables. Wide enums, scripted Bool/enum providers, weak-qualified targets,
identity, arbitrary String conversion, implicit condition conversions and native
literal retyping remain outside this experiment. Error disappearance is not
binding accuracy: newly reached code can expose further failures.

Detailed reproducible patches, per-mode diagnostic differences and archived
reports are disposable under `target/simple-conversions/experiment.md` and
`target/simple-conversions/guarded-run/`; the companion `census.md` labels
inferred source categories separately from observed pairs. Copied experimental
workspace/build directories were removed; evidence remains. These results are
investigation findings, not delivered conversion support.

## Findings worth retaining

- Existing free-function call IR and backend native binding can emit direct
  conversion-provider calls with return-qualified names such as
  `Cast;Int8;Int64`. A primitive conversion is not necessarily a standalone opcode.
- Calls, assignments, initializers and returns share coercion machinery. The delivered
  implementation reuses one bounded selector/application path rather than
  add per-context conversion exceptions.
- Exact applicable overloads must be considered before provider-converted
  alternatives. Multiple directly converted alternatives remain ambiguous; no
  numeric-distance ranking or conversion chaining is justified by current probes.
- Applicability must be read-only. A variable's current lower bound is not proof
  of a fixed final type. Provider/opcode selection needs concrete evidence or
  properly isolated candidate inference, followed by constraint commitment only
  after unique selection.
- Enum opcode width follows the integer operand/result width, not simply enum
  storage. The prototype encoded generated intrinsic calls with two type arguments
  even though the public intrinsic expects one; this was expedient, not a clean
  shared contract. The delivered bounded fallback instead uses the existing single-type-argument
    intrinsic contract; it does not retain this prototype shortcut.
- Both frontend and backend must validate supported enum/storage/width cells and
  operand/result consistency. Backend validation of metadata shape alone is not
  sufficient. Unsupported emission must prevent saving output.
- Conversion wrappers may preserve existing lazy guards if they produce no
  statement prefixes. New prefix-producing plans remain unsupported without
  expression-local execution. See [lazy limitations](lazy-evaluation-limitations.md).
- Native provider declarations alone do not prove installed ABI/binding. Synthetic
  fixtures validate compiler selection and serialization, not engine reconciliation.
  Arbitrary authored scripted conversions must remain fail-closed.

## Identity casts are not automatically an isolated fix

The prototype identity path became coupled to ordinary Cast precedence,
contextual literal typing and when operands are lowered. It reused native operand
handling across dialects and required extra applicability checks to avoid
replacing community Cast overloads. None of that work was retained.

Before implementing a small identity fallback, specify precedence and require
proven same-representation operands. Preserve contextual literal types, inference
constraints, source spans, evaluation exactly once, and ordinary user Cast
behavior. Do not treat equal-looking weak/strong/script-reference types as
interchangeable or resolve fresh variables merely to claim identity.

## Prototype evidence and limits

Before removal, focused tests passed for a generated 242-cell primitive matrix,
provider use in several coercion contexts, exact-overload preference, converted
ambiguity, identity precedence, serialized provider targets, single evaluation,
lazy ordering and selected opcode-width cases. Those tests and the implementation
were removed together; they are not ongoing regression coverage or delivered
support. No full workspace validation, fresh official comparison, full corpus
replay or runtime validation of the prototype was completed. Malformed-metadata
and compiler-API output-preservation tests were not completed.

The earlier retained batch delivered opaque engine builtins, native dynamic-array
Grow/Count descriptors, shared indexed subtype applicability and contextual
switch literals, each independently tested. It deliberately does not continue
the broad conversion prototype under a different filename.

# REDmod event return contract

Native events have an implicit Bool result, not an inferred result or Void.
Explicit event return annotations remain invalid. The contract applies to empty
bodies, callback prototypes, inheritance, additions, replacements, wrappers and
source calls. Bare `return;` is a Void-to-Bool error; fallthrough is legal.
See the [native guide](redmod-language.md) for dialect context.

## Normalization and metadata

- Script-class semicolon events become empty Bool implementations. Imported
  callback prototypes remain bodyless, retaining callback identity without
  invented bodies or native-method flags. Importonly scripted members still fail
  missing-body/legality validation.
- Free non-native Void semicolon functions normalize to empty implementations;
  value-returning free prototypes still require bodies. This does not change
  community source body requirements.
- Adapter finishing appends Nop to native callback implementations and patches,
  not a return expression or default false value. Community callback fallthrough
  is not rewritten.
- Base callback compatibility requires Bool. Bool patches are not rejected merely
  for being non-Void, and backend emission retains inherited slot metadata.
  Incompatible community callback contracts are not silently rewritten.
- Return expressions use ordinary shared coercion. There is no return-specific
  Int32-to-Bool policy hook or general numeric truthiness. The
  [bounded native provider rules](conversion-declarations.md#guarded-implicit-native-providers)
  apply at known return targets when an exact eligible provider is available;
  this is not unrestricted condition conversion, literal retyping or
  conversion-aware overload matching.

## Export and decompilation boundaries

Transpilation requires a non-const Bool callback contract, including prototypes,
and preserves Bool return expressions directly. Void/non-Bool/const-return
contracts produce source diagnostics, not invalid bare returns after discarding
values. Calls relying on callback results remain rejected by the transpiler.
A transpile-to-adapter roundtrip checks signatures and return ownership, beyond
native parser acceptance.

The decompiler's bounded reconstruction of grouped labels and detached dispatch
for non-final switch defaults is separate from events; unprovable layouts still
reject. See [switch semantics](control-flow-operators.md).

## Historical official evidence

Authored official compiler v1.2.2 probes established:

| Fixture | Observation |
| --- | --- |
| Empty event | Bool metadata; Profile then Nop, no default false return |
| Bool return | Accepted; Return and Bool expression emitted |
| Conditional Bool return with fallthrough | Accepted, trailing Nop |
| Bare return | Rejected as Void-to-Bool |
| Int32 literal/parameter return | Accepted via `Cast;Int32;Bool` |
| Float parameter return | Rejected: needs explicit Bool cast |
| String return | Rejected: unable to convert String to Bool |
| Event called from Bool-returning method | Accepted |
| Derived/imported callback implementation | Bool metadata and inherited base link |
| Script-class protected semicolon event | Accepted |
| Importonly protected semicolon event | Rejected as a scripted member |
| Void free semicolon function, implicit or explicit Void | Accepted |
| Bool free semicolon function | Rejected: must return a value |

The Int32 observations motivated conversion work, not a return-only exception.
An ordinary unmarked community `native func Cast(value: Int32) -> Bool;` does not
qualify as a marked native-dialect implicit provider; such return fixtures still
reject. Do not confuse the existence of a same-named Cast declaration with the
exact eligibility gates in the current bounded-provider contract.

Invented native-class probes failed binding; follow-up probes used actual
IScriptable. Isolated official wrap/replace probes failed with missing return-value
errors, so they did not establish official patch bytecode parity. Local patch
regressions instead check loaded Bool metadata, forwarding, replacement and
addition through serialization.

Some official validation bundles contain external type placeholders and profiling
records that prevent full ScriptBundle decoding. Bounded function-record inspection
establishes only the inspected metadata/instructions. Neither official acceptance
nor local serialization establishes the runtime value returned by fallthrough.
No new official or game-runtime validation is claimed here. See the
[investigation archive](language-investigations.md) for evidence context.

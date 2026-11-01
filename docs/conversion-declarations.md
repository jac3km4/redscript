# Conversion declarations: registration before selection

Step 3 of `redmod-semantic-parity.md` retains conversion providers without
assuming that declaration acceptance proves dispatch semantics.

## Shared representation

`FunctionProperties::conversion: Option<ConversionKind>` distinguishes ordinary
functions, explicit conversions, and implicit conversions. The REDmod bridge
retains both `cast` and `implicit cast` as `Cast` declarations with this property.
Parameter types/qualifiers, return type/constness, body, native/import flag and
source spans continue through the existing declaration path unchanged. Frontend
`FunctionType::conversion()` exposes the intent to queries. Registration does not
make these declarations eligible overloads.

Community syntax spells intent as `implicit func Cast(...) -> T` or
`explicit func Cast(...) -> T`, after item qualifiers (for example,
`native implicit func Cast(...) -> T;`). Formatting round-trips both. Existing
unmarked community `func Cast` functions keep their prior behavior. Binary
function records do not preserve authored intent; loading a bundle does not
invent it from a function name.

## Fail-closed selection

Authored conversions are excluded from ordinary call selection, including in
community consumers of native providers. Explicit cast selection reports
`UNSUPPORTED_NATIVE_CONVERSION` when only unsupported declarations remain for
the target. This historical diagnostic code is retained; its message now covers
shared conversion declarations rather than only native source.

Failed coercions with an exact, valid, nongeneric implicit source/target
signature also receive that diagnostic. This is diagnostic attribution only:
there is no conversion call emission, reference erasure, chaining, cycle search,
ranking, or universal numeric ladder. Other contexts can still report the normal
type/overload error when no exact declaration can be attributed. Existing valid
coercions and exact ordinary overloads are not displaced.

## Fresh official characterization

On 2026-11-01, `target/parity-investigation/step3_probe.py` used fresh directories
under `target/parity-investigation/step3-conversions-1793559287360585700` and the
installed official `redMod.exe validate-mod-scripts -compile`. Success required
both the official success marker and nonempty `final.redscripts`/`.h64` artifacts.
These use the installed engine scripts, not a fabricated standalone dependency
model. Logs, sources, definitions and summary remain in that target directory.

| Fixture | Official result |
| --- | --- |
| Imported Int8 -> Int64 implicit return | Accepted |
| Imported Int8 -> Int64 explicit cast | Accepted |
| Imported Uint8 -> Float implicit return | Accepted |
| Imported Int8 -> Float implicit return | Rejected: needs explicit cast |
| Scripted Data -> Int32 implicit declaration alone | Accepted |
| Same scripted conversion, implicit return use | Rejected: unable to convert `&BData` to Int32 |
| Same scripted conversion, explicit cast use | Rejected: unable to convert `&BData` to Int32 |
| Same declaration plus exact Data/Int32 overload alternatives | Accepted |

These results corroborate the earlier blocker evidence. They do not establish
IR opcode selection, value/ref/const equivalence, ambiguity/ranking, chaining, or
evaluation-once semantics for a general imported conversion implementation. No
new dispatch is enabled in this change, even for the accepted primitive controls.

## Coverage and remaining work

- Formatter regression covers explicit and implicit community declarations.
- Frontend regressions cover multi-file metadata queries and fail-closed explicit
  and implicit uses.
- Adapter regressions cover multiple providers in either registration order,
  imported/scripted intent, source identity, neighboring provider declarations,
  and unsupported explicit use across files.
- An ignored installed-source inventory test accepts `REDMOD_SCRIPTS_DIR` and
  checks all nine foundational files and their 52 implicit/75 explicit declarations.
  It was explicitly run against the installed `tools/redmod/scripts` tree and
  passed for all nine files.
- Full corpus semantic recovery and built-in dispatch are separate acceptance
  steps. Retaining these providers does not claim their consumers all compile.
- Invalid-signature recovery and `skip` behavior are intentionally unchanged.

Validation of this change: focused frontend conversion, adapter conversion/cast/
operator/bridge tests, the installed nine-file inventory, standalone frontend and
adapter checks, and pinned nightly formatting passed. Broader tests encountered
concurrent out-of-scope failures in frontend `qualifiers_are_checked_per_declaration`
and adapter `integer_returns_use_a_verified_native_conversion_not_truthiness`.
Clippy stopped on `collapsible_if` in community AST `visitor.rs:77`.

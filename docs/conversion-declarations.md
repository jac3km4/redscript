# Conversion declarations and bounded native selection

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

## Explicit native providers

Marked conversions remain excluded from ordinary overload selection. Native
consumers may explicitly cast through one exact native `Cast` provider, after
existing Variant extraction and object handling. An ordinary eligible Cast with
a compatible return target retains precedence, even if its arguments later fail.
Literal operands keep ordinary contextual typing; native literal retyping is not
implemented.

Both operand and target must be fixed nullary data types: monomorphic data or
equal concrete lower and upper bounds. Selection and diagnostic attribution are
read-only and never coalesce or freeze live inference variables. Providers must
be valid, native, nonintrinsic, nongeneric, marked implicit or explicit, and
native-dialect declarations. They must have exactly one non-out, non-optional,
non-skip, non-const parameter and a non-const exact return type. Generic types and
reference wrappers do not qualify. Scripted and community-marked providers and
community consumers remain unsupported. Duplicate exact eligible providers fail
conservatively with `UNSUPPORTED_NATIVE_CONVERSION`; no intrinsic or ordinary
fallback is attempted after that ambiguity.

Calls use existing free-function IR and native binding/mangling, including
return-qualified Cast names. No declaration or function is synthesized. As with
other native imports, a declaration is a binding contract, not proof that an
installed engine implements it. Synthetic serialization tests do not verify ABI
availability or game runtime behavior.

Other unsupported declarations retain `UNSUPPORTED_NATIVE_CONVERSION` (including
exact implicit diagnostic attribution), or normal type/overload errors when no
exact declaration can be attributed. Implicit dispatch, chaining, numeric ranking,
identity casts, general ToString, and weak-reference conversions are not enabled.

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
evaluation-once semantics for a general imported conversion implementation. The bounded explicit path above implements only exact native provider calls;
these observations do not establish a general conversion framework.

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
- Full corpus semantic recovery remains a separate acceptance step. Bounded
  provider dispatch does not claim all consumers compile.
- Provider tests cover explicit/implicit intent in explicit casts, conservative
  qualifier gates, scripted/community rejection, duplicate rejection, ordinary
  precedence, contextual literals and operand evaluation once.
- Bundle roundtrips assert the existing return-qualified native binding and a
  single source call. API tests ensure unsupported/invalid provider requests leave
  existing output untouched.
- Invalid-signature recovery and `skip` behavior are otherwise unchanged.

Validation is synthetic compiler-level coverage, not installed-engine binding or
runtime verification. Full corpus replay and broader workspace validation remain
separate follow-ups.

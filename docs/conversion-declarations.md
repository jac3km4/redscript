# Conversion declarations and bounded native selection

Conversion providers retain declaration intent without assuming that declaration
acceptance proves dispatch semantics. See the [language guide](README.md),
[remaining conversion work](language-future-work.md#conversions-and-native-bindings)
and [historical experiment findings](conversion-implementation-notes.md).

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
exact declaration can be attributed. Chaining, numeric ranking, identity casts,
general ToString, and weak-reference conversions are not enabled.

## Bounded explicit enum casts

After ordinary Cast precedence and exact provider selection, native explicit
casts also admit enum -> Int32 when storage is absent or at most four bytes and
all declared values fit signed Int32. Int64 storage is excluded even for small
values. The reverse direction requires exact Int32 and an enum with default
(absent) storage and all declared values fitting Int32; even explicitly written
Int32 storage is outside the reverse slice.

These use existing `EnumInt<E>` / `IntEnum<E>` with one concrete type argument,
typed operands and the normal intrinsic checker. No new IR, metadata, opcode,
integer widths, literal retyping or implicit enum conversion is introduced.
Wrappers preserve the operand's execution point and evaluation count. Declared
value checks are conservative eligibility, not a guarantee about every runtime
enum/integer value.

## Guarded implicit native providers

At known coercion targets (returns, initializers, assignments and selected call
arguments), incompatible fixed non-enum pairs may use a unique exact implicit
native provider under the same signature gates. Existing valid coercions retain
precedence. Explicit-only providers, implicit enum conversions, chaining and
conversion-aware overload matching/ranking remain unsupported.

Permission is passed explicitly through argument coercion and recursive
script-reference adaptation. Out arguments never enable providers. A typed
resolver's recovery-only fallback (`matches.is_empty()`) also disables them at
the point of application, including after dereferencing or later inference.
Single-candidate source resolution and genuinely matched signatures retain
permission. There is no global state, speculative conversion preflight or second
matching pass. When permission is false, normal coercion and diagnostic recovery
still run; declaration order can change the existing diagnostic but must not
turn a converted overload alternative into success.

Wrappers own the already-lowered operand once and add no statement prefixes.
Lazy/skip arguments retain their existing guards and execution point. This does
not add condition truthiness or general provider-aware operator applicability.

## Historical official characterization

Authored probes on 2026-11-01 used the installed official compiler and engine
scripts, not a fabricated standalone dependency model. Success required both the
official success marker and fresh nonempty compiled/hash artifacts. Disposable
local probe directories are not requirements for this contract.

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
evaluation-once semantics for a general imported conversion implementation. The bounded paths above implement only exact native provider calls;
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
  This is an optional installed-source check, not a portable test dependency.
- Full corpus semantic recovery remains a separate acceptance step. Bounded
  provider dispatch does not claim all consumers compile.
- Provider tests cover explicit/implicit intent in explicit casts, conservative
  qualifier gates, scripted/community rejection, duplicate rejection, ordinary
  precedence, contextual literals and operand evaluation once.
- Guarded implicit tests cover one/two converted overload alternatives in both
  orders, script-reference and generic recovery, exact/single-candidate positives,
  known-target contexts, omitted slots, unrelated bad arguments, out/qualifier
  rejection, no chaining and lazy operand nesting.
- Bundle roundtrips assert the existing return-qualified native binding and a
  single source call. API tests ensure unsupported/invalid provider requests leave
  existing output untouched.
- Invalid-signature recovery and `skip` behavior are otherwise unchanged.

Synthetic compiler-level coverage is not installed-engine binding or runtime
verification. Historical corpus measurements are in the
[investigation archive](conversion-implementation-notes.md), not a current-HEAD
validation claim.

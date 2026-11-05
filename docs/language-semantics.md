# Shared language semantics

This reference collects shared frontend, backend and source-recovery contracts.
Read the [community guide](community-language.md) or [native guide](redmod-language.md)
first for dialect context. Native exceptions below are per-file policies, not
relaxations for community dependencies in mixed compilations.

The sections follow declarations and initialization through execution, lookup and
recovery. Each topic keeps its implementation boundaries, regression evidence and
caveats together. Remaining goals (including transactional inference) are labeled
as such; they are not delivered support. See [future work](language-future-work.md)
for the cross-cutting backlog, [REDmod-specific semantics](redmod-semantics.md) for
target contracts, and [historical investigations](language-investigations.md) for
past measurements rather than current validation.

## Contents

- [Declarations, types and initialization](#declarations-types-and-initialization)
  - [Explicit enum storage](#explicit-enum-storage)
    - [Bundle representation](#bundle-representation)
    - [Official characterization (2026-10-30)](#official-characterization-2026-10-30)
  - [Const parameters and locals](#const-parameters-and-locals)
    - [Const locals](#const-locals)
    - [Export and decompilation](#export-and-decompilation)
    - [Analysis bounds](#analysis-bounds)
  - [Shared expression and aggregate-default APIs](#shared-expression-and-aggregate-default-apis)
    - [Const checks](#const-checks)
    - [Shared aggregate defaults](#shared-aggregate-defaults)
    - [Community omitted arguments](#community-omitted-arguments)
    - [Array literal evaluation boundary](#array-literal-evaluation-boundary)
  - [Conversion declarations and bounded native selection](#conversion-declarations-and-bounded-native-selection)
    - [Shared representation](#shared-representation)
    - [Explicit native providers](#explicit-native-providers)
    - [Bounded explicit enum casts](#bounded-explicit-enum-casts)
    - [Guarded implicit native providers](#guarded-implicit-native-providers)
    - [Historical official characterization](#historical-official-characterization)
    - [Coverage and remaining work](#coverage-and-remaining-work)
- [Evaluation and control flow](#evaluation-and-control-flow)
  - [Switch order and overloaded indexing](#switch-order-and-overloaded-indexing)
    - [Ordered switch defaults](#ordered-switch-defaults)
    - [Overloaded indexing](#overloaded-indexing)
  - [Lazy evaluation limitations](#lazy-evaluation-limitations)
    - [Typed resolver boundary](#typed-resolver-boundary)
- [Lookup, inference and diagnostic recovery](#lookup-inference-and-diagnostic-recovery)
  - [Call lookup, overloads and inheritance](#call-lookup-overloads-and-inheritance)
    - [Native implicit lookup](#native-implicit-lookup)
    - [Overloads and generated forwarders](#overloads-and-generated-forwarders)
    - [Override slots and access](#override-slots-and-access)
    - [Recovery and evidence](#recovery-and-evidence)
  - [Type inference safeguards and hardening](#type-inference-safeguards-and-hardening)
    - [Current mechanisms](#current-mechanisms)
    - [Remaining goal: atomic failed operations](#remaining-goal-atomic-failed-operations)
    - [Ongoing correctness criteria](#ongoing-correctness-criteria)
  - [Invalid signature recovery](#invalid-signature-recovery)
    - [Boundaries](#boundaries)

## Declarations, types and initialization

These contracts distinguish source declarations from stored representation and
execution-site initialization. Enum metadata, const validation and aggregate
default properties each impose different guarantees; conversion declarations
record intent separately from dispatch eligibility.

### Explicit enum storage

See the [language guide](README.md) for dialect context and
[conversion semantics](#bounded-explicit-enum-casts)
for the separately bounded enum/Int32 cast paths.

Both community and native source accept `enum Choice: Uint8 { A = 0, B = 255 }`.
Legal storage types are `Int8`, `Uint8`, `Int16`, `Uint16`, `Int32`, `Uint32`,
and `Int64`. `Uint64` and noninteger types are rejected. Explicit values and
implicit increments must fit the declared signed/unsigned range. Existing
maximum-so-far increment behavior is unchanged.

Community enum literals use the existing `l` suffix for values beyond Int32.
Native enum declaration literals are unsuffixed signed 64-bit decimal values;
this does not widen ordinary native expression literals. The official compiler
rejects the spelling `-9223372036854775808`, so native parsing/printing and export
do not claim support for that literal. Community source can use it with `l`.

Omitting storage preserves existing community behavior: signed widths are inferred
from the values, including zero (1, 2, 4, or 8 bytes). Native omitted storage keeps
the adapter's existing behavior too; official REDmod instead defaults to 4 bytes.
Use explicit storage when matching native representation matters.

The shared AST's existing `Enum::new` constructor still omits storage; use
`with_storage` to supply it. Resolved enum schemas expose `storage()` and
`storage_size()`. Storage syntax retains its source type span for queries.

#### Bundle representation

Version-14 bundles store visibility, one byte of width, member indices and a
native flag. Member values are signed 64-bit integers independent of storage.
There is no signedness field. Source schemas retain exact declared signedness;
loaded schemas expose the stored width and **unknown** signedness (`storage()`
returns `None`). Re-emission preserves loaded widths instead of recomputing them.

Decompilation prints explicit storage to preserve width. It chooses the signed
type when all values fit, otherwise the unsigned type of the same width. This is
a canonical representable declaration, not recovery of the original signedness.
Invalid widths/unrepresentable sets fail rather than silently narrowing.

#### Official characterization (2026-10-30)

Local REDmod `REDscript Compiler v1.2.2` accepted all seven legal storage types and
their boundaries (Int64 through +/-9223372036854775807), including Uint32's
4294967295. It rejected Uint64, Float, Bool and CName storage, negative Uint8 and
Uint32 values, Int8 128, Uint8 256, and incrementing Uint8 255. It rejected the
Int64 minimum spelling above during token conversion.

Inspection of freshly compiled authored bundles found widths 1/2/4/8 as declared,
4 for omitted storage, and identical enum payload metadata for Int8 and Uint8
with the same value. Signedness is checked at compile time but erased on disk.
These probes establish compilation/serialization behavior, not game-runtime
behavior. Disposable local probe artifacts are not required to use this contract.

Portable tests use authored fixtures, not installed game scripts. The opt-in
`authored_enum_storage_matches_official_bounds_and_bundle_representation` test in
`crates/compiler/redmod/transpiler/tests/official.rs` reproduces the key checks
with `REDMOD_EXE` and `REDMOD_GAME_ROOT` set to a trusted installation.

### Const parameters and locals

See the [language guide](README.md) for dialect context and
[future work](language-future-work.md#analysis-lookup-and-constness) for broader
constness gaps.

Const parameter flags are enforced by shared frontend analysis after function
bodies have been lowered and calls resolved. Community source uses the strict
semantic policy by default. Diagnostics are shared frontend errors (`OTHER`),
not REDmod capability errors. Proven writes use hard `Error` severity with
`cannot write through a const parameter` (or `const local`). Uncertain non-out
mutable forwarding and function-value forwarding use `ErrorAllowedAtRuntime`
with `forwarding a const ... may allow mutation`: strict mode rejects them,
while CLI `--runtime-compatible` and SCC tolerate them. This does not establish
that the callee is harmless or provide transitive immutability. See the
[mode contract](community-language.md#diagnostics-and-tool-boundaries).

Both policies reject assignment (including compound/value-producing assignment)
to const parameters or their fields, explicit `out` forwarding, and mutating array
intrinsics. Provenance follows inherited fields, script-reference `Deref`/`AsRef`,
strong/weak-reference `WeakRefToRef`/`RefToWeakRef` conversions (explicit or
inserted by coercion), casts, and both arms of conditional expressions. Array reads and ordinary scalar
by-value arguments are allowed. This is parameter enforcement, not new enforcement
of const fields, const returns, or const methods.

Two independent, per-source-file `SemanticPolicy` flags provide the characterized
REDmod exceptions; both are false by default and true in the adapter's `POLICY`:

- `const_param_element_writes`: indexed element places stop const provenance.
  Assigning elements (including fields within them) or passing them as `out` is
  permitted. Replacing the entire array or mutating its size is still rejected.
- `const_param_non_out_forwarding`: non-`out` forwarding is permitted, including
  to non-const reference parameters. It does not relax explicit `out` arguments
  or known mutating array intrinsics.

Strict forwarding checks use resolved non-const parameter types: script/strong/
weak references, arrays and class objects may expose mutable state. Known const
parameters remain legal targets. Function-value calls have no parameter flags;
analysis checks their resolved reference/container argument types instead. These
non-out checks diagnose possible mutation, not a proven write, and use the
runtime-tolerated severity above. Direct assignment, explicit `out`, known
mutating array intrinsics and the existing non-const local-receiver checks remain
hard errors in both modes. Native semantic policy flags and their exemptions are
unchanged; choosing a fatal threshold does not enable a native policy.
User method receivers are not presumed mutating merely because a method is
non-const: receiver effects are not characterized by this pass.

#### Const locals

Community syntax uses `const name: Type;`, `const name = value;`, and
`const name: Type = value;`. The former `const let` syntax is rejected; native
REDmod syntax remains `const var name: Type;`.
The AST qualifier and `ir::LocalInfo::is_const` survive bridging, hoisting and
lowering; constness is not encoded by changing or erasing the value's type.
The shared post-resolution validator distinguishes const parameter roots from
const local roots, and binding assignment from referent mutation.

Strict community defaults allow the declaration initializer exactly once, for
both inferred and explicitly typed const locals. All other binding assignments
are rejected, including the first assignment after an uninitialized declaration;
this is intentional, not definite-assignment analysis. Field/index writes,
mutating array intrinsics and explicit `out` forwarding remain hard errors.
Ordinary forwarding to mutable reference/container parameters and uncertain
function-value forwarding are diagnosed at runtime-tolerated severity and rejected
by the default strict threshold.
Scalar by-value reads and forwarding to const parameters remain legal. Non-const
instance methods on const local referents are rejected; const methods are legal.
Parameter receiver behavior is unchanged.

The independent, per-file `const_local_handle_rebinding` policy defaults to false
and is enabled by REDmod. For non-struct class objects and strong/weak class
handles, it permits repeated root assignment, inline initialization, and ordinary
or `out` forwarding of the handle. It does not permit field/index mutation,
forwarding a field as `out`, mutating array intrinsics, or non-const receiver
methods. Scalars, structs, arrays and script references do not get this exception.
Under this native policy, default storage initialization is not an explicit source
assignment; scalar/struct assignment, even the first assignment or an inline
initializer, is rejected. In particular, historical official compiler probes
record const Int32 inline initialization failing with “Value on the left side is
constant. Unable to assign to it.” This native restriction is deliberately distinct
from community initialization; it is not inferred from ordinary non-const
initializer acceptance.

`LocalInfo::initializer` records the exact declaration-store span during lowering,
including hoisted declarations. Validation matches both local ID and that span,
consumes the exemption once, and uses separate metadata for each closure scope.
The initializer value is still visited: nested binding writes, captured writes,
forwarding and referent mutation receive no initialization exemption. No new IR
expression variant or backend assignment behavior is introduced.

Local provenance follows inherited fields, indexes, dynamic casts, conditional
branches, script-reference wrappers, strong/weak conversions (including inserted
coercions), and closure captures. Captured bindings retain their binding-versus-
referent distinction; nested closure locals use their own metadata.

This is validation of the selected resolved method, not const-aware overload
ranking. Const returns, const fields and arbitrary method effects remain outside
this pass. No runtime serialization or full installed-corpus coverage measurement
is implied by the focused tests.

#### Export and decompilation

The REDmod transpiler explicitly rejects const locals rather than dropping their
qualifier or emitting native declarations with different initialization rules.
Decompilation preserves ordinary const declarations whose initializer can be
reconstructed, as well as read-only const locals using their VM-default value.
When a const initializer cannot be preserved at its execution site (including
expanded array writes or initialization inside expressions), it logs a warning
identifying the function and local and emits mutable `let` for that local only.
Other initialized/default const locals keep their qualifier. This is a lossy
source-recovery fallback, not a relaxation of compiler const rules; it avoids an
uninitialized const followed by an illegal assignment. It does not add const-local
support to the transpiler.

#### Analysis bounds

This is a direct-root check, not an interprocedural effect or alias analysis.
Copying a reference into a local or returning it does not propagate constness.
Calls returning references and generic parameters without a concrete mutable bound
can hide provenance or mutation. The pass does not infer effects of arbitrary
intrinsics beyond the known mutating array operations.

Closure bodies are visited, captured const roots remain checked, and closure
parameters are not mistaken for enclosing parameters with the same numeric ID.
Nested closures retain captured roots. The current IR identifies captures by
`Local` only, without defining-scope identity: nested captures with colliding local
IDs can be conservatively treated as const. Lambda syntax does not currently
accept const parameter qualifiers. These limits must not be interpreted as a
transitive immutability guarantee.

### Shared expression and aggregate-default APIs

See the [language guide](README.md) for dialect context. Community and REDmod
inputs share these frontend rules:

- An assignment yields its coerced destination type. Chains, arguments, returns,
  and conditional branches can use that value. Typed IR has one `Expr::Assign`
  with a `typ` field. The destination is evaluated once, before the RHS; the result
  is a temporary, not an assignable place. Assembly emits `Assign` without destination readback.
- Bodyless callbacks on native, non-import-only aggregates retain callback flags
  without becoming native methods or acquiring synthetic bodies. Scripted and
  import-only aggregates and ordinary methods retain their body checks. Native
  event normalization is detailed in [event semantics](redmod-semantics.md#redmod-event-return-contract).
- Script-reference coercions compose with other conversions: unwrap before
  converting to a value, or convert to the pointee type before wrapping. Generated
  conversions preserve the exact pointee, including weak wrappers. Member, method,
  and index receivers unwrap script references without a policy flag.

`script_ref_write_through` controls only assignment destinations and `out`
arguments: REDmod writes/forwards the referenced
place, while community source assigns/forwards the reference itself. It remains
file-local and defaults to false. `implicit_members`, `public_fields`, and
`native_object_cast` remain dialect policies, unchanged.

#### Const checks

See [const parameters and locals](#const-parameters-and-locals) for strict community
validation, runtime-tolerated uncertain forwarding, narrow per-file native
relaxations and alias/closure bounds. Constness is a direct-root validation
contract, not transitive immutability.

#### Shared aggregate defaults

Both languages accept aggregate members such as `default value = -1;`, including
own and inherited targets in classes and structs. The shared AST represents them
as `Item::Default(FieldDefault)` (`SourceFieldDefault` for spanned inputs), with
`target`, `value`, and optional `literal_text`. Community parsing/formatting and
AST visitors preserve these nodes; the REDmod adapter retains contiguous numeric
source spelling in `literal_text` where native metadata requires it.

The frontend consumes these AST members directly: no private adapter default plan,
supplemental input collection, or separate processing entry point is required.
Bare shared-AST callers retain defaults. REDmod's remaining native loop-fixup
plans still require complete `ParsedModule` values and `process_sources`; the
bare-AST API fails closed for those loop constructs, not defaults.

After inheritance binding, the frontend resolves target fields and checks bounded
literal/enum constants against their substituted field types. This is not general
constant-expression evaluation. `LoweredClass::defaults` contains ordered
`LoweredFieldDefault` entries with resolved `FieldId`, constant value, optional
literal spelling, and span. The supplying aggregate is the containing class entry;
the field identity may belong to a base.

The backend merges properties onto the declaring field under supplying-class
keys, without initialization bytecode or loss of existing base/sibling/bundle
entries. Own-field defaults are first-wins in source order (including inline
initializers); inherited defaults are last-wins per supplying class. Type checking
is intentionally stricter than official inherited defaults that accept some
invalid text. Parenthesized/comment-separated numeric forms use typed formatting
rather than retained spelling.

#### Community omitted arguments

Leading and doubled commas denote omitted positional arguments, checked against
optional parameters. A trailing comma does not add a slot: `Target(42,)` has one
argument, `Target(,)` has one hole, and `Target(42,,)` has a value followed by a
hole. Formatting preserves this arity in compact and multiline calls. REDmod
native syntax retains its final-hole behavior: `Target(42,)` includes a final
omitted slot there.

#### Array literal evaluation boundary

Array literals currently construct temporary arrays in statement prefixes, not
inside expression bytecode. Shared lowering reports
`UNSAFE_ARRAY_EVALUATION` (`Warning`) when this could move
construction past an earlier observable expression or out of a lazy branch.
This includes assignments inside
later array arguments/elements, complex assignment destinations with array RHSs,
logical RHSs, and conditional arms. Literal-only nested arrays and ordinary local
array initialization remain supported. Omitted argument slots have no evaluation
and are prefix-inert, so `Take(, [1])` is allowed when the first parameter is
optional. Type-qualified static calls such as `Host.Take([1])` have no runtime
callee evaluation and are accepted. Runtime member callees and closure/function-value
callees remain guarded: a prefix could change the callable before it is read.
The guard does not infer purity from function names and can diagnose some pure
expressions too. It reports a warning and continues legacy lowering in all modes, emitting the
existing statement-prefix behavior. That behavior may reorder or eagerly evaluate
expressions and has no correctness guarantee; the diagnostic is not an
expression-local lowering fix.

Extract the array to a local at its intended execution site, after any preceding
effects; use explicit control flow for lazy branches. This guard is not a new
expression-local array construction facility. See
[lazy evaluation limitations](#lazy-evaluation-limitations) for skipped arguments
and typed-call boundaries. REDmod capability checks and the transpiler's additional
prefix restrictions remain in place. Bytecode snapshots verify instruction
structure, not game-runtime execution.

### Conversion declarations and bounded native selection

Conversion providers retain declaration intent without assuming that declaration
acceptance proves dispatch semantics. See the [language guide](README.md),
[remaining conversion work](language-future-work.md#conversions-and-native-bindings)
and [historical experiment findings](language-investigations.md#conversion-investigation-archive).

#### Shared representation

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

#### Explicit native providers

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

#### Bounded explicit enum casts

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

#### Guarded implicit native providers

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

#### Historical official characterization

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

#### Coverage and remaining work

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
[investigation archive](language-investigations.md#conversion-investigation-archive), not a current-HEAD
validation claim.

## Evaluation and control flow

Control-flow representation preserves source order where it can be proven.
Array-prefix lowering has a separate safety boundary: supported switch or call
syntax does not make every nested lazy expression safe to emit.

### Switch order and overloaded indexing

See the [language index](README.md) for dialect guides and
[future work](language-future-work.md) for remaining capabilities. These features
are implemented, not remaining native syntax gaps.

#### Ordered switch defaults

Shared AST and IR retain case bodies plus `default_index`, the number of case
labels preceding default. Together these encode label order without moving or
duplicating bodies. Both parsers reject duplicate defaults; visitors, formatters
and native transpilation use source label order.

For a non-final default, backend dispatch tests matchers in case order and jumps
to the selected body, or to default after no match. Bodies remain in source order
for fallthrough, and the scrutinee is emitted once. Switch break targets are local;
enclosing loop continue targets survive. The final-default bytecode layout is
unchanged. Pattern cases reject fallthrough into a binding-dependent body,
including from a preceding default.

The decompiler reconstructs grouped labels, backward-linked labels sharing a
body, and detached dispatch with first/middle default bodies. It preserves matcher
evaluation order, shared bodies, source body order and fallthrough where the layout
can be proven. This is not arbitrary backward control flow: dispatch that moves
backward between distinct bodies, cyclic/invalid links, or unprovable body/exit
boundaries still errors rather than guessing. Declaration-only decompilation
remains available.

Contextual switch literals use the scrutinee's type. This does not enable all
Variant switch rules or script-reference scrutinee adaptation. Lazy matcher/prefix
shapes remain subject to [evaluation safety](#lazy-evaluation-limitations).

Regression tests cover first/middle defaults, fallthrough, break/continue,
formatting, serialized dispatch/body offsets and single scrutinee emission.
They do not execute bundles in the game.

#### Overloaded indexing

Non-array receivers resolve `OperatorArray(receiver, key)` through the shared
free-function call builder, preserving overload resolution, parameter/out checks,
result categories and argument order. The result is a call, not an array place.

Dynamic/static arrays retain built-in Int32 indexing and writable-place semantics,
even if an OperatorArray overload is visible. Native `operator[]` maps to
OperatorArray and requires two parameters. Community code can declare
`func operator[](out value: Data, key: String) -> Bool` or use the canonical
function name. Backend OperatorArray mangling includes parameter and result types,
matching observed official signatures.

Regression coverage includes native/community declarations, out-temporary
rejection, nonassignable results, invalid key types, built-in precedence and
serialized calls. Official acceptance of an out receiver/String key/Bool result
does not make that result assignable or prove runtime semantics.

### Lazy evaluation limitations

See the [language guide](README.md) and
[array literal boundary](#array-literal-evaluation-boundary)
for eager callee/operand ordering, including static versus runtime callees.

Source `skip` parameters retain lazy evaluation for supported expressions. This
is not general expression-local initialization: array literals still lower to
statement-prefix initialization, and expression-local initialization remains
unimplemented.

Shared call lowering checks the source arguments of the selected overload. It
reports `UNSAFE_ARRAY_EVALUATION` for array literals anywhere within a skipped
argument, including empty and nested arrays and arrays inside calls, casts,
receiver expressions, indexing,
construction, or interpolation. A static forwarder with a skipped `self` parameter
also checks its source receiver. An earlier eager argument may construct an array
without making a later prefix-free skipped argument unsafe. Unselected lazy
overloads do not diagnose an otherwise safe eager call.

`UNSAFE_ARRAY_EVALUATION` has severity `Warning`. Lowering reports it and
continues the legacy statement-prefix path rather than dropping the call or body.
The warning does not prevent output in strict or runtime-compatible modes.
Emitted code can therefore retain reordered or eager array evaluation, including
outside a lazy branch; it is **not a correctness guarantee**. Hard errors still prevent
output. See the [mode contract](community-language.md#diagnostics-and-tool-boundaries).

These source checks deliberately diagnose some forms that could be safe with more
precise control-flow information. They do not prove that a branch is reachable or
that an initializer is pure. Bind an array at its intended execution site before
passing the local; do not move initialization outside a lazy branch if that would
change observable behavior. Closure bodies have their own execution/lowering
scope, so an array in a closure body is not initialization of the closure operand.

#### Typed resolver boundary

Some operators and compiler-generated calls enter the typed resolver with IR
rather than source operands. IR no longer identifies statement-prefix producers.
For a selected declaration with a skipped argument, this source-free path
conservatively rejects with the distinct hard error `UNSUPPORTED_SKIP_ARGUMENT`,
even for a literal or local, except for the already source-guarded logical RHS
described below.
Ordinary source calls and native cast conversion fallback supply their source
operands and receive the more targeted checks above.

Logical `&&`/`||` RHS array construction is diagnosed by the source array guard
before typed operator dispatch, with the per-mode behavior above. Generated
logical calls use prefix-free operands.
This exception applies only to the RHS, not a user-declared skipped LHS.
Intrinsics with `skip` remain invalid at assembly. Argument coercions currently
wrap expression IR and do not
emit statement prefixes. If coercions or new expression forms begin producing
prefix initialization, their source guards must be extended before allowing them
in lazy positions; do not infer safety from an unchanged prefix-vector length.

The REDmod adapter may reject unsafe forms earlier than shared call lowering.
Adapter capability errors remain hard; the shared array diagnostic retains its
per-mode fatality. Neither strict rejection nor legacy compatibility acceptance
is evidence of runtime equivalence with the official compiler.
Expression-local initialization and corresponding
regressions remain tracked in [future work](language-future-work.md#evaluation-and-initialization).

## Lookup, inference and diagnostic recovery

Lookup must select verified declarations without using recovery as applicability
evidence. The solver safeguards below do not yet provide transactional rollback,
and invalid-signature recovery deliberately leaves some bodies unavailable.

### Call lookup, overloads and inheritance

These are shared frontend rules unless a native policy is stated. See the
[community](community-language.md) and [REDmod](redmod-language.md) guides for
dialect context, and [future work](language-future-work.md) for unsupported cells.

#### Native implicit lookup

Native lookup considers local/parameter callable values, eligible implicit methods,
type construction and then free functions. Eligible methods precede same-named
types; community lookup is unchanged. Native locals are function-wide and shadow
implicit members. One official type-versus-local probe selected the type instead;
local-first behavior remains an intentional difference.

Implicit static calls carry the enclosing type's semantic identity across module
boundaries, rather than looking up a short type name in the caller's scope.
This does not justify dropping a runtime receiver's evaluation or accepting all
static/free calls through value receivers. Lookup must use applicability, not
catch lowering errors and retry after mutating inference or evaluating operands.

#### Overloads and generated forwarders

Shared selection may choose a unique undominated applicable reference-parameter
signature. This preference is deliberately bounded to equal parameter counts and
flags, concrete nongeneric class parameters and matching strong-reference
representation (or bare class parameters). Every differing parameter must narrow
along declared ancestry. Crossing preferences remain ambiguous.

Omitted/null slots with differing types, out parameters, weak/script-reference
categories, Variant and numeric conversions do not establish this specificity.
These are implementation boundaries, not evidence that official REDmod rejects
all such cases. Optional-tail overlap remains ambiguous; there is no blanket
exact-arity/fewest-defaults preference or return-type-only selection.

Generated receiver-first static forwarders remain queryable fallback aliases, not
authored override declarations. Real instance methods suppress aliases with the
same effective parameter types across ancestry, independent of declaration order;
other alias overloads remain. Bodyless imported static declarations also receive
aliases. Forwarded receivers are evaluated once, first, and checked using the
original receiver parameter's out/ref rules.

Qualified static calls gather candidates across ancestry. Nearer declarations
hide only identical parameter signatures; inherited generic parameters are
substituted into the caller's type context before comparison. A same-named
instance method does not hide a base static. Same-class duplicates are retained
for diagnostics, and selected targets still undergo visibility checks. Native
inherited-static style warnings are suppressed through source policy, not by
changing call accessibility.

#### Override slots and access

Symbols retain immediate override predecessors and expose the canonical original
`override_slot`. Return contracts are instantiated through generic bases:

- Community overrides enforce covariance against the nearest override.
- REDmod overrides validate against the original slot, allowing bounded sibling
  returns and widening back to the root contract. Official probes distinguish
  this from arbitrary widening against an intermediate declaration.
- Covariance is restricted to the same strong/weak class-reference representation.
  Arrays, structs, script references and representation changes remain exact-only.

Source-visible return types and emitted return metadata remain declaration-specific.
Native-slot mangling follows the root through scripted overrides. Native policy
permits narrowed override visibility, but call-site access still applies and
omitted visibility still inherits. Written overrides retain final-method and
out-flag checks; native const/optional parameter flags may differ from the base.
Out-flag ABI differences require more characterization before relaxing validation.

Persistent-field access uses declaring-class ownership, not receiver spelling.
Declaring classes and descendants may access protected persistent fields, including
through base-typed receivers. Unrelated external/private restrictions remain;
this is not removal of persistent validation. Imported native classes and structs
retain their separate existing policy.

#### Recovery and evidence

[Invalid signatures](#invalid-signature-recovery) are not normal overload,
duplicate or override inputs and do not produce generated forwarders. A valid
sibling may still be analyzed without making the compilation successful.

Frontend/native regressions inspect selected IR identities; backend tests check
serialized targets, receiver order and override metadata. Historical official
probes established child-parameter preference, real-method precedence,
optional-tail ambiguity and bounded original-slot returns. These observations do
not establish all overload categories, full-corpus accuracy or runtime dispatch.

### Type inference safeguards and hardening

This separates current solver mechanisms from remaining design work. It replaces
an older transaction/cycle/join implementation plan whose cycle and generic-bound
sections were partly superseded. See the [language index](README.md) and
[future-work list](language-future-work.md#analysis-lookup-and-constness).

#### Current mechanisms

`crates/compiler/frontend/src/lower/types.rs` retains variable bounds and directed
subtype edges. Variable-to-variable subtyping is not generally equality;
strongly connected components may merge without equating unrelated variables.
Invariant constraints retain both bounds. Rigid annotation parameters are checked
through proven bounds, not treated as inference variables merely because their
upper bound resembles a requested type.

Application joins project operands to a nominal common base before combining
substituted arguments. Same-constructor merging composes parameter variance:

| Parameter variance | Join | Meet |
| --- | --- | --- |
| Covariant | Join arguments | Meet arguments |
| Contravariant | Meet arguments | Join arguments |
| Invariant | Enforce equality | Enforce equality |

Different-constructor meets build a nominally narrower candidate and constrain it
against both inputs instead of discarding the wider operand's generic arguments.
Indexed derived-object evidence participates in read-only applicability for base
parameters without selecting/freezing live variables.

Bound refinement performs an occurs check, validates prospective local bounds and
then stores them before neighbor propagation. Coalescing has independent cycle
protection. Ownership components retain internal bound/edge graphs while external
handles live and clear links when the last external owner is dropped. This permits
constraint cycles without intentionally adding recursive-type support or retaining
unreachable Rc cycles. Rejected constraints clear update guards/remove newly added
edges; these local safeguards are **not whole-operation transactional rollback**.

Solver regressions cover directional edges, invariant constraints, late bounds,
cyclic components, ownership release, recursive coalescing and generic joins/meets.
Their existence is not a claim that validation was rerun for this documentation.

#### Remaining goal: atomic failed operations

A recursive constraint operation can mutate earlier/nested variables before a
later step fails. Local validate-before-store does not restore all reachable
state. Transactional failure handling remains a separate correctness goal.

A possible implementation is an internal undo journal with checkpoints, commit
and rollback shared across the entire recursive operation. Keep public operations
as wrappers rather than committing each recursive step independently. Account for
bounds, representatives/path compression, directed edges and ownership-component
changes in the **current** representation; the old two-bound-only plan is
insufficient. Do not hold RefMut across recursive calls.

Separate speculative callers into:

- **Try-and-commit:** retain successful constraints and roll back failures, for
  compatible contextual hints.
- **Probe:** always roll back, including success, for read-only questions.

Audit ignored return hints, dynamic-cast alternative checks and fallback paths
that discard constraint errors. Read-only applicability must not resolve live
variables merely to select a provider or overload.

Required regressions should prove unchanged reachable inference after failure in
later generic arguments, invariant equality, representative compression, edge
propagation and speculative hints. Follow a failed operation with successful
inference to expose contamination rather than asserting only the original error.

#### Ongoing correctness criteria

Retain tests for recursive bounds without stack overflow/RefCell panic, valid
shared graphs versus cycles, component release and long representative chains.
For deterministic variance/nested/inherited matrices, check commutativity modulo
representative identity, idempotence, upper/lower-bound relationships, reversed
conditional branches, invariant-array rejection in both orders and eventual
transactional failure atomicity.

Do not replace the storage model or add recursive types merely to implement
rollback. Stage infrastructure, speculative callers and any newly isolated solver
fixes separately, with focused regressions. Review snapshots individually rather
than accepting them wholesale. Start with solver unit tests, then frontend and
backend tests and affected-package checks; finish with the commands in
[CONTRIBUTING.md](../CONTRIBUTING.md).

### Invalid signature recovery

See the [language guide](README.md) and
[remaining analysis work](language-future-work.md#analysis-lookup-and-constness).

Source signature resolution preserves parameter count, order, names, qualifiers,
source spans, and every successfully resolved type when a parameter or explicit
return type fails to resolve. An omitted return still means `Void`; an unresolved
explicit return does not.

`FunctionType::is_valid()` is false for these declarations (also for failed
function type-parameter bounds). Unresolved parameter/return slots store
`Type::Nothing`, but **this is not bottom-type inference recovery**: consumers must
check signature validity before using these types. Cloning a signature or changing
its parameters/return preserves its invalid state.

Invalid declarations remain in symbols, and the original resolution diagnostics
remain errors. They are not ordinary overload candidates, override/duplicate
comparison inputs, or sources of generated static forwarders. Bodies belonging to
invalid signatures are not lowered: no zipped, shifted parameter bindings or
fabricated return contracts are produced. Calls with no usable candidate report
`INVALID_FUNCTION_SIGNATURE` when an invalid declaration is present, rather than
an arity computed by deleting parameters. Valid overloads and unrelated siblings
can still be analyzed; this does not make the overall compilation successful.

The REDmod override validator skips invalid methods/base signatures. When a base
signature with the same name is invalid, it cannot prove `OverrideWithoutBase`
and suppresses that recovery cascade. This does not establish a resolved override.

The backend rejects symbols containing invalid function signatures before
mutating the output bundle, even if its caller ignores frontend diagnostics and
passes a partial lowered unit. Bundle-loaded signatures remain valid by default;
bundle decoding is unchanged.

#### Boundaries

This is analysis recovery, not support for unknown types or complete partial-unit
coverage reporting. Invalid bodies are intentionally unavailable, so independent
errors inside them are not diagnosed until their signatures are repaired. No
corpus semantic-parity percentage, official dispatch agreement, or runtime
behavior is implied. Parameter and return source syntax remains in the AST;
symbol storage does not retain unresolved type spelling beyond source spans and
the originating diagnostic.

Regression tests are `invalid_signatures.rs` in frontend, backend, and REDmod
adapter tests. They cover first/middle/last parameter failures with optional/out
flags, failed explicit versus omitted returns, nested types and generic bounds,
overload collapse, instance/static/operator calls, override recovery, unaffected
siblings and cross-file provider restoration in both orders, and backend refusal
without output mutation.

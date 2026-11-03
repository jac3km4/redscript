# REDmod semantics and export contracts

This reference covers native module/event rules and the bounded representations
used by community-to-REDmod source export. Start with the
[native language guide](redmod-language.md) for dialect support and the
[transpiler README](../crates/compiler/redmod/transpiler/README.md) for API and
packaging requirements. [Shared semantics](language-semantics.md) define the
frontend contracts these target-specific rules build on.

The sections move from module/declaration planning to storage access, receiver
evaluation, typed coercions and invocation-scoped borrowing. These are separate
eligibility checks: a supported field read does not imply a supported method
receiver, writable place or escaping borrow. Evidence stays with each contract;
source/IR roundtrips, official acceptance, decoded bytecode and game execution
must not be conflated. See [future work](language-future-work.md) for remaining
correctness issues and [investigations](language-investigations.md) for historical
corpus findings.

## Contents

- [Modules and event declarations](#modules-and-event-declarations)
  - [REDmod transpiler imports](#redmod-transpiler-imports)
    - [Native evidence (2026-11-02)](#native-evidence-2026-11-02)
  - [REDmod event return contract](#redmod-event-return-contract)
    - [Normalization and metadata](#normalization-and-metadata)
    - [Export and decompilation boundaries](#export-and-decompilation-boundaries)
    - [Historical official evidence](#historical-official-evidence)
- [Storage access and receivers](#storage-access-and-receivers)
  - [REDmod dynamic-array Grow and Count](#redmod-dynamic-array-grow-and-count)
  - [Bounded compound field reads](#bounded-compound-field-reads)
    - [Limits and place semantics](#limits-and-place-semantics)
    - [Tests and official evidence](#tests-and-official-evidence)
  - [Bounded compound method receivers](#bounded-compound-method-receivers)
    - [Why not mechanically spill temporaries?](#why-not-mechanically-spill-temporaries)
    - [Deliberate limits](#deliberate-limits)
    - [Tests and evidence](#tests-and-evidence)
- [Typed coercion and borrowing](#typed-coercion-and-borrowing)
  - [Bounded REDmod strong/weak coercions](#bounded-redmod-strongweak-coercions)
    - [Safety boundary](#safety-boundary)
    - [Evidence and limits](#evidence-and-limits)
  - [Bounded native and scripted script-reference borrows](#bounded-native-and-scripted-script-reference-borrows)
    - [Lifetime contract](#lifetime-contract)
    - [Validation and evidence](#validation-and-evidence)
    - [MarkToSell](#marktosell)

## Modules and event declarations

Import planning controls visible names, while event normalization controls the
native callback ABI. Neither directive acceptance nor a Bool event signature
establishes whole-program target compatibility.

### REDmod transpiler imports

The transpiler translates `import M.Name` to `@using "M.Name"` and
`import M.{First, Second}` to separate symbol-level `@using` directives. It does
**not** widen either form to `@using "M"`. A function import selects the complete
resolved overload family, as in community source. Wildcard imports retain their
existing module-level translation. Emitted `@module` and `@using` directives end
at the newline, without semicolons; the official compiler rejects directive
semicolons.

Exact/select support is bounded:

- The module path and selected names must be valid target identifiers. Root-level
  exact imports and empty selections are rejected. Resolution and visibility are
  checked by community analysis, not repeated during emission.
- A selected symbol path must not coincide with a module (or ancestor of a module)
  in the selected output: native `@using "First.Pick"` exposes both the function
  and the module `First.Pick`, if both exist. Such overlaps are rejected instead
  of silently widening scope. Module paths are indexed once per transpilation;
  imports do not enumerate the symbol table.
- Analysis-only dependencies do not introduce output modules or global-name
  collision errors. Duplicate native declarations and target call resolution are
  separate concerns, not inferred from an unrelated symbol's simple name.
- The selected output is not a complete inventory of external target modules.
  Overlaps introduced by the installed target or other REDmod packages cannot be
  ruled out by this check; official validation is needed for that environment.
- Existing import ordering and metadata checks remain in force. An error prevents
  returning partial artifacts.

This is import planning, **not certification that the entire emitted program
compiles with REDmod**. Existing typed emission uses qualified type names and
qualified free-function calls. Official REDscript v1.2.2 rejects the tested
qualified type spelling in signatures and the tested qualified free-function
call. These remain separate target-emission limitations, not a reason to widen
imports. Unrelated global type collisions elsewhere in the inputs are also not
solved by import planning.

#### Native evidence (2026-11-02)

Local official compiler v1.2.2 probes established:

- `@using "First.Pick"` permits calls to both tested `Pick` overloads (Int32 and
  Float), but a call to the distinct `First.Other` function is rejected as a module
  not declared Used. `@using "First"` permits `Other`.
- If the inputs also contain module `First.Pick` with function `Extra`, that same
  symbol-level directive permits `Extra(1)`. The prefix-overlap guard is required.
- `First.Pick(1)` is rejected with `Invalid identifier in global scope 'First'`;
  qualifying a resolved call is therefore not proof that widening is safe.
- `Models.Value` in a parameter type is rejected by the parser. Simple `Value`
  is accepted; types with that simple name in two different modules are rejected
  as already defined, regardless of the tested `@using` directive.
- Identical free-function signatures in different modules are rejected as already
  defined. Module identity alone does not make those native declarations distinct.

The separate `tests/imports.rs` integration suite checks exact/select output,
resolved qualified references, overload families, selected-module non-widening,
analysis-only/private dependencies and wildcard compatibility. Its
opt-in native lookup test uses raw native source to isolate lookup behavior.
The emitted-source probe writes transpiler output unchanged for module-only,
exact, select and wildcard cases, without qualified references. All four cases
passed official compiler v1.2.2 on 2026-11-03 with zero errors (only constexpr
suggestions). This certifies the tested directive spelling, not qualified-name
support, general import-use roundtrips or runtime parity. The same emitted source
is covered by a default-test snapshot. Run the native tests against a trusted
installation with:

```sh
REDMOD_EXE='.../redMod.exe' REDMOD_GAME_ROOT='.../Cyberpunk 2077' \
  cargo test -p redscript-redmod-transpiler --locked --test imports -- --ignored --nocapture
```

Each native invocation is bounded to 60 seconds and requires the final success
marker plus fresh nonempty cache artifacts for acceptance. Evidence is retained
under `target/redmod-official/imports-*` (including `imports-emitted-*` for
unchanged emitted-source probes).

### REDmod event return contract

Native events have an implicit Bool result, not an inferred result or Void.
Explicit event return annotations remain invalid. The contract applies to empty
bodies, callback prototypes, inheritance, additions, replacements, wrappers and
source calls. Bare `return;` is a Void-to-Bool error; fallthrough is legal.
See the [native guide](redmod-language.md) for dialect context.

#### Normalization and metadata

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
  [bounded native provider rules](language-semantics.md#guarded-implicit-native-providers)
  apply at known return targets when an exact eligible provider is available;
  this is not unrestricted condition conversion, literal retyping or
  conversion-aware overload matching.

#### Export and decompilation boundaries

Transpilation requires a non-const Bool callback contract, including prototypes,
and preserves Bool return expressions directly. Void/non-Bool/const-return
contracts produce source diagnostics, not invalid bare returns after discarding
values. Calls relying on callback results remain rejected by the transpiler.
A transpile-to-adapter roundtrip checks signatures and return ownership, beyond
native parser acceptance.

The decompiler's bounded reconstruction of grouped labels and detached dispatch
for non-final switch defaults is separate from events; unprovable layouts still
reject. See [switch semantics](language-semantics.md#switch-order-and-overloaded-indexing).

#### Historical official evidence

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

## Storage access and receivers

Array descriptors identify operations; field/place checks determine which storage
can receive them. Method invocations have a wider Context boundary than field
reads, so their receiver and argument ordering require separate evidence.

### REDmod dynamic-array Grow and Count

See the [native language guide](redmod-language.md#control-flow-arrays-and-constness)
for the complete supported descriptor list and
[future work](language-future-work.md#analysis-lookup-and-constness) for missing
array/Variant operations.

The native adapter exposes two additional dynamic-array member descriptors:

| Native spelling | Shared intrinsic | Arguments | Result |
| --- | --- | --- | --- |
| `values.Grow(amount)` | `ArrayGrow` | `amount: Int32` | `Void` |
| `values.Count(value)` | `ArrayCount` | `value: T` for `values: array<T>` | `Int32` |

These descriptors resolve the existing intrinsic signatures supplied by the boot
library (or loaded symbols); they do not synthesize declarations or accept an
ordinary function with the same name as an intrinsic. Backend serialization uses
the existing typed `ArrayGrow` and `ArrayCount` instructions.

Both operations require an array **place**. Temporary receivers such as
`MakeArray().Count(value)` remain rejected even though Count only reads. Grow
cannot mutate const parameters or const locals; Count can read either. Existing
script-reference receiver unwrapping and ordinary user-method resolution remain
unchanged.

The community-to-REDmod transpiler also emits `ArrayGrow` and `ArrayCount` as
these members on local arrays. Its existing array receiver guard still applies.
The authored `ArrayGrowCount` snapshot and optional official compiler fixture
cover both spellings.

This does not add new shared array instructions. Fixed-array member operations and
`EraseFast` are not enabled or aliased. Names are case-sensitive, and explicit
community-style `ArrayGrow(values, amount)` / `ArrayCount(values, value)` calls
remain unavailable in native source.

Regression coverage lives in the adapter's `tests/array_descriptors.rs` and
`tests/array_descriptors_backend.rs`. The latter checks instructions after bundle
serialization and decoding; it does not establish game-runtime behavior.

### Bounded compound field reads

Field reads use direct native postfix syntax, without receiver temporaries,
null guards, synthetic defaults, or lifted evaluation. Supported examples:

```reds
owner.child.flag;
Select(steps, owner).child.flag;
(enabled ? Select(steps, owner) : Select(steps, other)).child.flag;
owner.child.data.visualStateData.m_isIconic;
data.owner.child.flag;
Deref(data).visualStateData.m_isIconic;
```

The last example uses a local `script_ref` to an ordinary struct, matching the
shape needed by MutedMarkers. Tests use authored equivalents, not installed code.
Nested ordinary class handles and struct values retain their native
`Context`/`ObjectField` versus `StructField` nesting. Conditional class receivers
are parenthesized at postfix precedence. Calls and dereferences remain at their
original expression sites, including branches, lazy RHSs and loop conditions.
The existing expression/type and call checks still apply. Persistent fields use
frontend access checking rather than a transpiler-only `this` restriction; access
through another receiver is supported within the declaring class.

#### Limits and place semantics

Read eligibility is isolated in `src/field_reads.rs`. Nested class-handle fields
are also writable places and may be passed as `out` arguments, including through
weak handles. Variable-rooted ordinary struct fields also support assignments
and `out` arguments, including nested fields and `Deref(localScriptRef)` roots.
No struct copy or write-back scheme is introduced. The official probe checks
direct nested `StructField` access, exactly the source's assignments and no
synthesized locals. Temporary/call-rooted struct places remain rejected.
Stable fields may be borrowed by scripted `script_ref<T>` parameters; see
[borrow semantics](#bounded-native-and-scripted-script-reference-borrows).

`ArraySize`, `ArrayClear`, `ArrayPop` and `ArrayLast` support variable-rooted
arrays, including nested class fields. These have no later argument that could
replace the receiver. Official probes check direct Clear/Pop/Last instructions
on fields without synthesized copies.
Array indexing likewise supports variable-rooted array fields for both reads
and element assignments, with no copied array. The official probe checks direct
`ArrayElement` instructions and no synthesized locals.
The official compiler rejects call-rooted array receivers such as
`Make().values.Size()`, even though reading a scalar field from `Make()` is legal.
Operations with additional arguments (`ArrayResize`, `ArrayGrow`, `ArrayCount`,
`ArrayFindFirst`, `ArrayFindLast`, `ArrayContains`, `ArrayPush`, `ArrayInsert`,
`ArrayRemove` and `ArrayErase`) also accept variable-rooted arrays when each
argument is a local or constant, optionally with an immediate typed strong/weak
reference conversion. Element arguments use the array's element type as their
conversion context. For example, `ArrayPush(this.listeners, listener)`
emits `this.listeners.PushBack(listener)` directly. Calls, field reads and other
compound arguments retain the local-array guard: they are not covered by this
bounded evaluation-order check. No receiver is copied or hoisted.

The authored `FieldArrayArguments` fixture in `tests/official.rs` compiles with
v1.2.2 and checks all ten array instructions, their direct field receiver operands,
and the absence of synthesized assignments or calls. This is code-generation
evidence, not an in-game execution guarantee.

Official v1.2.2 requires a variable parent scope for struct member access.
Consequently a struct receiver must be a local-rooted field chain or an explicit
`Deref(localScriptRef)` (possibly followed by fields). The same variable-rooted
storage requirement applies to struct writes. `Select(...).data.visualStateData`, conditional-rooted
struct paths, and temporary struct call/conditional results remain rejected.
Class call/conditional chains may still read scalar or handle fields directly.
Class casts also support direct field reads, with parentheses at postfix
precedence, including null cast receivers. Variable-rooted indexed arrays support
both struct and class element field reads,
such as `owner.values[i].count`; the official probe checks direct `ArrayElement`
access with no synthesized local or assignment.

Compound script-reference values (other than explicit Deref), custom generic,
`mixedRef`/class `neverRef` emulation, assignment/construction receivers,
array materialization and arbitrary nonlocal borrows are not enabled. The
method receiver type gate is unchanged; struct method aliases require an exact
typed borrow context rather than field-read eligibility alone.

#### Tests and official evidence

`tests/field_reads.rs` snapshots generated source and passes it through the full
native adapter. Negative tests cover temporary struct writes/out, standalone
nonlocal `AsRef`, array operations that require locals, call-rooted/temporary
struct reads and unsupported receiver shapes. Scripted field, index and temporary
borrows are positive cases in `tests/as_ref.rs`, not blanket rejections. Method
receiver tests cover their receiver contract independently.

On 2026-11-02 the opt-in field-read test passed against trusted local official
REDscript compiler **v1.2.2**, optimizations disabled and profiling enabled:
**0 errors, 3 warnings** (constexpr/unused-local diagnostics). The validator
required the final success marker and fresh nonempty compiled/hash artifacts.
Named authored functions are decoded after the observed 14-byte profiling
preamble, bounded by their physical records; external placeholders are not fully
decoded. Assertions verify:

- Nested local and Deref struct reads retain `StructField` and Deref instructions,
  without assignments/materialized copies or object contexts for plain structs.
- Class-to-struct and struct-to-class chains retain the distinct instruction
  nesting, and every field Context exits at an instruction boundary immediately
  after its ObjectField.
- Side-effecting receiver calls occur once in expression order, before a sibling
  argument that rebinds the root. A null field Context ends before that sibling
  argument (unlike a method invocation Context, which encloses its arguments).
- Null-returning class chains, conditional receivers, both lazy Skip regions
  containing the whole field read, branch placement and a backward loop jump.

The initial authored call-rooted struct probe was rejected with `Cannot access
struct property 'visualStateData'. Parent scope does not come from variable`.
That evidence narrowed the implementation; no spill fallback was introduced.

Successful artifacts are retained at
`target/redmod-official/field-reads-45816-1793658671458245600/`, including emitted
`r6/scripts/authored.script`, validation logs, caches and `probe-bytecode.txt`.
The initial rejected probe is in `field-reads-45044-1793658488324019300/`.
These are compiler/code-generation observations, **not in-game proof** of null
return defaults, reference lifetime/destruction timing or runtime side effects.
Installed corpus subset validation is a separate post-commit handoff, not part
of this authored test and not a claim made by these results.

```sh
cargo test -p redscript-redmod-transpiler --locked --test field_reads --test receivers
# Set REDMOD_EXE / REDMOD_GAME_ROOT to a trusted local installation:
cargo test -p redscript-redmod-transpiler --locked --test field_reads -- --include-ignored --nocapture
```

### Bounded compound method receivers

The transpiler emits ordinary strong and weak class-reference receivers inline
for method call results (including chains), already-supported local-rooted fields,
conditional expressions, and class casts. For example:

```reds
Select(owner).Next().Read(Rebind(owner));
owner.child.Read(Rebind(owner));
(enabled ? Select(owner) : Select(other)).Read(Rebind(owner));
owner.handle.Read(owner.handle.Entry(Rebind(owner))); // handle is wref
```

Conditional receivers are parenthesized at postfix precedence. No local, prefix,
copy, null guard or synthetic default result is introduced. The existing typed
call, argument, generic, callback and dispatch checks still apply. `super` remains
restricted to its resolved `this` receiver. REDmod performs implicit weak-to-strong
promotion inside the method's Context. An immediate explicit `WeakRefToRef`
receiver is emitted as its underlying weak expression; nested conversions are
not recursively erased.

#### Why not mechanically spill temporaries?

Shared backend instance calls emit `Context(exit)`, the receiver, invocation,
arguments and `ParamEnd`, with the exit after the complete invocation. Computing
arguments before entering that context could evaluate effects that a null
receiver would skip. Re-reading a receiver after an argument rebinds its root
could select a different object. Hoisting a call from a conditional/lazy RHS or
loop condition could change whether or how often it runs. Copying a struct
receiver could lose mutation without a write-back plan.

Inline native syntax retains these expression boundaries and class-handle
identity. This is not a general materialization algorithm.

#### Deliberate limits

- Compound script-reference receivers, general value-struct materialization,
  custom generic, `mixedRef` and `neverRef` value emulation are not enabled.
- Fields and indexed elements can supply a stable `AsRef` to scripted parameters,
  including struct method aliases. Temporary values use invocation-owned borrows
  at exact typed parameters, not synthesized mutable receiver locals.
- Variable-rooted array fields support `ArraySize`, `ArrayClear`, `ArrayPop`
  and `ArrayLast`; other array operations still
  require locals. Nested class-handle fields support assignments and `out` arguments.
- [Field reads](#bounded-compound-field-reads) independently permit bounded
  `Make().field` and `owner.child.field` reads. Variable-rooted ordinary struct
  fields also support writes/out; call-rooted struct values remain unsupported.
- Class handles from indexed arrays can call methods directly, including weak
  handles. Official probes check direct array access, one index evaluation and
  weak promotion without receiver copies.
- Assignment and construction compound receivers remain outside
  this bounded slice. Existing simple local receivers are unchanged.

#### Tests and evidence

`crates/compiler/redmod/transpiler/tests/receivers.rs` snapshots emitted source,
passes it through the full native adapter, and checks typed struct method aliases
alongside retained temporary-struct and class script-reference place rejections.
Field-read coverage is in the separate `field_reads` tests. The authored fixture
includes nested side-effecting receiver calls, arguments rebinding the original
reference, conditional receiver arms,
branch-local calls, lazy RHS expressions, loop reevaluation/continue and a
null-returning receiver.

On 2026-11-02 the opt-in official test passed against local REDscript compiler
v1.2.2 using `validate-mod-scripts -compile -root ... -directory ...` (optimizations
disabled, profiling enabled). The validator required fresh compiled/hash artifacts
and the final commandlet success marker. Result: **0 errors, 3 warnings** (constexpr
and unused-local diagnostics). Existing borrowed-argument reports establish
lifetime masks, not receiver ordering; method receivers have their own probe.

The named `ReceiverProbe` bytecode is decoded and asserted, not merely scanned
for acceptance. Checks cover exact nested call sequences and invocation flags,
Context exits on instruction boundaries after arguments/ParamEnd, field contexts,
both lazy `Skip` regions containing the complete receiver/invocation/argument
sequence, conditional encoding and a backward loop jump. The null-returning
receiver's context encloses the mutating argument too. No added strong/weak
conversion, receiver spill or argument lift occurs.

Official external records are not fully decoded: the test selects the named
function header and skips the observed v1.2.2 14-byte profiling preamble before
bounded instruction decoding. This is compiler/code-generation evidence for the
existing Context ABI, **not an in-game observation** of null return defaults,
reference lifetime/destruction timing or runtime side effects. Runtime tests and
broader receiver forms require separate evidence.

On 2026-11-03 the expanded official fixture also passed for weak fields,
weak-returning calls, immediate explicit promotion, and nested calls whose
arguments rebind the original weak field. Decoded bytecode asserts five
`WeakRefToRef` instructions directly inside receiver Contexts, as well as exact
nested call order and Context exits after complete argument lists. Weak receivers
remain inline; no field is re-read by synthesized code after argument evaluation.

Assignments to fields of local strong or weak class handles also stay inline.
The assignment-place check unwraps the handle before checking the class schema,
without admitting struct, script-reference, `mixedRef` or `neverRef` value places.
The official `ReceiverWeakAssignments` probe covers scalar writes, object
construction, and an RHS that rebinds the original weak local. Its decoded
bytecode asserts `Assign`, `Context`, `WeakRefToRef`, receiver parameter and field
selection in that order, with no synthesized locals. The listing is retained as
`assignment-bytecode.txt` beside the method probe.

```sh
cargo test -p redscript-redmod-transpiler --locked --test receivers
# Set REDMOD_EXE / REDMOD_GAME_ROOT to a trusted local installation first:
cargo test -p redscript-redmod-transpiler --locked --test receivers -- --include-ignored --nocapture
```

Artifacts are retained under `target/redmod-official/receivers-<pid>-<timestamp>`,
including `r6/scripts/authored.script`, compiled/hash files and
`probe-bytecode.txt`. No installed scripts are copied into regression fixtures.

## Typed coercion and borrowing

Both adaptations below rely on an immediate typed context and conservative
overload checks. Strong/weak conversion lifetime and script-reference temporary
ownership are distinct contracts, not permission to erase arbitrary wrappers.

### Bounded REDmod strong/weak coercions

The transpiler can spell a single typed-IR `RefToWeakRef` or `WeakRefToRef`
conversion using REDmod's implicit coercion at an immediate typed sink:

- a supported assignment place or typed local initializer;
- a return with an explicit resolved return type;
- a non-`out` parameter of an ordinary, nongeneric resolved call with no
  competing viable overload after immediate argument erasure.

The sink type must equal the conversion's resolved result type, and the source
and result must refer to the same class with opposite strong/weak representations.
Bare reference-class types are canonical strong handles in shared IR. This path
handles both explicit intrinsic calls and conversions inserted by shared lowering.
It does not add a target cast to `wref<T>`.

#### Safety boundary

Only the immediate conversion is replaced by the operand's source spelling. The
operand stays at its original execution point: no temporary declaration, eager
prerequisite, duplicated evaluation, or branch/loop motion is introduced. The
typed sink reintroduces the conversion, including the strong handle acquired by
`WeakRefToRef`. Nested strong/weak conversions are not cancelled: for example,
`RefToWeakRef(WeakRefToRef(value))` can have an observable intermediate strong
lifetime and remains rejected.

Context is not propagated into conditional arms, operators, casts, intrinsic
arguments, receivers, or standalone expressions. A conversion directly inside an
ordinary call parameter can still be supported when that entire call occurs in a
lazy branch; it stays inside that branch. Conversions to a different class type
(including a base class) remain rejected even when source assignability permits
that relationship.

Call planning is deliberately conservative: any other possibly viable overload
at the emitted name blocks erasure. Arity-inapplicable and type-incompatible
candidates (for example, an `Int32` overload for a class handle) do not block it;
competing strong/weak signatures do. All arguments are checked after the planned
immediate erasures, not just the converted slot. This is a noncompetition proof,
not a claim that shared overload ranking is identical to the official compiler.
Unknown generic substitutions and null/bottom types remain possibly compatible,
not disjoint. Built-in numeric types are conservatively treated as one compatible
family: target numeric conversions and contextual literal typing are not proven
by shared subtype incompatibility, even for a sibling argument such as `1`.
Optional slots obey arity checks; `out` flags are not evidence of noncompetition.
These bounds may reject a call the target could resolve, rather than silently
changing overload selection.

Methods use frontend name-group lookup on the emitted receiver's hierarchy,
including inherited overloads and override deduplication, not unrelated classes.
Free-function groups use the frontend's existing indexed overload lookup, without
a duplicate index or whole-symbol scans. Ordinary calls and local-place
`AsRef` calls do not perform an overload query. Calls through static forwarders and
wrapper original-call aliases do not supply coercion context. Generic calls and
unsafe places retain their existing restrictions. `out` parameters require local
identity, not a converted copy.

`IsDefined` has not been widened. In particular, this feature does not erase a
conversion just because its result is compared with null.

#### Evidence and limits

`crates/compiler/redmod/transpiler/tests/coercions.rs` checks emitted source and
round-trips it through the REDmod adapter, comparing conversion calls and resolved
call parameter signatures. Coverage includes explicit/implicit conversions,
assignment/initialization/return, unique free/static/instance calls, side-effecting
operands, sibling argument order, lazy Boolean operands, and loop back edges.
Negative tests exercise nested/context-free conversions, conditional arms,
non-exact targets, competing overloads (including inherited methods), `out`
arguments, and `IsDefined`. Positive tests distinguish unrelated methods,
`Int32`/wrong-arity overloads, and repeated calls amid unrelated function groups.

These tests establish shared typed-IR reconstruction and source placement, not
independent official compiler or game-runtime equivalence. Official compiler
validation requires a trusted local installation (`REDMOD_EXE` and
`REDMOD_GAME_ROOT`); no broader runtime certification is implied.

### Bounded native and scripted script-reference borrows

The transpiler accepts nonlocal `AsRef` operands only at an immediate, exact,
non-`out` `script_ref<T>` parameter, with an operand of exactly `T`. Native and
scripted callees share borrowing; ordinary free/static/instance
calls and native operators share parameter-context handling and overload checks.
An unrelated `Int32` or wrong-arity overload does not block borrowing; a competing
String/value-reference signature does. Methods query only the emitted receiver's
hierarchy, including inherited candidates, not unrelated classes. Existing
local-place support is unchanged and does not trigger an overload query.

Borrowing is supplied per resolved parameter slot, not by an operator-name or
scalar-type allowlist. String concatenation, other native binary/unary operators,
and ordinary native calls use the same argument emitter, with the appropriate
precedence and associativity for each spelling. Generated inverted equality
guards use it too. Generic calls, wrappers and callbacks retain their existing
restrictions.

Native overloads may coexist when every rewritten argument recreates the same
exact `script_ref<T>` borrow, parameter flags agree, and other argument types remain
untouched. This preserves the existing overload ranking without reimplementing
it. Competing String/value-reference signatures still reject. Strong/weak
reference conversions do not use this exception.

Free functions use the frontend's existing indexed overload groups, without a
duplicate index or whole-symbol scans. Overload queries occur only for immediate
rewrites. Unknown generic substitutions block erasure. For other competitors,
numeric siblings remain possibly compatible across the full built-in numeric
family, regardless of literal spelling or shared subtype checks. Null/bottom and
unknown types are not treated as disjoint. Optional slots use arity checks, and
`out` flags do not prove noncompetition.

The operand stays inline at its original argument slot. No prefix statement,
hoisted local or copy is introduced. Conditional operands and nested right-hand
concatenations preserve grouping. Lazy RHS evaluation, sibling argument order,
and loop-condition re-evaluation therefore retain their original expression
sites. Existing receiver/place restrictions still apply inside the operand.

#### Lifetime contract

`AsRef` is not a generally erasable identity operation. The community assembler
marks a direct `AsRef(prvalue)` argument in the enclosing invocation's 16-bit
rvalue-reference mask. This requests call-owned temporary storage; a borrowed
local/field/index instead retains place identity. New temporary borrows beyond
slot 15 fail closed. Keeping the immediate typed parameter recreates the
target `AsRef` and its invocation lifetime metadata without extending storage to
a whole function or reusing it across loop iterations.

This is an invocation-scoped ABI contract, not an escape analysis or a promise
that arbitrary native code cannot retain an invalid pointer. The pointee is not
restricted to String. Nonlocal `AsRef` in returns, reference initializers,
assignments, standalone expressions, ambiguous overloads,
`out` arguments and nested context-free conversions remains rejected.

Stable variable-rooted fields and array elements may also be borrowed at an exact non-`out`
scripted parameter. They retain place identity rather than needing invocation-owned
temporary storage. Competing overloads still block erasure. Temporary values and
call-rooted fields are accepted at unique scripted sinks too, using the same
invocation lifetime handling as native sinks. The official `ScriptFieldBorrow` probe
checks `AsRef` and direct field access without synthesized locals or assignments.
The `IndexedBorrow` probe additionally checks direct `AsRef`/`ArrayElement`, zero
temporary flags and a single call to a side-effecting index function.

The official `ScriptTemporaryBorrow` probe confirms a scripted struct-result
borrow sets slot 0's rvalue-reference bit, keeps `AsRef` inline inside its branch,
and introduces no local or assignment. Native/scripted overload mixtures retain
the conservative noncompetition guard; the same-borrow overload exception is
still native-only.

#### Validation and evidence

`crates/compiler/redmod/transpiler/tests/as_ref.rs` checks source/target IR call
order, exact parameter types/flags, return types and per-argument temporary
classification through the full adapter API. It also covers fields, array
elements, side-effecting siblings, lazy expressions, loop back edges, precedence,
negative escape/competing-overload cases, native methods, scripted field/index/
temporary borrows, and positive `Int32`/wrong-arity overload and unrelated-class
method cases.

The opt-in official test was run on 2026-11-02 with trusted local REDscript
compiler v1.2.2, with zero errors/warnings and fresh compiled/hash artifacts. It
checks named probe definition ranges for native invocation operands/flags:

- Temporary `GetLocalizedText` argument: mask 1, immediately followed by `AsRef`.
- Local `GetLocalizedText` argument: mask 0, immediately followed by `AsRef`.
- Nested localization/concatenation: masks 1 and 0 on the respective localization
  calls, mask 2 on concatenation (constant String storage needs no temporary).
- Lazy and repeated loop `StrContains` calls: mask 1 at each call site.
- Mixed `Int32`, `Float`, and `Bool` concatenations (rerun on 2026-11-03): mask 0
  for the inner literal borrow and mask 1 for the outer concatenation's temporary
  String result. Both left- and right-hand String parameter slots are covered.
  The expanded fixture compiles with zero errors/warnings.
- Native `+=` borrows its nonlocal RHS in slot 1 (mask 2), confirming the shared
  operator path is not limited to `+`.
- Non-String scalar and struct function results recreate `AsRef` with temporary
  mask 1 (rerun on 2026-11-03). These use authored typed script sinks in the
  official fixture, not invented engine imports. The expanded fixture compiles
  with zero errors and one constexpr suggestion; native-context selection is
  covered separately by the source/target IR round trip.

Official constant String storage differs from the community prvalue
classification; literal arguments do not require identical temporary bits. These
checks establish compiler typing and invocation lifetime metadata, not in-game
execution, native pointer-retention behavior, or general runtime parity.

```sh
cargo test -p redscript-redmod-transpiler --locked --test as_ref
# With REDMOD_EXE / REDMOD_GAME_ROOT set to a trusted installation:
cargo test -p redscript-redmod-transpiler --locked --test as_ref -- --include-ignored --nocapture
```

Artifacts remain under `target/redmod-official/as-ref-<pid>-<timestamp>`.

#### MarkToSell

The localized String interpolation blocker is unblocked: String concatenation
can now borrow literal/call results, including a concatenated localization key
passed to `GetLocalizedText`, without hoisting any evaluation. The regression
fixture contains nested interpolation/localization. This does **not** claim the
entire MarkToSell mod transpiles: other signatures, imports, defaults, receivers
and unsupported operations still have their own gates; no full-mod corpus or
game-runtime run was performed for this feature.

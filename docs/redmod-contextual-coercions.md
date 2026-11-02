# Bounded REDmod strong/weak coercions

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

## Safety boundary

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

## Evidence and limits

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

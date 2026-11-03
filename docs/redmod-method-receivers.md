# Bounded compound method receivers (feature5)

The transpiler emits ordinary strong and weak class-reference receivers inline
for method call results (including chains), already-supported local-rooted fields,
and conditional expressions. For example:

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

## Why not mechanically spill temporaries?

Shared backend instance calls emit `Context(exit)`, the receiver, invocation,
arguments and `ParamEnd`, with the exit after the complete invocation. Computing
arguments before entering that context could evaluate effects that a null
receiver would skip. Re-reading a receiver after an argument rebinds its root
could select a different object. Hoisting a call from a conditional/lazy RHS or
loop condition could change whether or how often it runs. Copying a struct
receiver could lose mutation without a write-back plan.

Inline native syntax retains these expression boundaries and class-handle
identity. This is not a general materialization algorithm.

## Deliberate limits

- Compound script-reference receivers, value structs, custom generic,
  `mixedRef` and `neverRef` value emulation are not enabled.
- Struct receiver aliases requiring a nonlocal `AsRef` still fail closed; neither
  fields nor array elements are copied into mutable value receivers.
- Array receiver/place and `out` argument restrictions are unchanged.
- [Feature6 field reads](redmod-field-reads.md) now independently permits bounded
  `Make().field` and `owner.child.field` reads. It does not broaden writable places
  or mutable struct receivers.
- Cast, index, assignment and construction compound receivers remain outside
  this bounded slice. Existing simple local receivers are unchanged.

## Tests and evidence

`crates/compiler/redmod/transpiler/tests/receivers.rs` snapshots emitted source,
passes it through the full native adapter, and checks retained struct/place
rejections. Feature6 read coverage is in the separate `field_reads` tests. The authored fixture includes nested side-effecting receiver
calls, arguments rebinding the original reference, conditional receiver arms,
branch-local calls, lazy RHS expressions, loop reevaluation/continue and a
null-returning receiver.

On 2026-11-02 the opt-in official test passed against local REDscript compiler
v1.2.2 using `validate-mod-scripts -compile -root ... -directory ...` (optimizations
disabled, profiling enabled). The validator required fresh compiled/hash artifacts
and the final commandlet success marker. Result: **0 errors, 3 warnings** (constexpr
and unused-local diagnostics). Existing reports for feature4 establish borrowed
argument lifetime masks, not receiver ordering; this feature has its own probe.

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

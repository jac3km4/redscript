# Bounded compound field reads (feature6)

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

## Limits and place semantics

Read eligibility is isolated in `src/field_reads.rs`. Nested class-handle fields
are also writable places and may be passed as `out` arguments, including through
weak handles. Struct fields remain read-only in these compound paths; no struct
copy is used as a substitute for write-back. Nonlocal `AsRef` and
mutable struct receiver rules are unchanged.

`ArraySize` supports variable-rooted array fields, including nested class fields.
Array indexing likewise supports variable-rooted array fields for both reads
and element assignments, with no copied array. The official probe checks direct
`ArrayElement` instructions and no synthesized locals.
The official compiler rejects call-rooted array receivers such as
`Make().values.Size()`, even though reading a scalar field from `Make()` is legal.
Other array operations retain their local-array guard.

Official v1.2.2 requires a variable parent scope for struct member access.
Consequently a struct receiver must be a local-rooted field chain or an explicit
`Deref(localScriptRef)` (possibly followed by fields). These are **read** paths,
not new writable places. `Select(...).data.visualStateData`, conditional-rooted
struct paths, and temporary struct call/conditional results remain rejected.
Class call/conditional chains may still read scalar or handle fields directly.
Class casts also support direct field reads, with parentheses at postfix
precedence, including null cast receivers. Variable-rooted indexed arrays support
both struct and class element field reads,
such as `owner.values[i].count`; the official probe checks direct `ArrayElement`
access with no synthesized local or assignment.

Compound script-reference values (other than explicit Deref), custom generic,
`mixedRef`/class `neverRef` emulation, assignment/construction receivers,
array materialization and arbitrary nonlocal borrows are not enabled. Feature5's
method receiver type gate is unchanged; newly readable fields do not authorize
mutable struct methods.

## Tests and official evidence

`tests/field_reads.rs` snapshots generated source and passes it through the full
native adapter. Negative tests cover struct writes/out, mutable struct methods,
AsRef, arrays, temporary structs and unsupported receiver shapes. Feature5 tests
continue to cover their original receiver contract independently.

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

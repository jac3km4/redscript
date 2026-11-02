# Bounded native String borrows (feature4)

The transpiler accepts nonlocal `AsRef` operands only at an immediate, exact,
non-`out` native `script_ref<String>` parameter. Ordinary free/static/instance
calls require no competing viable overload after the immediate argument rewrites.
An unrelated `Int32` or wrong-arity overload does not block borrowing; a competing
String/value-reference signature does. Methods query only the emitted receiver's
hierarchy, including inherited candidates, not unrelated classes. Existing
local-place support is unchanged and does not trigger an overload query.

Native String `+` has a separate exact signature gate: two required, non-out
`script_ref<String>` parameters returning String, without generic parameters. Competing String/value-reference
signatures reject rather than risking overload reselection. Other operators do
not inherit this context. Generic calls, wrappers and callbacks retain their
existing restrictions. Ordinary calls and String operators share the same lazy
call planner: free functions use the frontend's existing indexed overload groups,
without a duplicate index or whole-symbol scans. Overload queries occur only for
immediate rewrites. Unknown generic substitutions block
erasure. Numeric sibling arguments remain possibly compatible across the full
built-in numeric family, regardless of literal spelling or shared subtype checks.
Null/bottom and unknown types are not treated as disjoint. Optional slots use
arity checks, and `out` flags do not prove noncompetition. This proves absence of
competition, not target-specific overload ranking.

The operand stays inline at its original argument slot. No prefix statement,
hoisted local or copy is introduced. Conditional operands and nested right-hand
concatenations preserve grouping. Lazy RHS evaluation, sibling argument order,
and loop-condition re-evaluation therefore retain their original expression
sites. Existing receiver/place restrictions still apply inside the operand.

## Lifetime contract

`AsRef` is not a generally erasable identity operation. The community assembler
marks a direct `AsRef(prvalue)` argument in the enclosing invocation's 16-bit
rvalue-reference mask. This requests call-owned temporary storage; a borrowed
local/field/index instead retains place identity. New temporary borrows beyond
slot 15 fail closed. Keeping the immediate native typed parameter recreates the
target `AsRef` and its invocation lifetime metadata without extending storage to
a whole function or reusing it across loop iterations.

This is an invocation-scoped ABI contract, not an escape analysis or a promise
that arbitrary native code cannot retain an invalid pointer. New support is
bounded to String. Nonlocal `AsRef` in returns, reference initializers,
assignments, standalone expressions, non-native calls, ambiguous overloads,
`out` arguments and nested context-free conversions remains rejected. Supporting
other pointee types or escaping references needs separate lifetime evidence.

## Validation and evidence

`crates/compiler/redmod/transpiler/tests/as_ref.rs` checks source/target IR call
order, exact parameter types/flags, return types and per-argument temporary
classification through the full adapter API. It also covers fields, array
elements, side-effecting siblings, lazy expressions, loop back edges, precedence,
negative escape/competing-overload cases, native methods, and positive
`Int32`/wrong-arity overload and unrelated-class method cases.

The opt-in official test was run on 2026-11-02 with trusted local REDscript
compiler v1.2.2, with zero errors/warnings and fresh compiled/hash artifacts. It
checks named probe definition ranges for native invocation operands/flags:

- Temporary `GetLocalizedText` argument: mask 1, immediately followed by `AsRef`.
- Local `GetLocalizedText` argument: mask 0, immediately followed by `AsRef`.
- Nested localization/concatenation: masks 1 and 0 on the respective localization
  calls, mask 2 on concatenation (constant String storage needs no temporary).
- Lazy and repeated loop `StrContains` calls: mask 1 at each call site.

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

## MarkToSell

The localized String interpolation blocker is unblocked: String concatenation
can now borrow literal/call results, including a concatenated localization key
passed to `GetLocalizedText`, without hoisting any evaluation. The regression
fixture contains nested interpolation/localization. This does **not** claim the
entire MarkToSell mod transpiles: other signatures, imports, defaults, receivers
and unsupported operations still have their own gates; no full-mod corpus or
game-runtime run was performed for this feature.

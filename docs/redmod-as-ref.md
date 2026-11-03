# Bounded native script-reference borrows

The transpiler accepts nonlocal `AsRef` operands only at an immediate, exact,
non-`out` native `script_ref<T>` parameter, with an operand of exactly `T`. Ordinary free/static/instance
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

## Lifetime contract

`AsRef` is not a generally erasable identity operation. The community assembler
marks a direct `AsRef(prvalue)` argument in the enclosing invocation's 16-bit
rvalue-reference mask. This requests call-owned temporary storage; a borrowed
local/field/index instead retains place identity. New temporary borrows beyond
slot 15 fail closed. Keeping the immediate native typed parameter recreates the
target `AsRef` and its invocation lifetime metadata without extending storage to
a whole function or reusing it across loop iterations.

This is an invocation-scoped ABI contract, not an escape analysis or a promise
that arbitrary native code cannot retain an invalid pointer. The pointee is not
restricted to String. Nonlocal `AsRef` in returns, reference initializers,
assignments, standalone expressions, non-native calls, ambiguous overloads,
`out` arguments and nested context-free conversions remains rejected.

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

## MarkToSell

The localized String interpolation blocker is unblocked: String concatenation
can now borrow literal/call results, including a concatenated localization key
passed to `GetLocalizedText`, without hoisting any evaluation. The regression
fixture contains nested interpolation/localization. This does **not** claim the
entire MarkToSell mod transpiles: other signatures, imports, defaults, receivers
and unsupported operations still have their own gates; no full-mod corpus or
game-runtime run was performed for this feature.

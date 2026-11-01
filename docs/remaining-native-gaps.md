# Bounded native parity gaps

Evidence: `docs/redmod-semantic-parity.md` (bounded remaining gaps) and the local
`target/parity-investigation/blockers.md`, sections D–F. No installed sources or
new official bundles are committed here. Official acceptance is not runtime
validation.

## Ordered switch defaults

Shared AST and IR retain case bodies plus a `default_index`: the number of case
labels preceding default. Together these encode the complete label order without
moving or duplicating bodies. Both parsers reject duplicate defaults. Visitors,
formatting and native transpilation use source label order.

For a non-final default, backend dispatch tests all case matchers in case order
and jumps to the selected body (or default after no match). Bodies remain in
source order for fallthrough. The scrutinee is emitted once. Switch break targets
are local; enclosing loop continue targets survive. Existing final-default
bytecode layout is unchanged. Pattern cases still reject fallthrough into a
binding-dependent body, including from a preceding default.

Tests cover first/middle defaults, fallthrough, break/continue, formatting,
serialized bytecode body/dispatch offsets, and single scrutinee emission. They do
not execute the resulting bundle in the game. The decompiler's existing inline
switch recognizer is not extended to reconstruct detached dispatch tables.

## Overloaded indexing

Non-array receivers resolve `OperatorArray(receiver, key)` through the shared
free-function call builder. This preserves overload resolution, parameter/out
checks, result categories and argument order; calls are not array places.
Dynamic/static array receivers retain built-in Int32 indexing and writable-place
semantics, even if an `OperatorArray` overload is visible.

Native `operator[]` maps to `OperatorArray` and requires two parameters. Community
sources can declare `func operator[](out value: Data, key: String) -> Bool` or use
the existing canonical function name. OperatorArray names include parameter and
result types in backend mangling, matching the observed official signature.
Tests exercise native/community declarations, out-temporary rejection,
nonassignable results, wrong key types, built-in precedence and backend calls.

## Scalar mutable

No-extra-flag normalization is accepted only for Float fields in non-imported
classes, with `instanceeditable inlined` and no additional modifiers except
`mutable` and optional `const`. This is the bounded observed form, not a universal mutable-scalar rule.
Existing array acceptance remains unchanged. Explicit `const` is never cleared,
regardless of modifier order. Tests check independent field flags and const
flag preservation even in writing bodies, and unsupported scalar/struct/import/qualifier forms.
The shared validator currently does not reject scalar const-field assignments;
this pre-existing limitation is not fixed here (const-local/parameter validation
is a separate workstream).

## Focused validation

Using stable Rust and an isolated `target/step10-validation` directory:

- Adapter `backend`, `bridge`, `control_flow`, `native_operators`, and
  `native_properties`: 60 passed, 2 official-tool tests ignored.
- Backend bytecode tests and existing snapshots: passed.
- Community formatter `ordered_switch`: 2 passed.
- Native transpiler tests: passed (official-tool tests ignored).
- Focused adapter library/test Clippy with `-D warnings`: passed.
- Adapter, transpiler, and decompiler package checks: passed.

Workspace/all-target checking encountered concurrent-work test constructor
errors (`SemanticPolicy::native_int32_bool_returns` and `Stmt::Let::is_const`).
No unrelated fixes were made. Full workspace/release/doc validation and game
runtime validation were not performed for this bounded change.

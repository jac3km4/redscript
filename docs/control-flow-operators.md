# Switch order and overloaded indexing

See the [language index](README.md) for dialect guides and
[future work](language-future-work.md) for remaining capabilities. These features
are implemented, not remaining native syntax gaps.

## Ordered switch defaults

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
shapes remain subject to [evaluation safety](lazy-evaluation-limitations.md).

Regression tests cover first/middle defaults, fallthrough, break/continue,
formatting, serialized dispatch/body offsets and single scrutinee emission.
They do not execute bundles in the game.

## Overloaded indexing

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

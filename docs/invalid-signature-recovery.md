# Invalid signature recovery

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

## Boundaries

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

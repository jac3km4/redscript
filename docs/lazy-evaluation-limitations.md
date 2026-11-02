# Lazy evaluation limitations

See the [language guide](README.md) and
[array literal boundary](shared-expression-defaults.md#array-literal-evaluation-boundary)
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

`UNSAFE_ARRAY_EVALUATION` has severity `ErrorAllowedAtRuntime`. Lowering reports
it and continues the legacy statement-prefix path rather than dropping the call
or body. Strict CLI `compile`/`lint` and default compiler API thresholds reject
output; CLI `--runtime-compatible` and SCC tolerate this severity. Compatibility
can therefore retain reordered or eager array evaluation, including outside a
lazy branch; it is **not a correctness guarantee**. Hard errors still prevent
output. See the [mode contract](community-language.md#diagnostics-and-tool-boundaries).

These source checks deliberately diagnose some forms that could be safe with more
precise control-flow information. They do not prove that a branch is reachable or
that an initializer is pure. Bind an array at its intended execution site before
passing the local; do not move initialization outside a lazy branch if that would
change observable behavior. Closure bodies have their own execution/lowering
scope, so an array in a closure body is not initialization of the closure operand.

## Typed resolver boundary

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

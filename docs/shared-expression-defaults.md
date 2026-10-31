# Shared expression defaults

Community and REDmod inputs now share these frontend rules:

- An assignment yields its coerced destination type. Chains, arguments, returns,
  and conditional branches can use that value. Typed IR has one `Expr::Assign`
  with a `typ` field; `AssignValue` and the `assignment_values` policy are removed.
  The destination is evaluated once, before the RHS; the result is a temporary,
  not an assignable place. Assembly emits `Assign` without destination readback.
- Bodyless callbacks on native, non-import-only aggregates retain callback flags
  without becoming native methods or acquiring synthetic bodies. Scripted and
  import-only aggregates and ordinary methods retain their body checks. The
  `native_callback_prototypes` policy is removed.
- Script-reference coercions compose with other conversions: unwrap before
  converting to a value, or convert to the pointee type before wrapping. Generated
  conversions preserve the exact pointee, including weak wrappers. Member, method,
  and index receivers unwrap script references without a policy flag.

`native_script_refs` is renamed to `script_ref_write_through`. It controls only
assignment destinations and `out` arguments: REDmod writes/forwards the referenced
place, while community source assigns/forwards the reference itself. It remains
file-local and defaults to false. `implicit_members`, `public_fields`, and
`native_object_cast` remain dialect policies, unchanged.

## Community omitted arguments

Leading and doubled commas denote omitted positional arguments, checked against
optional parameters. A trailing comma does not add a slot: `Target(42,)` has one
argument, `Target(,)` has one hole, and `Target(42,,)` has a value followed by a
hole. Formatting preserves this arity in compact and multiline calls. REDmod
native syntax retains its final-hole behavior: `Target(42,)` includes a final
omitted slot there.

## Array literal evaluation boundary

Array literals currently construct temporary arrays in statement prefixes, not
inside expression bytecode. Shared lowering fails closed with
`UNSAFE_ARRAY_EVALUATION` when this would move construction past an earlier
observable expression or out of a lazy branch. This includes assignments inside
later array arguments/elements, complex assignment destinations with array RHSs,
logical RHSs, and conditional arms. Literal-only nested arrays and ordinary local
array initialization remain supported. Omitted argument slots have no evaluation
and are prefix-inert, so `Take(, [1])` is allowed when the first parameter is
optional. The guard does not infer purity from
function names. It deliberately rejects some pure expressions rather than silently
changing their evaluation order.

Extract the array to a local at its intended execution site, after any preceding
effects; use explicit control flow for lazy branches. This guard is not a new
expression-local array construction facility. REDmod capability checks and the
transpiler's additional prefix restrictions remain in place. Bytecode snapshots
verify instruction structure, not game-runtime execution.

# Shared expression and aggregate-default APIs

Community and REDmod inputs now share these frontend rules:

- An assignment yields its coerced destination type. Chains, arguments, returns,
  and conditional branches can use that value. Typed IR has one `Expr::Assign`
  with a `typ` field; `AssignValue` and the `assignment_values` policy are removed.
  This replaces the former Void result in community source. The destination is
  evaluated once, before the RHS; the result is a temporary,
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

## Strict const checks and REDmod relaxations

Shared frontend analysis enforces const parameters by default; this is not a
REDmod-only check. `SemanticPolicy::default()` leaves both characterized REDmod
relaxations disabled:

- `const_param_element_writes` permits indexed element writes/`out` forwarding,
  including fields within elements, but not whole-array replacement or mutation
  of array size.
- `const_param_non_out_forwarding` permits non-`out` forwarding to non-const
  reference/container parameters. Explicit `out` and known mutating array
  intrinsics remain checked.

The adapter enables both per native source file; community dependencies stay
strict. Whole-root and field writes remain rejected in both modes. These are
bounded direct-root checks, not transitive immutability or const field/return
enforcement. See [const parameters](const-parameters.md) for provenance, alias,
and closure limits.

## Shared aggregate defaults

Both languages accept aggregate members such as `default value = -1;`, including
own and inherited targets in classes and structs. The shared AST represents them
as `Item::Default(FieldDefault)` (`SourceFieldDefault` for spanned inputs), with
`target`, `value`, and optional `literal_text`. Community parsing/formatting and
AST visitors preserve these nodes; the REDmod adapter retains contiguous numeric
source spelling in `literal_text` where native metadata requires it.

The frontend consumes these AST members directly: no private adapter default plan,
supplemental input collection, or separate processing entry point is required.
Bare shared-AST callers retain defaults. REDmod's remaining native loop-fixup
plans still require complete `ParsedModule` values and `process_sources`; the
bare-AST API fails closed for those loop constructs, not defaults.

After inheritance binding, the frontend resolves target fields and checks bounded
literal/enum constants against their substituted field types. This is not general
constant-expression evaluation. `LoweredClass::defaults` contains ordered
`LoweredFieldDefault` entries with resolved `FieldId`, constant value, optional
literal spelling, and span. The supplying aggregate is the containing class entry;
the field identity may belong to a base.

The backend merges properties onto the declaring field under supplying-class
keys, without initialization bytecode or loss of existing base/sibling/bundle
entries. Own-field defaults are first-wins in source order (including inline
initializers); inherited defaults are last-wins per supplying class. Type checking
is intentionally stricter than official inherited defaults that accept some
invalid text. Parenthesized/comment-separated numeric forms use typed formatting
rather than retained spelling.

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

# redscript-redmod-adapter

Partial native REDmod → shared semantic IR adapter for analysis. Accepted input
is not guaranteed to compile officially or behave correctly in the game.

## Integration conventions

- Use `parse_file_with_metadata`, retain/clone the complete `ParsedModule`, and
  pass it to `process_sources`. Its private control-flow plan is required for
  native `for`/`do-while`; extracting only the shared module loses loop fixups.
- `parse_file` is a bare shared-AST convenience API and rejects loops requiring
  fixups. `bridge::lower` accepts native AST directly; `validate_module` alone
  neither applies loop plans nor replaces metadata-aware processing.
- Syntax/bridge failures exclude modules. Check parsing errors, adapter processing
  errors **and** the shared `CompileErrorReporter`. Conditional/post-resolution
  errors can accompany returned IR; never treat that IR as target-valid.
- Supply already-parsed community dependencies and the usual boot library.
  Policies apply only to native source files, not their community dependencies.
- Keep all target-specific mappings and validators here. Shared `SemanticPolicy`
  hooks must remain generic; the default frontend keeps community behavior.
- Errors retain typed parse/capability/semantic variants, source spans and codes.
  Generated nodes must not hide written source from IDE queries.

## Supported behavior

- Explicit types, native casts, `name`/`weak<T>` mappings, bare class values and
  resolved object/weak-reference truthiness; no numeric-to-Bool conversion.
- Function-wide typed locals with duplicate checks, entry default initialization
  and source-site explicit initialization. Uninitialized local/field groups
  expand in order; standalone blocks flatten under the same binding policy.
- Native `for`/`do-while` through typed-IR fixups: headers lower once, `continue`
  executes the correct next header, and nested switches retain loop targets.
  Discarded sequences and condition prefixes preserve execution order.
- `wrapMethod` own-name calls rewrite only when unqualified and not shadowed by
  parameters or function-wide locals. Written overrides require matching resolved
  signatures/flags and respect final methods; events use Void callback metadata
  and allow omitted or `protected` visibility, but not `public` or `private`.
- String field metadata preserves exact keys, duplicates and order. Hints target
  one field in the same aggregate. Persistent class and struct fields use shared
  persistable-type checks and target-specific access restrictions.
- Bounded `ModuleExists`/single-negation conditions with deterministic first-active
  selection. Capability checks still apply to inactive declarations.
- Dynamic-array receiver methods: `Size`, `PushBack`, `PopBack`, `Clear`, `Resize`,
  `Contains`, `FindFirst`, `FindLast`, `Insert`, `Remove`, `Erase`, `Last`.
  Use `array_operations()` plus resolved intrinsic signatures for IDE suggestions;
  temporary receivers are rejected and user methods keep normal resolution.
- Bytecode-backed modifiers and parameter flags, including const metadata.
  Metadata preservation does not add readonly enforcement or bypass legality checks.

## Remaining semantic capabilities

The native parser retains these forms, but analysis rejects unsupported semantics:

| Gap | Next step |
| --- | --- |
| Assignment values/chains; general value sequences | Define typed result/place lowering without duplicating evaluation; shared assignments remain Void |
| Omitted positional arguments | Resolve skipped slots against signatures; do not substitute `null` |
| Const locals; initialized local/field groups | Establish mutation and initializer evaluation rules before lowering |
| Operator/conversion declarations; enum storage; const returns | Add registration/type rules, not syntax-only normalization |
| All `default` directives, including inherited fields | Implement target-specific initialization separately from local defaults |
| `skip` parameters | Add short-circuit call lowering; optional flags alone are insufficient |
| `export`, `virtual`, `constexpr`, `mutable` modifiers | Verify semantics before mapping or deliberately ignoring them |
| Nonfield/nonstring metadata; inherited/invalid hint targets | Add target rules while preserving arbitrary string field properties |
| Nonfinal or duplicate switch defaults | Preserve native label order in shared lowering |

Other limits:

- Custom generics, lambdas/function types, explicit `ref<T>`, `mixedRef`/`neverRef`,
  mixed-reference dependencies and strong-wrapper coercions are unsupported.
- Unsafe array-prefix lifting shapes are rejected: lazy operands, ternary arms,
  switch matchers and eager shapes that reorder prior evaluation. Keep prefixes
  at their execution site before relaxing guards; explicit local bindings can
  make source order unambiguous in the meantime.
- Conditional versions group by source spelling before resolution. Resolve aliases
  and cross-namespace patch targets before treating this as signature equivalence.
- Fixed-array methods, full event/patch return adaptation, covariant overrides,
  prefix/postfix ordering, native float matching and contextual function values
  need more work. Import-only extensions and patches on mod-defined classes fail.
- Unknown annotations fail. Optional flags do not implement omitted slots or
  authored parameter-default expressions.

## Tests

```sh
cargo test -p redscript-redmod-adapter --locked
```

Use snapshots for full diagnostics/IR; direct assertions for query spans,
execution order and identities. Backend tests serialize/reload bundles and check
loop destinations, properties, hints and flags. These tests require no game
installation and do not replace official compilation or runtime checks.

See the [roadmap](../../../../docs/redmod-support-roadmap.md) for shared prerequisites
and implementation order.

# redscript-redmod-adapter

Partial native REDmod → shared semantic IR adapter for analysis. Accepted input
is not guaranteed to compile officially or behave correctly in the game.

## Measured integration checkpoint (2026-11-01)

The cleanup working tree processes all **2,112 installed files / 21,873,807
bytes** read-only with boot/default symbols only (no game bundle): zero exclusions
and adapter processing errors, **12,655 shared errors / 3,705 warnings**.
This supersedes the previous integration checkpoint, whose totals were identical
but included 4,925 unsupported conversions and 838 unresolved functions; current
counts are **4,958 and 805**, respectively (unresolved `Cast`: 761 previously,
728 now). The source-aware cast helper improves attribution, not conversion support.
The earlier snapshot was 2,046 processed / 66 excluded, 1,456 adapter errors and
47,982 shared errors / 25,364 warnings. These are diagnostic/processing counts,
not semantic passes. Reports under `target/redmod-semantic-corpus-cleanup/`
replayed byte-for-byte across all seven files to the `-replay` directory; see
[CORPUS.md](CORPUS.md) for commands, counters, previous checkpoints and trust limits.
No final revision is assigned while history is being arranged.

Coverage observes 53,875 function declarations (53,819 valid-signature flags,
56 invalid), 38,824 retained of 38,861 explicit bodies, and 781,341 mapped of
1,144,329 written expression spans. Queries returned a type at 778,780 spans,
were unresolved at 2,561 and failed at zero mapped spans (38 mixed outcomes).
Recovery IR and synthetic nodes sharing written spans can contribute; these
numbers do not establish type accuracy or correctness of retained bodies.

The measured working tree implements signature-poison guards, source `skip`,
conversion declaration registration, readonly locals, event/overload/override
fixes, Variant extraction ordering, ordered defaults, overloaded indexing and
bounded scalar mutable. Conversion dispatch remains **fail closed** (4,958 errors);
4,921 invalid-signature diagnostics and boot-only dependency/layout gaps dominate
other residuals. **Partial declaration harvesting is not implemented**: failed
parse-stage modules still lose all siblings. Coverage reports do not enable
partial analysis. No full-accuracy, official-parity or game-runtime claim is made.
The older contract details below are updated for this working-tree checkpoint,
not a claim that the changes have been committed or released.

The cleanup also retains source-based lazy guards/native cast operands, community
`const` syntax round-tripping and const-local entry defaults, and removes the
special Int32-to-Bool event-return coercion. See
[lazy evaluation limitations](../../../../docs/lazy-evaluation-limitations.md)
for conservative source-free typed-call rejection and prefix-initialization limits.
Full contribution validation passed: default/all-feature workspace tests each
**738 passed, 0 failed, 9 ignored** (117 targets; previously 728 passed), both
all-targets Clippy configurations with `-D warnings`, pinned nightly fmt, release
workspace build, all-feature no-dependency docs, nine standalone package checks,
and five release corpus example tests. No source/test fixes were required during
validation; the existing `chumsky` advisory remains. See CORPUS.md for resource
settings and the successful retry after a release-test compilation timeout.

## Integration conventions

- Use `parse_file_with_metadata`, retain/clone the complete `ParsedModule`, and
  pass it to `process_sources`. Its private plans retain native `for`/`do-while`
  fixups; extracting only the shared module loses required loop semantics.
  Aggregate default directives are shared AST members, including retained numeric
  spelling, and do not require a private plan.
- `parse_file` is a bare shared-AST convenience API and rejects constructs requiring
  these plans. `bridge::lower` accepts native AST directly; `validate_module` alone
  neither applies plans nor replaces metadata-aware processing.
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
  Object casts support downcasts, identity and upcasts without conversion overloads.
- Unqualified fields and method calls resolve against the enclosing aggregate,
  including inherited and static methods. Parameters and function-wide locals
  shadow implicit members. Eligible implicit methods precede same-named types,
  with constructor and free-function fallback. Official probes show a type can
  precede a same-named local in one edge case; local-first behavior is an intentional
  remaining difference.
- Function-wide typed locals with duplicate checks, entry default initialization
  and source-site explicit initialization. Uninitialized local/field groups
  expand in order; standalone blocks flatten under the same binding policy.
- Native `for`/`do-while` through typed-IR fixups: headers lower once, `continue`
  executes the correct next header, and nested switches retain loop targets.
  Discarded sequences and condition prefixes preserve execution order.
- `wrapMethod` own-name calls rewrite only when unqualified and not shadowed by
  parameters or function-wide locals. Written overrides require matching resolved
  signatures and `out` flags and respect final methods; `const` and `optional`
  parameter flags may differ from the base. Native events use implicit Bool callback
  signatures/metadata and allow omitted or `protected` visibility, but not `public`
  or `private`. Int32-to-Bool returns have no special coercion hook; ordinary
  matching native `Cast` resolution applies, including fail-closed conversion
  declaration dispatch.
  Bodyless events in imported classes retain callback identity without synthetic
  bodies or a native flag; scripted and importonly prototypes remain checked.
  Omitted native field visibility defaults to public; explicit visibility and
  community defaults are unchanged.
- String field metadata preserves exact keys, duplicates and order. Hints target
  one field in the same aggregate. Persistent class and struct fields use shared
  persistable-type checks. Mod-defined class access permits the declaring class
  and descendants while preserving external/private restrictions; imported native
  classes and structs retain their existing policy.
- Bounded `ModuleExists`/single-negation conditions with deterministic first-active
  selection. Capability checks still apply to inactive declarations.
- Dynamic-array receiver methods: `Size`, `PushBack`, `PopBack`, `Clear`, `Resize`,
  `Contains`, `FindFirst`, `FindLast`, `Insert`, `Remove`, `Erase`, `Last`.
  Use `array_operations()` plus resolved intrinsic signatures for IDE suggestions;
  temporary receivers are rejected and user methods keep normal resolution.
- `export` and `constexpr` are analysis-only no-ops on verified function targets.
  `virtual` uses checked non-final dispatch, including private-method rules.
  Private ordinary methods are implicitly final. These mappings do not establish
  equivalent engine exposure or compile-time evaluation.
- Native `ref<T>` maps to script-reference semantics, **not** community strong
  `ref<T>`. Verified scalar/string/struct/class/array/weak parameter and field forms,
  forwarding, temporary arguments, null, dereferencing and const provenance are
  supported. Shared coercion composes script-reference wrapping/unwrapping with
  other conversions; member and index receivers unwrap script references in both
  dialects. Only REDmod enables `script_ref_write_through` for assignment places
  and `out` arguments. Ref locals/returns, nested refs and arrays of refs remain rejected.
- Checked operator declarations and positional omitted slots participate in overload
  resolution. Holes must match optional parameters and emit `Nop`, never `null`.
  Explicit and implicit conversion declarations retain intent and signatures for
  queries, but dispatch fails closed with `UNSUPPORTED_NATIVE_CONVERSION`; they
  are not ordinary call candidates. Exact implicit attribution is diagnostic-only.
  Source `skip` flags preserve lazy RHS evaluation; unsafe prefix shapes fail closed.
  Source-free typed calls conservatively reject selected skipped operands except
  the source-guarded logical RHS; see the linked lazy-evaluation limitations.
  Non-array indexing resolves `OperatorArray` calls, not writable array places.
- Assignments/chains in both dialects produce the coerced destination type and
  preserve place/RHS order without readback. Shared lowering rejects array-literal
  prefix lifting that would reorder evaluation or violate lazy execution; extract
  such literals to locals at their intended execution sites.
- Const return metadata and function `[profile = "label"]` are retained through
  lowering. Bundles retain the profile-enabled marker, not its label. `mutable`
  is supported on verified aggregate array fields and the bounded non-imported
  class `instanceeditable inlined Float` form (optional explicit const preserved),
  not general scalar fields. Plain array fields' implicit const enforcement remains
  incomplete.
- Own and inherited `default` directives resolve typed constants after inheritance
  binding. Properties are merged onto the declaring field, keyed by supplying
  class, without initialization bytecode. Own-field duplicates are first-wins;
  inherited duplicates are last-wins per supplying class. Existing base/sibling
  entries are preserved. Checking is deliberately stricter than official inherited
  defaults that accept some type-invalid text. Contiguous numeric spellings are
  preserved; parenthesized/comment-separated forms use typed formatting.
- Const parameter writes are checked in shared frontend analysis. REDmod explicitly
  permits indexed element writes and non-`out` forwarding; whole-root/field writes,
  `out` forwarding and mutating array intrinsics remain rejected. Community source
  defaults to strict enforcement. See [const parameters](../../../../docs/const-parameters.md)
  for policy flags and alias/closure bounds. Other bytecode-backed const metadata
  does not by itself add field/return enforcement or bypass legality checks.

Explicit enum storage is shared with community source: `Int8/16/32/64` and
`Uint8/16/32` preserve width and enforce signed/unsigned boundaries. Bundle loading
preserves width but cannot recover erased signedness. See
[enum storage](../../../../docs/enum-storage.md) for literal limits and official
characterization.

## Remaining semantic capabilities

The native parser retains these forms, but analysis rejects unsupported semantics:

| Gap | Next step |
| --- | --- |
| General value sequences; initialized local/field groups | Establish result and initializer evaluation rules before lowering |
| Conversion dispatch (implicit and explicit) | Registration exists; characterize selection before enabling call emission |
| General scalar `mutable` fields | Broaden only beyond the verified Float form with evidence |
| Nonfield/nonstring metadata; inherited/invalid hint targets | Add target rules while preserving arbitrary string field properties |
| Partial declaration harvesting | Not implemented; retain explicit incompleteness without inventing bodies before offering analysis-only recovery |

Other limits:

- Custom generics, lambdas/function types, `mixedRef`/`neverRef`, mixed-reference
  dependencies and strong-wrapper coercions are unsupported.
- Unsafe array-prefix lifting shapes are rejected: lazy operands, ternary arms,
  switch matchers and eager shapes that reorder prior evaluation. Keep prefixes
  at their execution site before relaxing guards; explicit local bindings can
  make source order unambiguous in the meantime.
- Conditional versions group by source spelling before resolution. Resolve aliases
  and cross-namespace patch targets before treating this as signature equivalence.
- Const locals now have readonly checks (with bounded native handle-rebinding
  policy), rather than discarded qualifiers. Events retain callback metadata while
  handling the verified Bool source contract. Native covariant overrides use the
  original slot with same-representation constraints; community rules stay separate.
  Ordered switch defaults preserve fallthrough; duplicate defaults remain rejected.
- Fixed-array methods, broader event/patch adaptation, prefix/postfix ordering,
  native float matching and contextual function values need more work. Import-only
  extensions and patches on mod-defined classes fail.
- Unknown annotations fail. Optional slots do not implement authored
  parameter-default expressions.

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

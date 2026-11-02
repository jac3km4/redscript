# redscript-redmod-adapter

Partial native REDmod → shared semantic IR adapter for analysis. Accepted input
is not guaranteed to compile officially or behave correctly in the game.

The [language guide](../../../../docs/README.md) is the entry point for both
dialects. See [native REDmod support](../../../../docs/redmod-language.md) for
current semantics and [future work](../../../../docs/language-future-work.md)
for unsupported behavior. Historical corpus counts are archived in the
[investigation summary](../../../../docs/language-investigations.md); they are
not current-HEAD validation or semantic-accuracy scores.

## Integration conventions

- Use `parse_file_with_metadata`, retain/clone the complete `ParsedModule`, and
  pass it to `process_sources`. Its private plans retain native `for`/`do-while`
  fixups; extracting only the shared module loses required loop semantics.
  Aggregate default directives are shared AST members, including retained numeric
  spelling, and do not require a private plan.
- `parse_file` is a bare shared-AST convenience API and rejects constructs requiring
  these plans. `bridge::lower` accepts native AST directly; `validate_module` alone
  neither applies plans nor replaces metadata-aware processing.
- Syntax/bridge failures exclude whole modules; partial declaration harvesting is
  not implemented. Check parsing errors, adapter processing errors **and** the
  shared `CompileErrorReporter`. Conditional/post-resolution errors can accompany
  returned recovery IR; never treat that IR as target-valid.
- Supply already-parsed community dependencies and the usual boot library.
  Policies apply only to native source files, not their community dependencies.
- Keep target-specific mappings and validators here. Shared `SemanticPolicy`
  hooks must remain generic; the default frontend keeps community behavior.
- Errors retain typed parse/capability/semantic variants, source spans and codes.
  Generated nodes must not hide written source from IDE queries.
- Use `array_operations()` plus resolved intrinsic signatures for IDE suggestions;
  native array receivers must be places, not temporaries. User methods keep normal
  resolution rather than being rewritten solely by their spelling.

## Semantic contracts

Detailed language knowledge lives in shared documents rather than a separate
adapter-specific support list:

| Area | Contract |
| --- | --- |
| Native declarations, modifiers, metadata, references and builtins | [Native language guide](../../../../docs/redmod-language.md) |
| Implicit members, overloads, static hiding, forwarders and override slots | [Lookup and inheritance](../../../../docs/lookup-inheritance.md) |
| Conversion registration and dispatch | [Conversion declarations](../../../../docs/conversion-declarations.md) |
| Source `skip`, array-prefix safety and typed-call restrictions | [Lazy evaluation](../../../../docs/lazy-evaluation-limitations.md) |
| Assignments, omitted slots, script references and aggregate defaults | [Shared expressions/defaults](../../../../docs/shared-expression-defaults.md) |
| Readonly locals, parameter policies and bounded native relaxations | [Const semantics](../../../../docs/const-parameters.md) |
| Enum widths, ranges and erased signedness | [Enum storage](../../../../docs/enum-storage.md) |
| Bool callback contracts and event/patch boundaries | [Native events](../../../../docs/redmod-events.md) |
| Switch ordering and overloaded indexing | [Control flow/operators](../../../../docs/control-flow-operators.md) |
| Invalid signature slots, unavailable bodies and recovery | [Signature recovery](../../../../docs/invalid-signature-recovery.md) |

In particular, conversion dispatch is **not universally unsupported**. Unique
exact native providers and bounded explicit enum/Int32 conversions are supported;
scripted providers, conversion-aware overload matching and wider conversion cells
remain gated. Unsupported conversions fail closed with
`UNSUPPORTED_NATIVE_CONVERSION`, rather than becoming ordinary call candidates.

Conditional versions support bounded `ModuleExists`/single-negation predicates
with deterministic first-active selection. Conversion keys include their return
destination, so different destinations do not suppress each other. Grouping still
uses source spellings rather than resolved alias/cross-namespace equivalence.
Capability checks also apply to inactive declarations.

Implicit static calls retain their resolved parent identity across modules.
Inherited generic static signatures are substituted before hiding comparisons.
Type-qualified static calls can accept array literals; runtime closure/member
callees remain guarded against argument-prefix reordering.

## Tests and optional corpus survey

```sh
cargo test -p redscript-redmod-adapter --locked
```

Use snapshots for full diagnostics/IR; direct assertions for query spans,
execution order and identities. Backend tests serialize/reload bundles and check
loop destinations, properties, hints and flags. These tests require no game
installation and do not replace official compilation or runtime checks.

[CORPUS.md](CORPUS.md) documents the optional boot-only semantic survey, its seven
reports, measurement limits and reduction workflow. Do not commit installed
scripts or make disposable local reports a test dependency. Full code-validation
commands are in [CONTRIBUTING.md](../../../../CONTRIBUTING.md).

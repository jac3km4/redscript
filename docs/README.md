# Language guide

These documents describe this repository's community REDscript compiler and its
bounded native REDmod support. For an introductory language tutorial, see the
[REDscript wiki](https://wiki.redmodding.org/redscript).

> [!IMPORTANT]
> **Implementation Contracts and Runtime Compatibility**
> The specifications in this guide represent compiler implementation contracts. Unit tests,
> bytecode snapshots, and official compiler acceptance establish tooling and encoding behavior,
> not in-game execution compatibility.

## Start here

- [Community REDscript](community-language.md): current language behavior, syntax, and
  compilation/decompilation boundaries.
- [Native REDmod](redmod-language.md): native dialect differences, supported semantics, and
  explicit limitations.
- [Future work](language-future-work.md): unresolved correctness issues, missing capabilities,
  and evidence needed before broadening support. In particular,
  [repeated Variant initialization](language-future-work.md#variant-default-initialization-on-repeated-declarations)
  is an unresolved backend risk.

### Dialect quick comparison

| Feature | Community REDscript (`.reds`) | Native REDmod (`.script`) |
| :--- | :--- | :--- |
| **`ref<T>`** | Strong class reference handle | Script reference (equivalent to `script_ref<T>`) |
| **`script_ref<T>`** | Explicit script reference type | Native script reference (spelled `ref<T>`) |
| **Reference assignment** | Rebinds the reference variable | Writes through to the pointee |
| **Const locals** | `const x = 1;` (declaration init allowed) | `const var x: Int32;` (scalar init rejected; handle rebinding allowed) |
| **Trailing comma `Call(42,)`** | 1 argument (`42`) | 2 arguments (`42` + trailing omitted hole) |
| **Omitted enum storage** | 4 bytes for Int32-range values, 8 bytes otherwise | 4 bytes for Int32-range values, 8 bytes otherwise (official REDmod defaults to 4 bytes) |
| **Event callbacks** | Authored return type enforced | Implicit `Bool` contract; fallthrough emits `Nop` |

## Detailed semantics

Two references collect the detailed contracts, with topic and subsection navigation:

- [Shared language semantics](language-semantics.md): declarations, initialization,
  evaluation, lookup, inference and diagnostic recovery, including per-file dialect
  differences.
- [REDmod semantics and export contracts](redmod-semantics.md): imports, events,
  array descriptors, field/method receivers, contextual coercions and borrowing.

Use the topic links below to jump directly to a contract. Current safeguards and
remaining design goals are distinguished within each reference.

| Topic | Contract |
| --- | --- |
| Const parameters and locals | [Per-mode forwarding diagnostics, native handle rebinding and analysis bounds](language-semantics.md#const-parameters-and-locals) |
| Assignments, script references, omitted arguments and field defaults | [Shared expression/default semantics and APIs](language-semantics.md#shared-expression-and-aggregate-default-apis) |
| Enum ranges, widths and erased signedness | [Explicit enum storage](language-semantics.md#explicit-enum-storage) |
| Conversion declarations and selection | [Exact native providers, bounded enum casts and recovery guards](language-semantics.md#conversion-declarations-and-bounded-native-selection) |
| Calls and inheritance | [Lookup, overloads, generated forwarders and override slots](language-semantics.md#call-lookup-overloads-and-inheritance) |
| Switches and indexing | [Label order, fallthrough and OperatorArray](language-semantics.md#switch-order-and-overloaded-indexing) |
| Lazy evaluation and array construction | [Skip parameters, strict rejection and legacy compatibility risks](language-semantics.md#lazy-evaluation-limitations) |
| Invalid signatures | [Preserved slots, unavailable bodies and fail-closed emission](language-semantics.md#invalid-signature-recovery) |
| Native events | [Implicit Bool contracts, fallthrough and patch/export limits](redmod-semantics.md#redmod-event-return-contract) |
| Native array members | [Dynamic-array Grow and Count descriptors](redmod-semantics.md#redmod-dynamic-array-grow-and-count) |
| Native imports | [Exact/select directives, collision guards and qualified-name limitations](redmod-semantics.md#redmod-transpiler-imports) |
| Native storage access | [Field reads and writable-place boundaries](redmod-semantics.md#bounded-compound-field-reads) |
| Native method receivers | [Inline Context evaluation and deliberate limits](redmod-semantics.md#bounded-compound-method-receivers) |
| Native reference adaptation | [Strong/weak coercions](redmod-semantics.md#bounded-redmod-strongweak-coercions) and [script-reference borrow lifetimes](redmod-semantics.md#bounded-native-and-scripted-script-reference-borrows) |
| Inference | [Current safeguards and transactional hardening work](language-semantics.md#type-inference-safeguards-and-hardening) |

## Type reference

- Scalars: [Bool](types/Bool.md), [Int8](types/Int8.md), [Int16](types/Int16.md),
  [Int32](types/Int32.md), [Int64](types/Int64.md), [Uint8](types/Uint8.md),
  [Uint16](types/Uint16.md), [Uint32](types/Uint32.md), [Uint64](types/Uint64.md),
  [Float](types/Float.md), [Double](types/Double.md).
- Text and identifiers: [String](types/String.md), [CName](types/Cname.md),
  [TweakDBID](types/TweakDbId.md), [ResRef](types/ResRef.md).
- Containers and references: [Array](types/Array.md), [ref](types/Ref.md),
  [wref](types/Wref.md), [script_ref](types/ScriptRef.md), [Variant](types/Variant.md).
- Opaque engine types: [NodeRef](types/NodeRef.md), [CRUID](types/Cruid.md),
  [LocalizationString](types/LocalizationString.md),
  [redResourceReferenceScriptToken](types/RedResourceReferenceScriptToken.md).
- Special types: [Void](types/Void.md), [Nothing](types/Nothing.md).

## Integration and evidence

CLI `compile`/`lint` default to strict diagnostics; `--runtime-compatible` matches
SCC's fatal threshold without guaranteeing correct legacy evaluation. See the
[diagnostic mode contract](community-language.md#diagnostics-and-tool-boundaries).

- [Adapter API](../crates/compiler/redmod/adapter/README.md) and
  [transpiler API](../crates/compiler/redmod/transpiler/README.md).
- Optional [native parser corpus](../crates/syntax/redmod/parser/CORPUS.md) and
  [semantic corpus survey](../crates/compiler/redmod/adapter/CORPUS.md).
- [Historical investigations](language-investigations.md): corpus findings,
  official evidence limits and [conversion experiments](language-investigations.md#conversion-experiments-and-bounded-follow-up),
  including rejected approaches and the narrower delivered follow-up. These are
  historical evidence, not current validation results.
- [Contribution rules and validation commands](../CONTRIBUTING.md).

Compiler tests, official acceptance, decoded representation and runtime behavior
are distinct evidence levels. Installed-source surveys are optional local checks;
no installed game scripts, generated bundles or disposable local reports are
required to use these guides. Historical counts must not be presented as current
results or semantic-accuracy percentages.

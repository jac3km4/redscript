# Language guide

These documents describe this repository's community REDscript compiler and its
bounded native REDmod support. They are implementation contracts, not a claim of
full official-compiler equivalence or game-runtime compatibility. For a general
language tutorial, see the [REDscript wiki](https://wiki.redmodding.org/redscript).

## Start here

- [Community REDscript](community-language.md): current language behavior and
  compilation, decompilation and export boundaries.
- [Native REDmod](redmod-language.md): dialect differences, supported semantics and
  explicit limitations.
- [Future work](language-future-work.md): unresolved correctness issues, missing
  capabilities and evidence needed before broadening support. In particular,
  [repeated Variant initialization](language-future-work.md#variant-default-initialization-on-repeated-declarations)
  is an unresolved backend risk, not a verified runtime behavior.

## Detailed semantics

| Topic | Contract |
| --- | --- |
| Const parameters and locals | [Strict community rules, native handle rebinding and analysis bounds](const-parameters.md) |
| Assignments, script references, omitted arguments and field defaults | [Shared expression/default semantics and APIs](shared-expression-defaults.md) |
| Enum ranges, widths and erased signedness | [Explicit enum storage](enum-storage.md) |
| Conversion declarations and selection | [Exact native providers, bounded enum casts and recovery guards](conversion-declarations.md) |
| Calls and inheritance | [Lookup, overloads, generated forwarders and override slots](lookup-inheritance.md) |
| Switches and indexing | [Label order, fallthrough and OperatorArray](control-flow-operators.md) |
| Lazy evaluation and array construction | [Skip parameters and statement-prefix restrictions](lazy-evaluation-limitations.md) |
| Invalid signatures | [Preserved slots, unavailable bodies and fail-closed emission](invalid-signature-recovery.md) |
| Native events | [Implicit Bool contracts, fallthrough and patch/export limits](redmod-events.md) |
| Native array members | [Dynamic-array Grow and Count descriptors](redmod-array-descriptors.md) |
| Inference | [Current safeguards and transactional hardening work](type-inference-hardening.md) |

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

- [Adapter API](../crates/compiler/redmod/adapter/README.md) and
  [transpiler API](../crates/compiler/redmod/transpiler/README.md).
- Optional [native parser corpus](../crates/syntax/redmod/parser/CORPUS.md) and
  [semantic corpus survey](../crates/compiler/redmod/adapter/CORPUS.md).
- [Historical investigation summary](language-investigations.md) and
  [conversion experiment findings](conversion-implementation-notes.md). These
  preserve evidence and rejected approaches, not current validation results.
- [Contribution rules and validation commands](../CONTRIBUTING.md).

Compiler tests, official acceptance, decoded representation and runtime behavior
are distinct evidence levels. Installed-source surveys are optional local checks;
no installed game scripts, generated bundles or disposable local reports are
required to use these guides. Historical counts must not be presented as current
results or semantic-accuracy percentages.

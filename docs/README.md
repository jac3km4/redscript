# Language Guide

This guide describes the REDscript compiler and its bounded native REDmod
support. For an introductory language tutorial, see the
[official REDscript wiki](https://wiki.redmodding.org/redscript).

> [!IMPORTANT]
> **Implementation Contracts and Runtime Compatibility**
> The specifications in this guide represent compiler implementation contracts. Unit tests,
> bytecode snapshots, and official compiler acceptance establish tooling and encoding behavior,
> not in-game execution compatibility.

## Dialect guides

The compiler supports two distinct language dialects:

- **[Community REDscript](community-language.md)**: The full community language (`.reds`) compiled
  by the frontend and backend, including classes, generics, closures, inference, and bytecode generation.
- **[Native REDmod](redmod-language.md)**: The native REDmod dialect (`.script`), interop boundaries,
  and community-to-REDmod source export (transpilation).

### Quick comparison

| Feature | Community REDscript (`.reds`) | Native REDmod (`.script`) |
| :--- | :--- | :--- |
| **`ref<T>`** | Strong class reference handle | Script reference (equivalent to `script_ref<T>`) |
| **`script_ref<T>`** | Explicit script reference type | Native script reference (spelled `ref<T>`) |
| **Reference assignment** | Rebinds the reference variable | Writes through to the pointee |
| **Const locals** | `const x = 1;` (declaration init allowed) | `const var x: Int32;` (scalar init rejected; handle rebinding allowed) |
| **Trailing comma `Call(42,)`** | 1 argument (`42`) | 2 arguments (`42` + trailing omitted hole) |
| **Omitted enum storage** | 4 bytes for Int32-range values, 8 bytes otherwise | 4 bytes for Int32-range values, 8 bytes otherwise (official REDmod defaults to 4 bytes) |
| **Event callbacks** | Authored return type enforced | Implicit `Bool` contract; fallthrough emits `Nop` |

## Topic reference

| Topic | Dialect | Reference Section |
| :--- | :--- | :--- |
| **Const parameters and locals** | Community | [Community Guide: Constness](community-language.md#const-parameters-and-locals) |
| **Const handle rebinding** | REDmod | [REDmod Guide: Native const semantics](redmod-language.md#native-const-semantics) |
| **Expressions & default values** | Shared | [Community Guide: Expressions](community-language.md#shared-expression-and-aggregate-default-apis) |
| **Enum storage & widths** | Shared | [Community Guide: Enum storage](community-language.md#explicit-enum-storage) |
| **Conversion declarations** | Shared / REDmod | [Community Guide: Conversions](community-language.md#conversion-declarations) & [REDmod Guide: Conversions](redmod-language.md#conversion-declarations-and-bounded-native-selection) |
| **Call lookup & overloads** | Shared | [Community Guide: Call lookup](community-language.md#call-lookup-overloads-and-inheritance) |
| **Ordered switch defaults** | Shared | [Community Guide: Switch defaults](community-language.md#ordered-switch-defaults) |
| **Overloaded indexing** | Shared | [Community Guide: Indexing](community-language.md#array-and-overloaded-indexing) |
| **Lazy evaluation & array safety** | Shared | [Community Guide: Lazy evaluation](community-language.md#lazy-evaluation-limitations) |
| **Type inference & rollback** | Shared | [Community Guide: Type inference](community-language.md#type-inference-safeguards-and-hardening) |
| **Invalid signatures** | Shared | [Community Guide: Invalid signatures](community-language.md#invalid-signature-recovery) |
| **Native events & callbacks** | REDmod | [REDmod Guide: Events](redmod-language.md#redmod-event-return-contract) |
| **Array Grow & Count** | REDmod | [REDmod Guide: Dynamic-array Grow and Count](redmod-language.md#redmod-dynamic-array-grow-and-count) |
| **Native module imports** | REDmod | [REDmod Guide: Imports](redmod-language.md#redmod-transpiler-imports) |
| **Compound field reads** | REDmod | [REDmod Guide: Field reads](redmod-language.md#bounded-compound-field-reads) |
| **Compound method receivers** | REDmod | [REDmod Guide: Method receivers](redmod-language.md#bounded-compound-method-receivers) |
| **Strong/weak coercions** | REDmod | [REDmod Guide: Coercions](redmod-language.md#bounded-redmod-strongweak-coercions) |
| **Script-reference borrows** | REDmod | [REDmod Guide: Borrows](redmod-language.md#bounded-native-and-scripted-script-reference-borrows) |

## Type reference

Detailed per-type documentation used by the compiler and IDE hover features:

- **Scalars:** [Bool](types/Bool.md), [Int8](types/Int8.md), [Int16](types/Int16.md),
  [Int32](types/Int32.md), [Int64](types/Int64.md), [Uint8](types/Uint8.md),
  [Uint16](types/Uint16.md), [Uint32](types/Uint32.md), [Uint64](types/Uint64.md),
  [Float](types/Float.md), [Double](types/Double.md).
- **Text and identifiers:** [String](types/String.md), [CName](types/Cname.md),
  [TweakDBID](types/TweakDbId.md), [ResRef](types/ResRef.md).
- **Containers and references:** [Array](types/Array.md), [ref](types/Ref.md),
  [wref](types/Wref.md), [script_ref](types/ScriptRef.md), [Variant](types/Variant.md).
- **Opaque engine primitives:** [NodeRef](types/NodeRef.md), [CRUID](types/Cruid.md),
  [LocalizationString](types/LocalizationString.md),
  [redResourceReferenceScriptToken](types/RedResourceReferenceScriptToken.md).
- **Special types:** [Void](types/Void.md), [Nothing](types/Nothing.md).

## Diagnostics and integration

CLI `compile` and `lint` default to strict diagnostics. Passing `--runtime-compatible` matches
SCC's fatal threshold without guaranteeing correct legacy evaluation. See the
[diagnostic mode contract](community-language.md#diagnostics-and-tool-boundaries).

- **Crate APIs:**
  - [REDmod Adapter API](../crates/compiler/redmod/adapter/README.md)
  - [REDmod Transpiler API](../crates/compiler/redmod/transpiler/README.md)
- **Investigation and Research:**
  - [Future Work & Limitations](language-future-work.md): Outstanding correctness issues, missing capabilities, and validation roadmaps (including [repeated Variant initialization](language-future-work.md#variant-default-initialization-on-repeated-declarations)).
  - [Historical Investigations](language-investigations.md): Archive of corpus findings, official evidence limits, and conversion experiments.
- **Development & Contribution:**
  - [Contribution rules and validation commands](../CONTRIBUTING.md).

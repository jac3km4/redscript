# Community REDscript

Community REDscript is the language compiled by the shared frontend and bytecode
backend. Native REDmod is a separate dialect, not an interchangeable spelling of
this language (see the [Native REDmod Guide](redmod-language.md)). This guide
provides the complete technical specification of community REDscript syntax,
type system, evaluation semantics, lookup, and compiler behavior. Introductory
tutorials are available on the [REDscript wiki](https://wiki.redmodding.org/redscript).

> [!IMPORTANT]
> **Implementation Contracts and Runtime Compatibility**
> The specifications in this guide represent compiler implementation contracts. Unit tests,
> bytecode snapshots, and official compiler acceptance establish tooling and encoding behavior,
> not in-game execution compatibility.

## Table of contents

- [Declarations and types](#declarations-and-types)
  - [Functions, locals, and bindings](#functions-locals-and-bindings)
  - [Reference types](#reference-types)
  - [Explicit enum storage](#explicit-enum-storage)
  - [Opaque engine primitives](#opaque-engine-primitives)
- [Expressions, calls, and mutation](#expressions-calls-and-mutation)
  - [Shared expression and aggregate-default APIs](#shared-expression-and-aggregate-default-apis)
  - [Community omitted arguments](#community-omitted-arguments)
  - [Array and overloaded indexing](#array-and-overloaded-indexing)
  - [Shared aggregate defaults](#shared-aggregate-defaults)
  - [Array literal evaluation boundary](#array-literal-evaluation-boundary)
  - [Conversion declarations](#conversion-declarations)
- [Constness and initialization](#constness-and-initialization)
  - [Const parameters and locals](#const-parameters-and-locals)
  - [Const locals](#const-locals)
  - [Export and decompilation](#export-and-decompilation)
  - [Analysis bounds](#analysis-bounds)
  - [Allocation vs execution-site initialization](#allocation-vs-execution-site-initialization)
- [Evaluation and control flow](#evaluation-and-control-flow)
  - [Switch order and overloaded indexing](#switch-order-and-overloaded-indexing)
  - [Lazy evaluation limitations](#lazy-evaluation-limitations)
- [Lookup, inference, and diagnostic recovery](#lookup-inference-and-diagnostic-recovery)
  - [Call lookup, overloads, and inheritance](#call-lookup-overloads-and-inheritance)
  - [Type inference safeguards and hardening](#type-inference-safeguards-and-hardening)
  - [Invalid signature recovery](#invalid-signature-recovery)
- [Diagnostics and tool boundaries](#diagnostics-and-tool-boundaries)

---

## Declarations and types

### Functions, locals, and bindings

Functions use `func`, locals use `let`, and const locals use `const` (the older `const let` syntax is rejected):

```redscript
// Functions, locals, and const bindings
func Add(a: Int32, b: Int32) -> Int32 {
  let result = a + b;
  const multiplier = 2;
  return result * multiplier;
}
```

Signatures and local types can be inferred where supported by the compiler frontend. Classes, structs, enums, generic types/functions, lambdas, and function values are full community features; accepting them here does not imply native export support.

### Reference types

Community REDscript defines three distinct reference handles:
- **`ref<T>`**: Strong class reference handle (retains object ownership).
- **`wref<T>`**: Weak class reference handle (non-owning reference; automatically invalidated when the object is destroyed).
- **`script_ref<T>`**: Explicit script-reference type representing a pass-by-reference pointer to storage.

Wrapping and unwrapping script references compose with other coercions (for example, unwrapping before converting to a value, or converting to the pointee type before wrapping). Member, method, and index receivers unwrap script references automatically.

Community reference assignment (`refVar = otherRef`) and `out` parameter passing rebind the reference variable itself, using community reference-storage rules rather than native write-through policy (see [Native REDmod Guide](redmod-language.md#declarations-and-references)).

### Explicit enum storage

Both community and native source accept explicit enum storage specifications:

```redscript
enum Choice: Uint8 {
  First = 0,
  Second = 1
}
```

- **Legal storage types:** `Int8`, `Uint8`, `Int16`, `Uint16`, `Int32`, `Uint32`, and `Int64`. `Uint64` and non-integer types are rejected.
- **Range checking:** Explicit values and implicit increments must fit the declared signed/unsigned range. Increments continue from the maximum value observed so far.
- **Literals:** Community enum literals use the `l` suffix for values beyond Int32 range (for example, `Large = 9223372036854775807l`).
- **Omitted storage default:** When storage is omitted, the compiler defaults to **4 bytes** for values within the signed Int32 range, and **8 bytes** if any value exceeds Int32. Use explicit storage when another width (such as 1 or 2 bytes) is required.

#### Bundle representation

Version-14 script bundles store visibility, one byte of width (1, 2, 4, or 8 bytes), member indices, and a native flag. Member values are stored as signed 64-bit integers regardless of declared storage. There is no signedness field on disk.

Consequently:
- Source schemas retain the exact declared signedness.
- Loaded schemas expose the stored width, while signedness is unknown (`storage()` returns `None`).
- Re-emission preserves loaded widths instead of recomputing them.
- **Decompilation:** Prints explicit storage to preserve width. It chooses the signed type when all member values fit, otherwise the unsigned type of the same width. This is a canonical representable declaration, not recovery of the original authored signedness.

#### Official characterization (2026-10-30)

Local REDmod `REDscript Compiler v1.2.2` accepted all seven legal storage types and their boundaries (Int64 through +/-9223372036854775807), including Uint32's 4294967295. It rejected Uint64, Float, Bool and CName storage, negative Uint8 and Uint32 values, Int8 128, Uint8 256, and incrementing Uint8 255. It rejected the Int64 minimum spelling `-9223372036854775808` during token conversion. Inspection of compiled bundles confirmed widths 1/2/4/8 as declared, 4 for omitted storage, and identical enum payload metadata for Int8 and Uint8 with the same value.

### Opaque engine primitives

The four opaque engine primitives:
- `NodeRef`
- `CRUID`
- `LocalizationString`
- `redResourceReferenceScriptToken`

are shared opaque builtins, not `Uint64` aliases or fabricated classes. Their registration in the compiler does not introduce literal syntax, default values, struct field layouts, or arbitrary conversions.

---

## Expressions, calls, and mutation

### Shared expression and aggregate-default APIs

- **Assignment is value-producing:** An assignment yields its coerced destination type and is not an assignable place. The destination is evaluated once, strictly before the RHS, with no destination readback. Chains, arguments, returns, and conditional branches can use that value:
  ```redscript
  let x: Int32;
  let y: Int32;
  x = y = 10; // y = 10 yields 10, then x is assigned 10
  ```
- **Callbacks on native aggregates:** Bodyless callbacks on native, non-import-only aggregates retain callback flags without becoming native methods or acquiring synthetic bodies. Scripted and import-only aggregates and ordinary methods retain body checks.
- **Script-reference coercions:** Unwrap before converting to a value, or convert to the pointee type before wrapping. Generated conversions preserve the exact pointee, including weak wrappers.
- **Reference policy:** In community source, assigning or passing a `script_ref<T>` as an `out` argument operates on the reference handle itself (`script_ref_write_through` is false). In native REDmod, it writes through to the referenced storage place.

### Community omitted arguments

Leading and doubled commas denote omitted positional arguments matching optional parameters and emit bytecode `Nop` (not `null`).

In community syntax, a trailing comma **does not** add a slot:
```redscript
Target(42,)   // 1 argument: 42
Target(42,,)  // 2 arguments: 42 followed by 1 omitted hole
Target(,)     // 1 argument: omitted hole
```
Formatting preserves this arity in compact and multiline calls. Note that native REDmod syntax differs: in native `.script`, `Target(42,)` emits 2 arguments (42 and a trailing hole).

### Array and overloaded indexing

- **Built-in array indexing:** Dynamic and static arrays have built-in Int32 indexing with writable-place semantics (`arr[0] = 5`), even if an `OperatorArray` overload is visible.
- **Overloaded indexing (`OperatorArray`):** Non-array indexing resolves `OperatorArray(receiver, key)` through the shared free-function call builder, preserving overload resolution, parameter/out checks, and argument order. The result is a function call, **not** an assignable place.
  Community code can declare overloaded indexing via:
  ```redscript
  func operator[](out value: Data, key: String) -> Bool
  ```
  or by using the canonical name `OperatorArray`. Backend mangling includes parameter and result types.

### Shared aggregate defaults

Both classes and structs accept member default directives:

```redscript
class Config {
  default timeout = 30;
  default enabled = true;
}
```

- Aggregate defaults are typed metadata stored on declaring fields keyed by the supplying class, not constructor assignments; they emit no initialization bytecode.
- The compiler resolves target fields across own and inherited members and validates bounded literal/enum constants against substituted field types.
- **Precedence:** Own-field defaults follow first-wins order (including inline field initializers); inherited defaults follow last-wins order per supplying class.
- Type checking is intentionally stricter than official inherited defaults that accept some invalid text. Constant lowering is bounded to supported literal/enum forms, not general compile-time expression evaluation.

### Array literal evaluation boundary

Array literals currently lower to temporary array construction in statement prefixes, not inside expression bytecode.

Shared lowering reports `UNSAFE_ARRAY_EVALUATION` (`Warning`) when this statement-prefix construction could move past an earlier observable expression or out of a lazy branch:
- Assignments inside later array arguments or elements.
- Array literals inside logical RHS expressions (`a && [1]`).
- Array literals inside conditional arms (`cond ? [1, 2] : []`).
- Array literals inside runtime member callees or closure/function-value callees where a prefix could alter the callable before it is read.

```redscript
// UNSAFE: array literal inside a lazy branch can evaluate eagerly
CallWithSkipParam(condition ? [1, 2] : []);

// SAFE: bind array to a local at its execution site
let items = [1, 2];
CallWithSkipParam(items);
```

Literal-only nested arrays, ordinary local array initializations, and arguments to type-qualified static calls (`Host.Take([1])`) are safe and accepted.

The warning reports legacy statement-prefix lowering and does not prevent output in strict or `--runtime-compatible` modes. However, that emitted code can retain reordered or eager evaluation; it is **not a correctness guarantee**. To guarantee safety, extract the array to a local at its execution site.

### Conversion declarations

Community syntax allows declaring explicit and implicit conversion intent:

```redscript
native implicit func Cast(value: Int32) -> MyType;
native explicit func Cast(value: MyType) -> Int32;
```

- Intent is recorded in `FunctionProperties::conversion: Option<ConversionKind>`.
- Community `implicit func Cast` / `explicit func Cast` declarations preserve authored intent, but **do not** enable arbitrary user-defined conversion dispatch in community expressions.
- Ordinary unmarked community `func Cast` functions keep their existing behavior.
- Marked conversion dispatch selection is a native-only capability (see [Native REDmod Guide](redmod-language.md#conversion-declarations-and-bounded-native-selection)). Binary bundles do not store authored conversion intent.

---

## Constness and initialization

### Const parameters and locals

Const qualifiers are enforced by frontend analysis after function bodies are lowered and calls resolved:

- **Direct writes:** Assigning to a `const` parameter, modifying its fields, passing it to an explicit `out` parameter, or calling mutating array intrinsics are hard errors (`CONST_PARAMETER_WRITE`).
- **Const locals:** A const local's declaration initializer is permitted exactly once. Subsequent binding assignments are rejected, even after an uninitialized declaration:
  ```redscript
  const limit: Int32 = 100; // Allowed: declaration assignment
  // limit = 200;           // ERROR: cannot assign to const local
  ```
  Mutating referent fields, indexing as a writable place, and passing const locals as `out` arguments are hard errors (`CONST_LOCAL_WRITE`).
- **Uncertain non-`out` forwarding:** Forwarding a const reference or container to an uncertain non-`out` parameter or function value is diagnosed with `ErrorAllowedAtRuntime` severity (`CONST_PARAMETER_FORWARD`, `CONST_LOCAL_FORWARD`). In default strict mode, this is rejected; `--runtime-compatible` and SCC tolerate it.
- **Receivers:** Calling non-const instance methods on const local referents is rejected; calling const methods is allowed.

Provenance tracks through inherited fields, script-reference `Deref`/`AsRef`, strong/weak conversions (`WeakRefToRef`/`RefToWeakRef`), dynamic casts, and both arms of conditional expressions. Array reads and scalar by-value arguments are always permitted.

### Const locals

Community syntax permits:
```redscript
const name: Type;
const name = value;
const name: Type = value;
```
The qualifier `ir::LocalInfo::is_const` survives lowering and hoisting. `LocalInfo::initializer` records the exact declaration-store span during lowering. Validation matches both local ID and span, consuming the initialization exemption once per local per closure scope.

Nested binding writes, captured writes, and referent mutation inside the initializer receive no exemption.

### Export and decompilation

- **REDmod Transpiler:** Explicitly rejects const locals rather than silently emitting them as mutable or dropping qualifiers (native const locals have different rebinding semantics).
- **Decompiler:** Reconstructs initialized const declarations and read-only VM-default const locals. When an initializer cannot be preserved at its execution site (e.g., expanded array writes or expressions requiring hoisting), it emits a warning with the function and local names and falls back to a mutable `let` only for that affected local.

### Analysis bounds

Const checking is direct-root validation, not interprocedural alias or effect analysis:
- Copying a reference into a mutable local or returning it does not propagate constness.
- Calls returning references and generic parameters without mutable bounds can hide mutation.
- Closure bodies are checked; captured const roots remain validated. Captures are identified by `Local` identity, so colliding IDs across nested closures are treated conservatively as const. Lambda syntax does not accept const parameter qualifiers.

### Allocation vs execution-site initialization

Declaration execution and storage allocation are separate:
- Array construction retains explicit execution-site operations for repeated loop declarations because a slot may have escaped via script references on an earlier iteration.
- **Variant reset risk:** Skipped `Stmt::InitDefault` for `Variant` locals in the backend relies on function-entry VM defaults. On loop back edges, repeated declarations do not currently re-initialize Variants to empty (see [Future Work](language-future-work.md#variant-default-initialization-on-repeated-declarations)).

---

## Evaluation and control flow

### Switch order and overloaded indexing

#### Ordered switch defaults

Shared AST and IR retain case bodies and `default_index` (the count of case labels preceding `default`):
- Both parsers reject duplicate defaults.
- For non-final defaults, backend dispatch tests case matchers in source order and jumps to the selected body, jumping to `default` only after all preceding matchers fail.
- Case bodies remain in source order for fallthrough, and the scrutinee is evaluated exactly once. Switch `break` targets are local, while enclosing loop `continue` targets survive.
- Pattern cases reject fallthrough into a binding-dependent body.

**Decompiler reconstruction:** Reconstructs grouped labels, backward-linked labels sharing a body, and detached dispatch with first/middle default bodies where layout is provable. Cyclic links, backward jumps between distinct bodies, or unprovable boundaries fail cleanly.

### Lazy evaluation limitations

Functions can declare parameters with the `skip` modifier to indicate lazy (short-circuiting) evaluation:

```redscript
func Assert(condition: Bool, skip message: String) -> Void
```

- The compiler checks arguments supplied to skipped parameters. Array literals anywhere within a skipped argument (including nested arrays, calls, casts, and interpolations) trigger `UNSAFE_ARRAY_EVALUATION`.
- The warning does not fail the build in default or runtime-compatible modes, but emitted code retains statement-prefix array construction that can evaluate eagerly outside the lazy branch.

#### Typed resolver boundary

When compiler-generated calls or operators enter the typed resolver with IR instead of source operands, statement-prefix origins can no longer be tracked. For a selected declaration with a skipped argument, this source-free path conservatively rejects with the hard error `UNSUPPORTED_SKIP_ARGUMENT`.

An exception is made for logical `&&`/`||` RHS array construction, which is diagnosed by the source array guard before typed operator dispatch. Generated logical calls use prefix-free operands.

---

## Lookup, inference, and diagnostic recovery

### Call lookup, overloads, and inheritance

#### Overloads and generated forwarders

Overload selection can choose a unique undominated applicable signature when reference parameters narrow along declared class ancestry:
- Bounded to equal parameter counts, identical flags, concrete non-generic class parameters, and matching strong-reference representations.
- Omitted arguments, out parameters, weak/script references, Variant, and numeric conversions do not establish specificity.
- Optional-tail overlap remains ambiguous; there is no blanket preference for fewest defaults.

**Generated static forwarders:**
Static methods generate receiver-first forwarder aliases (`Type.Method(instance, ...)`). Real instance methods across ancestry suppress generated aliases with matching effective parameter types, regardless of declaration order. Forwarded receivers evaluate once, first, adhering to the original parameter's out/ref rules.

**Qualified static inheritance:** Candidates are gathered across ancestry. Nearer declarations hide only identical parameter signatures. Inherited generic parameters are substituted before comparison. An instance method does not hide a base static method.

#### Override slots and access

Symbols retain immediate override predecessors and expose the canonical original `override_slot`:
- **Community overrides:** Enforce covariance against the nearest override.
- **Native REDmod overrides:** Validate covariance against the original root slot, allowing bounded sibling returns and widening back to the root contract.
- Covariance is restricted to strong/weak class references; arrays, structs, and script references require exact types.
- Protected persistent fields use declaring-class ownership: declaring classes and descendants can access protected persistent fields, including through base-typed receivers.

### Type inference safeguards and hardening

The solver (`crates/compiler/frontend/src/lower/types.rs`) maintains variable bounds and directed subtype edges:
- Variable-to-variable subtyping does not equate unrelated variables.
- Strongly connected components merge without collapsing unrelated variables.
- Invariant constraints enforce equality on both bounds.
- Rigid type parameters are validated via proven bounds, not converted to inference variables.

**Application joins:** Operands project to a common nominal base before combining substituted arguments:

| Parameter variance | Join | Meet |
| :--- | :--- | :--- |
| **Covariant** | Join arguments | Meet arguments |
| **Contravariant** | Meet arguments | Join arguments |
| **Invariant** | Enforce equality | Enforce equality |

Bound refinement checks for occurrences, validates prospective bounds, and stores them before propagating to neighbors. Coalescing includes cycle protection. Ownership components retain internal constraint graphs and release handles when external references drop. Rejected constraints clear update guards and remove newly added edges.

#### Remaining goal: atomic failed operations

Current validate-before-store does not provide full transactional rollback across reachable inference state when a multi-step constraint fails. Achieving complete failure atomicity requires an internal rollback journal (see [Future Work](language-future-work.md#transactional-inference-and-failure-rollback)).

### Invalid signature recovery

When a parameter or return type fails to resolve in a function declaration:
- The compiler preserves parameter count, order, names, qualifiers, source spans, and all successfully resolved types.
- An omitted return type defaults to `Void`; an unresolved explicit return does not.
- `FunctionType::is_valid()` is set to false. Unresolved slots store `Type::Nothing` as a placeholder (this is **not** bottom-type inference recovery; consumers must check validity).
- Invalid declarations remain in symbols so diagnostics can refer to them, but they are excluded from overload candidates, override comparisons, and generated static forwarders.
- Function bodies belonging to invalid signatures are not lowered, avoiding corrupted parameter bindings or fabricated return contracts.
- The backend rejects symbols containing invalid function signatures before modifying output bundles.

---

## Diagnostics and tool boundaries

### Diagnostic modes

The CLI commands `compile` and `lint` enforce strict validation by default:
- **Default (Strict):** Fatal threshold is `DiagnosticLevel::ErrorAllowedAtRuntime`. Any access/visibility violations or uncertain const forwarding fail the build.
- **`--runtime-compatible`:** Fatal threshold is `DiagnosticLevel::Error`, matching official SCC compiler behavior. Runtime-tolerated errors are reported as diagnostics without failing the compilation or preventing output bundle generation. Hard errors (syntax errors, definite const writes, unresolvable overloads) still fail the build.

### Compiler API

The `redscript-compiler-api` crate exposes programmatic interfaces for compilation and linting:
- Callers can configure `fatal_diagnostic_level` explicitly.
- Diagnostic display filters are decoupled from fatality thresholds.
- Backend errors prevent bundle serialization regardless of diagnostic mode.

### Decompiler and transpiler boundaries

- **Decompiler:** Preserves reconstructable const initializers and read-only VM defaults. When initialization cannot be reconstructed at its source site, it emits a warning and falls back to `let`. Structured switch reconstruction handles non-final defaults and shared-body cases when flow is provable.
- **REDmod Transpiler:** Operates on typed community IR and fails closed with diagnostics on unsupported community features (custom generics, lambdas, const locals, unsafe array prefixes). Callbacks require non-const `Bool` returns to match native event contracts.

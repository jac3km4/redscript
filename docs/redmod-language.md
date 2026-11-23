# Native REDmod Language Support

This repository provides native REDmod script (`.script`) parsing, formatting,
bounded shared semantic analysis, mixed-language IDE support, and strict
community-to-REDmod source export (transpilation).

Native REDmod is a separate dialect with its own syntax, type rules, reference
model, and compiler constraints. Full language parity, installed-engine binding
equivalence, and game-runtime compatibility are not established; parser
acceptance is broader than semantic or backend support.

> [!IMPORTANT]
> **Implementation Contracts and Runtime Compatibility**
> Bytecode snapshots and official compiler acceptance establish tooling and encoding
> behavior, not in-game execution compatibility. Historical corpus counts describe
> recorded investigation baselines, not current validation percentages.

## Table of contents

- [Processing and source policy](#processing-and-source-policy)
- [Declarations, modifiers, and references](#declarations-modifiers-and-references)
  - [Modifiers](#modifiers)
  - [Fields and metadata](#fields-and-metadata)
  - [Hoisted locals](#hoisted-locals)
  - [Native ref is a script reference](#native-ref-is-a-script-reference)
  - [Enum storage and engine primitives](#enum-storage-and-engine-primitives)
- [Modules and imports](#modules-and-imports)
  - [REDmod transpiler imports](#redmod-transpiler-imports)
- [Events and callbacks](#events-and-callbacks)
  - [REDmod event return contract](#redmod-event-return-contract)
  - [Normalization and metadata](#normalization-and-metadata)
  - [Export and decompilation boundaries](#export-and-decompilation-boundaries)
  - [Official event evidence](#official-event-evidence)
- [Storage access, receivers, and arrays](#storage-access-receivers-and-arrays)
  - [REDmod dynamic-array Grow and Count](#redmod-dynamic-array-grow-and-count)
  - [Bounded compound field reads](#bounded-compound-field-reads)
  - [Limits and place semantics](#limits-and-place-semantics)
  - [Bounded compound method receivers](#bounded-compound-method-receivers)
- [Calls, inheritance, and conversions](#calls-inheritance-and-conversions)
  - [Native implicit lookup](#native-implicit-lookup)
  - [Trailing comma argument holes](#trailing-comma-argument-holes)
  - [Conversion declarations and bounded native selection](#conversion-declarations-and-bounded-native-selection)
  - [Override slots and return widening](#override-slots-and-return-widening)
- [Control flow, constness, and defaults](#control-flow-constness-and-defaults)
  - [Control flow fixups and switches](#control-flow-fixups-and-switches)
  - [Dynamic array descriptors](#dynamic-array-descriptors)
  - [Native const semantics](#native-const-semantics)
  - [Mutable fields](#mutable-fields)
  - [Aggregate and struct defaults](#aggregate-and-struct-defaults)
- [Typed coercion and borrowing](#typed-coercion-and-borrowing)
  - [Bounded REDmod strong/weak coercions](#bounded-redmod-strongweak-coercions)
  - [Bounded native and scripted script-reference borrows](#bounded-native-and-scripted-script-reference-borrows)
  - [Lifetime contract](#lifetime-contract)
  - [Borrow lifetime evidence](#borrow-lifetime-evidence)
- [Community source export compatibility](#community-source-export-compatibility)

---

## Processing and source policy

The compiler architecture maintains a strict separation of concerns:
- **Syntax layer:** Grammar, AST definitions, parsing, and formatting reside in `crates/syntax/redmod/{ast,parser,formatter}`.
- **Adapter layer:** Mapping native syntax to shared semantic representations and validating target capabilities resides in `crates/compiler/redmod/adapter`.
- **Shared semantics:** Frontend extensions are language-neutral, applying narrow per-file policies for dialect-specific differences. Native policies never weaken community source checks in mixed compilations.

**Module processing:** Use `parse_file_with_metadata`, retain the complete `ParsedModule`, and pass it to `process_sources`. Native `for` and `do-while` loop fixups depend on this metadata; bare-AST convenience APIs fail closed for these constructs.

Compilation checks syntax errors, adapter processing errors, and shared frontend diagnostics. If parsing or bridging fails, entire modules are excluded; partial declaration harvesting is not implemented.

---

## Declarations, modifiers, and references

### Modifiers

- **`export` and `constexpr`:** Treated as analysis-only no-ops on verified function targets. They do not imply engine export or compile-time evaluation. Unsupported modifier targets fail.
- **`virtual`:** Dispatches via checked non-final dispatch. Ordinary private methods are implicitly final.

### Fields and metadata

- Omitted field visibility defaults to `public`; explicit visibility modifiers remain effective.
- String field properties preserve keys, duplicates, and declaration order. Property hints target one field within the same aggregate.
- `mutable` is accepted on aggregate dynamic array fields and bounded Float fields (see [Mutable fields](#mutable-fields)).

### Hoisted locals

In native functions, `var` local declarations are function-wide and hoisted with duplicate checks and entry defaults. Explicit initializers remain at their execution sites. Uninitialized groups expand sequentially in source order. Initialized groups (`var a, b = 1;`) and general value sequences are unsupported.

### Native ref is a script reference

In native REDmod, **`ref<T>` means a script reference**, equivalent to community `script_ref<T>`. It does **not** denote a strong class handle:

```redscript
public class Handler {
  // Native ref<T> is a script reference to Int32, not a strong class handle
  public func Increment(counter: ref<Int32>) -> Void {
    counter = counter + 1; // Writes through to the referenced integer
  }
}
```

Key characteristics:
- In official native `scripts/`, `ref<T>` appears only on function parameters (never on `var` local declarations or function return types).
- `out` and `ref<T>` are mutually exclusive on the same parameter.
- Non-const `ref<T>` parameters write through on assignment and indirect mutations.
- `ref<T>` locals, `ref<T>` returns, nested references (`ref<ref<T>>`), and arrays of references are rejected.

### Enum storage and engine primitives

- **Enum storage:** Accepts `enum Choice: Uint8 { ... }` with explicit width. Omitted storage defaults to 4 bytes for Int32-range values and 8 bytes otherwise (official REDmod defaults to 4 bytes in all cases).
- **Engine primitives:** `NodeRef`, `CRUID`, `LocalizationString`, and `redResourceReferenceScriptToken` are shared opaque primitives, not integer aliases or fabricated classes.

---

## Modules and imports

### REDmod transpiler imports

The community-to-REDmod transpiler converts module imports as follows:
- `import M.Name` emits `@using "M.Name"`.
- `import M.{First, Second}` emits separate symbol-level directives `@using "M.First"` and `@using "M.Second"`. It does **not** widen to `@using "M"`.
- Wildcard imports `import M.*` emit module-level `@using "M"`.
- Directives terminate with newlines, **never semicolons** (the official REDmod compiler rejects directive semicolons).

#### Bounded checks and collision guards

- Module paths and selected names must be valid target identifiers. Root-level exact imports and empty selections are rejected.
- **Prefix overlap guard:** If a selected symbol path coincides with a module (or ancestor module) in the output, the transpiler rejects it instead of risking ambiguous scope widening.
- Analysis-only dependencies do not emit output modules or cause global name collision errors.
- Official compiler v1.2.2 accepts `@using "First.Pick"` for calling all `Pick` overloads but rejects `First.Other` as an unused module; if module `First.Pick` also contains function `Extra`, `@using "First.Pick"` permits `Extra(1)`, which is why the prefix-overlap guard is required. Qualified calls such as `First.Pick(1)` are rejected as an invalid global identifier `First`. Emitted directives without semicolons compiled with no errors across module-only, exact, select, and wildcard test suites.

---

## Events and callbacks

### REDmod event return contract

Native REDmod events follow an **implicit `Bool` contract**, not `Void` or an inferred return type. Explicit event return type annotations are invalid.

```redscript
// Native event: implicit Bool return contract
public class MyCustomComponent extends ScriptableComponent {
  protected event OnAction(evt: ref<ActionEvent>) {
    // Fallthrough is legal and emits bytecode Nop
  }
}
```

### Normalization and metadata

- **Semicolon events:** Semicolon events on script classes normalize to empty `Bool` implementations with fallthrough.
- **Callback prototypes:** Imported callback prototypes remain bodyless, preserving callback identity without native method flags.
- **Fallthrough:** Adapter finishing appends bytecode `Nop` to callback implementations and patches on fallthrough, not a return expression or default false value.
- **Return expressions:** Return expressions use ordinary shared coercion; there is no return-specific Int32-to-Bool hook. Bare `return;` is rejected as a Void-to-Bool mismatch.
- **Transpiler requirements:** Community callbacks exported to REDmod require a non-const `Bool` return type. Incompatible contracts produce diagnostics.

### Export and decompilation boundaries

- Export requires a non-const `Bool` contract and preserves `Bool` return expressions. Void or non-Bool callbacks fail closed.
- Decompiler reconstructs non-final defaults and switch dispatch independently of event return contracts.

### Official event evidence

Official compiler v1.2.2 probes established the behavioral contract above:
- An empty event body compiles to `Bool` metadata with a `Profile` then `Nop`, and no default false return.
- `Bool` returns and conditional `Bool` returns with fallthrough are accepted; the latter emits a trailing `Nop`. Bare `return;` is rejected as a Void-to-Bool mismatch.
- `Int32` returns are accepted via native `Cast;Int32;Bool`; `Float` and `String` returns are rejected for lack of a conversion to `Bool`.
- Derived/imported callback implementations carry `Bool` metadata and an inherited base link. Semicolon events are accepted on script classes but rejected on `importonly` classes.

The shared adapter is stricter than official here: it has no implicit `Int32`-to-`Bool` return conversion and rejects such returns rather than emitting `Cast;Int32;Bool`.

---

## Storage access, receivers, and arrays

### REDmod dynamic-array Grow and Count

The native adapter exposes two member descriptors on dynamic array instances:

| Native syntax | Shared intrinsic | Parameters | Result |
| :--- | :--- | :--- | :--- |
| `values.Grow(amount)` | `ArrayGrow` | `amount: Int32` | `Void` |
| `values.Count(value)` | `ArrayCount` | `value: T` | `Int32` |

- Both operations require an **array place** (lvalue). Temporary receivers like `MakeArray().Count(val)` are rejected.
- `Grow` mutates array storage and rejects `const` parameters/locals; `Count` is read-only and accepts `const`.
- Transpiler emits `ArrayGrow` and `ArrayCount` as these member calls on local arrays.
- Fixed-array members and `EraseFast` are not aliased.

### Bounded compound field reads

Field reads use direct native postfix syntax without temporary copies or synthetic null guards:

```redscript
owner.child.flag;
Select(steps, owner).child.flag;
(enabled ? Select(steps, owner) : Select(steps, other)).child.flag;
owner.child.data.visualStateData.m_isIconic;
Deref(data).visualStateData.m_isIconic;
```

#### Limits and place semantics

- **Variable root requirement:** Official compiler v1.2.2 requires a variable parent scope for struct property access. Therefore, struct receivers must be a variable-rooted field chain or `Deref(localScriptRef)`. Temporary struct results (`Make().field`) are rejected for structs.
- **Class handle fields:** Nested class-handle fields are assignable places and can be passed as `out` arguments, even through weak handles.
- **Variable-rooted struct fields:** Variable-rooted struct fields support assignment and `out` arguments, including nested struct fields and `Deref(localScriptRef)` roots. No temporary struct copies or write-back schemes are introduced.
- **Array operations on fields:** `ArraySize`, `ArrayClear`, `ArrayPop`, `ArrayLast`, and indexing accept variable-rooted array fields directly. Other operations (`ArrayPush`, `ArrayContains`, `ArrayGrow`, etc.) accept variable-rooted array fields when all other arguments are non-mutating.
- **Class casts:** Direct field reads through class casts are supported with parentheses at postfix precedence (`((MyClass)obj).field`).

### Bounded compound method receivers

The transpiler emits strong and weak class-reference receivers inline for method call chains, local-rooted fields, conditional expressions, and class casts:

```redscript
Select(owner).Next().Read(Rebind(owner));
owner.child.Read(Rebind(owner));
(enabled ? Select(owner) : Select(other)).Read(Rebind(owner));
owner.handle.Read(owner.handle.Entry(Rebind(owner))); // handle is wref
```

#### Why not mechanically spill temporaries?

Instance calls emit `Context(exit)`, receiver, invocation, arguments, and `ParamEnd`, with `Context` exit after the full invocation:
- Evaluating arguments before entering `Context` could execute side effects that a null receiver should skip.
- Re-reading a receiver after an argument rebinds its root could target a different object.
- Hoisting calls from conditional or lazy branches could alter execution frequency.
- Emitting receivers inline preserves official REDengine `Context` semantics.

#### Deliberate limits

- Weak class-reference receivers undergo implicit promotion inside `Context`.
- Indexed class handles from arrays can call methods directly (`arr[i].Method()`).
- Direct assignments to fields of local strong or weak class handles stay inline (`handle.field = value`).
- Temporary struct receivers, assignment/construction receivers, and compound script-reference receivers remain unsupported.

---

## Calls, inheritance, and conversions

### Native implicit lookup

Native lookup order:
1. Local and parameter callable values.
2. Eligible implicit member methods on `this`.
3. Type constructors.
4. Free functions.

Native locals are function-wide and shadow implicit members. Implicit static calls preserve the enclosing type's semantic identity across module boundaries.

### Trailing comma argument holes

In native `.script` syntax, a trailing comma retains a final omitted argument hole:
```redscript
Target(42,); // Native syntax: 2 arguments (42 followed by 1 omitted hole)
```
This differs from community syntax where `Target(42,)` has arity 1.

### Conversion declarations and bounded native selection

Native conversion declarations use `cast` or `implicit cast`:

```redscript
implicit cast ( param : Int32 ) : MyType { ... }
cast ( param : MyType ) : Int32 { ... }
```

Conversion dispatch in native source is bounded to specific safe patterns:
- **Explicit native providers:** Exact native `Cast` provider calls for monomorphic, non-generic data types with one non-out, non-skip parameter and exact return type. Duplicate candidates fail conservatively with `UNSUPPORTED_NATIVE_CONVERSION`.
- **Bounded explicit enum casts:** Admitted for `enum -> Int32` when enum storage is at most 4 bytes and values fit `Int32`. The reverse direction (`Int32 -> enum`) requires exact `Int32` and default (absent) enum storage.
- **Guarded implicit native providers:** Incompatible fixed pairs at known coercion targets (returns, initializers, assignments, non-out call arguments) can use a unique exact implicit native provider.
- Arbitrary conversion chaining, numeric ranking, and general ToString conversions are not supported.

### Override slots and return widening

REDmod method overrides validate against the canonical original `override_slot`:
- Allows bounded sibling returns and widening back to the root contract.
- Restricts covariance to identical strong/weak class reference representations; arrays, structs, and script references require exact types.
- Omitted visibility inherits from base; narrowed visibility is permitted, but call-site access rules apply.

---

## Control flow, constness, and defaults

### Control flow fixups and switches

- Native `for` and `do-while` loops require metadata fixups from `ParsedModule` to retain execution order and continue targets.
- Ordered switches evaluate case matchers in source order before jumping to non-final defaults. Scrutinees are evaluated once.

### Dynamic array descriptors

In addition to `Grow` and `Count`, the adapter supports standard native dynamic-array descriptors: `Size`, `PushBack`, `PopBack`, `Clear`, `Resize`, `Contains`, `FindFirst`, `FindLast`, `Insert`, `Remove`, `Erase`, and `Last`.

Native fixed-size arrays use `Type[N]` on fields, locals, parameters, and returns (struct fields excluded).

### Native const semantics

- **Handle rebinding:** Native `const var target: ref<MyClass>;` locals permit sequential rebinding to new handles (`target = new MyClass();`) and passing the handle to non-`out` parameters. Referent mutation (`target.field = 5;`) remains rejected.
- **Scalar and struct const initialization:** Native `const var` locals for scalars and structs reject inline initialization and subsequent assignment.
- **Deep const parameters:** In native `scripts/`, `const name: ref<T>` acts as deep const on value types, structs, and arrays (no element or field writes). However, `const ref<array<ClassHandle>>` permits extracting handles and calling non-const methods on the referenced objects.

### Mutable fields

The `mutable` modifier is accepted on verified aggregate dynamic array fields and on a single bounded Float field configuration (`instanceeditable inlined`). It is never combined with `const` and does not represent a general mutable-scalar keyword.

### Aggregate and struct defaults

- Aggregate defaults (`default field = value;`) resolve on declared fields keyed by supplying class, emitting no initialization bytecode.
- Struct defaults emit zero-argument `Type()`, delegating initialization to the engine rather than enumerating field assignments.

---

## Typed coercion and borrowing

### Bounded REDmod strong/weak coercions

The transpiler can emit a single typed `RefToWeakRef` or `WeakRefToRef` conversion using REDmod implicit coercion at an immediate typed sink:
- Supported assignment places or typed local initializers.
- Returns with explicit resolved return types.
- Non-`out` parameters of resolved calls with no competing viable overloads.

```redscript
// Coercion emitted implicitly at typed sink
let weakHandle: wref<TargetClass> = strongHandle;
```

**Safety boundary:** The operand remains at its original execution point. Nested strong/weak conversions (`RefToWeakRef(WeakRefToRef(v))`) are not cancelled. Competing overloads with opposite strong/weak parameters block implicit coercion.

### Bounded native and scripted script-reference borrows

The transpiler accepts nonlocal `AsRef` operands at immediate, exact, non-`out` `script_ref<T>` parameters with matching pointee type `T`.

```redscript
// Nonlocal string borrow at exact parameter
StrContains(GetLocalizedText(key), "pattern");
```

### Lifetime contract

- Direct `AsRef(prvalue)` temporary arguments set the corresponding bit in the invocation's 16-bit rvalue-reference mask, requesting call-owned temporary storage.
- New temporary borrows beyond slot 15 fail closed.
- Variable-rooted fields and array elements retain place identity and do not require invocation temporary bits.
- Nonlocal `AsRef` in returns, variable initializers, assignments, or ambiguous overloads remains rejected.

### Borrow lifetime evidence

Official compiler v1.2.2 confirmed the mask behavior above: direct `AsRef(prvalue)` temporaries set the corresponding bit, local arguments use mask 0, and nested interpolations and scalar/struct results set the expected bits. Bytecode inspection found no synthesized locals or spilled assignments.

---

## Community source export compatibility

The REDmod transpiler (`redscript-cli transpile-redmod`) translates community REDscript into native REDmod `.script` files:

- **Inline receivers:** Method call chains, fields passed as `out`, class casts as receivers, and compound weak-handle reads are emitted inline without temporary spills.
- **Array operations:** `ArraySize`, `ArrayClear`, `ArrayPop`, `ArrayLast`, and indexing accept variable-rooted array fields.
- **Struct fields:** Ordinary struct fields support writes and `out` passing when rooted in a variable or `Deref(localScriptRef)`.
- **String and resource literals:** Resource paths and TweakDBIDs emit backslashes literally rather than using community escape sequences.
- **Unsupported constructs:** Custom generics, lambdas, function types, const locals, unsafe array prefixes, and non-Bool event callbacks fail closed with source diagnostics.

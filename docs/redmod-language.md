# Native REDmod language support

Native parsing/formatting, bounded shared semantic analysis, mixed-language IDE
analysis and strict community-to-REDmod source export exist. Full language parity,
installed-engine binding equivalence and game-runtime compatibility are not
established. Parser acceptance is broader than semantic or backend support.

See the [language guide](README.md), [community dialect](community-language.md),
[adapter API](../crates/compiler/redmod/adapter/README.md) and
[transpiler API](../crates/compiler/redmod/transpiler/README.md). Historical corpus
counts in component reports describe their recorded revisions, not current HEAD.

## Processing and source policy

Keep native AST/grammar/formatting in the native syntax crates and native mapping
and capability checks in the adapter. Shared semantic extensions should be
language-neutral, with narrow per-file policies for actual dialect differences.
Native policies must not weaken community dependencies in a mixed compilation.

Use `parse_file_with_metadata`, retain/clone the entire `ParsedModule`, and pass
it to `process_sources`. Native for/do-while fixups depend on this metadata.
Bare-AST convenience APIs fail closed for these loop forms. Aggregate defaults
are shared AST members and need no private adapter default plans.

Check syntax/capability errors, adapter processing errors **and** shared frontend
diagnostics. Returned IR can contain recovery. Parse/bridge failures still exclude
whole modules; partial declaration harvesting is not implemented. IDE query
availability is not semantic accuracy or permission to emit artifacts.

## Declarations and references

- `export` and `constexpr` are analysis-only no-ops on verified function targets,
  not equivalent engine exposure or compile-time evaluation. Unsupported modifier
  targets still fail. `virtual` uses checked non-final dispatch; ordinary private
  methods are implicitly final.
- Omitted field visibility defaults to public; explicit flags remain effective.
  String field properties preserve keys, duplicates and order. Hints target one
  field in the same aggregate. Broader metadata is not silently discarded.
- Function-wide typed locals are hoisted with duplicate checks and entry defaults;
  explicit initializers remain at their source execution sites. Uninitialized
  groups expand in order, and standalone blocks flatten under the same policy.
  Initialized groups and general value sequences remain unsupported.
- Native `ref<T>` means script reference, not community strong `ref<T>`. Verified
  scalar/string/struct/class/array/weak parameter and field forms compose with
  dereferencing/coercion. Native assignment/out places write through references:
  ```redscript
  public class Handler {
    // Native ref<T> is a script reference to an Int32, not a strong class handle
    public func Increment(counter: ref<Int32>) -> Void {
      counter = counter + 1; // Writes through to the referenced integer
    }
  }
  ```
  Ref locals/returns, nested refs, arrays of refs and mixed-reference forms remain
  rejected. See [shared reference semantics](language-semantics.md#shared-expression-and-aggregate-default-apis).
- [Enum storage](language-semantics.md#explicit-enum-storage) retains declared width/ranges. Omitted native
  storage currently follows inferred community widths, unlike official REDmod's
  four-byte default. Use explicit storage when representation matters.
- NodeRef, CRUID, LocalizationString and redResourceReferenceScriptToken are shared
  opaque primitives, not fabricated classes or integer aliases. No extra literal,
  default-value, layout or conversion behavior follows from registration.

## Calls, inheritance and conversions

[Lookup and inheritance](language-semantics.md#call-lookup-overloads-and-inheritance) defines implicit member precedence,
static ancestry/hiding, generated forwarders, bounded specificity, native original
slot return covariance and persistent access. Implicit static calls retain their
qualified type identity across modules, and generic static hiding compares
substituted parameter signatures. These fixes do not enable arbitrary calls
through runtime value receivers.

Native operators preserve checked signatures. Non-array `operator[]` resolves
OperatorArray; built-in arrays retain writable indexing. Omitted argument slots
must match optional parameters and emit Nop. Unlike community syntax, a final
comma retains a final hole:
```redscript
Target(42,); // Native syntax: 2 arguments (42 followed by 1 omitted hole)
```
Authored parameter-default expressions are separate and unsupported.

[Conversion declarations](language-semantics.md#conversion-declarations-and-bounded-native-selection) preserve explicit/implicit
intent without making providers ordinary overload candidates. Implemented native
selection is bounded to unique exact native providers, guarded implicit coercions
at known targets and selected explicit enum/Int32 conversions. There is no general
conversion-aware matching, chaining or numeric-distance ranking. Variant extraction
precedes object casts; native object casts support identity/upcast/downcast and
resolved object/weak-reference truthiness. Ordinary Cast precedence and contextual
literal typing remain intact. No special Int32-to-Bool return hook exists.

Conditions support bounded ModuleExists/single-negation selection, with deterministic
first-active versions and capability checking of inactive declarations. Conversion
version keys include the Cast return destination, so equal operands with different
destinations are not collapsed. Other grouping still uses source spelling, not
fully resolved semantic signatures; this is not general variant-aware analysis.

## Control flow, arrays and constness

- Native for/do-while fixups retain execution order and continue targets. Ordered
  switch defaults preserve label/body order, fallthrough, one scrutinee evaluation
  and enclosing loop continues. See [control flow and indexing](language-semantics.md#switch-order-and-overloaded-indexing).
- Source `skip` flags retain lazy call intent and serialized parameter flags.
  Shared unsafe-array diagnostics reject output at the strict threshold but can
  retain unsafe legacy lowering in runtime-compatible mode. Source-free typed
  skip calls use a distinct hard error; native capability checks remain hard.
  [Lazy limitations](language-semantics.md#lazy-evaluation-limitations) defines the boundary.
- Dynamic-array descriptors include Size, PushBack, PopBack, Clear, Resize,
  Contains, FindFirst, FindLast, Insert, Remove, Erase and Last, plus
  [Grow and Count](redmod-semantics.md#redmod-dynamic-array-grow-and-count). They require array places;
  temporary receivers are rejected:
  ```redscript
  values.Grow(10);
  let total: Int32 = values.Count(element);
  ```
  Fixed-array members and EraseFast are not aliases to these operations, and native
  source does not expose arbitrary community intrinsic spellings.
- [Const rules](language-semantics.md#const-parameters-and-locals) retain native qualifiers. Parameter element
  writes/non-out forwarding have narrow native relaxations; const local class
  handles permit rebinding/handle forwarding, not referent mutation. Native const
  scalar/struct explicit initialization is rejected, unlike community const
  initialization:
  ```redscript
  const var target: ref<MyClass>; // Class handle rebinding permitted
  target = new MyClass();         // Rebinding the handle is allowed
  // target.field = 5;            // ERROR: referent mutation remains rejected
  ```
  Const fields/returns and arbitrary effects remain incomplete.
- `mutable` is accepted for verified aggregate array fields. The bounded scalar
  form is a Float field on a non-imported class with `instanceeditable inlined`,
  optionally `const`, and no other modifiers. It adds no extra flag and never
  clears explicit const. It is not a general mutable-scalar rule; array-field
  implicit const and scalar const-field write enforcement remain incomplete.

## Defaults, events and output boundaries

[Aggregate defaults](language-semantics.md#shared-aggregate-defaults)
resolve typed own/inherited targets and merge metadata on declaring fields keyed
by supplying class, with no initialization bytecode. Checking is intentionally
stricter than official inherited defaults that accept some invalid text. These
properties do not prove local or runtime construction/reset behavior.

[Events](redmod-semantics.md#redmod-event-return-contract) have implicit Bool contracts, legal fallthrough and no
synthetic default false return. Script semicolon events become empty implementations;
imported prototypes retain callback identity without invented bodies. Importonly
scripted members remain invalid. Bool patch metadata support is not a general
claim of official wrapper/patch parity.

All struct defaults emit `Type()`, delegating construction to the engine rather
than enumerating fields or inferring layouts from imported fields. Explicit reset assignments
remain at repeated declaration sites. Official v1.2.2 probes cover `Vector2`,
`ResRef`, `ItemID`, `inkAnimOptions` and `stimInvestigateData`, including zero-argument
Construct bytecode inside a loop; a scripted nested-struct fixture verifies the
same emitted instruction. This is not in-game verification. Explicit positional
constructor expressions retain their original field arguments.
The transpiler can omit a local's first,
unconditional default initialization when backward liveness proves no read before
a whole-local overwrite. Branches and loop back edges participate in that proof;
reads, borrows, `out` arguments and partial field writes keep the default. Repeated
or conditional declarations retain resets, and switch occurrences are conservatively
treated as reads. This does not infer native layouts or change Variant reset behavior.
Unused declarations without overwrite evidence are not removed by this pass. Default/reset support, additional
builtins, broader patches, contextual function values, float matching and
prefix/postfix behavior remain bounded. Custom native generics, lambdas/function
types, mixedRef/neverRef, unknown annotations and unverified metadata fail closed.
See [future work](language-future-work.md), including the unresolved backend
Variant reset risk and the separate transpiler restrictions.

## Community source export compatibility

The transpiler emits supported receiver expressions inline, without hoisting or
copying them: nested class-field assignments, class fields passed as `out`, class
casts used as method receivers, and compound weak-handle field reads. Persistent
fields use frontend ownership/access checking, not a blanket `this`-only rule.
Official v1.2.2 authored compilation probes cover these forms; the `out` probe also
checks direct field bytecode with no synthesized local/copy assignment. This is
not an in-game execution guarantee.

`ArraySize`, `ArrayClear`, `ArrayPop`, `ArrayLast` and indexing accept
variable-rooted arrays. Indexed struct and class elements support field reads;
indexed class handles also support method calls. Class casts support field reads too.
Call-rooted arrays remain unsupported because the official compiler requires an
in-memory variable root. Other emitted array operations also accept variable-rooted
arrays when all additional arguments are non-mutating, including field chains and
indexing with non-mutating indices. Effectful arguments retain the local-array guard. `ArrayGrow` and `ArrayCount` emit native Grow/Count
members. See [array field limits](redmod-semantics.md#limits-and-place-semantics).

Variable-rooted ordinary struct fields support writes and `out`, including
`Deref(localScriptRef)` roots. Fields and indexed elements can supply scripted
`script_ref<T>` parameters without copies. Temporary arguments use invocation-owned
borrows at exact non-out parameters, with the same lifetime-slot checks as native
calls. Overload ambiguity, escaping references and unsupported borrow contexts
remain guarded. See [field access](redmod-semantics.md#bounded-compound-field-reads) and
[script-reference borrowing](redmod-semantics.md#bounded-native-and-scripted-script-reference-borrows) for the evidence and limits.

Resource and TweakDBID literals emit decoded backslashes literally, not with
community escape spelling. Ordinary resource paths retain their pool identity.
Leading/repeated backslashes in resource paths are rejected because the official
compiler normalizes them; TweakDBID backslashes remain raw. Embedded quotes, NUL
and unsupported controls still fail. The optional `resource_literals` probe
compares the compiled literal pools with the input values.

## Evidence and validation

Optional [parser](../crates/syntax/redmod/parser/CORPUS.md) and
[semantic](../crates/compiler/redmod/adapter/CORPUS.md) surveys use explicitly
supplied local inputs. Do not commit installed scripts or require disposable
reports. Bytecode snapshots establish instruction structure; official authored
probes establish bounded acceptance/representation; neither is game execution.
The [investigation archive](language-investigations.md) preserves key findings,
not a current validation certification or a parity percentage.

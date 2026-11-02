# Call lookup, overloads and inheritance

These are shared frontend rules unless a native policy is stated. See the
[community](community-language.md) and [REDmod](redmod-language.md) guides for
dialect context, and [future work](language-future-work.md) for unsupported cells.

## Native implicit lookup

Native lookup considers local/parameter callable values, eligible implicit methods,
type construction and then free functions. Eligible methods precede same-named
types; community lookup is unchanged. Native locals are function-wide and shadow
implicit members. One official type-versus-local probe selected the type instead;
local-first behavior remains an intentional difference.

Implicit static calls carry the enclosing type's semantic identity across module
boundaries, rather than looking up a short type name in the caller's scope.
This does not justify dropping a runtime receiver's evaluation or accepting all
static/free calls through value receivers. Lookup must use applicability, not
catch lowering errors and retry after mutating inference or evaluating operands.

## Overloads and generated forwarders

Shared selection may choose a unique undominated applicable reference-parameter
signature. This preference is deliberately bounded to equal parameter counts and
flags, concrete nongeneric class parameters and matching strong-reference
representation (or bare class parameters). Every differing parameter must narrow
along declared ancestry. Crossing preferences remain ambiguous.

Omitted/null slots with differing types, out parameters, weak/script-reference
categories, Variant and numeric conversions do not establish this specificity.
These are implementation boundaries, not evidence that official REDmod rejects
all such cases. Optional-tail overlap remains ambiguous; there is no blanket
exact-arity/fewest-defaults preference or return-type-only selection.

Generated receiver-first static forwarders remain queryable fallback aliases, not
authored override declarations. Real instance methods suppress aliases with the
same effective parameter types across ancestry, independent of declaration order;
other alias overloads remain. Bodyless imported static declarations also receive
aliases. Forwarded receivers are evaluated once, first, and checked using the
original receiver parameter's out/ref rules.

Qualified static calls gather candidates across ancestry. Nearer declarations
hide only identical parameter signatures; inherited generic parameters are
substituted into the caller's type context before comparison. A same-named
instance method does not hide a base static. Same-class duplicates are retained
for diagnostics, and selected targets still undergo visibility checks. Native
inherited-static style warnings are suppressed through source policy, not by
changing call accessibility.

## Override slots and access

Symbols retain immediate override predecessors and expose the canonical original
`override_slot`. Return contracts are instantiated through generic bases:

- Community overrides enforce covariance against the nearest override.
- REDmod overrides validate against the original slot, allowing bounded sibling
  returns and widening back to the root contract. Official probes distinguish
  this from arbitrary widening against an intermediate declaration.
- Covariance is restricted to the same strong/weak class-reference representation.
  Arrays, structs, script references and representation changes remain exact-only.

Source-visible return types and emitted return metadata remain declaration-specific.
Native-slot mangling follows the root through scripted overrides. Native policy
permits narrowed override visibility, but call-site access still applies and
omitted visibility still inherits. Written overrides retain final-method and
out-flag checks; native const/optional parameter flags may differ from the base.
Out-flag ABI differences require more characterization before relaxing validation.

Persistent-field access uses declaring-class ownership, not receiver spelling.
Declaring classes and descendants may access protected persistent fields, including
through base-typed receivers. Unrelated external/private restrictions remain;
this is not removal of persistent validation. Imported native classes and structs
retain their separate existing policy.

## Recovery and evidence

[Invalid signatures](invalid-signature-recovery.md) are not normal overload,
duplicate or override inputs and do not produce generated forwarders. A valid
sibling may still be analyzed without making the compilation successful.

Frontend/native regressions inspect selected IR identities; backend tests check
serialized targets, receiver order and override metadata. Historical official
probes established child-parameter preference, real-method precedence,
optional-tail ambiguity and bounded original-slot returns. These observations do
not establish all overload categories, full-corpus accuracy or runtime dispatch.

# Type inference hardening plan

## Scope

Address three remaining correctness and robustness problems in inference:

1. Failed constraints can leave partially mutated inference state.
2. Recursive bounds can create reference cycles or overflow during coalescing.
3. Generic joins and meets can discard constraints or produce order-dependent results.

Implementation order: **transactions → cycle safety → joins and meets**. All three
change recursive constraint operations; atomic failure handling provides the
foundation for safely rejecting cycles and incompatible generic applications.

Keep the existing `Rc<RefCell<VarState>>` representation. An arena-based solver,
recursive types, or a full directed-constraint graph are outside this plan.

## 1. Transactional constraint operations

Primary files:

- `crates/compiler/frontend/src/lower/types.rs`
- `crates/compiler/frontend/src/lower.rs`

### Implementation

Introduce an internal inference context with an undo journal:

- Record the previous `VarState` before each mutation.
- Support checkpoints, commit, and rollback.
- Thread the context through recursive constraints, unification, joins, and meets.
- Keep existing public methods as wrappers that open a transaction.
- Share the transaction across recursive calls rather than committing each call
  independently.

Rollback must include representative links and path compression, not just bounds.
Either journal compression writes or use non-compressing representative lookup
within transactions.

Refactor bound updates to compute the prospective merged bound, validate it
against the opposite bound, and store it only after validation succeeds. Recursive
validation can still mutate nested variables, so this ordering complements the
journal rather than replacing it. Do not hold a `RefMut` across recursive solver
calls.

### Speculative callers

Audit lowering call sites that continue after failed constraints, particularly:

- Ignored contextual return-hint constraints.
- Dynamic-cast checks that try one relationship and then another.
- Fallback paths that inspect `is_ok()` or discard errors.

Distinguish two operations:

- **Try-and-commit:** retain successful inference, rollback on failure. Use for
  contextual hints that should contribute constraints when compatible.
- **Probe:** always rollback, including on success. Use for checks intended only
  to answer a question.

### Regression tests

Assert that state is unchanged after:

- An incompatible upper or lower bound.
- Failure in a later generic argument after an earlier argument mutated.
- Failure halfway through invariant equality or unification.
- A failed speculative hint followed by successful inference.
- Representative compression performed during a failed operation.

**Acceptance criterion:** failure restores the entire reachable inference state,
not just the outer variable.

## 2. Occurs checks and cycle-safe coalescing

Primary files:

- `crates/compiler/frontend/src/lower/types.rs`
- `crates/compiler/frontend/src/lower/simplify.rs`
- `crates/compiler/frontend/src/lower/error.rs`

### Occurs checks

Add representative-aware occurs checks before storing bounds or linking
representatives. Traverse:

- Application arguments.
- Context-variable bounds.
- Inference-variable bounds and representatives.
- Prospective merged state, including cycles introduced by unification.

Use visited representatives to avoid repeatedly traversing shared acyclic graphs.
Direct identity constraints such as `X <: X` remain no-ops.

Under the current finite-type representation, reject recursive bounds rather than
attempting to support recursive types. Keep the check isolated so this policy can
evolve independently.

Add dedicated recursive-type errors. Diagnostics must not recursively format an
already cyclic graph.

### Coalescer protection

In `Simplifier`:

- Use canonical representative keys consistently for analysis and caching.
- Stop repeated analysis once the relevant variance information has been visited.
- Track active simplification separately from completed cached results.
- Return a cycle error when simplification re-enters an active variable.
- Distinguish shared subgraphs from cycles; a global “seen means error” rule would
  incorrectly reject valid sharing.

### Regression tests

Cover:

- Direct self-application: `(x) -> x(x)`.
- Indirect cycles through two variables.
- Cycles introduced only by representative merging.
- Nested function and array bounds.
- Acyclic shared graphs and long representative chains.
- Rollback after occurs-check failure.
- Rejection without retaining an `Rc` cycle.

**Acceptance criterion:** recursive inference produces a diagnostic, never a stack
overflow, `RefCell` panic, or newly retained reference cycle.

## 3. Variance-correct generic joins and meets

Primary file: `crates/compiler/frontend/src/lower/types.rs`.

### Same-constructor applications

Combine arguments according to parameter variance:

| Parameter variance | Join (`lub`) | Meet (`glb`) |
| --- | --- | --- |
| Covariant | Join arguments | Meet arguments |
| Contravariant | Meet arguments | Join arguments |
| Invariant | Enforce equality | Enforce equality |

Remove the current invariant `lub(...).glb(...)` expression.

Invariant equality must accept compatible inference variables while rejecting
incompatible rigid types. Execute the entire application operation transactionally.

### Different constructors

- **Join:** project both applications to their nominal common base, then combine
  that base's substituted arguments.
- **Meet:** select the nominally narrower constructor, project it to the wider
  constructor, and validate the generic relationship.
- Do not silently discard wider-constructor arguments or invent an inverse
  substitution. Reject cases the current representation cannot express safely.

Audit the variable cases in `PolyType::lub` and `glb` as well. They currently mutate
or unify operands; tests must establish that resulting substitutions satisfy the
advertised bound relationship.

### Regression tests

Use a small deterministic matrix covering all variances, nested applications,
inherited substitutions, and inference variables. Check:

- Commutativity, comparing results modulo representative identity.
- Idempotence.
- Every successful join is an upper bound of both inputs.
- Every successful meet is a lower bound of both inputs.
- Reversing conditional branches preserves acceptance and inferred type.
- Conflicting invariant arrays fail in both operand orders.
- Failure leaves inference state unchanged.

**Acceptance criterion:** successful operations preserve both inputs' constraints,
and operand order does not affect acceptance or the resolved result.

## Delivery sequence

Use separate commits for:

1. Transaction infrastructure and rollback regressions.
2. Speculative-call-site integration.
3. Occurs checks and recursive-type diagnostics.
4. Cycle-safe coalescing.
5. Variance-aware joins and meets.

Each commit should include its focused regression tests. Preserve unrelated work
in the working tree when staging changes.

## Validation

Start with focused solver tests, then run frontend tests:

```sh
cargo test -p redscript-compiler-frontend --lib lower::types::tests --offline
cargo test -p redscript-compiler-frontend --lib --tests --offline
```

Finish with backend tests and targeted linting:

```sh
cargo test -p redscript-compiler-backend --lib --tests --offline
cargo clippy -p redscript-compiler-frontend --lib --test lowering_inference --offline --no-deps
```

Include any new test targets in targeted linting. Inspect snapshot changes
individually, especially invariance and inference fixtures; do not accept them
wholesale. Invalid programs may gain diagnostics, but existing valid programs
should remain accepted unless a deliberate behavior change is documented.

## Deferred work

Replacing all variable-to-variable subtype constraints with directed constraint
edges is a broader solver redesign. Do not bundle it into these fixes. Likewise,
do not add recursive-type support or replace the variable storage model as part
of this plan.

# Type inference safeguards and hardening

This separates current solver mechanisms from remaining design work. It replaces
an older transaction/cycle/join implementation plan whose cycle and generic-bound
sections were partly superseded. See the [language index](README.md) and
[future-work list](language-future-work.md#analysis-lookup-and-constness).

## Current mechanisms

`crates/compiler/frontend/src/lower/types.rs` retains variable bounds and directed
subtype edges. Variable-to-variable subtyping is not generally equality;
strongly connected components may merge without equating unrelated variables.
Invariant constraints retain both bounds. Rigid annotation parameters are checked
through proven bounds, not treated as inference variables merely because their
upper bound resembles a requested type.

Application joins project operands to a nominal common base before combining
substituted arguments. Same-constructor merging composes parameter variance:

| Parameter variance | Join | Meet |
| --- | --- | --- |
| Covariant | Join arguments | Meet arguments |
| Contravariant | Meet arguments | Join arguments |
| Invariant | Enforce equality | Enforce equality |

Different-constructor meets build a nominally narrower candidate and constrain it
against both inputs instead of discarding the wider operand's generic arguments.
Indexed derived-object evidence participates in read-only applicability for base
parameters without selecting/freezing live variables.

Bound refinement performs an occurs check, validates prospective local bounds and
then stores them before neighbor propagation. Coalescing has independent cycle
protection. Ownership components retain internal bound/edge graphs while external
handles live and clear links when the last external owner is dropped. This permits
constraint cycles without intentionally adding recursive-type support or retaining
unreachable Rc cycles. Rejected constraints clear update guards/remove newly added
edges; these local safeguards are **not whole-operation transactional rollback**.

Solver regressions cover directional edges, invariant constraints, late bounds,
cyclic components, ownership release, recursive coalescing and generic joins/meets.
Their existence is not a claim that validation was rerun for this documentation.

## Remaining goal: atomic failed operations

A recursive constraint operation can mutate earlier/nested variables before a
later step fails. Local validate-before-store does not restore all reachable
state. Transactional failure handling remains a separate correctness goal.

A possible implementation is an internal undo journal with checkpoints, commit
and rollback shared across the entire recursive operation. Keep public operations
as wrappers rather than committing each recursive step independently. Account for
bounds, representatives/path compression, directed edges and ownership-component
changes in the **current** representation; the old two-bound-only plan is
insufficient. Do not hold RefMut across recursive calls.

Separate speculative callers into:

- **Try-and-commit:** retain successful constraints and roll back failures, for
  compatible contextual hints.
- **Probe:** always roll back, including success, for read-only questions.

Audit ignored return hints, dynamic-cast alternative checks and fallback paths
that discard constraint errors. Read-only applicability must not resolve live
variables merely to select a provider or overload.

Required regressions should prove unchanged reachable inference after failure in
later generic arguments, invariant equality, representative compression, edge
propagation and speculative hints. Follow a failed operation with successful
inference to expose contamination rather than asserting only the original error.

## Ongoing correctness criteria

Retain tests for recursive bounds without stack overflow/RefCell panic, valid
shared graphs versus cycles, component release and long representative chains.
For deterministic variance/nested/inherited matrices, check commutativity modulo
representative identity, idempotence, upper/lower-bound relationships, reversed
conditional branches, invariant-array rejection in both orders and eventual
transactional failure atomicity.

Do not replace the storage model or add recursive types merely to implement
rollback. Stage infrastructure, speculative callers and any newly isolated solver
fixes separately, with focused regressions. Review snapshots individually rather
than accepting them wholesale. Start with solver unit tests, then frontend and
backend tests and affected-package checks; finish with the commands in
[CONTRIBUTING.md](../CONTRIBUTING.md).

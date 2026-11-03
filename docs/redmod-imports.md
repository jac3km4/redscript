# REDmod transpiler imports

The transpiler translates `import M.Name` to `@using "M.Name"` and
`import M.{First, Second}` to separate symbol-level `@using` directives. It does
**not** widen either form to `@using "M"`. A function import selects the complete
resolved overload family, as in community source. Wildcard imports retain their
existing module-level translation. Emitted `@module` and `@using` directives end
at the newline, without semicolons; the official compiler rejects directive
semicolons.

Exact/select support is bounded:

- The module path and selected names must be valid target identifiers. Root-level
  exact imports and empty selections are rejected. Resolution and visibility are
  checked by community analysis, not repeated during emission.
- A selected symbol path must not coincide with a module (or ancestor of a module)
  in the selected output: native `@using "First.Pick"` exposes both the function
  and the module `First.Pick`, if both exist. Such overlaps are rejected instead
  of silently widening scope. Module paths are indexed once per transpilation;
  imports do not enumerate the symbol table.
- Analysis-only dependencies do not introduce output modules or global-name
  collision errors. Duplicate native declarations and target call resolution are
  separate concerns, not inferred from an unrelated symbol's simple name.
- The selected output is not a complete inventory of external target modules.
  Overlaps introduced by the installed target or other REDmod packages cannot be
  ruled out by this check; official validation is needed for that environment.
- Existing import ordering and metadata checks remain in force. An error prevents
  returning partial artifacts.

This is import planning, **not certification that the entire emitted program
compiles with REDmod**. Existing typed emission uses qualified type names and
qualified free-function calls. Official REDscript v1.2.2 rejects the tested
qualified type spelling in signatures and the tested qualified free-function
call. These remain separate target-emission limitations, not a reason to widen
imports. Unrelated global type collisions elsewhere in the inputs are also not
solved by import planning.

## Native evidence (2026-11-02)

Local official compiler v1.2.2 probes established:

- `@using "First.Pick"` permits calls to both tested `Pick` overloads (Int32 and
  Float), but a call to the distinct `First.Other` function is rejected as a module
  not declared Used. `@using "First"` permits `Other`.
- If the inputs also contain module `First.Pick` with function `Extra`, that same
  symbol-level directive permits `Extra(1)`. The prefix-overlap guard is required.
- `First.Pick(1)` is rejected with `Invalid identifier in global scope 'First'`;
  qualifying a resolved call is therefore not proof that widening is safe.
- `Models.Value` in a parameter type is rejected by the parser. Simple `Value`
  is accepted; types with that simple name in two different modules are rejected
  as already defined, regardless of the tested `@using` directive.
- Identical free-function signatures in different modules are rejected as already
  defined. Module identity alone does not make those native declarations distinct.

The separate `tests/imports.rs` integration suite checks exact/select output,
resolved qualified references, overload families, selected-module non-widening,
analysis-only/private dependencies and wildcard compatibility. Its
opt-in native lookup test uses raw native source to isolate lookup behavior.
The emitted-source probe writes transpiler output unchanged for module-only,
exact, select and wildcard cases, without qualified references. All four cases
passed official compiler v1.2.2 on 2026-11-03 with zero errors (only constexpr
suggestions). This certifies the tested directive spelling, not qualified-name
support, general import-use roundtrips or runtime parity. The same emitted source
is covered by a default-test snapshot. Run the native tests against a trusted
installation with:

```sh
REDMOD_EXE='.../redMod.exe' REDMOD_GAME_ROOT='.../Cyberpunk 2077' \
  cargo test -p redscript-redmod-transpiler --locked --test imports -- --ignored --nocapture
```

Each native invocation is bounded to 60 seconds and requires the final success
marker plus fresh nonempty cache artifacts for acceptance. Evidence is retained
under `target/redmod-official/imports-*` (including `imports-emitted-*` for
unchanged emitted-source probes).

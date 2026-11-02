# Explicit enum storage

See the [language guide](README.md) for dialect context and
[conversion semantics](conversion-declarations.md#bounded-explicit-enum-casts)
for the separately bounded enum/Int32 cast paths.

Both community and native source accept `enum Choice: Uint8 { A = 0, B = 255 }`.
Legal storage types are `Int8`, `Uint8`, `Int16`, `Uint16`, `Int32`, `Uint32`,
and `Int64`. `Uint64` and noninteger types are rejected. Explicit values and
implicit increments must fit the declared signed/unsigned range. Existing
maximum-so-far increment behavior is unchanged.

Community enum literals use the existing `l` suffix for values beyond Int32.
Native enum declaration literals are unsuffixed signed 64-bit decimal values;
this does not widen ordinary native expression literals. The official compiler
rejects the spelling `-9223372036854775808`, so native parsing/printing and export
do not claim support for that literal. Community source can use it with `l`.

Omitting storage preserves existing community behavior: signed widths are inferred
from the values, including zero (1, 2, 4, or 8 bytes). Native omitted storage keeps
the adapter's existing behavior too; official REDmod instead defaults to 4 bytes.
Use explicit storage when matching native representation matters.

The shared AST's existing `Enum::new` constructor still omits storage; use
`with_storage` to supply it. Resolved enum schemas expose `storage()` and
`storage_size()`. Storage syntax retains its source type span for queries.

## Bundle representation

Version-14 bundles store visibility, one byte of width, member indices and a
native flag. Member values are signed 64-bit integers independent of storage.
There is no signedness field. Source schemas retain exact declared signedness;
loaded schemas expose the stored width and **unknown** signedness (`storage()`
returns `None`). Re-emission preserves loaded widths instead of recomputing them.

Decompilation prints explicit storage to preserve width. It chooses the signed
type when all values fit, otherwise the unsigned type of the same width. This is
a canonical representable declaration, not recovery of the original signedness.
Invalid widths/unrepresentable sets fail rather than silently narrowing.

## Official characterization (2026-10-30)

Local REDmod `REDscript Compiler v1.2.2` accepted all seven legal storage types and
their boundaries (Int64 through +/-9223372036854775807), including Uint32's
4294967295. It rejected Uint64, Float, Bool and CName storage, negative Uint8 and
Uint32 values, Int8 128, Uint8 256, and incrementing Uint8 255. It rejected the
Int64 minimum spelling above during token conversion.

Inspection of freshly compiled authored bundles found widths 1/2/4/8 as declared,
4 for omitted storage, and identical enum payload metadata for Int8 and Uint8
with the same value. Signedness is checked at compile time but erased on disk.
These probes establish compilation/serialization behavior, not game-runtime
behavior. Disposable local probe artifacts are not required to use this contract.

Portable tests use authored fixtures, not installed game scripts. The opt-in
`authored_enum_storage_matches_official_bounds_and_bundle_representation` test in
`crates/compiler/redmod/transpiler/tests/official.rs` reproduces the key checks
with `REDMOD_EXE` and `REDMOD_GAME_ROOT` set to a trusted installation.

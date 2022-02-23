## [0.5.2] - Unreleased

## [0.5.1]
- fix an encoding issue which would cause the game to hang for 10 seconds before starting (af8857e6c22c5204c6b643b1193a39302b578686)
- handle empty `CName`s correctly (e980cc5cb553db4b1da542a216f38ec072b765a0)
  - this enables use of methods like `TDBID.None` and enum members who were previously shown as `Unknown`
- only show fatal errors in the `scc` popup (the previous behaviour would misleadingly show mods with compilation warnings as broken) (fc02231bcdd52d6d3621f306246c77a4ca77dd93)
- log a nice warning when running against an incompatible game version (5f038bbf8d470d8875f4bfc7e096dfa01c46742f)
- fix an issue which would lead to confusing pool errors (53e55a02b05f07c6bd4f2551b883a31daf68c6d9)
- be more permissive for some uses of `null` (like in `IsDefined`) (ad5d21bd32aabf87b1575d493f7102f0a37fa54b)

## [0.5.0]
- support for the 1.5 game update (af21a68)

## [0.4.2]
- support `persistent` fields (5b28213)

## [0.4.1]
- fix an issue with wrapped callback methods (a6a22cf)

## [0.4.0]
- add string interpolation (493b983)
```swift
// create interpolated strings using the 's' prefix
// expressions inside the \() blocks will be converted to strings
Log(s"My name is \(name) and I am \(year - birthYear) years old");
```
- add conditional compilation (currently restricted to classes, functions and imports) (1fc9d2c)
```swift
module My.Mod
// conditional import
// it'll be available only when the module is present
@if(ModuleExists("Other.Mod"))
import Other.Mod.*

// you can write two different versions of a function based on existence of another module
// imports from Other.Mod will be available only in the first version, since the import was conditional
@if(ModuleExists("Other.Mod"))
func Testing() -> Int32 {
    return 1;
}
@if(!ModuleExists("Other.Mod"))
func Testing() -> Int32 {
    return 2;
}
```
- enforce basic correctness rules for native and abstract classes/functions (da8dd33)
- allow native and importonly classes (7fc4e29)
- allow semicolon after all function declarations (f91bb35)
- fix an issue affecting locals with conflicting names (50c6955)
- extended support for **Variant** native type (c9850e6)
  - added implicit `ToVariant()` conversion when the expected `TypeId` is **Variant**
  - added the remaining **Variant** intrinsic operations (`VariantIsRef`, `VariantIsArray` and `VariantTypeName`)
  - extended `ToString()` and `IsDefined()` intrinsics to support their **Variant** counterparts
```swift
// Implicit conversion
let v: Variant = new SomeClass();
// ToString support
let s: String = ToString(v);
// Get the type stored in a variant
let n: CName = VariantTypeName(v);
// IsDefined support and variant intrinsics
if IsDefined(v) && VariantIsRef(v) && !VariantIsArray(v) {
  // ...
}
```
- new function resolution algorithm (8d604af)
  - the new algorithm is a bit more strict, but it makes operator overloads and casts work more predictably
  - it no longer allows using expressions that can resolve to different types inside 'overloaded' contexts - this means that you can no longer use `Cast(x)`, `FromVariant(x)` and `IntEnum(x)` inside calls to operators, overloaded methods and casts
  - a new syntax has been introduced to solve this - you can now write `Cast<Float>(x)`, `FromVariant<ItemID>` and `IntEnum<gamedataItemType>` to explicitly specify the desired type
  - most of your code will probably still work - this change rarely affects anything except for `Casts`s inside operators
```swift
func Testing() {
  // the line below will no longer compile!
  // let a = 0.1 + Cast(1);
  // you can write it like this:
  let a = 0.1 + Cast<Float>(1);
  // IMPORTANT
  // you can make your code forwards-compatible though!
  // you can support both versions of the redscript compiler by extracting the Cast to a local:
  let tmp: Float = Cast(1);
  let a = 0.1 + tmp;
}
```
- improved type inference for conditionals and array literals (79d2dc8)
```swift
func Testing() {
    // we infer the most general type to use for the array (array<ref<a>>)
    let a = [ new C(), new B(), new D() ];
    // the type of 'b' is ref<B>
    let b = true ? new C() : new B();
}

class A {}
class B extends A {}
class C extends B {}
class D extends A {}
```

## [0.3.4]
- fix encoding of native functions (7504d41)
- fix decoding of multi-byte unicode characters (c25bca7)
- fix an issue with classes being sorted incorrectly sometimes (85ebd54)

## [0.3.3]
- use `mmap` for reading script bundles - this dramatically reduces startup time of the compiler (especially on HDDs) (8fa3ee1)
- add implicit conversions on member access (0488527)
- fix a switch-case fallthrough bug in the decompiler (9145e0d)

## [0.3.2]
- no longer stop at the first error when compiling functions (9c28420)
  - the compiler will output all errors in the stdout
  - the error popup in `scc.exe` will list all broken scripts on startup (not just the first one)
  - it's limited to errors inside function bodies for now
- do not allow class redefinitions (06336d8)
- fix duplication of classes in the decompiler output (0dcce57)

## [0.3.1]
- fix an issue with some calls that include `script_ref` values (33e2ecc)
- display a popup when the compilation fails in scc (b35d7d1)

## [0.3.0]
- support for the 1.3 patch - this is the first version of the compiler that drops compatibility with older versions of the game due to many breaking changes in the binary format of the scripts introduced in the patch (6e7030e)
- fix an issue with mixed wrap/replace annotations (d75382f)
- fix an issue with return instructions being encoded incorrectly in void functions (147e512)
- improvements to method resolution compilation errors (5b11ebf)
- sort classes after compilation to prevent game crashes (364d3d4)

# 0.6 version notes
The 0.6 version of REDscript introduces a revamped type system and many new features, including generics and lambdas.

All snippets in this file are valid REDscript code, which compile and run.

## Type system
The new type system is based on a novel approach proposed in [The Simple Essence of Algebraic Subtyping](https://infoscience.epfl.ch/record/278576). It extends Hindley-Milner type inference frequently used in modern functional languages with subtyping. This enables REDscript to be able to automatically infer types of most expressions.

```swift
// inferred as ((String) -> 'a) -> (('a, (String) -> 'a) -> 'a) -> 'a
let f1 = (fa) -> (fc) -> fc(fa("a"), fa);
// local binding support let-polymorphism
let f2 = (a) -> a;
let r1 = f2(1); // r1 is an Int32
let r2 = f2("a"); // r2 is a String
```

## Generics
This release also adds generics. Data types and functions can now have generic parameters with support for variance and upper bounds.
Generics are implemented through type-erasure and auto-boxing similarly to JVM-based languages.
```swift
public class Map<K extends Hashable, V>
  extends Extendable<Tuple<K, V>> { ... }

abstract class Function1<-A0, +R> { ... }
```

## Lambdas
REDscript now also supports lambdas. Lambdas can capture variables from their environment.
```swift
func ReturnsCapture() -> () -> Int32 {
  let local = 10;
  return () -> local;
}
```
Lambdas compile into instances of `FunctionX` abstract classes which are regular types in the type hierarchy and can even be extended, making the sub-type act like a lambda.

The compiler offers a shorthand syntax for lambda types: `(A, B) -> C` for functions up to 8 arguments.

## Standard library
REDscript now comes with a small standard library with some collections, iterators and utilities.
```swift
import Std.Iter.Range
import Std.Collection.Map
import Std.Tuple

func FunStuff() {
    // create and consume an iterator
    Range.New(0, 24)
        .Filter((x) -> x % 2 == 0)
        .Take(4)
        .ForEach((a) -> LogChannel(n"DEBUG", ToString(a)));

    // populate and use a hash map
    let map = Map.New();
    map.Insert("a", 2);
    map.Update("a", (x) -> x + 2);
    LogChannel(n"DEBUG", ToString(map.Get("a")));

    // populate a map from an iterator, the type is inferred to be Map<Int32, String>
    let myothermap = Range.New(0, 10)
       .Map((x) -> Tuple.New(ToString(x), x))
       .Map((x) -> x.Flipped())
       .Take(3)
       .Into(Map.New());
}
```

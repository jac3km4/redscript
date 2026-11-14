
/// Test two values for equality.
///
/// ### Example
/// ```
/// Equals(n"A", n"B"); // false
/// Equals(n"A", n"A"); // true
/// ```
@intrinsic(Equals)
public func Equals<A>(lhs: A, rhs: A) -> Bool

/// Test two values for inequality.
///
/// ### Example
/// ```
/// NotEquals(n"A", n"B"); // true
/// NotEquals(n"A", n"A"); // false
/// ```
@intrinsic(NotEquals)
public func NotEquals<A>(lhs: A, rhs: A) -> Bool

/// Empties an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayClear(arr); // []
/// ```
@intrinsic(ArrayClear)
public func ArrayClear<A>(out array: [A])

/// Returns the size of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArraySize(arr); // 3
/// ```
@intrinsic(ArraySize)
public func ArraySize<A>(out array: [A]) -> Int32

/// Resizes an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayResize(arr, 5); // [1, 2, 3, 0, 0]
/// ```
@intrinsic(ArrayResize)
public func ArrayResize<A>(out array: [A], newSize: Int32)

/// Finds the first occurrence of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayFindFirst(arr, 2); // 1
/// ```
@intrinsic(ArrayFindFirst)
public func ArrayFindFirst<A>(out array: [A], needle: A) -> Int32

/// Finds the last occurrence of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3, 2];
/// ArrayFindLast(arr, 2); // 3
/// ```
@intrinsic(ArrayFindLast)
public func ArrayFindLast<A>(out array: [A], needle: A) -> Int32

/// Checks if an element exists in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayContains(arr, 2); // true
/// ```
@intrinsic(ArrayContains)
public func ArrayContains<A>(out array: [A], needle: A) -> Bool

/// Counts the number of occurrences of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3, 2];
/// ArrayCount(arr, 2); // 2
/// ```
@intrinsic(ArrayCount)
public func ArrayCount<A>(out array: [A], needle: A) -> Int32

/// Pushes an element onto the end of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayPush(arr, 4); // [1, 2, 3, 4]
/// ```
@intrinsic(ArrayPush)
public func ArrayPush<A>(out array: [A], elem: A)

/// Pops an element from the end of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayPop(arr); // 3
/// ```
@intrinsic(ArrayPop)
public func ArrayPop<A>(out array: [A]) -> A

/// Inserts an element at a specific index in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayInsert(arr, 1, 4); // [1, 4, 2, 3]
/// ```
@intrinsic(ArrayInsert)
public func ArrayInsert<A>(out array: [A], index: Int32, elem: A)

/// Removes an element from an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayRemove(arr, 2); // [1, 3]
/// ```
@intrinsic(ArrayRemove)
public func ArrayRemove<A>(out array: [A], elem: A) -> Bool

/// Grows an array by a specified number of elements.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayGrow(arr, 2); // [1, 2, 3, 0, 0]
/// ```
@intrinsic(ArrayGrow)
public func ArrayGrow<A>(out array: [A], count: Int32)

/// Erases an element at a specific index in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayErase(arr, 1); // [1, 3]
/// ```
@intrinsic(ArrayErase)
public func ArrayErase<A>(out array: [A], index: Int32)

/// Returns the last element of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayLast(arr); // 3
/// ```
@intrinsic(ArrayLast)
public func ArrayLast<A>(out array: [A]) -> A

/// Sorts an array in ascending order.
///
/// ### Example
/// ```
/// let arr = [3, 1, 2];
/// ArraySort(arr); // [1, 2, 3]
/// ```
@intrinsic(ArraySort)
public func ArraySort<A>(out array: [A])

/// Converts a value to its string representation.
///
/// ### Example
/// ```
/// let num = 42;
/// ToString(num); // "42"
/// ```
@intrinsic(ToString)
public func ToString<A>(a: A) -> String

/// Converts an enum value to its integer representation.
///
/// ### Example
/// ```
/// EnumInt(Color.Red); // 0
/// ```
@intrinsic(EnumInt)
public func EnumInt<A>(enum: A) -> Int32

/// Converts an integer value to its enum representation.
///
/// ### Example
/// ```
/// IntEnum<Color>(0); // Color.Red
/// ```
@intrinsic(IntEnum)
public func IntEnum<A>(value: Int32) -> A

/// Converts an integer value to its enum representation.
///
/// ### Example
/// ```
/// IntEnum<Color>(0); // Color.Red
/// ```
@intrinsic(LongIntEnum)
public func IntEnum<A>(value: Int64) -> A

/// Converts a value into a variant.
@intrinsic(ToVariant)
public func ToVariant<A>(a: A) -> Variant

/// Converts a variant into a value.
@intrinsic(FromVariant)
public func FromVariant<A>(variant: Variant) -> A

@intrinsic(VariantIsRef)
public func VariantIsRef<A>(variant: Variant) -> Bool

@intrinsic(VariantIsArray)
public func VariantIsArray<A>(variant: Variant) -> Bool

@intrinsic(VariantTypeName)
public func VariantTypeName(variant: Variant) -> String

@intrinsic(AsRef)
public func AsRef<A>(a: A) -> script_ref<A>

@intrinsic(Deref)
public func Deref<A>(a: const_script_ref<A>) -> A

@intrinsic(RefToWeakRef)
public func RefToWeakRef<A extends IScriptable>(weak: A) -> wref<A>

@intrinsic(WeakRefToRef)
public func WeakRefToRef<A>(weak: wref<A>) -> A

/// Tests if a value is defined.
///
/// ### Example
/// ```
/// let player = new PlayerPuppet();
/// IsDefined(player); // true
/// IsDefined(null); // false
/// ```
@intrinsic(IsDefined)
public func IsDefined(a: IScriptable) -> Bool

/// Tests if a variant is defined.
///
/// ### Example
/// ```
/// let variant = ToVariant(1);
/// IsDefined(variant); // true
/// ```
@intrinsic(VariantIsDefined)
public func IsDefined(a: Variant) -> Bool

/// Returns the fully qualified name of a type including its full module path.
///
/// ### Example
/// ```
/// NameOf<Int32>() // "Int32"
/// ```
@intrinsic(NameOf)
public func NameOf<A>() -> CName

public abstract class Function0<+R> {
  public func Call() -> R
}

public abstract class Function1<-A, +R> {
  public func Call(a: A) -> R

  public final func Compose<A1>(f: (A1) -> A) -> (A1) -> R =
    (a) -> this(f(a))

  public final func AndThen<R1>(f: (R) -> R1) -> (A) -> R1 =
    (a) -> f(this(a))
}

abstract class Function2<-A, -B, +R> {
  public func Call(a: A, b: B) -> R
}

abstract class Function3<-A, -B, -C, +R> {
  public func Call(a: A, b: B, c: C) -> R
}

abstract class Function4<-A, -B, -C, -D, +R> {
  public func Call(a: A, b: B, c: C, d: D) -> R
}

abstract class Function5<-A, -B, -C, -D, -E, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E) -> R
}

abstract class Function6<-A, -B, -C, -D, -E, -F, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F) -> R
}

abstract class Function7<-A, -B, -C, -D, -E, -F, -G, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F, g: G) -> R
}

abstract class Function8<-A, -B, -C, -D, -E, -F, -G, -H, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) -> R
}

import Std.{Hashable, Integral}

// intrinsics
func Equals<A>(lhs: A, rhs: A) -> Bool;
func NotEquals<A>(lhs: A, rhs: A) -> Bool;
func ArrayClear<A>(out array: array<A>);
func ArraySize<A>(array: array<A>) -> Int32;
func ArrayResize<A>(out array: array<A>, size: Int32);
func ArrayFindFirst<A>(array: array<A>, needle: A) -> A;
func ArrayFindLast<A>(array: array<A>, needle: A) -> A;
func ArrayContains<A>(array: array<A>, needle: A) -> Bool;
func ArrayCount<A>(array: array<A>, needle: A) -> Int32;
func ArrayPush<A>(out array: array<A>, elem: A);
func ArrayPop<A>(out array: array<A>) -> A;
func ArrayInsert<A>(out array: array<A>, idx: Int32, elem: A);
func ArrayRemove<A>(out array: array<A>, elem: A);
func ArrayGrow<A>(out array: array<A>, size: Int32);
func ArrayErase<A>(out array: array<A>, idx: Int32) -> Bool;
func ArrayLast<A>(array: array<A>) -> A;

func ToString<A>(a: A) -> String;
func EnumInt<A>(a: A) -> Int32;
func IntEnum<A>(a: Int32) -> A;
func ToVariant<A>(a: A) -> Variant;
func FromVariant<A>(a: Variant) -> A;
func VariantTypeName(a: Variant) -> CName;
func VariantIsRef(a: Variant) -> Bool;
func VariantIsArray(a: Variant) -> Bool;

func AsRef<A>(a: A) -> script_ref<A>;
func Deref<A>(a: script_ref<A>) -> A;
func IsDefined<A>(a: A) -> Bool;

func RefToWeakRef<A>(a: A) -> wref<A>;
func WeakRefToRef<A>(a: wref<A>) -> A;

abstract class Function0<+R> {
  public func Apply() -> R;
}

abstract class Function1<-A0, +R> {
  public func Apply(a: A0) -> R;

  final func Compose<A1>(f: (A1) -> A0) -> (A1) -> R =
    (a) -> this(f(a))

  final func AndThen<R1>(f: (R) -> R1) -> (A0) -> R1 =
    (a) -> f(this(a))
}

abstract class Function2<-A0, -A1, +R> {
  public func Apply(a: A0, b: A1) -> R;
}

abstract class Function3<-A0, -A1, -A2, +R> {
  public func Apply(a: A0, b: A1, c: A2) -> R;
}

abstract class Function4<-A0, -A1, -A2, -A3, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3) -> R;
}

abstract class Function5<-A0, -A1, -A2, -A3, -A4, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4) -> R;
}

abstract class Function6<-A0, -A1, -A2, -A3, -A4, -A5, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5) -> R;
}

abstract class Function7<-A0, -A1, -A2, -A3, -A4, -A5, -A6, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5, g: A6) -> R;
}

abstract class Function8<-A0, -A1, -A2, -A3, -A4, -A5, -A6, -A7, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5, g: A6, h: A7) -> R;
}

class BoxedString extends Hashable {
  private let value: String;

  public static func New(value: String) -> BoxedString {
    let self: BoxedString = new BoxedString();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedString;
    return IsDefined(casted)
      ? UnicodeStringCompare(this.value, casted.value)
      : -1;
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(this.value))
}

class BoxedInt32 extends Integral {
  private let value: Int32;

  public static func New(value: Int32) -> BoxedInt32 {
    let self: BoxedInt32 = new BoxedInt32();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedInt32;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= 1;
  }

  public func Increment() {
    this.value += 1;
  }
}

class BoxedBool extends Hashable {
  private let value: Bool;

  static func New(value: Bool) -> BoxedBool {
    let self: BoxedBool = new BoxedBool();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedBool;
    return !IsDefined(casted) || !this.value && casted.value
      ? -1
      : (Equals(this.value, casted.value) ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))
}

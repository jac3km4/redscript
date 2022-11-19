import Std.{Hashable, Integral}

// intrinsics
public func Equals<A>(lhs: A, rhs: A) -> Bool;
public func NotEquals<A>(lhs: A, rhs: A) -> Bool;
public func ArrayClear<A>(out array: array<A>);
public func ArraySize<A>(array: array<A>) -> Int32;
public func ArrayResize<A>(out array: array<A>, size: Int32);
public func ArrayFindFirst<A>(array: array<A>, needle: A) -> A;
public func ArrayFindLast<A>(array: array<A>, needle: A) -> A;
public func ArrayContains<A>(array: array<A>, needle: A) -> Bool;
public func ArrayCount<A>(array: array<A>, needle: A) -> Int32;
public func ArrayPush<A>(out array: array<A>, elem: A);
public func ArrayPop<A>(out array: array<A>) -> A;
public func ArrayInsert<A>(out array: array<A>, idx: Int32, elem: A);
public func ArrayRemove<A>(out array: array<A>, elem: A);
public func ArrayGrow<A>(out array: array<A>, size: Int32);
public func ArrayErase<A>(out array: array<A>, idx: Int32) -> Bool;
public func ArrayLast<A>(array: array<A>) -> A;

public func ToString<A>(a: A) -> String;
public func EnumInt<A>(a: A) -> Int32;
public func IntEnum<A>(a: Int32) -> A;
public func ToVariant<A>(a: A) -> Variant;
public func FromVariant<A>(a: Variant) -> A;
public func VariantTypeName(a: Variant) -> CName;
public func VariantIsRef(a: Variant) -> Bool;
public func VariantIsArray(a: Variant) -> Bool;

public func AsRef<A>(a: A) -> script_ref<A>;
public func Deref<A>(a: script_ref<A>) -> A;
public func IsDefined<A>(a: A) -> Bool;

public func RefToWeakRef<A>(a: A) -> wref<A>;
public func WeakRefToRef<A>(a: wref<A>) -> A;

public abstract class Function0<+R> {
  public func Apply() -> R;
}

public abstract class Function1<-A0, +R> {
  public func Apply(a: A0) -> R;

  public final func Compose<A1>(f: (A1) -> A0) -> (A1) -> R =
    (a) -> this(f(a))

  public final func AndThen<R1>(f: (R) -> R1) -> (A0) -> R1 =
    (a) -> f(this(a))
}

public abstract class Function2<-A0, -A1, +R> {
  public func Apply(a: A0, b: A1) -> R;
}

public abstract class Function3<-A0, -A1, -A2, +R> {
  public func Apply(a: A0, b: A1, c: A2) -> R;
}

public abstract class Function4<-A0, -A1, -A2, -A3, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3) -> R;
}

public abstract class Function5<-A0, -A1, -A2, -A3, -A4, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4) -> R;
}

public abstract class Function6<-A0, -A1, -A2, -A3, -A4, -A5, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5) -> R;
}

public abstract class Function7<-A0, -A1, -A2, -A3, -A4, -A5, -A6, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5, g: A6) -> R;
}

public abstract class Function8<-A0, -A1, -A2, -A3, -A4, -A5, -A6, -A7, +R> {
  public func Apply(a: A0, b: A1, c: A2, d: A3, e: A4, f: A5, g: A6, h: A7) -> R;
}

// boxed types

public class BoxedInt8 extends Integral {
  let value: Int8;

  public static func New(value: Int8) -> BoxedInt8 {
    let self: BoxedInt8 = new BoxedInt8();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedInt8;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Int8>(1);
  }

  public func Increment() {
    this.value += Cast<Int8>(1);
  }
}

public class BoxedInt16 extends Integral {
  let value: Int16;

  public static func New(value: Int16) -> BoxedInt16 {
    let self: BoxedInt16 = new BoxedInt16();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedInt16;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Int16>(1);
  }

  public func Increment() {
    this.value += Cast<Int16>(1);
  }
}

public class BoxedInt32 extends Integral {
  let value: Int32;

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

public class BoxedInt64 extends Integral {
  let value: Int64;

  public static func New(value: Int64) -> BoxedInt64 {
    let self: BoxedInt64 = new BoxedInt64();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedInt64;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Int64>(1);
  }

  public func Increment() {
    this.value += Cast<Int64>(1);
  }
}

public class BoxedFloat {
  let value: Float;

  public static func New(value: Float) -> BoxedFloat {
    let self: BoxedFloat = new BoxedFloat();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedFloat;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))
}

public class BoxedDouble {
  let value: Double;

  public static func New(value: Double) -> BoxedDouble {
    let self: BoxedDouble = new BoxedDouble();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedDouble;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))
}

public class BoxedUint8 extends Integral {
  let value: Uint8;

  public static func New(value: Uint8) -> BoxedUint8 {
    let self: BoxedUint8 = new BoxedUint8();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedUint8;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Uint8>(1);
  }

  public func Increment() {
    this.value += Cast<Uint8>(1);
  }
}

public class BoxedUint16 extends Integral {
  let value: Uint16;

  public static func New(value: Uint16) -> BoxedUint16 {
    let self: BoxedUint16 = new BoxedUint16();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedUint16;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Uint16>(1);
  }

  public func Increment() {
    this.value += Cast<Uint16>(1);
  }
}

public class BoxedUint32 extends Integral {
  let value: Uint32;

  public static func New(value: Uint32) -> BoxedUint32 {
    let self: BoxedUint32 = new BoxedUint32();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedUint32;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Uint32>(1);
  }

  public func Increment() {
    this.value += Cast<Uint32>(1);
  }
}

public class BoxedUint64 extends Integral {
  let value: Uint64;

  public static func New(value: Uint64) -> BoxedUint64 {
    let self: BoxedUint64 = new BoxedUint64();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedUint64;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))

  public func Decrement() {
    this.value -= Cast<Uint64>(1);
  }

  public func Increment() {
    this.value += Cast<Uint64>(1);
  }
}

class BoxedBool extends Hashable {
  private let value: Bool;

  public static func New(value: Bool) -> BoxedBool {
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

class BoxedString extends Hashable {
  let value: String;

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

class BoxedCName extends Hashable {
  let value: CName;

  public static func New(value: CName) -> BoxedCName {
    let self: BoxedCName = new BoxedCName();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedCName;
    return IsDefined(casted)
      ? UnicodeStringCompare(NameToString(this.value), NameToString(casted.value))
      : -1;
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(NameToString(this.value)))
}

class BoxedTweakDBID extends Hashable {
  let value: TweakDBID;

  public static func New(value: TweakDBID) -> BoxedTweakDBID {
    let self: BoxedTweakDBID = new BoxedTweakDBID();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedTweakDBID;
    return IsDefined(casted)
      ? UnicodeStringCompare(ToString(this.value), ToString(casted.value))
      : -1;
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(this.value)
}

class BoxedVariant {
  let value: Variant;

  public static func New(value: Variant) -> BoxedVariant {
    let self: BoxedVariant = new BoxedVariant();
    self.value = value;
    return self;
  }
}

class BoxedEnum {
  let value: Int64;

  public static func FromIntRepr(value: Int64) -> BoxedEnum {
    let self: BoxedEnum = new BoxedEnum();
    self.value = value;
    return self;
  }

  public func Compare(other: Any) -> Int32 {
    let casted = other as BoxedEnum;
    return !IsDefined(casted) || this.value < casted.value
      ? -1
      : (this.value == casted.value ? 0 : 1);
  }

  public func Hash() -> Uint64 =
    TDBID.ToNumber(TDBID.Create(ToString(this.value)))
}

class BoxedStruct {
  let value: Variant;

  public static func FromVariant(value: Variant) -> BoxedStruct {
    let self: BoxedStruct = new BoxedStruct();
    self.value = value;
    return self;
  }
}

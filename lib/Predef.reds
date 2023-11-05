import Std.{Primitive, Primitive}

// intrinsics
public func Equals<A>(lhs: A, rhs: A) -> Bool;
public func NotEquals<A>(lhs: A, rhs: A) -> Bool;
public func ArrayClear<A>(array: array<A>);
public func ArraySize<A>(array: array<A>) -> Int32;
public func ArrayResize<A>(array: array<A>, size: Int32);
public func ArrayFindFirst<A>(array: array<A>, needle: A) -> A;
public func ArrayFindLast<A>(array: array<A>, needle: A) -> A;
public func ArrayContains<A>(array: array<A>, needle: A) -> Bool;
public func ArrayCount<A>(array: array<A>, needle: A) -> Int32;
public func ArrayPush<A>(array: array<A>, elem: A);
public func ArrayPop<A>(array: array<A>) -> A;
public func ArrayInsert<A>(array: array<A>, idx: Int32, elem: A);
public func ArrayRemove<A>(array: array<A>, elem: A);
public func ArrayGrow<A>(array: array<A>, size: Int32);
public func ArrayErase<A>(array: array<A>, idx: Int32) -> Bool;
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

public class BoxedInt8 extends Primitive {
  let value: Int8;

  public static func New(value: Int8) -> BoxedInt8 {
    let self = new BoxedInt8();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedInt16 extends Primitive {
  let value: Int16;

  public static func New(value: Int16) -> BoxedInt16 {
    let self = new BoxedInt16();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedInt32 extends Primitive {
  let value: Int32;

  public static func New(value: Int32) -> BoxedInt32 {
    let self = new BoxedInt32();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedInt64 extends Primitive {
  let value: Int64;

  public static func New(value: Int64) -> BoxedInt64 {
    let self = new BoxedInt64();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedFloat extends Primitive {
  let value: Float;

  public static func New(value: Float) -> BoxedFloat {
    let self = new BoxedFloat();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedDouble extends Primitive {
  let value: Double;

  public static func New(value: Double) -> BoxedDouble {
    let self = new BoxedDouble();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedUint8 extends Primitive {
  let value: Uint8;

  public static func New(value: Uint8) -> BoxedUint8 {
    let self = new BoxedUint8();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedUint16 extends Primitive {
  let value: Uint16;

  public static func New(value: Uint16) -> BoxedUint16 {
    let self = new BoxedUint16();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedUint32 extends Primitive {
  let value: Uint32;

  public static func New(value: Uint32) -> BoxedUint32 {
    let self = new BoxedUint32();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedUint64 extends Primitive {
  let value: Uint64;

  public static func New(value: Uint64) -> BoxedUint64 {
    let self = new BoxedUint64();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedBool extends Primitive {
  let value: Bool;

  public static func New(value: Bool) -> BoxedBool {
    let self = new BoxedBool();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedString extends Primitive {
  let value: String;

  public static func New(value: String) -> BoxedString {
    let self = new BoxedString();
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

  public func ToString() -> String =
    this.value
}

public class BoxedCName extends Primitive {
  let value: CName;

  public static func New(value: CName) -> BoxedCName {
    let self = new BoxedCName();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedTweakDBID extends Primitive {
  let value: TweakDBID;

  public static func New(value: TweakDBID) -> BoxedTweakDBID {
    let self = new BoxedTweakDBID();
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedVariant {
  let value: Variant;

  public static func New(value: Variant) -> BoxedVariant {
    let self = new BoxedVariant();
    self.value = value;
    return self;
  }
}

public class BoxedNodeRef {
  let value: NodeRef;

  public static func New(value: NodeRef) -> BoxedNodeRef {
    let self = new BoxedNodeRef();
    self.value = value;
    return self;
  }
}

public class BoxedLocalizationString {
  let value: LocalizationString;

  public static func New(value: LocalizationString) -> BoxedLocalizationString {
    let self = new BoxedLocalizationString();
    self.value = value;
    return self;
  }
}

public class BoxedCRUID {
  let value: CRUID;

  public static func New(value: CRUID) -> BoxedCRUID {
    let self = new BoxedCRUID();
    self.value = value;
    return self;
  }
}

public class BoxedResRef {
  let value: redResourceReferenceScriptToken;

  public static func New(value: redResourceReferenceScriptToken) -> BoxedResRef {
    let self = new BoxedResRef();
    self.value = value;
    return self;
  }
}

public class BoxedEnum extends Primitive {
  let value: Int32;

  public static func FromIntRepr(value: Int32) -> BoxedEnum {
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

  public func ToString() -> String =
    ToString(this.value)
}

public class BoxedStruct {
  let value: Variant;

  public static func FromVariant(value: Variant) -> BoxedStruct {
    let self: BoxedStruct = new BoxedStruct();
    self.value = value;
    return self;
  }
}

// Conversions that depend on type information: converted with the types recorded by the
// compiler, most of the warnings of the untyped conversion go away
module MyMod.Typed

enum EMode {
  Idle = 0,
  Busy = 1,
}

struct Size {
  public let X: Float;
  public let Y: Float;
}

public class Widget {
  public let names: [CName];

  public func GetSize() -> Size = Size(1.0, 2.0);

  public func GetNames() -> [CName] = this.names;

  public func GetCount() -> Int32 = ArraySize(this.names);

  public func SetSize(x: Float, y: Float) {}
}

func Log(text: String) {}

func Scale(factor: Float) -> Float = factor;

func Apply(mode: EMode) {}

func Typed(w: ref<Widget>, weak: wref<Widget>, r: script_ref<Int32>, ratio: Float, flag: Bool, id: CName, v: Variant) -> Float {
  // objects in conditions
  if IsDefined(w) {
    w.SetSize(445, -125);
  } else if IsDefined(weak) {
    Log(ToString(w.GetCount()));
  }
  // the truthiness of a negated object is not documented for REDmod
  if !IsDefined(w) {
    return 0;
  }
  let count = w.GetCount();
  let names = w.GetNames();
  let size = w.GetSize();
  let y = size.Y;
  if ArrayContains(names, n"a") {
    Log(ToString(ratio));
  }
  Log(ToString(flag));
  Log(ToString(id));
  Log(ToString(s"\(count)"));
  let value: Int32 = Deref(r);
  Apply(IntEnum(1));
  let scaled = Scale(FromVariant(v));
  for name in w.GetNames() {
    Log(ToString(name));
  }
  while flag {
    // reset every iteration in the community dialect
    let mode: EMode;
    let other: Widget;
    let current: Size;
    mode = EMode.Busy;
    other = w;
    current = size;
    flag = false;
  }
  Log(s"\(count) at \(ratio) for \(id)");
  return Scale(2) + y + size.X;
}

// ToString and interpolation by type: enums, small ints, structs with a static ToString
struct GameTime {
  public let seconds: Int32;

  public static func ToString(self: GameTime) -> String = "time";
}

func Describe(mode: EMode, small: Uint8, big: Uint32, time: GameTime, size: Size, w: ref<Widget>) {
  Log(ToString(mode));
  Log(ToString(small));
  Log(ToString(big));
  Log(ToString(time));
  Log(ToString(size));
  Log(ToString(w));
  Log(s"\(mode) \(small) \(big) \(time) \(size) \(w)");
}

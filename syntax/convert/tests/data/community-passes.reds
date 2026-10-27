// License header comment
// spanning two lines
module MyMod.Runtime

import Other.Utils.*

enum EState {
  Idle = 0,
  Running = 1,
}

/// A documented class
public class Tracker extends ScriptableSystem {
  // tracked values
  private let values: [Int32];
  private let label: String;
  private persistent let saved: Int32;

  // inline body
  public func Count() -> Int32 = ArraySize(this.values);

  public func Values() -> [Int32] = this.values;

  public func Empty(flag: Bool) -> [String] {
    if flag {
      // early exit
      return [];
    }
    return [];
  }

  public func Sum(items: [Int32]) -> Int32 {
    let total = 0;
    // iterate the items
    for item in items {
      if item < 0 {
        continue;
      }
      total += item;
    }
    for value in this.Values() {
      total += value;
    }
    return total;
  }

  public func Describe(name: String, state: EState, ratio: Float) -> String {
    let i = EnumInt(state);
    let back: EState = IntEnum(i);
    let other = IntEnum<EState>(1);
    let v = ToVariant(ratio);
    let r: Float = FromVariant(v);
    let big: Uint64 = 10ul;
    let small: Uint32 = 1u;
    let wide: Int64 = 2l;
    if Equals(name, this.label) || NotEquals(i, 2) {
      ArrayPush(this.values, i);
    }
    if ArrayContains(this.Values(), 3) {
      ArrayClear(this.values);
    }
    return s"\(name): \(i) of \(ArraySize(this.values)) at \(ratio) \(state)";
  }

  public func Shadowing(a: Int32) -> Int32 {
    let x = a;
    if a > 0 {
      let x = a * 2;
      return x;
    }
    while a > 10 {
      let x = 1;
      a -= x;
    }
    return x;
  }

  protected cb func OnRefresh(evt: ref<Event>) -> Bool {
    if !IsDefined(evt) {
      return false;
    }
    let msg = ToString(evt);
    return true;
  }
}

@wrapMethod(PlayerPuppet)
public func OnGameAttached() {
  // call the original
  wrappedMethod();
  let sys = GameInstance.GetScriptableSystemsContainer(this.GetGame()).Get(n"MyMod.Runtime.Tracker") as Tracker;
  let n = sys.Count();
}

@replaceMethod(PlayerPuppet)
protected func GetLabel() -> String = "label";

@addField(PlayerPuppet)
public let tracker: wref<Tracker>;

@addMethod(Tracker)
public func Extra() -> Int32 = 1;

native func ScriptLog(text: String);

func Helpers(r: script_ref<Int32>, f: Float) -> Float {
  let a: Int32 = Deref(r);
  let b: Float = RoundF(f);
  let c: Int32 = Cast<Int32>(a * 2);
  let t: ref<Tracker> = new Tracker();
  let quoted: String = "say \"hi\"";
  let states: [Int32] = [EnumInt(EState.Idle), 2];
  let cb = (x: Int32) -> x + 1;
  return b;
}

func Patterns(name: CName, o: ref<Tracker>, xs: [String]) {
  // downcast with a pattern
  if let name = o as Tracker {
    name.Count();
  }
  if let t as Tracker = o {
    t.Count();
  } else {
    o.Count();
  }
  if let w? = o {
    w.Count();
  }
  for x in xs {
    // reset every iteration in the community dialect
    let found: [String];
    let count: Int32;
    ArrayPush(found, x);
    count += 1;
  }
}

func ShadowedPatterns(o1: ref<Tracker>, o2: ref<Tracker>) {
  // inner patterns reuse the outer name
  if let t as Tracker = o1 {
    if let t as Tracker = o2 {
      t.Count();
    }
    t.Count();
  }
  if let t as Tracker = o1 {
    let cb = (t: Int32) -> t + 1;
    t.Count();
  }
}

@wrapMethod(PlayerPuppet)
public func OnOther() {
  OnOther();
}

// trailing comment

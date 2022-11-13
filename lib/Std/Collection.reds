module Std.Collection
import Std.{Hashable, Tuple}
import Std.Iter.{Iter, Extendable}

class MapEntry<K, V> {
  let key: K;
  let value: V;
  let occupied: Bool;
}

public class Map<K extends Hashable, V>
  extends Extendable<Tuple<K, V>> {
  
  let buckets: array<array<MapEntry<K, V>>>;

  public static func New() -> Map<K, V> {
    let self = new Map();
    ArrayResize(self.buckets, 16);
    return self;
  }

  final public func Get(key: K) -> V {
    let index = Cast(key.Hash() % Cast<Uint64>(ArraySize(this.buckets)));
    let i = 0;
    while i < ArraySize(this.buckets[index]) {
      let cur = AsRef(this.buckets[index][i]);
      if Deref(cur).occupied && Deref(cur).key.Equals(key) {
        return Deref(cur).value;
      }
      i += 1;
    }
  }

  final public func Insert(key: K, val: V) {
    let entry = new MapEntry();
    entry.key = key;
    entry.value = val;
    entry.occupied = true;

    let index = Cast(key.Hash() % Cast<Uint64>(ArraySize(this.buckets)));
    let i = 0;
    while i < ArraySize(this.buckets[index]) {
      let cur = AsRef(this.buckets[index][i]);
      if !Deref(cur).occupied || Deref(cur).key.Equals(key) {
        Deref(cur) = entry;
        return;
      }
      i += 1;
    }
    ArrayPush(this.buckets[index], entry);
  }

  final func Update(k: K, f: (V) -> V) {
    this.Insert(k, f(this.Get(k)));
  }

  final func Extend(elem: Tuple<K, V>) {
    this.Insert(elem.first, elem.second);
  }
}

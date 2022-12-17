module Std.Collection
import Std.{Hashable, Tuple}
import Std.Iter.{Iter, Extendable}

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

  public func Iter() -> Iter<Tuple<K, V>> =
    MapIter.New(this)

  public func Extend(elem: Tuple<K, V>) {
    this.Insert(elem.first, elem.second);
  }
}

public class MapEntry<K, V> {
  let key: K;
  let value: V;
  let occupied: Bool;

  public func ToTuple() -> Tuple<K, V> =
    Tuple.New(this.key, this.value)
}

public class MapIter<K, V> extends Iter<Tuple<K, V>> {
  let map: Map<K, V>;
  let bucket: Int32;
  let item: Int32;

  static func New(map: Map<K, V>) -> MapIter<K, V> {
    let self = new MapIter();
    self.map = map;
    self.bucket = 0;
    self.item = 0;
    return self;
  }

  public func HasNext() -> Bool {
    while this.bucket < ArraySize(this.map.buckets) {
      while this.item < ArraySize(this.map.buckets[this.bucket]) {
        if this.map.buckets[this.bucket][this.item].occupied {
          return true;
        }
        this.item += 1;
      }
      this.item = 0;
      this.bucket += 1;
    }
    return false;
  }

  public func Next() -> Tuple<K, V> {
    let current = this.map.buckets[this.bucket][this.item].ToTuple();
    this.item += 1;
    return current;
  }
}

public class Array<A>
  extends Extendable<A> {
  
  let buffer: array<A>;

  public static func New() -> Array<A> =
    new Array()

  public func Push(a: A) {
    ArrayPush(this.buffer, a);
  }

  public func Get(i: Int32) -> A =
    this.buffer[i]

  public func Size() -> Int32 =
    ArraySize(this.buffer)

  public func Iter() -> Iter<A> =
    ArrayIter.New(this)

  public func Extend(elem: A) {
    this.Push(elem);
  }
}

public class ArrayIter<A> extends Iter<A> {
  let array: Array<A>;
  let index: Int32;

  static func New(array: Array<A>) -> ArrayIter<A> {
    let self = new ArrayIter();
    self.array = array;
    self.index = 0;
    return self;
  }

  public func HasNext() -> Bool =
    this.index < this.array.Size()

  public func Next() -> A {
    let current = this.array.Get(this.index);
    this.index += 1;
    return current;
  }
}

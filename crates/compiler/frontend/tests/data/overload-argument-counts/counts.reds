native func Gapped()
native func Gapped(first: Int32, second: Int32)

native func Disjoint(first: Int32, second: Int32, third: Int32, opt fourth: Int32)
native func Disjoint(opt first: Int32)

native func Overlapping(first: Int32, second: Int32, opt third: Int32)
native func Overlapping(first: Int32, opt second: Int32)

native func Adjacent(first: Int32, opt second: Int32)
native func Adjacent()

native func Duplicate(first: String)
native func Duplicate(first: Int32)

native func Fixed(first: Int32, second: Int32)

struct Pair {
  let first: Int32;
  let second: Int32;
}

func TestArgumentCounts() {
  Gapped(1);
  Gapped(1, 2, 3);
  Disjoint(1, 2);
  Disjoint(1, 2, 3, 4, 5);
  Overlapping();
  Overlapping(1, 2, 3, 4);
  Adjacent(1, 2, 3);
  Duplicate();
  Fixed(1);
  Pair(1);

  Gapped();
  Gapped(1, 2);
  Disjoint();
  Disjoint(1);
  Disjoint(1, 2, 3);
  Disjoint(1, 2, 3, 4);
  Overlapping(1);
  Overlapping(1, 2, 3);
  Adjacent();
  Adjacent(1);
  Adjacent(1, 2);
  Duplicate(1);
  Duplicate("one");
  Fixed(1, 2);
  Pair(1, 2);
}

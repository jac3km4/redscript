func Test() {
  let values = [1, 2, 3];
  let first = ArrayFindFirst(values, 2);
  let contains = ArrayContains(values, 2);
  let count = ArrayCount(values, 2);
  ArrayInsert(values, 1, 4);
  let removed = ArrayRemove(values, 2);
  ArraySort(values);
}

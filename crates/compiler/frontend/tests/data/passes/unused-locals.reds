func TestUnusedLocals() -> Int32 {
  let initializedUnread: Int32 = 1;

  let assignedUnread: Int32;
  assignedUnread = 2;

  let readControl: Int32 = 3;
  return readControl;
}

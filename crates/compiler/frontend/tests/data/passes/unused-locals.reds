func TestUnusedLocals() -> Int32 {
  let capturedRead: Int32 = 1;
  let capturedWrite: Int32 = 2;
  let closure = () -> {
    let initializedUnread: Int32 = 1;
    let readControl: Int32 = capturedRead;
    let nestedCapture: Int32 = 3;
    let nested = () -> {
      let assignedUnread: Int32;
      assignedUnread = 2;

      let nestedRead: Int32 = nestedCapture;
      capturedWrite = 4;
      Consume(nestedRead);
    };
    nested();
    Consume(readControl);
  };
  closure();
  return 0;
}

native func Consume(value: Int32)

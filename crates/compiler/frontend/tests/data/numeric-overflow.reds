func TestInt8Bounds() {
  let belowMinimum: Int8 = -129;
  let aboveMaximum: Int8 = 128;
  let minimum: Int8 = -128; // OK
  let maximum: Int8 = 127; // OK
}

func TestUint8Bounds() {
  let belowMinimum: Uint8 = -1;
  let aboveMaximum: Uint8 = 256;
  let minimum: Uint8 = 0; // OK
  let maximum: Uint8 = 255; // OK
}

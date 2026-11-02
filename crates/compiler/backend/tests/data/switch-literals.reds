func Signed(value: Int8) {
  switch value {
    case -128:
    case 127:
      break;
  }
}

func Unsigned(value: Uint8, run: Bool) {
  while run {
    switch value {
      case 0:
        value = 1;
      default:
        continue;
      case 255:
        break;
    }
    break;
  }
}

func Wide(value: Uint64) {
  switch value {
    case 1:
      break;
  }
}

func Read() -> Uint8 {
  return 255;
}

func Ordered() {
  switch Read() {
    case 0:
      Read();
    case Read():
      break;
    case 255:
      break;
  }
}

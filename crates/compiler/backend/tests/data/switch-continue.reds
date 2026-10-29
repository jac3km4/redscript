func Test(value: Int32, run: Bool) {
  while run {
    switch value {
      case 0:
        continue;
      case 1:
        break;
      default:
        switch value {
          case 2:
            continue;
          default:
            break;
        }
        continue;
    }
    break;
  }
}

func TestInnerLoop(value: Int32, run: Bool) {
  while run {
    switch value {
      case 0:
        while run {
          continue;
        }
        break;
      default:
        continue;
    }
    continue;
  }
}

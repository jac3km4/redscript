func CaptureLambdaLocal() {
  let f = () -> {
    let x = 1;
    if true {
      let g = () -> x;
    }
  };
}

func CaptureLambdaParameter() {
  let f = (x: Int32) -> {
    if true {
      if true {
        let g = () -> x;
      }
    }
  };
}

func CaptureOuterLocal() {
  let x = 1;
  let f = () -> {
    if true {
      let g = () -> {
        if true {
          let h = () -> x;
        }
      };
    }
  };
}

func CaptureFromDifferentBlocks() {
  let x = 1;
  let f = () -> {
    let a = x;
    if true {
      let b = x;
      let g = () -> x;
    }
    if true {
      if true {
        let c = x;
      }
    }
  };
}

func CaptureShadowedLocal() {
  let x = 1;
  let f = () -> {
    let a = x;
    if true {
      let x = 2;
      if true {
        let g = () -> x;
      }
    }
  };
}

module Consumer

import First.SharedClass
import First.SharedStruct

func UseOriginalExports(value: SharedClass) -> Int32 {
  value.Value();
  return SharedStruct.Value();
}

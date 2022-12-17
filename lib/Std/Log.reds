module Std.Log
import Std.Stringify

public func LogDbg(str: script_ref<String>) {
  LogChannel(n"DEBUG", str);
}

public func Dump(val: Any) {
  LogDbg(Stringify(val));
}

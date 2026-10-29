class Receiver {
  public let text: String;

  public func Take(
    first: script_ref<String>,
    second: script_ref<String>,
    third: script_ref<String>,
    fourth: script_ref<String>
  ) {}
}

native func MakeString() -> String

func Test(receiver: Receiver, strings: [String], i: Int32) {
  let local = "local";
  receiver.Take(local, MakeString(), receiver.text, strings[i]);
}

native func NextValue() -> Int32

func Store(slot: script_ref<Int32>) {
  Deref(slot) = NextValue();
}

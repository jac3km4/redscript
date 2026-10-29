func NestedPatterns(value: Variant) {
  if let Outer { inner: Inner { value: /* aggregate nullable */ bound? } } = value {}

  if let [/* array as */ first as Int32, /* array nullable */ second?] = value {}

  if let [Outer { inner: [/* deeply nested */ bound? as Int32?] }] = value {}

  while let Outer { inner: [/* while binding */ bound?] } = value {}

  switch value {
    case let Outer { inner: [/* switch binding */ bound as Int32] }:
      break;
    default:
      break;
  }
}

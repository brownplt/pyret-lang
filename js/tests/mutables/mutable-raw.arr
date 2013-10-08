  o = { mutable x: 5 }
  mut-value = o:x
  o3 = o.{ x: mut-value }
  o3!{ x: "o3-set"}
  test-print(o3!x)
  test-print(o!x)
  
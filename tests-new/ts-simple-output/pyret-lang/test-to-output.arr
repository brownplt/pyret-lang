### Looks shipshape

import global as G
include valueskeleton
include raw-array
include option
include string-dict
include lists

check:
  o = { x: 3, y: {5; "a"}, method z(self): 22 end}
  output = G.to-output(o)
  output satisfies is-vs-record
  cases(ValueSkeleton) output block:
    | vs-record(field-names, vals) =>
      field-names is=~ [raw-array: "x", "y", "z"]
      raw-array-get(vals, 0) is vs-num(3)
      raw-array-get(vals, 1) is=~ vs-tuple([raw-array: vs-num(5), vs-str("a")])
      raw-array-get(vals, 2) satisfies is-vs-method
    | else =>
      output satisfies is-vs-record
  end
end

check:
  G.to-output(5) is vs-num(5)
  G.to-output("a") is vs-str("a")
  G.to-output(true) is vs-bool(true)
  G.to-output(nothing) is vs-nothing
  G.to-output(lam(x :: Number): x end) satisfies is-vs-function
  G.to-output({portions: 1/4}) is=~ vs-record([raw-array: "portions"], [raw-array: vs-num(1/4)])
end

check:
  G.to-output(some(5)) is=~ vs-constr("some", [raw-array: "value"], [raw-array: vs-num(5)])
end

check:
  G.to-output([string-dict: {"a";10}]) is=~
    vs-collection("string-dict", [raw-array: vs-tuple([raw-array: vs-str("a"), vs-num(10)])])
end

check:
  o1 = { x: 3, y: {5; "a"}, method z(self): 22 end}
  G.to-repr(o1) is "{ x: 3, y: {5; \"a\"}, z: <method> }"
  G.to-repr({portions: 1/4}) is "{ portions: 1/4 }"
  G.to-repr(true) is "true"
  G.to-repr(nothing) is "nothing"
  G.to-repr("hi") is '"hi"'
  G.to-repr([raw-array: 1, 2, 3]) is "[raw-array: 1, 2, 3]"
  G.to-repr(some(5)) is "some(5)"
  G.to-repr(none) is "none"
  G.to-repr([list: some(some(5)), some(none), none]) is "[list: some(some(5)), some(none), none]"
end

check:
  #G.to-string("hi") is "hi"
  G.to-repr("hi") is '"hi"'
  G.to-repr('"hi"') is '\"\\\"hi\\\"\"'
  G.to-string('"hi"') is '"hi"'
end
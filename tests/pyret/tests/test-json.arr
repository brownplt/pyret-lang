import json as J
import string-dict as SD

check "conversion":
  p = J.read-json
  
  p("0") satisfies J.is-j-num
  p('"a"') satisfies J.is-j-str
  p("[]") satisfies J.is-j-arr
  p("{}") satisfies J.is-j-obj
  p("true") satisfies J.is-j-bool
  p("false") satisfies J.is-j-bool
  p("null") satisfies J.is-j-null

  p('{"foo": 1, "baz": true}') is
    J.j-obj([SD.string-dict: "foo", J.j-num(1), "baz", J.j-bool(true)])
  p('[1,2,3]') is J.j-arr([list: J.j-num(1), J.j-num(2), J.j-num(3)])
  p('[[[]]]') is J.j-arr([list: J.j-arr([list: J.j-arr([list:])])])
  p('[5, null, {"hello": "world"}]') is
    J.j-arr([list: J.j-num(5), J.j-null,
      J.j-obj([SD.string-dict: "hello", J.j-str("world")])])
end

check "native":
  n = lam(x): J.read-json(x).native() end

  n("0") satisfies is-number
  n('"a"') satisfies is-string
  n("[]") satisfies is-empty
  # n("{}") satisfies SD.is-string-dict # apparently there's no is-string-dict?
  n("{}") is [SD.string-dict:]
  n("true") satisfies is-boolean
  n("false") satisfies is-boolean
  n("null") satisfies is-nothing

  n('{"foo": 1, "baz": true}') is
    [SD.string-dict: "foo", 1, "baz", true]
  n('[1,2,3]') is [list: 1, 2, 3]
  n('[[[]]]') is [list: [list: [list:]]]
  n('[5, null, {"hello": "world"}]') is
    [list: 5, nothing, [SD.string-dict: "hello", "world"]]
end

check "serialize":
  s = lam(x): J.read-json(x).serialize() end

  s("0") is "0"
  s('"a"') is '"a"'
  s("[]") is "[]"
  s("{}") is "{}"
  s("true") is "true"
  s("false") is "false"
  s("null") is "null"

  s('{"foo": 1, "baz": true}') is
    '{"baz": true, "foo": 1}' # should write better comparison for this
  s('[1,2,3]') is '[1, 2, 3]'
  s('[[[]]]') is '[[[]]]'
  s('[5, null, {"hello": "world"}]') is
    '[5, null, {"hello": "world"}]'
end

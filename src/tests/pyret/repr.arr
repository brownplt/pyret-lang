#lang pyret

check:
  torepr(5) is "5"
  torepr('f') is '"f"'
  torepr('f\ns') is '"f\\ns"'
  torepr(torepr('f\ns')) is '"\\"f\\\\ns\\""'
  tostring(torepr('f\ns')) is '"f\\ns"'
  torepr(true) is "true"
  torepr(false) is "false"
  torepr(nothing) is "nothing"
  torepr({}) is "{}"
  torepr({a: 'b'}) is "{a: \"b\"}"
  torepr({a: "b"}) is "{a: \"b\"}"
  torepr({a: "b", c: 5}) is "{a: \"b\", c: 5}"
  torepr({a: "b", c: 5, d: {z: "z"}}) is
               "{a: \"b\", c: 5, d: {z: \"z\"}}"

  b = brander()
  s1 = "foo"
  s2 = b.brand(s1)
  b.test(tostring(s2)) is false

  torepr([1,["2"],"3"]) is '[1, ["2"], "3"]'
  torepr([]) is '[]'
  torepr([[]]) is '[[]]'

  torepr([{a: "b"}]) is '[{a: "b"}]'

  data MyRepr:
    | d1 with: 
      _torepr(self):
        "d1"
      end
    | d2(a :: MyRepr, b :: String) with:
      _torepr(self):
        "d2(" + torepr(self.a) + ", " + torepr(self.b) + ")"
      end
  end

  torepr(d2(d1, "foo")) is 'd2(d1, "foo")'
  torepr(d2(d2(d1, "bar"), "foo")) is 'd2(d2(d1, "bar"), "foo")'
  torepr(d1) is 'd1'
end

check:

  data MyRepr:
    | d1
    | d4()
    | d2(a :: MyRepr, b :: String)
  end

  torepr(d2(d1, "foo")) is 'd2(d1, "foo")'
  torepr(d2(d2(d1, "bar"), "foo")) is 'd2(d2(d1, "bar"), "foo")'
  torepr(d1) is 'd1'

  data MyOtherRepr:
    | d3(x :: MyRepr, y :: list.List)
  end

  torepr(d3(d1, ["1"])) is 'd3(d1, ["1"])'
  torepr(d3(   d2( d4(), "2"),[ "1"])) is 'd3(d2(d4(), "2"), ["1"])'
end


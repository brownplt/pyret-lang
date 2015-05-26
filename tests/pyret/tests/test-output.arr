import valueskeleton as VS

data Foo:
  | singleton
  | nullary()
  | unary(x)
  | binary(x, y)
end

check "Should produce correct toreprs":
  torepr(singleton) is "singleton"
  torepr(nullary()) is "nullary()"
  torepr(unary(5)) is "unary(5)"
  torepr(unary(nullary())) is "unary(nullary())"
  torepr(unary(singleton)) is "unary(singleton)"
  torepr(binary(4,5)) is "binary(4, 5)"
end

data DerivedStrings:
  | w-sing
  | w()
  | x-sing with:
    _output(self): VS.vs-value("output-x-sing") end
  | x() with:
    _output(self): VS.vs-constr("output-x", empty) end
sharing:
  _output(self): VS.vs-value("shared-output") end
end

check "Should correctly derive tostring and torepr only when needed":
  tostring(w-sing) is "shared-output"
  torepr(w-sing) is "\"shared-output\""
  tostring(w()) is "shared-output"
  torepr(w()) is "\"shared-output\""
  tostring(x-sing) is "output-x-sing"
  torepr(x-sing) is "\"output-x-sing\""
  tostring(x()) is "output-x()"
  torepr(x()) is "output-x()"
end

check "Should correctly torepr raw-arrays":
  torepr(raw-array-of(1, 2)) is "[raw-array: 1, 1]"
  torepr(raw-array-of(2, 1)) is "[raw-array: 2]"
  torepr([raw-array: [set:]]) is "[raw-array: [list-set: ]]"
  torepr([raw-array:]) is "[raw-array: ]"
end

data M:
  | m(ref x)
end

check "Should print refs as refs only when necessary":
  m1 = m(5)
  torepr(m1) is "m(5)"
  torepr(m1!x) is "5"
  torepr(m1.x) is "ref(5)"
end

check "Cyclic objects":
  rec o = {
    _output(self): VS.vs-collection("myself", [list: VS.vs-value(self)]) end
  }

  torepr(o) is "[myself: <cyclic-object-1>]"
  
  torepr({x: o, y: o})
    is "{x: [myself: <cyclic-object-1>], y: [myself: <cyclic-object-2>]}"

  rec olong = {
    _output(self):
      for each(i from range(0, 10000)):
        "do nothing"
      end
      VS.vs-collection("myself", [list: VS.vs-value(self)])
    end
  }
  torepr(olong) is "[myself: <cyclic-object-1>]"
  torepr({x: olong, y: olong})
    is "{x: [myself: <cyclic-object-1>], y: [myself: <cyclic-object-2>]}"

  o2 = { _output(self): VS.vs-value("myself") end }
  torepr({x: o2, y: o2}) is "{x: \"myself\", y: \"myself\"}"

end

check "Cyclic arrays":
  a = [raw-array: "dummy"]
  raw-array-set(a, 0, a)
  torepr(a) is "[raw-array: <cyclic-array-1>]"
end

check "Shared but not cyclic values":

  o = {}
  o2 = {x: o, y: o}
  torepr(o2) is "{x: {}, y: {}}"

  a = [raw-array:]
  o3 = {x: a, y: a}
  torepr(o3) is "{x: [raw-array: ], y: [raw-array: ]}"

end

check "run-task printing should be internal":
  torepr(run-task(lam(): 1 / 0 end)) is "right(<internal value>)"
  torepr(run-task(lam(): 1 / 1 end)) is "left(1)"
end


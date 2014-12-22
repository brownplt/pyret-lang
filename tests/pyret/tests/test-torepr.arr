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
    _tostring(self, shadow tostring): "tostring-x-sing" end,
    _torepr(self, shadow torepr): "torepr-x-sing" end,
  | x() with:
    _tostring(self, shadow tostring): "tostring-x" end,
    _torepr(self, shadow torepr): "torepr-x" end,
  | y-sing with:
    _tostring(self, shadow tostring): "tostring-y-sing" end
  | y() with:
    _tostring(self, shadow tostring): "tostring-y" end
  | z-sing with:
    _torepr(self, shadow torepr): "torepr-z-sing" end
  | z() with:
    _torepr(self, shadow torepr): "torepr-z" end    
sharing:
  _tostring(self, shadow tostring): "tostring-shared" end,
  _torepr(self, shadow torepr): "torepr-shared" end
end

check "Should correctly derive tostring and torepr only when needed":
  tostring(w-sing) is "tostring-shared"
  torepr(w-sing) is "torepr-shared"
  tostring(w()) is "tostring-shared"
  torepr(w()) is "torepr-shared"
  tostring(x-sing) is "tostring-x-sing"
  torepr(x-sing) is "torepr-x-sing"
  tostring(x()) is "tostring-x"
  torepr(x()) is "torepr-x"
  tostring(y-sing) is "tostring-y-sing"
  torepr(y-sing) is "torepr-shared"
  tostring(y()) is "tostring-y"
  torepr(y()) is "torepr-shared"
  tostring(z-sing) is "tostring-shared"
  torepr(z-sing) is "torepr-z-sing"
  tostring(z()) is "tostring-shared"
  torepr(z()) is "torepr-z"
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
    _torepr(self, shadow torepr):
      "{ myself is: " + torepr(self) + " }"
    end
  }

  torepr(o) is "{ myself is: <cyclic-object-1> }"

  torepr({x: o, y: o}) is
    "{x: { myself is: <cyclic-object-1> }, y: { myself is: <cyclic-object-2> }}"

  rec olong = {
    _torepr(self, shadow torepr):
      for each(i from range(0, 10000)):
        "do nothing"
      end
      "{ myself is: " + torepr(self) + " }"
    end
  }
  torepr(olong) is "{ myself is: <cyclic-object-1> }"
  torepr({x: olong, y: olong}) is
    "{x: { myself is: <cyclic-object-1> }, y: { myself is: <cyclic-object-2> }}"

  o2 = { _torepr(self, shadow torepr): "myself" end }
  torepr({x: o2, y: o2}) is "{x: myself, y: myself}"

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

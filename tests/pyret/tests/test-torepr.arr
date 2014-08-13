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
    tostring(self, shadow tostring): "tostring-x-sing" end,
    _torepr(self, shadow torepr): "torepr-x-sing" end,
  | x() with:
    tostring(self, shadow tostring): "tostring-x" end,
    _torepr(self, shadow torepr): "torepr-x" end,
  | y-sing with:
    tostring(self, shadow tostring): "tostring-y-sing" end
  | y() with:
    tostring(self, shadow tostring): "tostring-y" end
  | z-sing with:
    _torepr(self, shadow torepr): "torepr-z-sing" end
  | z() with:
    _torepr(self, shadow torepr): "torepr-z" end    
sharing:
  tostring(self, shadow tostring): "tostring-shared" end,
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

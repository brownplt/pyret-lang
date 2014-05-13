data Foo:
  | singleton
  | nullary()
  | unary(x)
end

check "Should produce correct toreprs":
  torepr(singleton) is "singleton"
  torepr(nullary()) is "nullary()"
  torepr(unary(5)) is "unary(5)"
  torepr(unary(nullary())) is "unary(nullary())"
  torepr(unary(singleton)) is "unary(singleton)"
end

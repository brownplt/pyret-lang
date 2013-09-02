#lang pyret/whalesong

check:
  read-sexpr("symbol") is ["symbol", "symbol"]
  read-sexpr("+") is ["symbol", "+"]
  read-sexpr("+38") is 38
  read-sexpr("-4.29") is -4.29
  read-sexpr("x3 x4") is ["symbol", "x3"]
  read-sexpr(" (++  () 987 \"str\") ")
    is [["symbol", "++"], [], 987, "str"]
  read-sexpr("((-13 +14 88.8) cats ++ \"dogs\")") is
    [[-13, 14, 88.8], ["symbol", "cats"], ["symbol", "++"], "dogs"]
end
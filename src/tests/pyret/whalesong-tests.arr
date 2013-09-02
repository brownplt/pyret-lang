#lang pyret/whalesong

check:
  read-sexpr(" (++  () 987 \"str\") ")
    is [["symbol", "++"], [], 987, "str"]
  read-sexpr("symbol") is ["symbol", "symbol"]
  read-sexpr("+") is ["symbol", "+"]
  read-sexpr("+38") is 38
  read-sexpr("-4.29") is -4.29
  read-sexpr("x3 x4") is ["symbol", "x3"]
end
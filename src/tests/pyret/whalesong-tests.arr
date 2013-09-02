#lang pyret/whalesong

check:
  read-sexpr(" (++  () 987 \"str\") ")
  is [["symbol", "++"], [], 987, "str"]
end
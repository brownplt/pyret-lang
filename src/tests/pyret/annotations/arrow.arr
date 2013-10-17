#lang pyret

f :: ( -> Number) = fun(): 10 end
f1 :: (Number -> String) = fun(n): tostring(n) end

check:
  f() is 10
  f1(10) is "10"
end
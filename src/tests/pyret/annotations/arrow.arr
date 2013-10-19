#lang pyret

f :: ( -> Number) = fun(): 10 end
f1 :: (Number -> String) = fun(n): tostring(n) end
fun<T> f2(a :: (T -> T), v): v end
fun<T,U> f3(a :: ((U -> T), U -> T), b :: (U -> T), u :: U) -> T:
  a(b,u)
end

check:
  f() is 10
  f1(10) is "10"
  f2(fun(x): x end, 10) is 10
  f3(fun(g,x): g(x) end, (_ + 1), 1) is 2
end
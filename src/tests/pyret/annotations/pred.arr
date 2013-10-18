#lang pyret

fun<T> f(a :: T(fun(o): true end)) -> Number:
  10
end

check:
  f("foo") is 10
end
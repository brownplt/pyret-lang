#lang pyret

fun<T> f(a :: {x : T}) -> T:
  a.x
end

check:
  f({x: 10}) is 10
end
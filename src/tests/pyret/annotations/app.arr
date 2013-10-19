#lang pyret

fun f1(a :: Any<Number>) -> Number:
  10
end

fun<T> f(a :: T<Number>) -> Number:
  10
end

check:
  f1("foo") is 10
  f([1]) is 10
end
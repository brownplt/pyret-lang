

fun f<A>(a :: A, b :: (A -> Any)):
  b(a)
end

fun g(a :: Any) -> Number:
  5
end

f(6, g)

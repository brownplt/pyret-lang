
a :: (Number -> Number) = (_ + 2)
a(2)

fun example<A,B>(n :: A, l :: (A -> A), f :: (A -> B)):
  f(l(n))
end

example<Number,Number>(5, (_ + 2), (_ + 2))

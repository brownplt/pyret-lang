
a :: (Number -> Number) = (_ + 2)
a(2)

fun <A,B> example(n :: A, l :: (A -> A), f :: (A -> B)):
  f(l(n))
end

example<Number,Number>(5, (_ + 2), (_ + 2))

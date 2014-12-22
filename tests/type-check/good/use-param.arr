
fun id<A>(a :: A) -> A:
  a
end

fun wants-num-to-num(f :: (Number -> Number), n :: Number):
  f(n)
end

five :: Number = wants-num-to-num(id, 5)

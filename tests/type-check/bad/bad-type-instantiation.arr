
fun add2(n :: Number):
  n + 2
end

fun f<A>(a :: A, b :: (A -> A)) -> A:
  a
end

f("hello", add2)

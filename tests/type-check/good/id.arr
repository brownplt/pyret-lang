

fun id<A>(v :: A) -> A:
  v
end 

a :: Number = id(5)
b :: String = id("hello")
c :: Boolean = id(true)
d :: (Number -> Number) = id(lam(x :: Number): 5 end)



fun <A> id(v :: A):
  v
end 

a :: Number = id(5)
b :: String = id("hello")
c :: Boolean = id(true)
d :: (Number -> Number) = id(lam(x :: Number): 5 end)

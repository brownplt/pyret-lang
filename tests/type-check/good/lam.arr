

fun a(f :: (Number -> Number)):
  f(5)
end

b :: Number = a(lam(x): x end)

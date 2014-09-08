
fun apply-num(f :: (Number -> Number), n :: Number) -> Number:
  f(n)
end

five :: Number = apply-num(lam<A>(a :: A): a;, 5)

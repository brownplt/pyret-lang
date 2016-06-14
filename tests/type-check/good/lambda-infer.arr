tru  = lam <A>(t :: A): lam <B>(f :: B): t end end
fals = lam <A>(t :: A): lam <B>(f :: B): f end end
test = lam <A>(cond :: (A -> (A -> A))): lam(consq :: A): lam(altern :: A): cond(consq)(altern) end end end

a = tru(5)
b = fals("string")(5)
tru(0 + b)(6)
fals(5)(6)
test(tru)(5)(6)

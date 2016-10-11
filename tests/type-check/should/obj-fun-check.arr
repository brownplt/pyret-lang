
obj :: ({ a :: Number, b :: Number } -> Number) = if true:
  lam(v):
    v.a
  end
else:
  lam(v):
    v.b
  end
end

c :: Number = obj({ a : 5, b : 6 })

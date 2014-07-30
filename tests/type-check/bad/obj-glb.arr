
obj = if true:
  lam(v :: { a :: Number, obj :: { } }):
    v.a
  end
else:
  lam(v :: { b :: Number, obj :: { c :: Number } }):
    v.b
  end
end

c :: Number = obj({ a : 5, b : 6 })

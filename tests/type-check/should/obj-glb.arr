
obj = if true:
  lam(v :: { a :: Number, obj :: { } }):
    v.a
  end
else:
  lam(v :: { b :: Number, obj :: { c :: Number } }):
    v.b + v.obj.c
  end
end

c :: Number = obj({ a : 5, b : 6, obj : { c : 5 } })

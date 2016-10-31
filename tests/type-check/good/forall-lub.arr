
f = if true:
  lam<A>(a :: A, b :: A) -> A:
    a
  end
else:
  lam<A,B>(a :: A, b :: B) -> B:
    b
  end
end

c :: Number = f(5, 6)

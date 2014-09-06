data D:
  | singleton
  | nullary()
  | unary(v)
  | trinary(v1, v2, v3)
end

check:

  id = lam(v): v end

  singleton._match({ singleton: lam(): 5 end }, id) is 5

  singleton._match({ failton: lam(): 5 end }, id) is singleton

  nullary()._match({ nullary: lam(): 5 end }, id) is 5

  nullary()._match({ failary: lam(): 5 end }, id) is nullary()

  unary(10)._match({ unary: id }, id) is 10

  unary(10)._match({ funary: id }, id) is unary(10)

  trinary(1, 2, 3)._match({ trinary: lam(v1, v2, v3): v1 + v2 + v3 end }, id) is 6

  trinary(1, 2, 3)._match({ trifail: lam(v1, v2, v3): v1 + v2 + v3 end }, id) is trinary(1, 2, 3)

  trinary(1, 2, 3)._match({ trinary: lam(v1, v2, v3, v4): v1 + v2 + v3 end }, id) raises "arity"

end

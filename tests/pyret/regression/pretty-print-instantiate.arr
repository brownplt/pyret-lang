import ast as A

check:
  d = A.dummy-loc
  A.s-instantiate(d, A.s-num(d, 0), [list: A.a-any(d)]).tosource().pretty(80) is [list: "0<Any>"]
end

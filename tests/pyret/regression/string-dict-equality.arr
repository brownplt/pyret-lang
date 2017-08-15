include string-dict
include equality

l1 = lam(): 5 end

d1 = [mutable-string-dict: "a", 1, "b", l1, "c", 3]
d2 = d1.clone-now()

check:
  identical(d1, d2) is false
  equal-now3(d1, d2) satisfies is-Unknown
  equal-now3(d1.get-value-now("b"), d2.get-value-now("b")) satisfies is-Unknown
  d2.set-now("c", 4)
  equal-now(d1, d2) is false
end

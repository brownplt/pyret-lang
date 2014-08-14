#lang pyret

check:
  identical(1, 1) is true
  identical(1, 2) is false
  equal-always(1, 1) is true
  equal-always(1, 2) is false
  equal-now(1, 1) is true
  equal-now(1, 2) is false
end

data Nat:
  | Z
  | S(n)
end

check:
  identical(Z, Z) is true
  identical(Z, S(Z)) is false
  identical(S(Z), S(Z)) is false
  equal-always(Z, Z) is true
  equal-always(Z, S(Z)) is false
  equal-always(S(Z), S(Z)) is true
  equal-now(Z, Z) is true
  equal-now(Z, S(Z)) is false
  equal-now(S(Z), S(Z)) is true
end

data Box:
  | box(ref v)
end

check:
  x = box(5)
  y = box(5)
  identical(x, x) is true
  identical(x, y) is false
  equal-always(x, x) is true
  equal-always(x, y) is false
  equal-now(x, x) is true
  equal-now(x, y) is true
end

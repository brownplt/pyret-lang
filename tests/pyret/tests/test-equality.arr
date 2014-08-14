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

data NLink:
  | nlink(n, ref l)
  | nempty
end

graph:
  ones = nlink(1, ones)
  other-ones = nlink(1, other-ones)
  third-ones = nlink(1, ones)
end

check:
  identical(ones, ones) is true
  identical(ones, other-ones) is false
  identical(ones, third-ones) is false
  equal-always(ones, ones) is true
  equal-always(ones, other-ones) is true
  equal-always(ones, third-ones) is true
  equal-now(ones, ones) is true
  equal-now(ones, other-ones) is true
  equal-now(ones, third-ones) is true
end

data MLink:
  | mlink(ref n, ref l)
  | mempty
end

block:
  graph:
    mt1 = mempty
    BOS = mlink(PRO, rest1)
    rest1 = mlink(WOR, mt1)
    PRO = mlink(BOS, mt1)
    WOR = mlink(BOS, mt1)
  end

  graph:
    mt2 = mempty
    CHI = mlink(CLE, mt2)
    DEN = mlink(CLE, mt2)
    CLE = mlink(CHI, rest2)
    rest2 = mlink(DEN, mt2)
  end

  check:
    identical(BOS, CLE) is false
    identical(PRO, CHI) is false
    identical(WOR, DEN) is false
    equal-always(BOS, CLE) is true
    equal-always(PRO, CHI) is true
    equal-always(WOR, DEN) is true
    equal-now(BOS, CLE) is true
    equal-now(PRO, CHI) is true
    equal-now(WOR, DEN) is true
  end
end

block:
  m-graph:
    mt1 = mempty
    BOS = mlink(PRO, rest1)
    rest1 = mlink(WOR, mt1)
    PRO = mlink(BOS, mt1)
    WOR = mlink(BOS, mt1)
  end

  m-graph:
    mt2 = mempty
    CHI = mlink(CLE, mt2)
    DEN = mlink(CLE, mt2)
    CLE = mlink(CHI, rest2)
    rest2 = mlink(DEN, mempty)
  end

  check:
    identical(BOS, CLE) is false
    identical(PRO, CHI) is false
    identical(WOR, DEN) is false
    equal-always(BOS, CLE) is false
    equal-always(PRO, CHI) is false
    equal-always(WOR, DEN) is false
    equal-now(BOS, CLE) is true
    equal-now(PRO, CHI) is true
    equal-now(WOR, DEN) is true
  end
end


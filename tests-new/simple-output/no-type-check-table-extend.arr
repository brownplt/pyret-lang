### true

# no-type-check-table-extend.arr
# `extend` syntax.

import global as G
import tables as T

my-table = table: a, b, c
  row: 2, 2, 1
  row: 4, 5, 3
  row: 8, 8, 5
end

running-mean = {
  one: lam(n): {{n; 1}; n} end,
  reduce: lam({sum; count}, n):
    { {sum + n; count + 1}; (sum + n) / (count + 1) }
  end
}

my-extended-table = extend my-table using a, b, c:
  d: a / 2,
  e: T.running-sum of b,
  f: a + b,
  g: running-mean of c
end

my-correct-extended-table = table: a, b, c, d, e, f, g
  row: 2, 2, 1, 1, 2, 4, 1
  row: 4, 5, 3, 2, 7, 9, 2
  row: 8, 8, 5, 4, 15, 16, 3
end

passes-when-true =
  T._primitiveEqual(my-correct-extended-table, my-extended-table)

G.console-log(passes-when-true)
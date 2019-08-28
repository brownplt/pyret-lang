### true

# no-type-check-table-sieve.arr
# `sieve` syntax.

import global as G
import tables as T

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

my-sieved-table = sieve my-table using b:
  (b == 5) or (b == 8)
end

my-correct-sieved-table = table: a, b, c
  row: 4, 5, 6
  row: 7, 8, 9
end

are-equal = T._primitiveEqual(my-correct-sieved-table, my-sieved-table)

are-not-equal = T._primitiveEqual(my-correct-sieved-table, my-table)

passes-when-true = are-equal and G.not(are-not-equal)

G.console-log(passes-when-true)

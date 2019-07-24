### true

# no-type-check-table-sieve.arr
# `sieve` syntax.

import global as g
import js-file("../object-equality-helper") as helper

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

are-equal = helper._objectDeepEqual(my-correct-sieved-table, my-sieved-table)

are-not-equal = helper._objectDeepEqual(my-correct-sieved-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)